;;; ewc.el --- Wayland client in Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Michael Bauer, 2026 Thomas Koch
;; Author: Michael Bauer <michael-bauer@posteo.de>
;; Maintainer: Thomas Koch <thomas@koch.ro>
;; URL: http://perma-curious.eu/repo-ewx/
;; Keywords: unix
;; Version: 0.3
;; Package-Requires: ((emacs "28.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a Wayland client implementation in Elisp.
;;
;; Protocol XML is translated into pack/unpack functions using bindat.
;; Objects are represented by `ewc-object' structs.
;; Global client state is represented by `ewc-client'.
;;
;; Main entry points:
;;
;; - Event listener functions with common PREFIX, e.g. "YOURAPP-on-"
;; - Protocol XML parsing via ewc-read
;; - ewc-start to connect and send initial get-registry request
;; - ewc-object-add to create new wayland objects
;; - ewc-request

;;; Code:

(require 'cl-lib)
(require 'map)          ; needed for `map' pcase pattern
(require 'seq)
(require 'pcase)
(require 'subr-x)
(require 'bindat)
(require 'dom)

(defvar ewc-debug nil
  "When non-nil, enable verbose ewc debugging messages.")

(defun ewc--log (&rest args)
  "Log ARGS with `message' when `ewc-debug' is non-nil."
  (when ewc-debug
    (apply #'message args)))

;;; Read Wayland XML protocols

;; bindat internal state variables.
(defvar bindat-raw)
(defvar bindat-idx)

(defmacro ewc-read (&rest protocols)
  "Read Wayland PROTOCOLS from XML files into an interfaces hash table.
Each PROTOCOL is either a path to a Wayland XML protocol
or a list (path interface ...) restricting the interfaces
read to those specified.

Path should be a string and interface a symbol.

This is the Elisp version of wayland-scanner."
  (let ((all-interfaces
         (mapcan (lambda (protocol)
                   (apply #'ewc-read-protocol (ensure-list protocol)))
                 protocols)))
    `(progn
       (defvar bindat-idx)
       (let ((table (make-hash-table :test 'eq)))
         ,@(mapcar (lambda (iface-form)
                     `(puthash ,(cadr iface-form) (list ,@(cddr iface-form)) table))
                   all-interfaces)
         table))))

(define-inline ewc--node-name (node)
  "Return the Elisp symbol name for DOM NODE."
  (inline-quote
   (intern (string-replace "_" "-" (dom-attr ,node 'name)))))

(defun ewc-read-protocol (protocol &rest select-interfaces)
  "Read one Wayland protocol XML file PROTOCOL.
Return a list of interface forms as produced by `ewc--read-interface'.
If SELECT-INTERFACES is non-nil, only read those interfaces."
  (let ((protocol (with-temp-buffer
                    (insert-file-contents protocol)
                    (libxml-parse-xml-region (point-min) (point-max)))))
    (mapcar #'ewc--read-interface
            (let ((interfaces (dom-by-tag protocol 'interface)))
              (if select-interfaces
                  (seq-filter
                   (lambda (interface)
                     (member (ewc--node-name interface)
                             select-interfaces))
                   interfaces)
                interfaces)))))

(defun ewc--read-interface (interface)
  "Translate one Wayland INTERFACE dom node into Elisp."
  `(list
    ',(ewc--node-name interface)
    ,(string-to-number (dom-attr interface 'version))
    (list ,@(mapcar #'ewc--read-event (dom-by-tag interface 'event)))
    (list ,@(seq-map-indexed #'ewc--read-request
                             (dom-by-tag interface 'request)))))

(defun ewc--read-event (event)
  "Translate one Wayland EVENT dom node into Elisp."
  `(cons ',(ewc--node-name event)
         ,(bindat--toplevel 'unpack
                            (mapcan #'ewc--read-arg
                                    (dom-by-tag event 'arg)))))

(defun ewc--read-request (request opcode)
  "Translate one Wayland REQUEST dom node into Elisp.
OPCODE is the request opcode."
  (let ((spec (mapcan #'ewc--read-arg (dom-by-tag request 'arg))))
    `(cons ',(ewc--node-name request)
           (cons ,opcode
                 (cons ,(bindat--toplevel 'length spec)
                       ,(bindat--toplevel 'pack spec))))))

;; TODO: Wayland uses cpu endianess. Detect it or make it configurable,
;; see `byteorder'.
(defun ewc--read-arg (node)
  "Translate one Wayland argument dom NODE into a bindat spec fragment."
  (let ((name (ewc--node-name node)))
    (pcase (dom-attr node 'type)
      ((and "new_id" (guard (not (dom-attr node 'interface))))
       `((interface-len uint 32 t)
         (interface strz)
         (_ align 4)
         (version uint 32 t)
         (,name uint 32 t)))

      ((or "uint" "object" "new_id")
       `((,name uint 32 t)))

      ("int"
       `((,name sint 32 t)))

      ("string"
       (let ((len-field (intern (format "%s-len" name))))
        `((,len-field uint 32 t)
          (,name strz ,len-field)
          ;; Hack: If not constructed with list this leads to circular lists due to nconc.
          ;; Have a look at the expansion of this backquote and (elisp) Repeated Expansion.
          ,(list '_ 'align 4))))

      ((or "fixed" "array" "fd")
       `((,name not-implemented))))))

;;; Objects

(cl-defstruct (ewc-object (:constructor ewc-object-make)
                          (:copier nil))
  "A client-side object implementing a Wayland interface.

The DATA slot is free to use for arbitrary data about this object."
  (interface nil :type symbol :read-only t)
  (id nil :type integer :read-only t)
  (events nil :type list :read-only t)
  (requests nil :type list :read-only t)
  (listeners nil :type vector :read-only t)
  (request-cache nil :type vector :read-only t)
  (data nil)
  (tags nil :type list))

(defmacro ewc-define-data-accessors (struct-type &optional prefix)
  "Define accessor functions for slots of STRUCT-TYPE stored in ewc-object data.
STRUCT-TYPE is a cl-defstruct name.  PREFIX is an optional symbol used as
the function name prefix; if nil, defaults to STRUCT-TYPE-wl.
For each slot, defines PREFIX-SLOT that takes an ewc-object and returns
the value of that slot from the struct in its data slot.
Also defines setf-able places."
  (declare (indent 1))
  (let* ((prefix (or prefix (intern (format "%s-wl" struct-type))))
         (prefix-str (symbol-name prefix))
         (struct-str (symbol-name struct-type))
         (slot-names (delq nil
                           (mapcar (lambda (entry)
                                     (unless (eq (car entry) 'cl-tag-slot)
                                       (car entry)))
                                   (cl-struct-slot-info struct-type))))
         (forms nil))
    (dolist (slot slot-names)
      (let ((ewc-object-accessor (intern (format "%s-%s" prefix-str slot)))
            (data-struct-accessor (intern (format "%s-%s" struct-str slot))))
        (push `(defsubst ,ewc-object-accessor (ewc-object)
                 ,(format "Access `%s' of %s stored in EWC-OBJECT's data slot.
This function was defined by ewc-define-data-accessors macro."
                          slot struct-type)
                 (when-let* ((data (ewc-object-data ewc-object)))
                   (,data-struct-accessor data)))
              forms)
        (push `(gv-define-setter ,ewc-object-accessor (val ewc-object)
                 `(let ((data (ewc-object-data ,ewc-object)))
                    (setf (,',data-struct-accessor data) ,val)))
              forms)))
    `(progn ,@(nreverse forms))))

(cl-defstruct (ewc-client (:constructor ewc-client-make)
                           (:copier nil))
  "Global state of the Wayland client.

NEW-ID is the next client-allocated object id.
TABLE maps object ids to `ewc-object' structs.
TAGS maps tag symbols to lists of ewc-object structs.
INTERFACES is a hash table of Wayland interfaces as returned by `ewc-read'.
RX holds incomplete incoming Wayland bytes."
  (new-id 0 :type integer)
  (connection)
  (table (make-hash-table) :type hash-table :read-only t)
  (tags (make-hash-table :test 'eq) :type hash-table :read-only t)
  (interfaces nil :type hash-table :read-only t)
  (listeners nil :type list)
  (rx "" :type string))

(defun ewc-interface-version (client interface)
  "Return the XML-declared version of INTERFACE in CLIENT."
  (when-let* ((def (gethash interface (ewc-client-interfaces client))))
    (car def)))

(define-inline ewc-object-get (client id)
  "Get object with ID from CLIENT, an `ewc-client' struct."
  (inline-quote (gethash ,id (ewc-client-table ,client))))

(defun ewc-object-tag (client object tag)
  "Attach TAG to OBJECT and register OBJECT under TAG in CLIENT.
TAG must be a symbol.  Tagging an object that already carries TAG
is a no-op.  Note that `ewc-object-add' automatically tags every
new object with its interface."
  (cl-check-type tag symbol)
  (unless (memq tag (ewc-object-tags object))
    (push tag (ewc-object-tags object))
    (let ((index (ewc-client-tags client)))
      (puthash tag (cons object (gethash tag index)) index))))

(defun ewc-object-untag (client object tag)
  "Detach TAG from OBJECT and unregister OBJECT under TAG in CLIENT.
Untagging an object that does not carry TAG is a no-op."
  (cl-check-type tag symbol)
  (setf (ewc-object-tags object)
        (delq tag (ewc-object-tags object)))
  (let ((index (ewc-client-tags client)))
    (puthash tag (delq object (gethash tag index)) index)))

(define-inline ewc-object-tagged-p (object tag)
  "Return non-nil if OBJECT carries TAG."
  (inline-quote (memq ,tag (ewc-object-tags ,object))))

(defun ewc-first-object (client tag)
  "Fetch the first ewc-object in CLIENT for TAG.
This is meant to be used
for TAGs that should only be attached to one object, e.g. global interfaces."
  (car (ewc-objects client tag)))

(defun ewc-objects (client tag)
  "Return the list of objects in CLIENT carrying TAG.
The result is a copy, so callers may mutate it and may remove
objects from CLIENT while iterating over it."
  (copy-sequence (gethash tag (ewc-client-tags client))))

(defun ewc-object-remove-id (client id)
  "Shorthand to get object with ID and call ewc-object-remove."
  (if-let* ((object (ewc-object-get client id)))
      (ewc-object-remove client object)
    (message "ewc: no object to remove with id %d." id)))

(defun ewc-object-remove (client object)
  "Remove OBJECT from CLIENT's table and from all of its tag lists.
Idempotent: removing an object that was never added (or was already
removed) is a no-op."
  (let ((index (ewc-client-tags client))
        (id (ewc-object-id object)))
    (ewc--log "removing object: id=%d, interface=%s"
             id
             (ewc-object-interface object))
    (dolist (tag (ewc-object-tags object))
      (puthash tag (delq object (gethash tag index)) index))
    (setf (ewc-object-tags object) nil)
    (remhash id (ewc-client-table client))))

(defun ewc-object-add (client interface &optional id)
  "Add a new object implementing INTERFACE to CLIENT.
If no ID is provided, a client initiated id is generated.

Returns the newly created object."
  (let* ((interface-def (gethash interface (ewc-client-interfaces client))))
    (unless interface-def
      (error "ewc: Unknown interface %s" interface))

    (pcase-let* ((`(,_version ,events ,requests) interface-def)
                 (id (or id (cl-incf (ewc-client-new-id client))))
                 (object (ewc-object-make
                          :interface interface
                          :id id
                          :events events
                          :requests requests
                          :request-cache (make-vector (length requests) nil)
                          :listeners (cdr (assq interface (ewc-client-listeners client))))))
      (puthash id object (ewc-client-table client))
      (ewc-object-tag client object interface)
      (ewc--log "ewc: added object id=%s interface=%s" id interface)
      object)))

(defun ewc--event-index (object event)
  "Return the opcode index of EVENT for OBJECT."
  (cl-loop for spec in (ewc-object-events object)
           for i from 0
           when (eq (car spec) event)
           return i
           finally (error "ewc: Unknown event %s for interface %s"
                          event
                          (ewc-object-interface object))))

;;; Wire messages

(defun ewc-to-utf8 (s)
  "Decode a Wayland string S as UTF-8.
Return nil if S is nil or empty.  Emacs stores whether a string is
unibyte or multibyte, see struct Lisp_String in src/lisp.h."
  (when (and s (not (string-empty-p s)))
    (decode-coding-string s 'utf-8 t)))

(defvar ewc--msg-head
  (bindat-type (id uint 32 t)
               (opcode uint 16 t)
               (len uint 16 t))
  "Wayland message header.")

(defun ewc--event (client str _str-len idx)
  "Parse one Wayland event message STR in CLIENT."
  (pcase-let* ((bindat-idx idx)
               (bindat-raw str)
               ((map id opcode _len)
                (funcall (bindat--type-ue ewc--msg-head))))
    (if-let* ((object (ewc-object-get client id)))
        (let ((listeners (ewc-object-listeners object))
              (spec (nth opcode (ewc-object-events object))))
          (ewc--log "ewc: event if=%s opcode=%s (%s)"
                   (ewc-object-interface object)
                   opcode
                   (car spec))
          (if (and (< opcode (length listeners))
                   (aref listeners opcode))
              (let* ((listener (aref listeners opcode))
                     (ue (cdr spec))
                     (args (when ue (funcall ue))))
                (when args (ewc--log "ewc: args %S" args))
                (condition-case err
                    (funcall listener object args)
                  (error (message "ewc: listener error for %s opcode %s: %S"
                            (ewc-object-interface object)
                            opcode
                            err))))
            (ewc--log "ewc: no listener for %s opcode %s"
                     (ewc-object-interface object)
                     opcode)))
      (ewc--log "ewc: event for unknown object id %s" id))))

(defun ewc--pack (object-id request-def arguments)
  "Return Wayland wire message for OBJECT-ID with ARGUMENTS."
  (pcase-let* ((`(,_ ,opcode ,le . ,pe) request-def)
                (bindat-idx 0)
                (len (+ 8 (if le (funcall le arguments) 0)))
                (bindat-idx 0)
                (bindat-raw (make-string len 0)))
      (funcall (bindat--type-pe ewc--msg-head)
               `((id . ,object-id)
                 (opcode . ,opcode)
                 (len . ,len)))
      (when pe
        (funcall pe arguments))
      bindat-raw))

;;; Connection and process filter

(defun ewc--filter (client)
  "Return a process filter for CLIENT.
The filter accumulates partial Wayland messages and dispatches
complete ones."
  (lambda (_proc str)
    (setq str (encode-coding-string str 'binary))
    (setf (ewc-client-rx client)
          (concat (ewc-client-rx client) str))

    (let ((progress t))
      (while progress
        (let ((buf (ewc-client-rx client)))
          (if (< (string-bytes buf) 8)
              (setq progress nil)
            (pcase-let* ((bindat-raw buf)
                         (bindat-idx 0)
                         ((map _id _opcode len)
                          (funcall (bindat--type-ue ewc--msg-head))))
              (cond
               ((< len 8)
                (ewc--log "ewc: invalid message length %s; dropping buffer" len)
                (setf (ewc-client-rx client) "")
                (setq progress nil))

               ((< (string-bytes buf) len)
                ;; Incomplete message; wait for more bytes.
                (setq progress nil))

               (t
                (let ((msg (substring buf 0 len)))
                  (setf (ewc-client-rx client)
                        (substring buf len))
                  (condition-case err
                      (ewc--event client msg len 0)
                    (error (message "ewc: dispatch error: %S" err)))))))))))))

(defun ewc-build-listeners (client prefix)
  "Populate the `listeners' slot of CLIENT by scanning the obarray.

PREFIX is a string such as \"reka-on-\".  Every function whose
name starts with PREFIX is expected to follow the naming scheme

    PREFIX-INTERFACE-EVENT

for example

    reka-on-river-window-v1-title

The name is decomposed by matching INTERFACE against the
interfaces declared in CLIENT's interfaces.  EVENT must be a known
event of that interface; the function symbol is then stored at
the event's opcode index in a per-interface vector.

The resulting alist maps each interface symbol to a vector of
listener symbols (or nil for unhandled events).  Interfaces with
no registered listeners get an all-nil vector.

Signals an error listing every function matching PREFIX that does
not correspond to a known (interface event) pair.  This catches
typos at startup rather than silently dropping events."
  (let* ((interfaces (ewc-client-interfaces client))
         (iface-events (cl-loop for iface being the hash-keys of interfaces
                                using (hash-values v)
                                collect (cons iface (mapcar #'car (cl-second v)))))
         (table (mapcar (lambda (entry)
                          (cons (car entry)
                                (make-vector (length (cdr entry)) nil)))
                        iface-events))
         (unmatched nil))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (string-prefix-p prefix (symbol-name sym)))
         (let ((rest (substring (symbol-name sym) (length prefix)))
               (found nil))
           ;; TODO: Use cl-loop with "until found"
           (dolist (entry iface-events)
             (when-let* (((not found))
                         (iface      (car entry))
                         (events     (cdr entry))
                         (iface-prefix  (concat (symbol-name iface) "-"))
                         ((string-prefix-p iface-prefix rest))
                         (event-str (substring rest (length iface-prefix)))
                         (event-sym (intern event-str))
                         (opcode    (cl-position event-sym events))
                         (vec (cdr (assq iface table))))
               (aset vec opcode sym)
               (setq found t)))
           (unless found
             (push sym unmatched))))))
    (when unmatched
      (error "ewc: Listener(s) match no known interface/event: %s"
             (mapconcat #'symbol-name unmatched ", ")))
    (setf (ewc-client-listeners client) table)
    table))

(defun ewc-connect (client &optional socket)
  "Connect to Wayland SOCKET using CLIENT for event dispatch.
SOCKET defaults to the value of WAYLAND_DISPLAY.

The network process is set in the CONNECTION slot of CLIENT."
  (when-let* ((old (get-process "emacs-wayland-client")))
    (delete-process old))

  (let* ((display (or socket
                      (getenv "WAYLAND_DISPLAY")
                      "wayland-1"))
         (remote (if (file-name-absolute-p display)
                     display
                   (file-name-concat
                    (or (getenv "XDG_RUNTIME_DIR")
                        (error "XDG_RUNTIME_DIR is not set"))
                    display)))
         (connection (make-network-process
                      :name "emacs-wayland-client"
                      :remote remote
                      :service nil ;silence warning: "called without required keyword argument :service"
                      :coding 'binary
                      :noquery t
                      :filter (ewc--filter client)
                      :sentinel (lambda (_proc msg)
                                  (message "ewc: connection sentinel: %s" msg)))))
    (setf (ewc-client-connection client) connection)))

(defun ewc-request (client object request &optional arguments nocache)
  "Issue REQUEST with ARGUMENTS on OBJECT using CLIENT."
  (let* ((connection (ewc-client-connection client))
         (request-def (assq request (ewc-object-requests object)))
         (cache (ewc-object-request-cache object))
         (id (ewc-object-id object)))
    (unless (and connection (process-live-p connection))
      (error "ewc: No live Wayland connection for request %S" request))
    (unless request-def
      (error "ewc: Interface %s has no request %S"
             (ewc-object-interface object) request))
    (let ((opcode (cl-second request-def)))
      (when (or nocache
                (null arguments)
                (not (equal arguments (aref cache opcode))))
        (ewc--log "ewc: rq %s::%s(%s)"
                 (ewc-object-interface object)
                 request
                 (mapconcat (lambda (arg)
                              (format "%s=%S" (car arg) (cdr arg)))
                            arguments
                            " "))
        (process-send-string connection
                             (ewc--pack id request-def arguments))
        (when arguments
          (aset cache opcode arguments))))))

(defun ewc-start (interfaces listener-prefix)
  "Setup ewc-client, send get-registry request and return the client.
INTERFACES are the interfaces as read by ewc-read. LISTENER-PREFIX is
the prefix string of event listener functions to be registered with the
client."
  (let ((client (ewc-client-make :interfaces interfaces)))
    (ewc-build-listeners client listener-prefix)
    (ewc-connect client)
    (let ((display-wl (ewc-object-add client 'wl-display))
          (registry-wl (ewc-object-add client 'wl-registry)))

      (ewc-request client display-wl 'get-registry
                   `((registry . ,(ewc-object-id registry-wl)))))
    client))

(provide 'ewc)

;;; ewc.el ends here
