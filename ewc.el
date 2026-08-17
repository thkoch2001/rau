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
;;   (ewc-connect client) -> connection
;;   (ewc-object-add ...)    -> ewc-object
;;   (ewc-request connection object request args)

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

(defun ewc-log (&rest args)
  "Log ARGS with `message' when `ewc-debug' is non-nil."
  (when ewc-debug
    (apply #'message args)))

;;; Read Wayland XML protocols

;; bindat internal state variables.
(defvar bindat-raw)
(defvar bindat-idx)

(defmacro ewc-read (&rest protocols)
  "Read Wayland PROTOCOLS from XML files into Elisp.
Each PROTOCOL is either a path to a Wayland XML protocol
or a list (path interface ...) restricting the interfaces
read to those specified.

Path should be a string and interface a symbol.

This is the Elisp version of wayland-scanner."
  `(progn
     (defvar bindat-idx)
     (list ,@(mapcar (lambda (protocol)
                       (apply #'ewc-read-protocol (ensure-list protocol)))
                     protocols))))

(define-inline ewc-node-name (node)
  "Return the Elisp symbol name for DOM NODE."
  (inline-quote
   (intern (string-replace "_" "-" (dom-attr ,node 'name)))))

;; TODO: Add a table of all interfaces and eliminate this function
(defun ewc-find-protocol (protocols interface)
  "Find the protocol defining INTERFACE in PROTOCOLS.
PROTOCOLS is a list of protocols as returned by `ewc-read-protocol'."
  (cl-loop for protocol-def in protocols
           thereis (when (cl-find-if (lambda (iface-def)
                                       (eq (car iface-def) interface))
                                     (cdr protocol-def))
                     (car protocol-def))))

(defun ewc-read-protocol (protocol &rest select-interfaces)
  "Read one Wayland protocol XML file PROTOCOL.
If SELECT-INTERFACES is non-nil, only read those interfaces."
  (let ((protocol (with-temp-buffer
                    (insert-file-contents protocol)
                    (libxml-parse-xml-region (point-min) (point-max)))))
    `(cons ',(ewc-node-name protocol)
           (list ,@(mapcar #'ewc-read-interface
                           (let ((interfaces (dom-by-tag protocol 'interface)))
                             (if select-interfaces
                                 (seq-filter
                                  (lambda (interface)
                                    (member (ewc-node-name interface)
                                            select-interfaces))
                                  interfaces)
                               interfaces)))))))

(defun ewc-read-interface (interface)
  "Translate one Wayland INTERFACE dom node into Elisp."
  `(list
    ',(ewc-node-name interface)
    ,(string-to-number (dom-attr interface 'version))
    (list ,@(mapcar #'ewc-read-event (dom-by-tag interface 'event)))
    (list ,@(seq-map-indexed #'ewc-read-request
                             (dom-by-tag interface 'request)))))

(defun ewc-read-event (event)
  "Translate one Wayland EVENT dom node into Elisp."
  `(cons ',(ewc-node-name event)
         ,(bindat--toplevel 'unpack
                            (mapcan #'ewc-read-arg
                                    (dom-by-tag event 'arg)))))

(defun ewc-read-request (request opcode)
  "Translate one Wayland REQUEST dom node into Elisp.
OPCODE is the request opcode."
  (let ((spec (mapcan #'ewc-read-arg (dom-by-tag request 'arg))))
    `(cons ',(ewc-node-name request)
           (cons ,opcode
                 (cons ,(bindat--toplevel 'length spec)
                       ,(bindat--toplevel 'pack spec))))))

;; TODO: Wayland uses cpu endianess. Detect it or make it configurable.
(defun ewc-read-arg (node)
  "Translate one Wayland argument dom NODE into a bindat spec fragment."
  (let ((name (ewc-node-name node)))
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
       `((,(intern (format "%s-len" name)) uint 32 t)
         (,name strz)
         ;; Hack: If not constructed with list this leads to circular lists due to nconc.
         ;; Have a look at the expansion of this backquote and (elisp) Repeated Expansion.
         ,(list '_ 'align 4)))

      ((or "fixed" "array" "fd")
       `((,name not-implemented))))))

;;; Objects

(cl-defstruct (ewc-object (:constructor ewc-object-make)
                          (:copier nil))
  "A client-side object implementing a Wayland interface.

The DATA slot is free to use for arbitrary data about this object."
  (protocol nil :type symbol :read-only t)
  (interface nil :type symbol :read-only t)
  (id nil :type integer :read-only t)
  (events nil :type list :read-only t)
  (requests nil :type list :read-only t)
  (listeners nil :type vector :read-only t)
  (data nil)
  (tags nil :type list))

(cl-defstruct (ewc-client (:constructor ewc-client-make)
                           (:copier nil))
  "Global state of the Wayland client.

NEW-ID is the next client-allocated object id.
TABLE maps object ids to `ewc-object' structs.
TAGS maps tag symbols to lists of ewc-object structs.
PROTOCOLS is an alist as returned by `ewc-read'.
RX holds incomplete incoming Wayland bytes."
  (new-id 0 :type integer)
  (table (make-hash-table) :type hash-table :read-only t)
  (tags (make-hash-table :test 'eq) :type hash-table :read-only t)
  (protocols nil :type list :read-only t)
  (listeners nil :type list)
  (rx "" :type string))

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
  "Fetch the first ewc-object in CLIENT for TAG. This is meant to be used
for TAGs that should only be attached to one object, e.g. global
interfaces."
  (car (ewc-objects client tag)))

(defun ewc-objects (client tag)
  "Return the list of objects in CLIENT carrying TAG.
The result is a copy, so callers may mutate it and may remove
objects from CLIENT while iterating over it."
  (copy-sequence (gethash tag (ewc-client-tags client))))

(defun ewc-object-remove (client object)
  "Remove OBJECT from CLIENT's table and from all of its tag lists.
Idempotent: removing an object that was never added (or was already
removed) is a no-op."
  (let ((index (ewc-client-tags client)))
    (dolist (tag (ewc-object-tags object))
      (puthash tag (delq object (gethash tag index)) index)))
  (setf (ewc-object-tags object) nil)
  (remhash (ewc-object-id object) (ewc-client-table client)))

(defun ewc-object-add (client interface &optional id)
  "Add a new object implementing INTERFACE to CLIENT.
If no ID is provided, a client initiated id is generated.

Returns the newly created object."
  (let* ((protocols (ewc-client-protocols client))
         (protocol (ewc-find-protocol protocols interface))
         (protocol-def (alist-get protocol protocols))
         (interface-def (alist-get interface protocol-def)))
    (unless interface-def
      (error "ewc: unknown interface %s/%s" protocol interface))

    (pcase-let* ((`(,_version ,events ,requests) interface-def)
                 (id (or id (cl-incf (ewc-client-new-id client))))
                 (object (ewc-object-make
                          :protocol protocol
                          :interface interface
                          :id id
                          :events events
                          :requests requests
                          :listeners (cdr (assq interface (ewc-client-listeners client))))))
      (puthash id object (ewc-client-table client))
      (ewc-object-tag client object interface)
      (ewc-log "ewc: added object id=%s interface=%s" id interface)
      object)))

(defun ewc--event-index (object event)
  "Return the opcode index of EVENT for OBJECT."
  (cl-loop for spec in (ewc-object-events object)
           for i from 0
           when (eq (car spec) event)
           return i
           finally (error "ewc: unknown event %s for interface %s"
                          event
                          (ewc-object-interface object))))

(defun ewc-listener (object event)
  "Return listener for EVENT on OBJECT."
  (aref (ewc-object-listeners object)
        (ewc--event-index object event)))

;;; Wire messages

(defvar ewc-msg-head
  (bindat-type (id uint 32 t)
               (opcode uint 16 t)
               (len uint 16 t))
  "Wayland message header.")

(defun ewc-event (client str _str-len idx)
  "Parse one Wayland event message STR in CLIENT."
  (pcase-let* ((bindat-idx idx)
               (bindat-raw str)
               ((map id opcode _len)
                (funcall (bindat--type-ue ewc-msg-head))))
    (if-let* ((object (ewc-object-get client id)))
        (let ((listeners (ewc-object-listeners object)))
          (if (and (< opcode (length listeners))
                   (aref listeners opcode))
              (let* ((listener (aref listeners opcode))
                     (spec (nth opcode (ewc-object-events object)))
                     (ue (cdr spec))
                     (args (when ue (funcall ue))))
                (ewc-log "ewc: event if=%s opcode=%s (%s)"
                         (ewc-object-interface object)
                         opcode
                         (car spec))
                (condition-case err
                    (funcall listener object args)
                  (error (message "ewc: listener error for %s opcode %s: %S"
                            (ewc-object-interface object)
                            opcode
                            err))))
            (ewc-log "ewc: no listener for %s opcode %s"
                     (ewc-object-interface object)
                     opcode)))
      (ewc-log "ewc: event for unknown object id %s" id))))

(defun ewc-pack (object request arguments)
  "Return Wayland REQUEST wire message for OBJECT with ARGUMENTS."
  (ewc-log "ewc: pack %s::%s::%s nr args=%d"
           (ewc-object-protocol object)
           (ewc-object-interface object)
           request
           (length arguments))

  (let ((entry (assq request (ewc-object-requests object))))
    (unless entry
      (error "ewc: interface %s has no request %s"
             (ewc-object-interface object) request))
    (pcase-let* ((`(,_ ,opcode ,le . ,pe) entry)
                (bindat-idx 0)
                (len (+ 8 (if le (funcall le arguments) 0)))
                (bindat-idx 0)
                (bindat-raw (make-string len 0)))
      (funcall (bindat--type-pe ewc-msg-head)
               `((id . ,(ewc-object-id object))
                 (opcode . ,opcode)
                 (len . ,len)))
      (when pe
        (funcall pe arguments))
      bindat-raw)))

;;; Connection and process filter

(defun ewc-filter (client)
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
                          (funcall (bindat--type-ue ewc-msg-head))))
              (cond
               ((< len 8)
                (ewc-log "ewc: invalid message length %s; dropping buffer" len)
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
                      (ewc-event client msg len 0)
                    (error (message "ewc: dispatch error: %S" err)))))))))))))

(defun ewc--interfaces-events (client)
  "Return an alist of (INTERFACE . EVENT-NAMES) from CLIENT.
INTERFACE is a symbol.  EVENT-NAMES is a list of event symbols
in opcode order, as declared in the protocol XML."
  (cl-loop for (_protocol . interfaces) in (ewc-client-protocols client)
           append (cl-loop for (iface _version events _requests) in interfaces
                           collect (cons iface (mapcar #'car events)))))

(defun ewc-build-listeners (client prefix)
  "Populate the `listeners' slot of CLIENT by scanning the obarray.

PREFIX is a string such as \"reka-on-\".  Every function whose
name starts with PREFIX is expected to follow the naming scheme

    PREFIX-INTERFACE-EVENT

for example

    reka-on-river-window-v1-title

The name is decomposed by matching INTERFACE against the
interfaces declared in CLIENT's protocols.  EVENT must be a known
event of that interface; the function symbol is then stored at
the event's opcode index in a per-interface vector.

The resulting alist maps each interface symbol to a vector of
listener symbols (or nil for unhandled events).  Interfaces with
no registered listeners get an all-nil vector.

Signals an error listing every function matching PREFIX that does
not correspond to a known (interface event) pair.  This catches
typos at startup rather than silently dropping events."
  (let* ((iface-events (ewc--interfaces-events client))
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
      (error "ewc: listener(s) match no known interface/event: %s"
             (mapconcat #'symbol-name unmatched ", ")))
    (setf (ewc-client-listeners client) table)
    table))

(defun ewc-connect (client &optional socket)
  "Connect to Wayland SOCKET using CLIENT for event dispatch.
SOCKET defaults to the value of WAYLAND_DISPLAY.

Returns a network client process."
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
                    display))))

    (make-network-process
     :name "emacs-wayland-client"
     :remote remote
     :service nil ;silence warning: "called without required keyword argument :service"
     :coding 'binary
     :noquery t
     :filter (ewc-filter client)
     :sentinel (lambda (_proc msg)
                 (message "ewc: connection sentinel: %s" msg)))))

(defun ewc-request (connection object request &optional arguments)
  "Issue REQUEST with ARGUMENTS on OBJECT using CONNECTION."
  (unless (and connection (process-live-p connection))
    (error "ewc: no live Wayland connection for request %S" request))

  (process-send-string connection
                       (ewc-pack object request arguments)))

(provide 'ewc)

;;; ewc.el ends here
