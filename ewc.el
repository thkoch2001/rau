;;; ewc.el --- A Wayland client in Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Michael Bauer
;; Author: Michael Bauer <michael-bauer@posteo.de>
;; Keywords: unix
;; Version: 0.2
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
;; Global client state is represented by `ewc-objects'.
;;
;; Main entry points:
;;
;;   (ewc-connect protocols) -> ewc-objects
;;   (ewc-object-add ...)    -> ewc-object
;;   (ewc-request object request args)
;;   (setf (ewc-listener object event) listener)

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
  "A client-side object implementing a Wayland interface."
  (protocol nil :type symbol :read-only t)
  (interface nil :type symbol :read-only t)
  (id nil :type integer :read-only t)
  (events nil :type list :read-only t)
  (requests nil :type list :read-only t)
  (listeners nil :type vector :read-only t))

(cl-defstruct (ewc-objects (:constructor ewc-objects-make)
                           (:copier nil))
  "Global state of the Wayland client.

NEW-ID is the next client-allocated object id.
TABLE maps object ids to `ewc-object' structs.
PROTOCOLS is an alist as returned by `ewc-read'.
RX holds incomplete incoming Wayland bytes."
  (new-id 0 :type integer)
  (table (make-hash-table) :type hash-table :read-only t)
  (protocols nil :type list :read-only t)
  (rx "" :type string))

(define-inline ewc-object-get (id objects)
  "Get object with ID from OBJECTS, an `ewc-objects' struct."
  (inline-quote (gethash ,id (ewc-objects-table ,objects))))

;; TODO: use cl-defun
(defun ewc-object-add (&rest arguments)
  "Add a new object implementing INTERFACE of PROTOCOL to OBJECTS.
Optional ID may be provided.

Returns the newly created object.

PROTOCOL and INTERFACE are symbols.
ID is a uint32 object id; provide it for server-initiated objects.

\(fn &key OBJECTS PROTOCOL INTERFACE ID)"
  (pcase-let* (((map :objects :protocol :interface :id)
                arguments)
               (protocol-def (alist-get protocol
                                        (ewc-objects-protocols objects)))
               (interface-def (alist-get interface protocol-def)))
    (unless interface-def
      (error "ewc: unknown interface %s/%s" protocol interface))

    (pcase-let* ((`(,_version ,events ,requests) interface-def)
                 (id (or id (cl-incf (ewc-objects-new-id objects))))
                 (object (ewc-object-make
                          :protocol protocol
                          :interface interface
                          :id id
                          :events events
                          :requests requests
                          :listeners (make-vector (length events) nil))))
      (puthash id object (ewc-objects-table objects))
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

(defun ewc-set-listener (object event listener)
  "Set LISTENER for EVENT on OBJECT."
  (aset (ewc-object-listeners object)
        (ewc--event-index object event)
        listener))

;;; Wire messages

(defvar ewc-msg-head
  (bindat-type (id uint 32 t)
               (opcode uint 16 t)
               (len uint 16 t))
  "Wayland message header.")

(defun ewc-event (objects str _str-len idx)
  "Parse one Wayland event message STR in OBJECTS."
  (pcase-let* ((bindat-idx idx)
               (bindat-raw str)
               ((map id opcode _len)
                (funcall (bindat--type-ue ewc-msg-head))))
    (if-let* ((object (ewc-object-get id objects)))
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

(defun ewc-filter (objects)
  "Return a process filter for OBJECTS.
The filter accumulates partial Wayland messages and dispatches
complete ones."
  (lambda (_proc str)
    (setq str (encode-coding-string str 'binary))
    (setf (ewc-objects-rx objects)
          (concat (ewc-objects-rx objects) str))

    (let ((progress t))
      (while progress
        (let ((buf (ewc-objects-rx objects)))
          (if (< (string-bytes buf) 8)
              (setq progress nil)
            (pcase-let* ((bindat-raw buf)
                         (bindat-idx 0)
                         ((map id opcode len)
                          (funcall (bindat--type-ue ewc-msg-head))))
              (cond
               ((< len 8)
                (ewc-log "ewc: invalid message length %s; dropping buffer" len)
                (setf (ewc-objects-rx objects) "")
                (setq progress nil))

               ((< (string-bytes buf) len)
                ;; Incomplete message; wait for more bytes.
                (setq progress nil))

               (t
                (let ((msg (substring buf 0 len)))
                  (setf (ewc-objects-rx objects)
                        (substring buf len))
                  (condition-case err
                      (ewc-event objects msg len 0)
                    (error (message "ewc: dispatch error: %S" err)))))))))))))

(defun ewc-connect (objects &optional socket)
  "Connect to Wayland SOCKET using OBJECTS for event dispatch.
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
     :filter (ewc-filter objects)
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
