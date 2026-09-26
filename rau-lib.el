;; -*- lexical-binding: t; -*-

(require 'lgr)

(defvar rau--propagate-errors nil
  "See rau--condition-case.")

(defmacro rau--condition-case (location &rest body)
  "Wrap BODY in a condition-case unless `rau--propagate-errors' is non-nil.
LOCATION identifies where the error occurred."
  `(if rau--propagate-errors
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error (message "Error at %s: %S" ,location err)))))

(defvar rau--lgr nil)

(provide 'rau-lib)
