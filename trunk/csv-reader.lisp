;; Copyright 2012 Kevin Mas Ruiz <kmr@conscell.net>
;; ------------------------------------------------
; This file is part of cl-sgdos-csv.
;
;   cl-sgdos-csv is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   any later version.
;
;   cl-sgdos-csv is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   long with cl-sgdos-csv.  If not, see <http://www.gnu.org/licenses/>.
; ****************************************************************************

(in-package :cl-sgdos-csv)

(defun %to-input-stream (value)
  (etypecase value
    (string (make-string-input-stream value))
    (pathname (open value))
    (stream value)))

(defparameter [default-input-stream] *standard-input*)

(defmacro for-each-char ((variable &key in) &body body)
  `(loop
      do
	(progn
	  (setq ,variable (read-char ,in nil nil))
	  ,@body)))

(defun bind-row (row bindings &aux result)
  (if bindings
      (progn
	(loop 
	   for index from 0 to (1- (list-length row))
	   for i = (nth index row)
	   for k = (nth index bindings)
	   do (progn
		(setf (getf result i) k)))
	(reverse result))
      row))

(defun whitespace-p (c)
  (member c (list #\newline #\return #\linefeed #\tab #\space)))

(defun newline-p (
		  &optional &key
		  (stream [default-input-stream])
		  (template [default-template])
		  &aux
		  (char #\nul)
		  (index 0)
		  (word (make-array (length (linebreak template))
			 :element-type 'character
			 :adjustable t
			 :fill-pointer 0)))
  (for-each-char (char :in stream)
    (unless char
      (return t))
    (incf index)
    (vector-push char word)
    (unless (char= char (aref (linebreak template) index))
      (loop for i from 1 to index
	 do (unread-char (aref word (- index i)) stream))
      (return nil))
    (when (= (1+ index) (length (linebreak template))) 
      (loop for i from 1 to index
	 do (unread-char (aref word (- index i)) stream))
      (return t))))

(defun read-unquoted-value (result
			  &optional &key
			  (stream [default-input-stream])
			  (template [default-template])
			  &aux
			  (char #\nul))
  (for-each-char (char :in stream)
     (unless char
       (return result))
     (when (char= char (separator-char template))
       (unread-char (separator-char template) stream)
       (return result))
     (when (char= char #\newline))
     (when (char= char (aref (linebreak template) 0))
       (when (newline-p :stream stream :template template)
	 (return result)))
     (vector-push char result)))

(defun read-quoted-value (result
			  &optional &key
			  (stream [default-input-stream])
			  (template [default-template])
			  &aux
			  (char #\nul))
  (for-each-char (char :in stream)
     (unless char
       (return result))
     (cond
       ((char= char (escape-char template))
	;; that happens on the RFC4180
	(when (char= (escape-char template) (encloser-char template))
	  ;; if the next character is also the same, escape, otherwise exit
	  (let ((k (peek-char nil stream)))
	    (if (char= k char)
		(progn
		  (vector-push char result)
		  ;; ignore the next character
		  (read-char stream))
		;; exit if we must not 
		(return result)))))
       ;; do nothing otherwise because it does not have any sense
       ((char= char (encloser-char template))
	(return result))
       (t (vector-push char result)))))

;; PUBLIC

(defun read-csv-row (
		     &optional &key
		     (stream [default-input-stream])
		     (template [default-template])
		     (bindings nil)
		     &aux
		     (char #\nul)
		     result
		     (state :waiting)
		     buffer)
  "Reads a row from stream using template and, if set, binds symbols
to rows in order. For example, when binding is '(:first :second :third) and
the row is 1,2,3 the result list is (:first 1 :second 2 :third 3)"
  (unless (peek-char nil stream nil nil)
    (return-from read-csv-row nil))
  (flet ((new-buffer ()
	   (setq buffer (make-array 20
				    :element-type 'character
				    :adjustable t
				    :fill-pointer 0))))
    (new-buffer)
    (for-each-char (char :in stream)
      (unless char
	(return (bind-row (reverse result) bindings)))
      (when (char= char (aref (linebreak template) 0))
	(when (newline-p :stream stream :template template)
	  (return (bind-row (reverse result) bindings))))
      (when (char= #\newline char)
	(return (bind-row (reverse result) bindings)))
      (case state
	(:waiting
	 (unless (whitespace-p char)
	   (cond
	     ((char= char (encloser-char template))
	      (push (read-quoted-value buffer :stream stream :template template) 
		    result)
	      (new-buffer))
	     ((char= char (separator-char template))
	      (push nil result))
	     (t 
	      (vector-push char buffer)
	      (push (read-unquoted-value buffer :stream stream :template template)
		    result)
	      (new-buffer)))
	   (unless (char= char (separator-char template))
	     (setq state :waiting-next))))
	(:waiting-next
	 (unless (whitespace-p char)
	   (cond
	     ((char= char (separator-char template))
	      (setq state :waiting))
	     (t
	      (error "junk (~a) not allowed between data in rows (near ~a)" 
		       char (first result))))))))))

(defmacro with-csv-input ((value 
			   &key
			    (template [default-template]))
			   &body body)
  "Helper macro to read from a stream

PARAMETERS:
value:        The stream where we want to read

OPTIONAL KEYS:
template:     The template to use (default RFC4180)"
   (let ((stream (gensym "stream-")) 
	 (old-stream (gensym "stream-")) 
	 (tpl (gensym "template-")))
    `(let ((,stream (%to-input-stream ,value)) 
	   (,old-stream [default-input-stream])
	   (,tpl ,template))
       (setq [default-input-stream] ,stream)
       (with-csv-template (,tpl)
	 (unwind-protect
	      (progn ,@body)
	   (setq [default-input-stream] ,old-stream)
	   (close ,stream))))))

(defun load-csv (value
		 &optional &key
		 (template [default-template])
		 (bindings nil)
		 (ignore-header nil))
  "Load a CSV file into a list with optional bindings"
  (with-csv-input (value :template template)
    (when ignore-header
      (read-csv-row))
    (loop for i = (read-csv-row  :bindings bindings) 
       while i collect i)))

(defun import-csv (value class slots
		   &optional &key
		   (template [default-template])
		   (ignore-header nil))
  "Imports a CSV file to a list of objects of class filling slots."
  (loop 
     with i = (load-csv value :template template :ignore-header ignore-header)
     for k = (make-instance class)
     do (progn
	  (loop for j from 0 to (1- (list-length slots))
	     do (setf (slot-value k (nth j slots)) (nth j i))))
     collect k))

(defun map-csv (stream work
		&optional &key
		(template [default-template])
		(bindings nil)
		(ignore-header nil))
"Maps the function work for each row in stream"
  (with-csv-input (stream :template template)
    (when ignore-header
      (read-csv-row))
    (loop for i = (read-csv-row :bindings bindings)
       while i do (funcall work i))))
