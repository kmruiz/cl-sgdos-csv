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

(define-condition variable-row-size-error () ())

(defun %string-replace (part replacement string &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun %to-output-stream (&optional value)
  (etypecase value
    (stream value)
    ((or string pathname) (open value 
		    :direction :output 
		    :if-exists :supersede 
		    :if-does-not-exist :create))))

(defparameter [default-output-stream] *standard-output*)

(defun format-csv-value (value 
			 &optional &key
			 (template [default-template]))
  (format nil "~C~A~C"
	  (encloser-char template)
	  (%string-replace ; escape the encloser char so we not make format errors
	   (string (encloser-char template))
	   (concatenate 'string 
			(string (escape-char template)) 
			(string (encloser-char template)))
	   (etypecase value
	     ((or float ratio) (format nil "~f" value))
	     (integer (write-to-string value))
	     (null "")
	     (string value)
	     (symbol (symbol-name value))
	     (character (string value))
	     (list (format nil "~{~a~^, ~}" value))))
	  (encloser-char template)))

(defun write-csv-list-row (row 
			   &optional &key 
			   (stream [default-output-stream])
			   (template [default-template]))
  "Writes a list of items to the selected stream using the desired CSV template.

PARAMETERS:
row:             the list of atoms or lists

OPTIONAL KEYS:
stream:          the output stream
template:        the CSV template"
  (loop for iter on row
     do (progn
	  (format stream (format-csv-value (first iter) :template template))
	  (unless (cdr iter)
	    (format stream "~a" (linebreak template))
	    (return t))
	  (format stream "~C" (separator-char template)))))

(defun write-csv-object-row (object slots
			     &optional &key
			     (stream [default-output-stream])
			     (template [default-template]))
  "Writes object properties according to the selected slots.

PARAMETERS:
object:        the value object
slots:         slots where values are

OPTIONAL KEYS:
stream:        the output stream
template:      the CSV template"
  (let ((parsed-list (loop for i in slots collect (slot-value object i))))
    (write-csv-list-row parsed-list :stream stream :template template)))

(defmacro with-csv-output ((value 
			    &optional &key
			    (template [default-template]))
			   &body body)
  "Helper macro to write CSV to a stream

PARAMETERS:
value:         The stream, when it's value is a pathname or an string
               it's evaluated as a path to a file. When it's a stream
               it uses itselfs.

OPTIONAL KEYS:
template:      The CSV template to use"
  (let ((stream (gensym "stream-")) (old-stream (gensym "stream-")))
    `(let ((,stream (%to-output-stream ,value)) 
	   (,old-stream [default-output-stream]))
       (setq [default-output-stream] ,stream)
       (with-csv-template (,template)
	 (unwind-protect
	      (progn ,@body)
	   (setq [default-output-stream] ,old-stream)
	   (close ,stream))))))

(defun save-csv (stream rows 
		 &optional &key
		 (header nil)
		 (template [default-template])
		 (slots nil))
  "Writes automatically the rows to the stream.

PARAMETERS:
stream:        The stream to use according to with-csv-output values.
rows:          A list of objects, it can mix lists and objects (@view slots)

OPTIONAL KEYS:
header:        The CSV file header, it's a list of strings
template:      The CSV template to use
slots:         When rows has any object, you must define here which
               slots are going to be printed (it's a list of symbols)
               This data is ignored when parsing lists."
  (let ((row-size (max (list-length header)
		       (list-length slots)
		       (if (listp (first rows)) 
			   (list-length (first rows)) 
			   (list-length header)))))
    (when slots
      (unless (= (list-length slots) row-size)
	(error 'variable-row-size-error)))
    (with-csv-output (stream :template template)
      (when header
	(write-csv-list-row header))
      (loop for i in rows
	 when (listp i)
	 do (progn
	      (unless (= (list-length i) row-size)
		(error 'variable-row-size-error))
	      (write-csv-list-row i))
	 unless (listp i)
	 do (write-csv-object-row i slots)))) t)

(defun export-csv (stream objects binds 
		   &optional &key 
		   (template [default-template])
		   (write-header-p t))
"Exports a list of objects to a CSV. Useful for database dumps.

PARAMETERS:
stream:         The stream to use according to with-csv-output values.
objects:        The list of objects to export
binds:          Slots binding. It's an special parameter. When write-header-p
                is true (by default) it must be a list where each element
                is a list where the first element is a string (the header
                of the column) and the second is the slot's symbol asociated.
                For example:
                    '((\"NAME\" name-slot) (\"AGE\" age-slot))
         
                When write-header-p is nil it's a list of slots. For example:
                     '(name-slot age-slot)

OPTIONAL KEYS:
template:       The CSV template to use
write-header-p: If it must write the header or not (@view binds)"
  (let ((header nil) (slots nil))
    (loop for i in binds do
	 (progn
	   (if write-header-p
	       (progn
		 (push (first i) header)
		 (push (second i) slots))
	       (push i slots))))
	       (save-csv stream objects 
			 :header header 
			 :template template 
			 :slots (reverse slots))))

       
