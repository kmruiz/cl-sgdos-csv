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

(defpackage :cl-sgdos-csv 
  (:use :cl)
  (:export
   ;; csv-template.lisp
   :define-csv-template :RFC4180 :with-csv-template
   ;; csv-writer.lisp
   :write-csv-list-row :write-csv-object-row :with-csv-output
   :save-csv :export-csv
   ;; csv-reader.lisp
   :read-csv-row :with-csv-input :load-csv :import-csv :map-csv))

(in-package :cl-sgdos-csv)

(defclass csv-template ()
  ((linebreak 
    :initarg :linebreak
    :accessor linebreak
    :type string)
   (separator-char
    :initarg :separator-char
    :accessor separator-char
    :type character)
   (encloser-char
    :initarg :encloser-char
    :accessor encloser-char
    :type character)
   (escape-char
    :initarg :escape-char
    :accessor escape-char
    :type character)))

(defmacro define-csv-template ((name) 
			       &key 
			       linebreak
			       separator-char 
			       encloser-char 
			       escape-char)
  `(progn
     (defparameter ,name
       (make-instance 'csv-template
		      :linebreak ,linebreak
		      :separator-char ,separator-char
		      :encloser-char ,encloser-char
		      :escape-char ,escape-char))
     ,name))

(define-csv-template (RFC4180)
    :linebreak (format nil "~C~C" #\return #\linefeed)
    :separator-char #\,
    :encloser-char #\"
    :escape-char #\")

(defparameter [default-template] RFC4180)

(defmacro with-csv-template ((template) &body body)
  (let ((old-storage (gensym "csv-template-")) (result (gensym "csv-result-")))
    `(let ((,old-storage [default-template]))
       (setq [default-template] ,template)
       (let ((,result (progn ,@body)))
	 (setq [default-template] ,old-storage)
	 ,result))))
