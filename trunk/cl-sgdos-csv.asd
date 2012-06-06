;;; -*- Lisp -*- mode

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-sgdos-csv.system)
    (defpackage :cl-sgdos-csv.system
	(:use :common-lisp :asdf))))

(in-package :cl-sgdos-csv.system)

(defsystem :cl-sgdos-csv
  :name "cl-sgdos-csv"
  :description "Utilities for mapping from/to CSV files with high-level and low-level reading support"
  :licence "GPLv3"
  :version "0.1"
  :serial t
  :components ((:file "csv-template")
	       (:file "csv-writer")
	       (:file "csv-reader")))
