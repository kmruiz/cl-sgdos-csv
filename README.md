cl-sgdos-csv
============

cl-sgdos-csv is a library (or pretends to be) for mapping CSV files to LISP
lists of CLOS objects in a totally configurable way using templates.

A csv-template is a definition of a "standard" format for a CSV files. By
this way, you can write or read multiple CSV files without making manual
configurations to variables.

How To Start
------------

### Defining Templates

To define a CSV template you must use the define-csv-template macro. The
definition of the RFC4180 template is as follows:

	(define-csv-template (RFC4180)
    		:linebreak (format nil "~C~C" #\return #\linefeed)
    		:separator-char #\,
   		:encloser-char #\"
    		:escape-char #\")

Be aware of using characters in linebreak: it must be a string.

### Reading CSV Files

The are a some ways for reading files: the easiest way is using the
load-csv function. You only need the stream or the path to the file
to read.

Be aware of using load-csv or import-csv functions for very large files! It
caches the entire file and it can be a full-killing memory user.

For loading huge files use with-csv-input macro or the high-level map-csv
function. This last one only lets you bind to lists, I'm working on
it for accept also object binding. You don't have this problem with
with-csv-input macro. But you must iterate by row manually (easy using loop and read-csv-row however)

### Writing CSV Files

That's a lot easier. You only need your data in a list. Data can be a list of lists, a list of objects
or mixed. Remember to use slot binding when using objects!

Check out the save-csv and export-csv functions for easy writing.
