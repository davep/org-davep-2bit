;;; org-davep-2bit --- 2bit file reader for Common Lisp.
;;
;; org-davep-2bit.asd --- asdf package definition file.
;; Copyright 2020 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2020
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Code:

(defpackage #:org-davep-2bit-system
  (:use #:common-lisp #:asdf))

(in-package :org-davep-2bit-system)

(defsystem org-davep-dict
  :name        "org-davep-2bit"
  :author      "Dave Pearson <davep@davep.org>"
  :maintainer  "Dave Pearson <davep@davep.org>"
  :licence     "LLGPL"
  :version     "0.1"
  :description "2bit reader for Common Lisp."
  :long-description
  "org-davep-2bit provides a set of classes, functions and macros for Common
Lisp for reading data from 2bit-format files. Details on the format of 2bit
files can be found here:

  <URL:http://genome.ucsc.edu/FAQ/FAQformat.html#format7>"
  :components  ((:file "packages")
                (:file "2bit" :depends-on ("packages"))))

;;; org-davep-2bit.asd ends here
