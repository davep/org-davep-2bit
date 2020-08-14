;;; org-davep-2bit --- 2bit file reader for Common Lisp.
;;
;; packages.lisp --- Defines packages for org-davep-2bit.
;; Copyright 2020 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2020
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Code:

(defpackage #:org.davep.2bit
  (:nicknames #:2bit #:twobit)
  (:use #:common-lisp)
  (:export
   ;; 2bit-sequence class.
   "2BIT-SEQUENCE"
   "MAKE-2BIT-SEQUENCE"
   "INVALID-LOCATION"
   "BASES"
   ;; reader class.
   "READER"
   "INVALID-SIGNATURE"
   "INVALID-VERSION"
   "BIG-ENDIAN-P"
   "LITTLE-ENDIAN-P"
   "POS"
   "OPEN-READER"
   "CLOSE-READER"
   "SEQUENCES"
   "SEQ"
   ;; file-reader class.
   "FILE-READER"
   "MAKE-FILE-READER"
   ;; Helper code.
   "WITH-2BIT-FILE"))

;;; packages.lisp ends here
