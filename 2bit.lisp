;;; org-davep-2bit --- 2bit file reader for Common Lisp.
;;
;; 2bit.lisp --- Main code for org-davep-2bit.
;; Copyright 2020 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2020
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; The following code provides a set of classes and functions for reading
;; data from 2bit files. The format of such a file is described here:
;;
;; http://genome.ucsc.edu/FAQ/FAQformat.html#format7
;;
;; You can always find the latest version of this code here:
;;
;; https://github.com/davep/org-davep-2bit

;;; Code:

(in-package :org.davep.2bit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful constants.

(defconstant +byte+ '(unsigned-byte 8)
  "Type of a byte array element.")

(defconstant +2bit-version+ 0
  "The only valid version number of 2bit data.")

(defconstant +signature+ #x1a412743
  "2bit file signature.")

(defconstant +bases+ #("T" "C" "A" "G")
  "Vector of the bases.

Note that the positions of each base in the vector map to the 2bit decoding
for them.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utility functions and macros.

(defmacro with-saved-location ((reader pos) &rest body)
  "Helper macro to save position while visiting elsewhere in the data."
  (let ((old-pos (gensym)))
    `(let ((,old-pos (pos ,reader)))
       (unwind-protect
            (progn
              (setf (pos ,reader) ,pos)
              ,@body)
         (setf (pos ,reader) ,old-pos)))))

(declaim (inline swap-long))
(defun swap-long (value)
  "Swap the endianness of a long integer VALUE."
  (logior
   (logand (ash value -24) #xff)
   (logand (ash value 8) #xff0000)
   (logand (ash value -8) #xff00)
   (logand (ash value 24) #xff000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The sequence access class.

(defclass block-collection ()
  ((count
    :accessor block-count
    :type     integer
    :documentation "The number of blocks in the sequence.")
   (starts
    :accessor block-starts
    :type     list
    :initform nil
    :documentation "List of 0-based integers indicating the start position of each block")
   (sizes
    :accessor block-sizes
    :type     list
    :documentation "List of integers indicating the length of each block"))
  (:documentation "Holds details of a collection of blocks in a sequence."))

(defun make-block-collection (reader)
  "Create a block collection based on the current position in the reader."
  (let ((blocks (make-instance 'block-collection)))
    ;; Get the count of blocks.
    (setf (block-count blocks) (long-read reader))
    ;; Next up is the list of block start positions.
    (setf (block-starts blocks) (longs-read reader (block-count blocks)))
    ;; Finally there's the lengths of the blocks.
    (setf (block-sizes blocks) (longs-read reader (block-count blocks)))
    ;; Return the new object.
    blocks))

(defmethod relevant-blocks ((sequence-start integer) (sequence-end integer) (blocks block-collection))
  "Return a list of relevant start/end positions from BLOCKS.

The return value isn't all the blocks in BLOCKS, but is all the blocks in
BLOCKS that intersect with the sequence bounded by START and END."
  (loop for start in (block-starts blocks)
        for size  in (block-sizes blocks)
        if (and (>= sequence-end start) (> (+ start size) sequence-start))
          collect (cons start (+ start size))))

(defclass 2bit-sequence ()
  ((reader
    :accessor      reader
    :initarg       :reader
    :type          reader
    :documentation "The reader object to use to read the sequence from the 2bit data.")
   (name
    :accessor      name
    :initarg       :name
    :type          srtring
    :documentation "The name of the sequence.")
   (offset
    :accessor      offset
    :initarg       :offset
    :type          integer
    :documentation "The offset of the sequence in the 2bit data.")
   (dna-size
    :accessor      dna-size
    :type          integer
    :documentation "Size of the DNA in the sequence.")
   (n-blocks
    :accessor      n-blocks
    :type          block-collection
    :documentation "Details of the blocks of Ns in the sequence.")
   (mask-blocks
    :accessor      mask-blocks
    :type          block-collection
    :documentation "Details of the masked blocks in the sequence.")
   (dna-offset
    :accessor      dna-offset
    :type          integer
    :documentation "The location in the data where the actual DNA data starts."))
  (:documentation "Class that provides access to a specific sequence."))

(defmethod print-object ((sequence 2bit-sequence) stream)
  "Format SEQUENCE for easy reading when output to STREAM."
  (print-unreadable-object (sequence stream :type t)
    (format stream "~S ~S" (name sequence) (offset sequence))))

(defun make-2bit-sequence (reader name offset)
  "Crete a new 2bit sequence object."
  ;; Create a sequence object.
  (let ((seq (make-instance '2bit-sequence
                            :reader reader
                            :name   name
                            :offset offset)))
    (with-saved-location (reader offset)
      ;; Get the size of the DNA.
      (setf (dna-size seq) (long-read reader))
      ;; Get the N-block information.
      (setf (n-blocks seq) (make-block-collection reader))
      ;; Get the mask block information.
      (setf (mask-blocks seq) (make-block-collection reader))
      ;; Skip a reserved value.
      (raw-long-read reader)
      ;; Finally, record where we ended up as this is where the actual
      ;; sequence data starts and we'll want to constantly revisit this
      ;; location.
      (setf (dna-offset seq) (pos reader))
      ;; Return the new sequence.
      seq)))

(define-condition invalid-location (error)
  ((reason
    :initarg  :reason
    :initform ""
    :reader   reason)
   (start
    :initarg  :start
    :initform nil
    :reader   start)
   (end
    :initarg  :end
    :initform nil
    :reader   end))
  (:documentation "Error thrown when a request for bases has a location problem."))

(defmethod bases :before ((sequence 2bit-sequence) (start integer) (end integer))
  "Perform checks on START and END in relation to SEQUENCE.

This :before method ensures that START and END are within bounds."
  (when (>= start end)
    (error 'invalid-location
           :reason "Start is greater than or equal to the end"
           :start  start
           :end    end))
  (when (< start 0)
    (error 'invalid-location
           :reason "Start is less than 0"
           :start  start
           :end    end))
  (when (>= start (dna-size sequence))
    (error 'invalid-location
           :reason "Start is beyond the end of the sequence"
           :start  start
           :end    end))
  (when (> end (dna-size sequence))
    (error 'invalid-location
           :reason "End is beyond the end of the sequence"
           :start  start
           :end    end)))

(defmethod bases ((sequence 2bit-sequence) (start integer) (end integer))
  "Get the bases between START and END from SEQUENCE."
  (let* ((start-byte (+ (dna-offset sequence) (floor (/ start 4))))
         (end-byte   (+ (dna-offset sequence) (floor (/ (1- end) 4))))
         (position   (* (- start-byte (dna-offset sequence)) 4))
         (buffer      (with-saved-location ((reader sequence) start-byte)
                        (bytes-read (reader sequence) (1+ (- end-byte start-byte)))))
         (n-blocks    (relevant-blocks start end (n-blocks sequence)))
         (mask-blocks (relevant-blocks start end (mask-blocks sequence)))
         (out         (make-string-output-stream)))
    (loop
      ;; For each byte in the buffer..
      for byte across buffer
      do (loop
           ;; For each 2 bits in the byte...
           for shift from 6 downto 0 by 2
           ;; ...while we've not hit the end of what we're interested in...
           while (< position end)
           ;; ...if we're interested in this particular base...
           if (>= position start)
             ;; ...collect it
             do (princ
                 ;; If the position we're looking at is within an N block...
                 (if (loop for block in n-blocks thereis (and (< (1- (car block)) position (cdr block))))
                     ;; ...it's an N, obviously.
                     "N"
                     ;; ...otherwise decode the base.
                     (let ((base (elt +bases+ (ash (logand (ash #b11 shift) byte) (- shift)))))
                       ;; If we're masking and this base is within a masked region...
                       (if (and (masking (reader sequence))
                                (loop for block in mask-blocks thereis (and (< (1- (car block)) position (cdr block)))))
                           ;; ...downcase the base...
                           (string-downcase base)
                           ;; ...otherwise go with it as-is.
                           base)))
                 out)
           do (incf position))) ;; Bump the base position along one.
    (get-output-stream-string out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main reader class. This does the work of pulling information out of
;; the 2bit data. This specific class does not implement the code to
;; actually do the reading -- the aim down the line is to allow reading via
;; different methods (file IO, via HTTP, etc) -- it just implements the base
;; class that other usable classes will inherit from.

(defclass reader ()
  ((source
    :accessor      source
    :initarg       :source
    :type          string
    :documentation "The name of the source of the 2bit data.")
   (signature
    :accessor      signature
    :type          integer
    :documentation "The signature of the 2bit data.")
   (version
    :accessor      version
    :type          integer
    :documentation "The version ID of the 2bit data.")
   (sequence-count
    :accessor      sequence-count
    :type          integer
    :documentation "The count of sequences inside the 2bit data.")
   (masking
    :accessor      masking
    :initarg       :masking
    :initform      t
    :type          boolean
    :documentation "Should masking be taken into account?")
   (index
    :accessor      index
    :type          hash-table
    :documentation "Hash table that holds the index of the 2bit data."))
  (:documentation "Class that handles the basics of reading from 2bit data."))

(define-condition invalid-signature (error)
  ((signature
    :initarg  :signature
    :initform nil
    :reader   signature))
  (:documentation "Error thrown when an invalid signature value is found in a 2bit header."))

(define-condition invalid-version (error)
  ((version
    :initarg  :version
    :initform nil
    :reader   version))
  (:documentation "Error thrown when an invalid version value is found in a 2bit header."))

(define-condition not-implemented (error)
  ()
  (:documentation "Type of error raised if a method hasn't been implemented."))

(defmethod valid-signature-p ((reader reader))
  "Does the 2bit data have a valid signature?"
  (or (= (signature reader) +signature+)
      (= (swap-long (signature reader)) +signature+)))

(defmethod swapped-p ((reader reader))
  "Is the data in READER byte-swapped?"
  (= (swap-long (signature reader)) +signature+))

(defmethod pos ((reader reader))
  "Get the current data position."
  (error 'not-implemented))

(defmethod (setf pos) (pos (reader reader))
  "Set the current data position."
  (error 'not-implemented))

(defmethod byte-read ((reader reader))
  "Read a byte from READER"
  (error 'not-implemented))

(defmethod raw-long-read ((reader reader))
  "Read a long (4-byte) numeric value from READER."
  (error 'not-implemented))

(defmethod long-read ((reader reader))
  "Read a long (4-byte) numeric value from READER."
  (let ((value (raw-long-read reader)))
    (if (swapped-p reader)
        (swap-long value)
        value)))

(defmethod bytes-read ((reader reader) (len integer))
  "Read an array of bytes from READER."
  (error 'not-implemented))

(defmethod string-read ((reader reader) (len integer))
  "Read a string of LEN length from READER."
  (map 'string #'code-char (bytes-read reader len)))

(defmethod longs-read ((reader reader) (count integer))
  "Read in a list of COUNT long (32-bit) values from READER."
  (let ((buffer (bytes-read reader (* count 4))))
    (loop for n from 0 below count
          for pos = (* n 4)
          for value = (logior
                       (aref buffer pos)
                       (ash (aref buffer (1+ pos )) 8)
                       (ash (aref buffer (+ 2 pos)) 16)
                       (ash (aref buffer (+ 3 pos)) 24))
          collect (if (swapped-p reader) (swap-long value) value))))

(defmethod load-index-entry ((reader reader) (index hash-table))
  "Read a single entry from READER and add to INDEX.

This method reads from the current position, reading the name of the entry
in the index of sequences, and then reads its offset and adds that to
INDEX."
  ;; We should be looking at the size of the name of the next entry in the
  ;; index. Grab that and make a buffer big enough to load it up.
  (let ((name (string-read reader (byte-read reader))))
    (setf (gethash name index)
          (make-2bit-sequence reader name (long-read reader)))))

(defmethod read-index ((reader reader))
  "Read the 2bit data index from READER."
  ;; Set up a new hash table to hold the index.
  (let ((index (make-hash-table :test #'equal)))
    ;; Now loop over all the entries in the index and add them to the table.
    (loop for n from 1 to (sequence-count reader)
          do (load-index-entry reader index)
          finally (return index))))

(defmethod open-reader ((reader reader))
  "Open READER for further reading."
  ;; Pull the signature out of the header.
  (setf (signature reader) (raw-long-read reader))
  ;; Does the signature look valid?
  (unless (valid-signature-p reader)
    (error 'invalid-signature :signature (signature reader)))
  ;; Pull the version out of the header.
  (setf (version reader) (long-read reader))
  ;; Does the version look valid? Note that only one valid version number
  ;; has ever been defined, so we test exactly for that. Yeah, I know...
  (unless (= (version reader) +2bit-version+)
    (error 'invalid-version :version (version reader)))
  ;; Finally, load up the sequence count.
  (setf (sequence-count reader) (long-read reader))
  ;; Skip a reserved log value.
  (raw-long-read reader)
  ;; Now load up the index of the file.
  (setf (index reader) (read-index reader))
  ;; Finally, just to allow easy chaining of calls, return the reader
  ;; object.
  reader)

(defmethod close-reader ((reader reader))
  "Close READER."
  (error 'not-implemented))

(defmethod sequences ((reader reader))
  "Returns a list of the names of all sequences found in READER."
  (loop for name being the hash-keys of (index reader) collect name))

(defmethod seq ((reader reader) (name string))
  "Get the sequence named NAME from READER."
  (values (gethash name (index reader))))

(defmethod seq ((reader reader) (name symbol))
  "Get the sequence named NAME from READER."
  (seq reader (let ((*print-case* :downcase)) (format nil "~S" name))))

(defmethod print-object ((reader reader) stream)
  "Format READER for easy reading when output to STREAM."
  (print-unreadable-object (reader stream :type t)
    (format stream "~S :masking ~S" (source reader) (masking reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2bit data reader class that reads from a local file.

(defclass file-reader (reader)
  ((file :accessor file))
  (:documentation "Class that handles reading from 2bit data held in a local file."))

(defun make-file-reader (source &key (masking t))
  "Create a new instance of a 2bit file-reader, reading from SOURCE."
  (make-instance 'file-reader :source source :masking masking))

(defmethod pos ((reader file-reader))
  "Get the current data position."
  (file-position (file reader)))

(defmethod (setf pos) (pos (reader file-reader))
  "Set the current data position."
  (file-position (file reader) pos))

(defmethod byte-read ((reader file-reader))
  "Read a byte from READER."
  (read-byte (file reader)))

(defmethod raw-long-read ((reader file-reader))
  "Read a long (4-byte) numeric value from READER."
  (let ((buffer (make-array 4 :element-type +byte+ :initial-element 0)))
    (read-sequence buffer (file reader))
    (logior (aref buffer 0)
            (ash (aref buffer 1) 8)
            (ash (aref buffer 2) 16)
            (ash (aref buffer 3) 24))))

(defmethod bytes-read ((reader file-reader) (len integer))
  "Read an array of bytes of LEN length from READER."
  (let ((buffer (make-array len :element-type +byte+)))
    (read-sequence buffer (file reader))
    buffer))

(defmethod open-reader :before ((reader file-reader))
  "Open READER for further reading."
  ;; Start out by opening the source file for reading and hanging on to the
  ;; stream. Note that we don't do any sort of file checks here; we'll
  ;; simply let the normal file I/O errors bubble up.
  (setf (file reader) (open (source reader) :element-type +byte+)))

(defmethod close-reader ((reader file-reader))
  "Close READER."
  (close (file reader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level utility stuff.

(defmacro with-2bit-file ((handle file &rest rest) &body body)
  "Perform BODY against data from FILE, naming reader as HANDLE."
  `(let ((,handle (make-file-reader ,file ,@rest)))
     (open-reader ,handle)
     (unwind-protect
          (progn ,@body)
       (close-reader ,handle))))

;; Local Variables:
;; eval: (put 'with-saved-location 'common-lisp-indent-function 1)
;; eval: (put 'with-2bit-file 'common-lisp-indent-function 1)
;; End:

;;; 2bit.lisp ends here
