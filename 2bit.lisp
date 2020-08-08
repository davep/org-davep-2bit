;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful constants.

(defconstant +2bit-version+ 0
  "The only valid version number of 2bit data.")

(defconstant +le-signature+ #x1a412743
  "Little-endian 2bit file signature.")

(defconstant +be-signature+ #x4327411a
  "Big-endian 2bit file signature.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The sequence access class.

(defclass 2bit-sequence ()
  ((reader
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
   (masking
    :accessor      masking
    :initarg       :masking
    :initform      t
    :type          boolean
    :documentation "Should masking be taken into account?"))
  (:documentation "Class that provides access to a specific sequence."))

(defmethod print-object ((sequence 2bit-sequence) stream)
  "Format SEQUENCE for easy reading when output to STREAM."
  (print-unreadable-object (sequence stream :type t)
    (format stream "~S ~S" (name sequence) (offset sequence))))

(defun make-2bit-sequence (reader name offset)
  "Crete a new 2bit sequence object."
  (make-instance '2bit-sequence
                 :reader reader
                 :name   name
                 :offset offset))

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

(defmethod big-endian-p ((reader reader))
  "Does the 2bit data have the signature of a big-endian file?"
  (and (signature reader) (= (signature reader) +be-signature+)))

(defmethod little-endian-p ((reader reader))
  "Does the 2bit data have the signature of a little-endian file?"
  (and (signature reader) (= (signature reader) +le-signature+)))

(defmethod valid-signature-p ((reader reader))
  "Does the 2bit data have a valid signature?"
  (or (big-endian-p reader) (little-endian-p reader)))

(defmethod byte-read ((reader reader))
  "Read a byte from READER"
  (error 'not-implemented))

(defmethod long-read ((reader reader))
  "Read a long (4-byte) numeric value from READER."
  (error 'not-implemented))

(defmethod string-read ((reader reader) (len integer))
  "Read a string of LEN length from READER."
  (error 'not-implemented))

(defmethod load-index-entry ((reader reader) (index hash-table))
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
    (loop for n from 1 to (sequence-count reader) do (load-index-entry reader index))
    ;; Return the index.
    index))

(defmethod open-reader ((reader reader))
  "Open READER for further reading."
  ;; Pull the signature out of the header.
  (setf (signature reader) (long-read reader))
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
  (long-read reader)
  ;; Now load up the index of the file.
  (setf (index reader) (read-index reader))
  ;; Finally, just to allow easy chaining of calls, return the reader
  ;; object.
  reader)

(defmethod sequences ((reader reader))
  "Returns a list of the names of all sequences found in READER."
  (loop for name being the hash-keys of (index reader) collect name))

(defmethod print-object ((reader reader) stream)
  "Format READER for easy reading when output to STREAM."
  (print-unreadable-object (reader stream :type t)
    (format stream "~S" (source reader))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2bit data reader class that reads from a local file.

(defclass file-reader (reader)
  ((file :accessor file))
  (:documentation "Class that handles reading from 2bit data held in a local file."))

(defun make-file-reader (source)
  "Create a new instance of a 2bit file-reader, reading from SOURCE."
  (make-instance 'file-reader :source source))

(defmethod byte-read ((reader file-reader))
  "Read a byte from READER."
  (read-byte (file reader)))

(defmethod long-read ((reader file-reader))
  "Read a long (4-byte) numeric value from READER."
  (let ((buffer (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
    (read-sequence buffer (file reader))
    (+ (aref buffer 0)
       (ash (aref buffer 1) 8)
       (ash (aref buffer 2) 16)
       (ash (aref buffer 3) 24))))

(defmethod string-read ((reader reader) (len integer))
  "Read a string of LEN length from READER."
  (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence buffer (file reader))
    (map 'string #'code-char buffer)))

(defmethod open-reader :before ((reader file-reader))
  "Open READER for further reading."
  ;; Start out by opening the source file for reading and hanging on to the
  ;; stream. Note that we don't do any sort of file checks here; we'll
  ;; simply let the normal file I/O errors bubble up.
  (setf (file reader) (open (source reader) :element-type '(unsigned-byte 8))))

(defmethod close-reader ((reader file-reader))
  "Close READER."
  (close (file reader)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level utility stuff.

(defmacro with-2bit-file ((handle file) &body body)
  "Perform BODY against data from FILE, naming reader as HANDLE."
  `(let ((,handle (make-file-reader ,file)))
     (open-reader ,handle)
     (unwind-protect
          (progn ,@body)
       (close-reader ,handle))))

;;; 2bit.lisp ends here
