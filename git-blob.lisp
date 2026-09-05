;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK")

;;; Serialization of Lisp atoms to/from the byte-vector payload of a
;;; GIT-BLOB.
;;;
;;; The wire format is a UTF-8-encoded, tagged S-expression envelope
;;; -- a list (TAG . DATA), where TAG identifies the Lisp type and
;;; DATA is plain, unambiguously-readable Lisp data (strings, small
;;; integers, characters, or lists thereof) sufficient to reconstruct
;;; the exact original atom: not merely an EQUAL atom, but one that
;;; is EQL to the original (and, for symbols, interned in the same
;;; package under the same name).
;;;
;;; Supported atom types: INTEGER, KEYWORD, other SYMBOL,
;;; SINGLE-FLOAT, DOUBLE-FLOAT, CHARACTER, STRING, BIT-VECTOR, and
;;; 1-D (UNSIGNED-BYTE 8) vectors ("byte vectors"). Any other type,
;;; including CONS, is unsupported: this layer serializes only
;;; atoms, not compound structures.

(defun %normalize-string (string)
  "Coerce STRING to a full (SIMPLE-ARRAY CHARACTER (*)), so that
PRIN1 always prints it with plain \"...\" syntax instead of the
#A(...) array syntax that SBCL's *PRINT-READABLY* uses for
SIMPLE-BASE-STRINGs (e.g. those returned by SYMBOL-NAME or
CHAR-NAME)."
  (coerce string '(simple-array character (*))))

(defun %float->marked-string (value other-float-format)
  "Print VALUE (a SINGLE-FLOAT or DOUBLE-FLOAT) with an explicit
exponent marker (\"f0\"/\"d0\") that is present no matter the ambient
*READ-DEFAULT-FLOAT-FORMAT*, by temporarily binding that variable to
OTHER-FLOAT-FORMAT -- a float type distinct from VALUE's own -- so
the printer can never omit the marker as redundant."
  (let ((*read-default-float-format* other-float-format)
        (*print-readably* t))
    (%normalize-string (prin1-to-string value))))

(defun %symbol->envelope (symbol)
  "Return the serialization envelope for a non-keyword SYMBOL,
recording its home package name and its own name as plain strings so
DESERIALIZE-ATOM can always re-intern it -- as if using the ::
fully-qualified convention -- regardless of whether it is external
in its home package."
  (let ((home (symbol-package symbol)))
    (unless home
      (error "Cannot serialize the uninterned symbol ~S." symbol))
    (list :symbol (%normalize-string (package-name home)) (%normalize-string (symbol-name symbol)))))

(defun %character->envelope (char)
  "Return the serialization envelope for CHAR. Standard characters
are stored directly (letting the Lisp printer/reader's own #\\
syntax handle them); other characters are stored by CHAR-NAME when
one exists, falling back to their raw CHAR-CODE otherwise."
  (cond
    ((standard-char-p char) (list :character char))
    ((char-name char) (list :named-character (%normalize-string (char-name char))))
    (t (list :character-code (char-code char)))))

(defgeneric %atom->envelope (atom)
  (:documentation
   "Return an envelope list describing ATOM, suitable for printing
with PRIN1 and later reconstructing with %ENVELOPE->ATOM. Signals an
error if ATOM's type is not supported. Dispatches on ATOM's concrete
class; KEYWORD and (ARRAY (UNSIGNED-BYTE 8) (*)) are not themselves
CLOS classes usable as method specializers, so those two cases are
distinguished by an explicit type check inside the SYMBOL and VECTOR
methods respectively."))

(defmethod %atom->envelope ((atom integer))
  (list :integer atom))

(defmethod %atom->envelope ((atom symbol))
  (if (keywordp atom)
      (list :keyword (%normalize-string (symbol-name atom)))
      (%symbol->envelope atom)))

(defmethod %atom->envelope ((atom single-float))
  (list :single-float (%float->marked-string atom 'double-float)))

(defmethod %atom->envelope ((atom double-float))
  (list :double-float (%float->marked-string atom 'single-float)))

(defmethod %atom->envelope ((atom character))
  (%character->envelope atom))

(defmethod %atom->envelope ((atom string))
  (list :string (%normalize-string atom)))

(defmethod %atom->envelope ((atom bit-vector))
  (list :bit-vector (coerce atom 'list)))

(defmethod %atom->envelope ((atom vector))
  (if (typep atom '(array (unsigned-byte 8) (*)))
      (list :byte-vector (coerce atom 'list))
      (call-next-method)))

(defmethod %atom->envelope ((atom t))
  (error "SERIALIZE-ATOM does not support objects of type ~S." (type-of atom)))

(defun %envelope->atom (envelope)
  "Inverse of %ATOM->ENVELOPE: reconstructs the exact Lisp atom
described by ENVELOPE."
  (unless (consp envelope)
    (error "Malformed atom envelope: ~S." envelope))
  (destructuring-bind (tag &rest data) envelope
    (ecase tag
      (:integer (first data))
      (:keyword (intern (first data) (find-package "KEYWORD")))
      (:symbol
       (destructuring-bind (package-name symbol-name) data
         (let ((package (find-package package-name)))
           (unless package
             (error "Cannot deserialize symbol: no package named ~S." package-name))
           (intern symbol-name package))))
      ((:single-float :double-float)
       (let ((*read-eval* nil))
         (read-from-string (first data))))
      (:character (first data))
      (:named-character
       (or (name-char (first data))
           (error "Unknown character name ~S." (first data))))
      (:character-code (code-char (first data)))
      (:string (first data))
      (:bit-vector
       (let ((bits (first data)))
         (make-array (length bits) :element-type 'bit :initial-contents bits)))
      (:byte-vector
       (let ((bytes (first data)))
         (make-array (length bytes) :element-type '(unsigned-byte 8)
                                     :initial-contents bytes))))))

(defun serialize-atom (atom)
  "Convert ATOM -- an INTEGER, SYMBOL (KEYWORD or otherwise),
SINGLE-FLOAT, DOUBLE-FLOAT, CHARACTER, STRING, BIT-VECTOR, or 1-D
(UNSIGNED-BYTE 8) vector -- into a UTF-8-encoded octet vector from
which DESERIALIZE-ATOM reconstructs the exact same Lisp value: the
same type, the same numeric precision, and, for symbols, the same
package and name (even when the symbol is not external in that
package). Signals an error for unsupported types, such as conses."
  (let ((*print-readably* t)
        (*print-circle* nil)
        (*print-pretty* nil)
        (*print-case* :upcase)
        (*package* (find-package "KEYWORD")))
    (sb-ext:string-to-octets
     (prin1-to-string (%atom->envelope atom))
     :external-format :utf-8)))

(defun deserialize-atom (octets)
  "Inverse of SERIALIZE-ATOM: reconstructs the exact Lisp atom
encoded in the octet vector OCTETS."
  (let ((*read-eval* nil)
        (*package* (find-package "KEYWORD")))
    (%envelope->atom
     (read-from-string (sb-ext:octets-to-string octets :external-format :utf-8)))))
