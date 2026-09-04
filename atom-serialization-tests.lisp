;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

(def-suite atom-serialization-suite
  :in githack-suite
  :description "Tests for SERIALIZE-ATOM and DESERIALIZE-ATOM.")

(in-suite atom-serialization-suite)

(defun %round-trips-p (atom)
  "Return true iff serializing ATOM and then deserializing the
result produces a value EQUALP to ATOM."
  (equalp atom (deserialize-atom (serialize-atom atom))))

(test integers-round-trip
  "Small, negative, and bignum integers all round-trip exactly."
  (is (%round-trips-p 0))
  (is (%round-trips-p 42))
  (is (%round-trips-p -7))
  (is (%round-trips-p (expt 2 100))))

(test keywords-round-trip
  "Keywords round-trip to the identical (EQ) keyword."
  (is (eq :foo (deserialize-atom (serialize-atom :foo))))
  (is (eq :||  (deserialize-atom (serialize-atom :||)))))

(test exported-symbol-round-trips
  "A symbol exported from its home package round-trips to the
identical (EQ) symbol."
  (is (eq 'git-object (deserialize-atom (serialize-atom 'git-object)))))

(test non-exported-symbol-round-trips
  "A non-exported (internal) symbol round-trips to the identical
(EQ) symbol even though it is not external in its home package."
  (is (eq 'cl-user::my-internal-var
          (deserialize-atom (serialize-atom 'cl-user::my-internal-var)))))

(test uninterned-symbol-signals-error
  "An uninterned symbol cannot be uniquely identified by package and
name, so SERIALIZE-ATOM must signal an error rather than silently
losing its identity."
  (signals error (serialize-atom (make-symbol "FOO"))))

(test single-float-round-trips
  "Single-floats round-trip exactly and are distinguishable from
double-floats in the wire format."
  (is (%round-trips-p 1.0))
  (is (%round-trips-p -0.5))
  (is (find #\f (map 'string #'code-char (serialize-atom 1.0)))))

(test double-float-round-trips
  "Double-floats round-trip exactly and are distinguishable from
single-floats in the wire format."
  (is (%round-trips-p 1.0d0))
  (is (%round-trips-p -0.5d0))
  (is (find #\d (map 'string #'code-char (serialize-atom 1.0d0)))))

(test standard-character-round-trips
  "Standard characters round-trip exactly."
  (is (%round-trips-p #\a))
  (is (%round-trips-p #\Z))
  (is (%round-trips-p #\Space)))

(test non-standard-character-round-trips
  "Non-standard characters (such as control characters or extended
Unicode characters) round-trip exactly, using CHAR-NAME where one is
available."
  (is (%round-trips-p #\Tab))
  (is (%round-trips-p (code-char 955))))

(test string-round-trips
  "Strings, including ones containing quotes, backslashes, and
non-ASCII characters, round-trip exactly."
  (is (%round-trips-p ""))
  (is (%round-trips-p "hello, world"))
  (is (%round-trips-p "quotes \" and \\ backslashes"))
  (is (%round-trips-p "unicode: \\u03bb")))

(test bit-vector-round-trips
  "Bit-vectors round-trip exactly, preserving their specialized
element type."
  (let* ((original #*10110)
         (result (deserialize-atom (serialize-atom original))))
    (is (equalp original result))
    (is (typep result 'simple-bit-vector))))

(test byte-vector-round-trips
  "1-D (UNSIGNED-BYTE 8) vectors round-trip exactly, preserving
their specialized element type."
  (let* ((original (make-array 3 :element-type '(unsigned-byte 8)
                                  :initial-contents '(1 2 255)))
         (result (deserialize-atom (serialize-atom original))))
    (is (equalp original result))
    (is (typep result '(simple-array (unsigned-byte 8) (*))))))

(test unsupported-type-signals-error
  "SERIALIZE-ATOM only supports atoms; conses are not atoms and must
signal an error."
  (signals error (serialize-atom (list 1 2 3))))
