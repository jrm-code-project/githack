;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GITHACK-TEST")

;;; A mechanical audit that every symbol exported from the GITHACK
;;; package carries a DOCUMENTATION string appropriate to its
;;; role(s) -- a permanent regression guard for TECHNICAL_DEBT.md
;;; items #7/#8. A symbol may play more than one role at once (e.g.
;;; GET-REPOSITORY names both a GIT-OBJECT reader and a
;;; BRANCH-NOT-FOUND-ERROR reader, but only ever one FUNCTION
;;; DOCUMENTATION slot); this test infers every applicable role via
;;; the same technique used to prototype the original audit
;;; (FIND-CLASS, FBOUNDP, and SB-INT:INFO for specials), and requires
;;; DOCUMENTATION to be non-NIL for each one.

(def-suite documentation-suite
  :in githack-suite
  :description "Every exported GITHACK symbol has DOCUMENTATION appropriate to its role(s).")

(in-suite documentation-suite)

(defun %exported-symbol-roles (symbol)
  "Return a list of the DOCUMENTATION doc-types (a subset of
'(TYPE FUNCTION VARIABLE) -- the plain CL symbols DOCUMENTATION
itself dispatches on, not keywords) applicable to SYMBOL, inferred
from whether it names a class/type (FIND-CLASS), a function/macro/
generic-function (FBOUNDP), and/or a DEFVAR/DEFPARAMETER/DEFCONSTANT
special variable (SB-INT:INFO), respectively. A symbol may have more
than one role at once (e.g. a slot-accessor name reused, per
GITHACK's own naming convention, across several unrelated classes)."
  (let ((roles nil))
    (when (find-class symbol nil)
      (push 'type roles))
    (when (fboundp symbol)
      (push 'function roles))
    (when (member (sb-int:info :variable :kind symbol)
                  '(:special :constant :global :alien))
      (push 'variable roles))
    (nreverse roles)))

(test every-exported-symbol-has-documentation-for-each-of-its-roles
  "Every symbol exported from the GITHACK package has a non-NIL
DOCUMENTATION string for every doc-type its role(s) require: :TYPE
for a class/type name, :FUNCTION for a function/macro/generic-
function name, and :VARIABLE for a special variable name. Symbols
with no discoverable role at all (which would indicate a stale or
orphaned export) are also reported as failures rather than silently
skipped."
  (let ((undocumented nil)
        (roleless nil))
    (do-external-symbols (symbol (find-package "GITHACK"))
      (let ((roles (%exported-symbol-roles symbol)))
        (if (null roles)
            (push symbol roleless)
            (dolist (role roles)
              (unless (documentation symbol role)
                (push (list symbol role) undocumented))))))
    (is (null roleless)
        "Exported symbol(s) with no discoverable role (class/function/variable): ~S"
        roleless)
    (is (null undocumented)
        "Exported symbol(s)/role(s) missing DOCUMENTATION: ~S"
        (reverse undocumented))))
