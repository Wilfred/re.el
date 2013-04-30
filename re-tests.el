(ert-deftest re-test-find-all ()
  (should
   (equal
    (list "a" "c")
    (re-find-all "abcd" "a\\|c"))))

(ert-deftest re-test-find-all-case ()
  (should
   (equal
    (list "A" "C")
    (re-find-all "ABCD" "a\\|c" t))))

(ert-deftest re-test-from-pcre-basic-class ()
  "Test basic character classes for `re-from-pcre'."
  (should (equal "[a-z]" (re-from-pcre "[a-z]")))
  (should (equal "[^a-z]" (re-from-pcre "[^a-z]"))))

(ert-deftest re-test-from-pcre-parens ()
  "Test groups and literal parens for `re-from-pcre'."
  (should (equal "\\(ab\\)" (re-from-pcre "(ab)")))
  (should (equal "()" (re-from-pcre "\\(\\)"))))

(ert-deftest re-test-from-pcre-alternation ()
  (should (equal "a\\|b" (re-from-pcre "a|b"))))

(ert-deftest re-test-from-pcre-comments ()
  (should (equal "" (re-from-pcre "(# I am a comment!)"))))

(defun re-run-tests ()
  "Run all the unit tests in re.el."
  (interactive)
  (ert-run-tests-interactively "re-test-"))

(provide 're-tests)
;;; re-tests.el ends here
