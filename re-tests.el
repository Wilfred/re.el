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

(defun re-run-tests ()
  "Run all the unit tests in re.el."
  (interactive)
  (ert-run-tests-interactively "re-test-"))

(provide 're-tests)
;;; re-tests.el ends here
