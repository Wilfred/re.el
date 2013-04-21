(ert-deftest re-test-find-all ()
  (should
   (equal
    (list "a" "c")
    (re-find-all "abcd" "a\\|c"))))

(defun re-run-tests ()
  "Run all the unit tests in re.el."
  (interactive)
  (ert-run-tests-interactively "re-test-"))

(provide 're-tests)
;;; re-tests.el ends here
