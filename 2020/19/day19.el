#!/usr/bin/emacs --script
;;; Code:
(defvar rules (make-hash-table))

(defun parse-rule (s)
  "Parse a rule from string S."
  (let* ((sp1 (split-string s ":" t " *"))
         (rn (string-to-number (car sp1))))
    (cons rn
          (cl-loop for alt-string in (split-string (cadr sp1) "|" t " *")
                   collect (cl-loop for ss in (split-string alt-string " " t " *")
                                    collect (string-to-number ss))))))
(condition-case nil
    (let (line (reading-rules t))
      (while (setq line (read-from-minibuffer ""))
        (message line)
        (when (equal line "")
          (setq reading-rules nil))
        (if reading-rules
          (let ((r (parse-rule line)))
            (puthash rules (car r) (cdr r))))))
  (error nil))

(maphash (defun dump-rules (k v)
           (message "%s: %s" k v))
         rules)

(provide 'day19)
;;; day19.el ends here
