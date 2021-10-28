(require 'cl)

(defun read-tree (&optional in)
  "Lol."
  (let ((child-count (read in))
        (metadata-count (read in)))
    (list
     (loop for i from 1 upto child-count
           collect (read-tree in))
     (loop for i from 1 upto metadata-count
           collect (read in)))))

(defun part1 (tree)
  "Sum up the TREE."
  (if tree
    (let ((subtree (car tree))
          (total (apply '+ (cadr tree))))
      (+ (apply '+ (mapcar 'part1 subtree)) total))
    0))

(defun part2 (tree)
  "Sum up TREE for part 2."
  (cond
   ((not tree) 0)
   ((car tree)
    (lexical-let ((tree2 tree))
      (apply '+ (mapcar #'(lambda (idx) (if (<= idx (length (car tree2)))
                                          (part2 (nth (-  idx 1) (car tree2)))
                                          0))
                        (cadr tree2)))))
   (t (apply '+ (cadr tree)))))

(defun batch ()
  (let ((tree (read-tree (current-buffer))))
    (message "%s" (part1 tree))
    (message "%s" (part2 tree))))


;; Local Variables:
;; compile-command: "emacs --batch input.txt --load day08.el --funcall batch"
;; End:
