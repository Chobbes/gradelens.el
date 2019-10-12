(require 'org-element)
(require 'seq)

(defun gradelens-get-exercise (file start-delim grade-delim)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((start (search-forward-regexp start-delim nil t 1)))
      (if (null start) nil
        (let ((end (search-forward-regexp grade-delim nil t 1)))
          (if (null end) nil
            (list :file         file
                  :answer       (buffer-substring-no-properties start end)
                  :answer-start start
                  :answer-end   end)))))))

(defun gradelens-grade (start end dir org)
  (interactive "r
DDirectory to grade: 
FFile for grades: 
")
  (with-current-buffer
      (let ((start-delim (progn (goto-char start) (regexp-quote (thing-at-point 'line))))
            (end-delim (progn (goto-char end) (regexp-quote (thing-at-point 'line)))))
        (gradelens-to-org-file (gradelens-group (gradelens-grade-with-delims dir start-delim end-delim)) org))))

(defun gradelens-how-many (dir start end)
  (interactive "DDirectory to grade:
r")
  (length (gradelens-group (gradelens-grade dir start end))))

(defun gradelens-grade-with-delims (dir start-delim end-delim)
  (let* ((files (directory-files-recursively dir ".*\\.v$" nil))
         (file-answers (mapcar (lambda (f) (gradelens-get-exercise f start-delim end-delim)) files)))
    (seq-filter (lambda (x) (not (null (plist-get x :answer)))) file-answers)))

(defun gradelens-group (answers &optional groupfn)
  "Group similar answers together. Default to grouping by hash."
  (let ((gf (if (null groupfn) (lambda (x) (secure-hash 'sha256 (plist-get x :answer))) groupfn)))
    (mapcar #'(lambda (x) `(,(car x) . (:answers ,(cdr x)))) (-group-by gf answers))))

(defun gradelens-grade-answer (answer grade)
  (with-temp-buffer
    (insert-file-contents (plist-get answer :file))
    (goto-char (plist-get answer :answer-end))
    (move-end-of-line nil)
    (kill-whole-line)
    (insert grade)
    (write-file (plist-get answer :file) nil)))

(defun gradelens-answer-to-org-data (answer)
  `((headline (:title ,(plist-get answer :file) :level 2)
    (section nil
             (property-drawer
              nil
              (node-property (:key file :value ,(plist-get answer :file)))
              (node-property (:key answer-start :value ,(plist-get answer :answer-start)))
              (node-property (:key answer-end :value   ,(plist-get answer :answer-end))))
             (src-block (:language "coq" :value ,(plist-get answer :answer)))))))

(defun gradelens-answer-group-to-org-data (answer-group)
  `(headline (:title ,(car answer-group) :level 1)
             (section nil
                      (property-drawer nil
                        (node-property (:key grade :value ,(plist-get (cdr answer-group) :grade)))))
             ,(mapcar #'gradelens-answer-to-org-data (plist-get (cdr answer-group) :answers))))

(defun gradelens-to-org-data (answer-groups)
  `(org-data nil
             ,(mapcar #'gradelens-answer-group-to-org-data answer-groups)))

(defun gradelens-to-org-file (answer-groups file)
  (with-temp-buffer
    (insert (org-element-interpret-data (gradelens-to-org-data answer-groups)))
    (write-file file)))

(defun gradelens-answer-from-org (org-answer)
  (let* ((section (assoc 'section org-answer))
         (property-drawer (assoc 'property-drawer section))
         (properties (cddr property-drawer))
         (drawer (seq-reduce (lambda (x y)
                                 (let* ((key (plist-get (cadr y) :key))
                                        (key-sym (intern-soft (concat ":" key)))
                                        (val (plist-get (cadr y) :value)))
                                   (if (null key) x
                                     (plist-put x key-sym val))))
                         properties nil))
         (src (plist-get (cadr (assoc 'src-block (caddr org-answer))) :value)))
    (plist-put drawer :answer src)))


(defun gradelens-group-from-org (org-group)
  (let* ((section (assoc 'section org-group))
         (property-drawer (assoc 'property-drawer section))
         (properties (cddr property-drawer))
         (drawer (seq-reduce (lambda (x y)
                                 (let* ((key (plist-get (cadr y) :key))
                                        (key-sym (intern-soft (concat ":" key)))
                                        (val (plist-get (cadr y) :value)))
                                   (if (null key) x
                                     (plist-put x key-sym val))))
                         properties nil)))
    `(,(plist-get (cadr org-group) :raw-value) . ,(plist-put drawer :answers (mapcar #'gradelens-answer-from-org (cdddr org-group))))))

(defun gradelens-from-org (org-groups)
  (mapcar #'gradelens-group-from-org (cddr org-groups)))

(defun gradelens-from-org-buffer ()
  (gradelens-from-org (org-element-parse-buffer)))

(defun gradelens-from-org-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (gradelens-from-org (org-element-parse-buffer))))
