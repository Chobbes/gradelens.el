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

(defun gradelens-grade (start end dir org &optional groupfn)
  (interactive "r
DDirectory to grade: 
FFile for grades: 
")
  (let ((start-delim (progn (goto-char start) (regexp-quote (thing-at-point 'line))))
        (end-delim (progn (goto-char end) (regexp-quote (thing-at-point 'line)))))
    (gradelens-to-org-file (gradelens-group (gradelens-grade-with-delims dir start-delim end-delim) groupfn) org)))

(defun gradelens-grade-ungrouped (start end dir org)
  (interactive "r
DDirectory to grade: 
FFile for grades: 
")
  (gradelens-grade start end dir org #'gradelens-dont-groupfn))

(defun gradelens-how-many (dir start end)
  "Count how many distinct grades their are"
  (interactive "DDirectory to grade:
r")
  (length (gradelens-group (gradelens-grade dir start end))))

(defun gradelens-grade-with-delims (dir start-delim end-delim)
  (let* ((files (directory-files-recursively dir ".*\\.v$" nil))
         (file-answers (mapcar (lambda (f) (gradelens-get-exercise f start-delim end-delim)) files)))
    (seq-filter (lambda (x) (not (null (plist-get x :answer)))) file-answers)))

(defun gradelens-group (answers &optional groupfn)
  "Group similar answers together. Default to grouping by hash."
  (let ((gf (if (null groupfn) #'gradelens-hash-groupfn groupfn)))
    (mapcar #'(lambda (x) `(,(car x) . (:answers ,(cdr x)))) (-group-by gf answers))))

(defun gradelens-hash-groupfn (x)
  "Hash answer, trim all whitespace"
  (let* ((answer  (plist-get x :answer))
         (trim-comments (replace-regexp-in-string "(\\*[^\n\r\v]*\\*\)" "" answer))
         (trimmed (replace-regexp-in-string "[[:space:]]*" "" trim-comments)))
    (secure-hash 'sha256 trimmed)))

(defun gradelens-dont-groupfn (x)
  "Always return the group 'Ungrouped'"
  "Ungrouped")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assigning grades.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gradelens-string-to-number (str)
  (if (stringp str)
      (string-to-number str)
    str))

(defun gradelens-grade-answer (answer default-grade)
  (let ((grade (or (plist-get answer :grade) default-grade)))
    (if (null grade) ()
     (with-temp-buffer
       (insert-file-contents (plist-get answer :file))
       (goto-char (- (gradelens-string-to-number (plist-get answer :answer-end)) 1))
       (kill-whole-line)
       (insert grade)
       (newline)
       (write-file (plist-get answer :file) nil)))))

(defun gradelens-grade-group (group-alist)
  "grade a GROUP which is an alist entry"
  (let* ((group (cdr group-alist))
         (default-grade (plist-get group :grade)))
    (mapcar (lambda (ans) (gradelens-grade-answer ans default-grade))
            (plist-get group :answers))))

(defun gradelens-grade-groups (answer-groups)
  (mapcar #'gradelens-grade-group answer-groups))

(defun gradelens-write-grades ()
  (interactive)
  (let ((groups (gradelens-from-org-buffer)))
    (gradelens-grade-groups groups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert gradelens dictionary to org.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert org to gradelens dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
