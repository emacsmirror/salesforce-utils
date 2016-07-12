(defconst salesforce-table "AQIYEUM2CSK0GWO4BRJZFVN3DTL1HXP5")

(defun salesforce-id-suffix-char (str)
  (assert (= 5 (length str)))
  (let* ((case-fold-search nil)
         (index (cl-reduce (lambda (acc x) (+ (* 2 acc) (if x 1 0)))
                           (cl-map 'list (lambda (x) (string-match-p (rx (any upper)) (string x))) str)
                           :initial-value 0)))
    (substring salesforce-table index (1+ index))))

(defun salesforce-id-suffix (id)
  (assert (= 15 (length id)))
  (mapconcat (lambda (i) (salesforce-id-suffix-char (substring id i (+ 5 i)))) '(0 5 10) ""))

(defun salesforce-id-convert (id)
  (concat id (salesforce-id-suffix id)))

(defun salesforce-append-id-suffix ()
  (interactive)
  (let ((suffix (salesforce-id-suffix (word-at-point))))
    (save-excursion
      (unless (looking-at (rx word-end))
        (forward-word))
      (insert suffix))))
