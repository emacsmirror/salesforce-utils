;; Copyright 2016 Sean McAfee

;; This file is part of emacs-salesforce.

;; emacs-salesforce is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; emacs-salesforce is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs-salesforce.  If not, see
;; <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl))

;; Typically the Salesforce table would be expressed as the uppercase
;; letters A-Z followed by the digits 0-5.  This version of the table
;; has each character moved from its original position at index N, a
;; 5-bit number, to a position given by reversing the bits of N.  For
;; example, the character Q, originally at position 10000, is here at
;; position 00001.  This saves us a list reversal in
;; salesforce--id-suffix-char below.

(defconst salesforce-table "AQIYEUM2CSK0GWO4BRJZFVN3DTL1HXP5"
  "Salesforce ID checksum lookup table")

(defun salesforce--id-suffix-char (str)
  "Returns the checksum character for one of the three
five-character blocks in a fifteen-character Salesforce ID."
  (cl-assert (= 5 (length str)) nil "ID block not exactly 5 characters long")
  (let* ((case-fold-search nil)
         (index (cl-reduce (lambda (acc x) (+ (* 2 acc) (if x 1 0)))
                           (cl-map 'list (lambda (x) (string-match-p (rx (any upper)) (string x))) str)
                           :initial-value 0)))
    (substring salesforce-table index (1+ index))))

(defun salesforce-id-suffix (id)
  "Returns the three-character checksum suffix for a
fifteen-character Salesforce ID."
  (cl-assert (= 15 (length id)) nil "Salesforce ID must be exactly 15 characters long")
  (mapconcat (lambda (i) (salesforce--id-suffix-char (substring id i (+ 5 i)))) '(0 5 10) ""))

(defun salesforce-id-convert (id)
  "Computes the three-character checksum suffix for a
fifteen-character Salesforce ID and returns the original ID with
the checksum appended."
  (concat id (salesforce-id-suffix id)))

(defun salesforce-append-id-suffix ()
  "Appends the three-character checksum to the fifteen-character
Salesforce ID at point."
  (interactive)
  (let ((suffix (salesforce-id-suffix (word-at-point))))
    (save-excursion
      (unless (looking-at (rx word-end))
        (forward-word))
      (insert suffix))))
