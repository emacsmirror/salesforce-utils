;;; salesforce-tests.el --- Tests for salesforce.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sean McAfee

;; Author: Sean McAfee <eefacm@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ert-deftest salesforce-tests ()
  "Test salesforce.el"
  (let ((debug-on-error nil))
    (should (string= "A" (salesforce--id-suffix-char "xxxxx")))
    (should (string= "5" (salesforce--id-suffix-char "XXXXX")))
    (should (string= "AAA" (salesforce-id-suffix "000000000000000")))
    (should (string= "AA5" (salesforce-id-suffix "0000000000AAAAA")))
    (should (string= "A5A" (salesforce-id-suffix "abcdeABCDEabcde")))
    (should (string= "5AA" (salesforce-id-suffix "ABCDEabcdeabcde")))
    (should (string= "VKV" (salesforce-id-suffix "AaAaAaAaAaAaAaA")))
    (should-error (salesforce--id-suffix-char ""))
    (should-error (salesforce--id-suffix-char "A"))
    (should-error (salesforce--id-suffix-char "AAAAAA"))))

;;; salesforce-tests.el ends here
