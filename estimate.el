;;; estimate.el --- 

;; Copyright (C) 2010  

;; Author:  <lieutar@TREEFROG>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

;;(defconst
(defvar estimate-items '() "")


(defvar estimate-total-str nil)


(defun estimate-mode-update-row-total-string (total)
  (let ((inhibit-read-only t))
    (save-excursion
      (end-of-line)
      (unless (= ? (char-before (point)))
        (insert " "))
      (set-text-properties
       (1- (point)) (point)
       `(
         read-only t
         display ,(concat " "
                          (number-to-string total))
         face ((:background "#EEE"))
         rear-nonsticky t
         )))))

(defun estimate-mode-update-total-string (total)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (unless (and (= ?\n  (or (char-before (point-max)) 0))
                   (= ?\n  (or (char-before (1- (point-max))) 0)))
        (insert "\n\n"))
      (set-text-properties
       (1- (point-max)) (point-max)
       `(
         read-only t
         display ,(concat "\n----------\nTotal: "
                          (format "%d\n" total))
         face ((:background "#EEE")))))))

;;(estimate-mode-update-total-string 5000)
(defun estimate-mode-calc-and-update-total ()
  (interactive)
  (save-excursion
    (let ((total 0))
      (goto-char (point-min))
      (re-search-forward "^----+$" nil t)
      (while (re-search-forward "\\([0-9]+\\)\\s +\\([0-9]+\\) *$" nil t)
        (let* ((unit      (string-to-number (match-string 1)))
               (count     (string-to-number (match-string 2)))
               (row-total (* unit count)))
          (setq total (+ total row-total))
          (estimate-mode-update-row-total-string row-total)))
      (estimate-mode-update-total-string total))))

(define-derived-mode estimate-mode fundamental-mode "estimate"
  ""
  (make-variable-buffer-local 'estimate-total-str)
  (make-variable-buffer-local 'post-command-hook)
  (add-hook 'post-command-hook 'estimate-mode-calc-and-update-total)
  (estimate-mode-calc-and-update-total))

;;(add-to-list 'auto-mode-alist '("\\.estimate\\'" . estimate-mode))



(provide 'estimate)
;;; estimate.el ends here
 
