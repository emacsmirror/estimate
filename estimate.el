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

(defconst estimate-row-regexp
  "\\([0-9]+\\)\\s +\\([0-9]+\\)[^0-9 \t]*\\(\\s +:\\s *\\([0-9]+\\s *\\)?\\)$")

;; local variables
(defvar estimate-previous-content nil)

;;(estimate-mode-update-total-string 5000)
(defun estimate-mode-calc-and-update-total ()
  (interactive)
  (save-excursion
    (let ((total 0)
          (inhibit-read-only t))
      (goto-char (point-min))
      (re-search-forward "^-+\n" nil t)
      (next-line)
      (let ((continue t))
        (while continue
          (beginning-of-line)
          (let ((start (point))
                (end   (save-excursion
                         (end-of-line)
                         (point))))

            (cond
             ((re-search-forward
               "^-+$" end t)
              (setq continue nil))

             ((re-search-forward estimate-row-regexp end t)
              (let* ((unitprice (string-to-number (match-string 1)))
                     (count     (string-to-number (match-string 2)))
                     (row-total (* unitprice count)))
                (setq total (+ total row-total))
                (goto-char (match-beginning 3))
                (delete-region (point) end)
                (insert (propertize 
                         (format " : %d" row-total)
                         'face '((:foreground "#33C"))
                         'estimate-total t
                         'read-only t
                         'rear-nonsticky t
                         ))))
             ((and (re-search-forward ":\\s *[0-9]+\\s *$" end t)
                   (plist-get
                    (text-properties-at (1+ (match-beginning 0)))
                    'estimate-total))
                (delete-region (match-beginning 0) (match-end 0))
                (end-of-line)
                (insert ":")))

            (condition-case nil (next-line)
              (end-of-buffer
               (when continue
                 (insert "\n----------\n")
                 (setq continue nil)))))))
      (beginning-of-line)
      (delete-region (point) (point-max))
      (let ((start (save-excursion (previous-line) (point))))
        (insert (format "Total : %d" total))
        (set-text-properties
         start (point)
         '(
           read-only t
           face ((:foreground "#693"))
           ))))))


(defun estimate-after-command ()
  (let ((current-content (buffer-substring-no-properties (point-min)
                                                         (point-max))))
    (unless (string= current-content (or estimate-previous-content ""))
      (estimate-mode-calc-and-update-total)
      (setq estimate-previous-content current-content))))

(define-derived-mode estimate-mode fundamental-mode "estimate"
  ""
  (make-variable-buffer-local 'post-command-hook)
  (make-variable-buffer-local 'estimate-previous-content)
  (add-hook 'post-command-hook 'estimate-after-command)
  (estimate-after-command))

;;(add-to-list 'auto-mode-alist '("\\.estimate\\'" . estimate-mode))



(provide 'estimate)
;;; estimate.el ends here
 
