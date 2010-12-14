;;; estimate-mode-commands.el --- 

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


(defsubst estimate--has-row-p-internal (all category item)
  (when all
    (let ((first (car all)))
      (or (and (equal category (nth 1 first))
               (equal item     (nth 2 first)))
          (estimate--has-row-p-internal (cdr all) category item)))))

(defun estimate--extract-candidate ()
  (apply 'append
         (mapcar
          (lambda (format)
            (let* ((fields   (split-string format " +"))
                   (category (car fields))
                   (item     (cadr fields)))
              (if (estimate--has-row-p-internal estimate-all-rows
                                                category item)
                  ()
                (list format))))
          estimate-all-items)))

(defconst anything-c-source-estimate-items
  `((name       . "Estimate items")
    (init       . (lambda ()
                    (setq estimate-candidate (estimate--extract-candidate))))
    (candidates . estimate-candidate)
    (action     . (("Insert" . estimate-do-insert-item)))))

(defconst estimate-current-estimate-buffer nil)

(defun estimate-do-insert-item (item-format)
  (save-excursion
    (set-buffer estimate-current-estimate-buffer)
    (beginning-of-line)
    (let* ((end-of-line (= (point)
                           (save-excursion (end-of-line) (point)))))
      (insert (format item-format
                      (read-number "How mutch? "
                                   (plist-get estimate-default-quantities
                                              item-format)))
              " :"
              (if end-of-line "" "\n")))))



(defun estimate-mode-kill-line ()
  (interactive)
  (let ((inhibit-read-only t))
    (call-interactively 'kill-line)))

(defun estimate-mode-newline ()
  (interactive)
  (let ((pt (point)))
    (cond 

     ((or
       (save-excursion
         (beginning-of-line)
         (let ((begin (point))
               (end   (save-excursion (end-of-line)(point))))
           (and (or (re-search-forward "^-+$" end t)
                    (re-search-forward "^=+$" end t)))))
       (and (save-excursion
              (previous-line)
              (beginning-of-line)
              (re-search-forward "^-+$"
                                 (save-excursion (end-of-line)(point)) t))
            (save-excursion
              (next-line)
              (beginning-of-line)
              (re-search-forward "^=+$"
                                 (save-excursion (end-of-line)(point)) t))
            )
       (and (save-excursion
              (previous-line)
              (beginning-of-line)
              (re-search-forward "^=+$"
                                 (save-excursion (end-of-line)(point)) t))
            (save-excursion
              (next-line)
              (beginning-of-line)
              (re-search-forward "^-+$"
                                 (save-excursion (end-of-line)(point)) t))
            )
       )
      (next-line))

     ((save-excursion
        (beginning-of-line)
        (and (not (= pt (point)))
             (re-search-forward estimate-row-regexp
                                (save-excursion (end-of-line)(point)) t)))
      (next-line)
      (beginning-of-line)
      (call-interactively 'newline)
      (goto-char pt)
      (next-line))

     (t
      (call-interactively 'newline)))))

(defun estimate-mode-increase-item (step)
  (interactive (list 1))
  (when (estimate-on-estimate-row-p)
    (let ((estimate-rearrange-flag nil)
          (quant (estimate-read-number (match-string-no-properties
                                        estimate-row-pos-quantity)))
          (beg (match-beginning estimate-row-pos-quantity)))
      (delete-region beg (match-end estimate-row-pos-quantity))
      (goto-char beg)
      (insert (format "%s" (max 0 (+ quant (or step 1)))))))
  (estimate--after-change nil nil nil))

(defun estimate-mode-decrease-item (step)
  (interactive (list 1))
  (estimate-mode-increase-item (- step)))

(defun estimate-up-item ()
  (interactive)
  (when (and (estimate-on-estimate-row-p)
             (save-excursion
               (previous-line)
               (estimate-on-estimate-row-p)))
    (let ((inhibit-read-only t)
          (estimate-rearrange-flag nil))
      (beginning-of-line)
      (kill-line 1)
      (previous-line)
      (yank)
      (previous-line))
    (estimate--after-change nil nil nil)))

(defun estimate-down-item ()
  (interactive)
  (when (and (estimate-on-estimate-row-p)
             (save-excursion
               (next-line)
               (estimate-on-estimate-row-p)))
    (let ((inhibit-read-only t))
      (next-line)
      (beginning-of-line)
      (kill-line 1)
      (previous-line)
      (yank))
    (estimate--after-change nil nil nil)))


(defconst estimate-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-j")      'estimate-mode-newline)
    (define-key km (kbd "<RET>")    'estimate-mode-newline)
    (define-key km (kbd "C-k")      'estimate-mode-kill-line)
    (define-key km (kbd "C-c i")    'estimate-insert-item)
    (define-key km (kbd "M-<up>")   'estimate-mode-increase-item)
    (define-key km (kbd "M-<down>") 'estimate-mode-decrease-item)
    (define-key km (kbd "C-<up>")   'estimate-up-item)
    (define-key km (kbd "C-<down>") 'estimate-down-item)
    km))


(provide 'estimate-mode-commands)
;;; estimate-mode-commands.el ends here
