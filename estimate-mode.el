;;; estimate-mode.el --- 

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
(require 'cl)
(require 'json)
(require 'anything)

(require 'estimate-util)
(require 'estimate-item)
(require 'estimate-display)
;;;
;;; controler variables (buffer scope)
;;;

(defconst estimate-rearrange-flag   t)
(defconst estimate-with-tax         t)
(defconst estimate-all-rows       nil)

(require 'estimate-regexp)

;;(estimate-format-number 6566536)


(defun estimate-insert-item ()
  (interactive)
  (let ((estimate-current-estimate-buffer (current-buffer)))
    (anything
     :sources '(anything-c-source-estimate-items)
     :buffer  "*Estimate item*")))



;;;
;;;
;;;
(defsubst estimate--column (str width align)
  (let* ((sub (- width (string-width str))))
    (case align
      ((left)
       (concat str (make-string sub ? )))

      ((center)
       (concat (make-string (/ sub 2) ? )
               str
               (make-string (- sub (/ sub 2)) ? )))

      (t
       (concat (make-string sub ? ) str)))))


(defmacro estimate--with-row (&rest body)
  `(let* ((category (match-string-no-properties estimate-row-pos-category))
          (item     (match-string-no-properties estimate-row-pos-item))
          (price    (match-string-no-properties estimate-row-pos-price))
          (quantity (match-string-no-properties estimate-row-pos-quantity))
          (unit     (match-string-no-properties estimate-row-pos-unit))
          (subtotal (match-string-no-properties estimate-row-pos-total)))
     ,@body

     (setq max-category-width (max (string-width category)
                                   max-category-width))
     (setq max-item-width     (max (string-width item)
                                   max-item-width))
     (setq max-price-width    (max (string-width price)
                                   max-price-width))
     (setq max-quantity-width (max (string-width quantity)
                                   max-quantity-width))
     (setq max-unit-width     (max (string-width unit)
                                   max-unit-width))
     (setq max-total-width    (max (string-width subtotal)
                                   max-total-width))))


(defmacro estimate--kill-line ()
  `(save-excursion
     (beginning-of-line)
     (delete-region (point) (progn (end-of-line)(point)))))

(defmacro estimate--with-insert-row (row align-field &optional props &rest body)
  `(let ((marker   (nth 0 ,row))
         (category (nth 1 ,row))
         (item     (nth 2 ,row))
         (price    (nth 3 ,row))
         (quant    (nth 4 ,row))
         (unit     (nth 5 ,row))
         (subtotal (nth 6 ,row)))
     (goto-char marker)
     (estimate--kill-line)
     (insert
      (propertize
       (concat
        (estimate--column category max-category-width
                          (estimate-get-column-spec   :category ,align-field))
       " "
       (estimate--column item max-item-width
                         (estimate-get-column-spec   :item ,align-field))
       " "
       (estimate--column price max-price-width
                         (estimate-get-column-spec   :price ,align-field))
       " "
       (estimate--column quant max-quantity-width
                         (estimate-get-column-spec   :quantity ,align-field))
       " "
       (estimate--column unit max-unit-width
                         (estimate-get-column-spec   :unit ,align-field))
       (propertize
        (concat
         " : "
         (estimate--column subtotal max-total-width
                           (estimate-get-column-spec :subsubtotal
                                                     ,align-field)))
        'face 'estimate-subtotal-face
        'estimate-subtotal t
        'read-only      estimate-total-number-read-only
        'rear-nonsticky t
        ))
       ,@props))))


;;(estimate-mode-update-total-string 5000)
(defun estimate-mode-calc-and-update-total ()
  (interactive)
  (setq estimate-all-rows nil)
  (save-excursion

    (let ((total                0)
          (total-string       nil)
          (inhibit-read-only    t)
          (header-content     nil)
          (marker-footer      nil)
          (max-category-width   0)
          (max-item-width       0)
          (max-price-width      0)
          (max-unit-width       0)
          (max-quantity-width   0)
          (max-total-width      0))

      (goto-char (point-min))
      ;;============================================================
      ;; find and read table header
      ;;
      (when (re-search-forward "^-+$" nil t)
        (save-excursion
          (previous-line)
          (beginning-of-line)
          (when (re-search-forward estimate-header-regexp nil t)
            (estimate--with-row
             (setq header-content
                   (list (point-marker)
                         category
                         item
                         price
                         quantity
                         unit
                         subtotal))))))
      (next-line)


      ;;------------------------------------------------------------
      ;; read rows
      ;;
      (let ((continue t))
        (while continue
          (beginning-of-line)
          (let ((start (point))
                (end   (save-excursion (end-of-line) (point))))

            (cond
             ;;
             ;; rows end
             ;;
             ((re-search-forward "^-+$" end t)
              (estimate--kill-line)
              (setq continue nil))

             ;;
             ;; estimate row
             ;;
             ((re-search-forward estimate-row-regexp end t)
              (estimate--with-row
               (let* ((nquant    (estimate-read-number quantity))
                      (nprice    (estimate-read-number price))
                      (nsubtotal (* nquant nprice)))
                 (setq price    (estimate-format-number nprice))
                 (setq subtotal (estimate-format-number nsubtotal))
                 (setq total (+ total nsubtotal))
                 (setq estimate-all-rows
                       (cons (list (point-marker)
                                   category
                                   item
                                   price
                                   quantity
                                   unit
                                   subtotal)
                             estimate-all-rows)))))

             ;;
             ;; malformed row
             ;;
             ((and (re-search-forward estimate-malformed-row-regexp end t)
                   (plist-get
                    (text-properties-at (1+ (match-beginning 0)))
                    'estimate-total))
                (delete-region (match-beginning 0) (match-end 0))
                (end-of-line)
                (insert ":"))

             ;;
             ;; note row
             ;;
             (t))

            ;;
            ;; next-line or end-of-buffer
            ;;
            (condition-case nil (next-line)
              (end-of-buffer (setq continue nil))))))

      (setq total-string (estimate-format-number total))

      (setq max-total-width (max (string-width total-string)
                                 max-total-width))
      (setq marker-footer (point-marker))


      ;;
      ;; update-buffer
      ;;
      (let* ((row-width (+ max-category-width 1 max-item-width     1
                           max-price-width    1 max-quantity-width 1
                           max-unit-width     3 max-total-width    ))
             (rule  (propertize
                     (concat
                      (propertize (concat (make-string row-width ?-))
                                  'read-only t)
                       "\n")
                     'rear-nonsticky t
                     'face 'estimate-rule-face))
             (drule (propertize
                     (concat 
                      (propertize (make-string row-width ?=)
                                  'read-only t)
                      "\n")
                     'rear-nonsticky t
                     'face 'estimate-rule-face)))

        ;;
        ;; update-header
        ;;
        (when header-content
          (save-excursion
            (estimate--with-insert-row header-content :h-align)
            (save-excursion
              (previous-line)
              (beginning-of-line)
              (estimate--kill-line)
              (delete-char 1)
              (insert drule))
            (next-line)
            (estimate--kill-line)
            (insert rule)
            (delete-char 1)
            ))

        ;;
        ;; update-rows
        ;;
        (save-excursion
          (let ((n 0))
            (dolist (row (reverse estimate-all-rows))
              (eval
               `(estimate--with-insert-row
                 row :align
                 ,(if (< (mod n 4) 2) '('face 'estimate-zebra-face) ())))
              (setq n (1+ n)))))

        ;;
        ;; update total
        ;;
        (goto-char marker-footer)
        (beginning-of-line)
        (estimate--kill-line)(delete-backward-char 1)
        (insert
         rule
         (propertize
          (concat
           (estimate--column (estimate-get-column-name :total)
                             (- row-width max-total-width 3)
                             (estimate-get-column-align :total))
           " : "
           (estimate--column total-string max-total-width 'right)
           "\n")
          'read-only      estimate-total-number-read-only
          'face           'estimate-total-face
          'rear-nonsticky t
          )
         )
        (when (re-search-forward "^=+$" nil t)
          (estimate--kill-line)(delete-backward-char 1))
        (insert drule)
        (delete-backward-char 1)
        );; end of updating block (let*)
      )));; end of procedure environment (let, save-excursion, defun)


;;;
;;; major mode
;;;
(require 'estimate-mode-commands)

(defsubst -estimate-mode-after-change-internal ()
  (if estimate-rearrange-flag
      (save-excursion
        (let ((buffer-undo-list t))
          (setq estimate-rearrange-flag nil)
          (estimate-mode-calc-and-update-total)
          (setq estimate-rearrange-flag t)
          ()))
    ()))

(defun -estimate-mode-after-change (beg end old-len)
  (-estimate-mode-after-change-internal))


(defconst anything-c-source-estimate-source-jsons
  '((name       . "estimate-source-json")
    (candidates . estimate-source-jsons)
    (action . (("Edit" . estimate-edit-source-json)))))

(defun -estimate-mode-init-buffer ()
  (let ((pre-str (buffer-substring-no-properties (point-min)(point-max))))
    (-estimate-mode-after-change-internal)
    (let ((new-str (buffer-substring-no-properties (point-min)(point-max))))
      (when (string= pre-str new-str)
        (set-buffer-modified-p nil)))))


(define-derived-mode estimate-mode fundamental-mode "estimate"
  ""
  (make-variable-buffer-local 'estimate-rearrange-flag)
  (unless estimate-source-loaded
    (setq estimate-source-loaded t)
    (estimate-load-sources))
  (make-variable-buffer-local 'estimate-all-rows)
  (make-variable-buffer-local 'after-change-functions)
  (add-hook 'after-change-functions '-estimate-mode-after-change)
  (-estimate-mode--init-buffer))

(defun estimate-insert-template ()
  (interactive)
  )


;;(add-to-list 'auto-mode-alist '("\\.estimate\\'" . estimate-mode))

(provide 'estimate-mode)
;;; estimate-mode.el ends here
