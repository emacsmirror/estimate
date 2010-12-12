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

;;;
;;; options
;;;
(defvar estimate-source-jsons nil)
(defvar estimate-total-number-read-only t)

(defvar estimate-consumption-tax-rate 0.05)

(defconst estimate-column-spec
  '(
    :category (
               :name "分類"
               :align right
               )

    :item     (
               :name "品目"
               :align left
               )

    :price    (
               :name "単価"
               :align right
               )

    :quantity (
               :name "分量"
               :align right
               )

    :unit     (
               :name "単位"
               :align left
               )

    :subtotal (
               :name "小計"
               :align right
               )

    :total    (
               :name "合計"
               :align right
               )

    :total-with-tax (
                     :name "税込合計"
                     :align right
                     )
    
    ))

(defun estimate-get-column-spec (p1 p2)
  (plist-get (plist-get estimate-column-spec p1) p2))

(defun estimate-get-column-name (p1)
  (estimate-get-column-spec p1 :name))
(defun estimate-get-column-align (p1)
  (estimate-get-column-spec p1 :align))

(defun estimate-set-column-spec (prop spec)
  (dolist (field (list :category :item :price
                       :quantity :unit :subtotal
                       :total :total-with-tax))
    (let ((data (plist-get opt field)))
      (when data
        (set-plist (plist-get estimate-set-column-align field) prop data)))))
(defun estimate-set-diplay-name (&rest opt)
  (estimate-set-column-spec :name opt))
(defun estimate-set-column-align (&rest opt)
  (estimate-set-column-spec :align opt))

;;;
;;; faces
;;;

(defface estimate-subtotal-face
  '(
    (((class color) (min-colors 16) (background light))
     (:foreground "#36C")
     )
    )
  ""
  )
;;(put 'estimate-subtotal-face 'face-defface-spec nil)
;;(symbol-plist 'estimate-subtotal-face)


(defface estimate-total-face
  '(
    (((class color) (min-colors 16) (background light))
     (:foreground "#693"))
    )
  "")

(defface estimate-rule-face
  '(
    (((class color) (min-colors 16) (background light))
     (:foreground "#C33"))
    )
  "")


;;;
;;; local variables
;;;
;;(setq estimate-total-number-read-only nil)
(defconst estimate-source            nil)
(defconst estimate-all-items          ())
(defconst estimate-default-quantities ())
(defconst estimate-rearrange-flag      t)
(defconst estimate-with-tax            t)

;;;
;;; row forfmats
;;;
(defconst estimate-number-regexp "[0-9]+\\|[0-9]+\\.[0-9]+")
(defconst estimate-row-regexp
  (concat
   "^\\s *"
   "\\([^ \t]+\\)\\s +"
   "\\([^ \t]+\\)\\s +"
   "\\(" estimate-number-regexp "\\)\\s +"
   "\\(" estimate-number-regexp "\\)\\s *"
   "\\([^0-9 \t]*\\)\\s *"
   "\\(:\\s *\\(\\(" estimate-number-regexp "\\)\\s *\\)?\\)$")
  )
(defconst estimate-row-pos-category 1)
(defconst estimate-row-pos-item     2)
(defconst estimate-row-pos-price    3)
(defconst estimate-row-pos-quantity 4)
(defconst estimate-row-pos-unit     5)
(defconst estimate-row-pos-total    7)

(defconst estimate-malformed-row-regexp ":\\s *[0-9]+\\s *$")
(defconst estimate-rule-regexp  "-+")
(defconst estimate-drule-regexp "=+")
(defconst estimate-header-regexp
  (concat
   "^\\s *"
   "\\([^ \t]+\\)\\s +"
   "\\([^ \t]+\\)\\s +"
   "\\([^ \t]+\\)\\s +"
   "\\([^ \t]+\\)\\s +"
   "\\([^ \t]+\\)\\s +"
   "\\(:\\)\\s *"
   "\\([^ \t]+\\)\\s *$"
   )
  )

;;;
;;; loading sources
;;;
(defun estimate-load-sources--add-source (alist)
  (dolist (slot alist)
    (let* ((category  (car slot))
           (content   (cdr slot))
           (prestored (or (assoc category estimate-source)
                          (let ((tmp (list category)))
                            (setq estimate-source (cons tmp estimate-source))
                            tmp))))
      (dolist (item content)
        (let* ((name      (car item))
               (detail    (cdr item))
               (item-slot (or (assoc name (cdr prestored))
                              (let ((tmp (list name)))
                                (setcdr prestored
                                         (cons tmp (cdr prestored)))
                                tmp))))
          (setcdr item-slot detail))))))

(defun estimate-load-sources ()
  (interactive)
  (progn
    (setq estimate-source             nil
          estimate-all-items          nil
          estimate-default-quantities nil)

    (dolist (json estimate-source-jsons)
      (estimate-load-sources--add-source (json-read-file json)))
    (dolist (catslot (reverse estimate-source))
      (let ((category (car catslot)))
        (dolist (itemslot (reverse (cdr catslot)))
          (let* ((item    (car itemslot))
                 (price   (cdr (or (assoc 'price   (cdr itemslot))
                                   (cons nil 0))))
                 (unit    (cdr (or (assoc 'unit    (cdr itemslot))
                                   (cons nil ""))))
                 (default (cdr (or (assoc 'default (cdr itemslot))
                                   (cons nil 1))))
                 (item-format (format "%s %s %s %%d %s"
                                      category item price unit)))
            (setq estimate-default-quantities
                  `(,item-format ,default . estimate-default-quantities))
            (setq estimate-all-items
                  (cons item-format
                        estimate-all-items))))))))

(defconst anything-c-source-estimate-items
  `((name       . "Estimate items")
    (init       . (lambda ()))
    (candidates . estimate-all-items)
    (action     . (("Insert" . estimate-do-insert-item)))))

(defconst estimate-current-estimate-buffer nil)


(defun estimate-do-insert-item (item-format)
  (save-excursion
    (set-buffer estimate-current-estimate-buffer)
    (insert (format item-format
                    (read-number "How mutch? "
                                 (plist-get estimate-default-quantities
                                            item-format))) " :\n")))

(defun estimate-insert-item ()
  (interactive)
  (let ((estimate-current-estimate-buffer (current-buffer)))
    (anything
     :sources '(anything-c-source-estimate-items)
     :buffer  "*Estimate item*")))

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
  `(let* ((category (match-string estimate-row-pos-category))
          (item     (match-string estimate-row-pos-item))
          (price    (match-string estimate-row-pos-price))
          (quantity (match-string estimate-row-pos-quantity))
          (unit     (match-string estimate-row-pos-unit))
          (subtotal (match-string estimate-row-pos-total)))
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

(defmacro estimate--with-insert-row (row &rest body)
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
      (estimate--column category max-category-width
                        (estimate-get-column-align          :category))
      " "
      (estimate--column item max-item-width
                        (estimate-get-column-align          :item))
      " "
      (estimate--column price max-price-width
                        (estimate-get-column-align          :price))
      " "
      (estimate--column quant max-quantity-width
                                 (estimate-get-column-align :quantity))
      " "
      (estimate--column unit max-unit-width
                        (estimate-get-column-align          :unit))
      (propertize
       (concat
        " : "
        (estimate--column subtotal max-total-width
                          (estimate-get-column-align        :subsubtotal)))
       'face 'estimate-subsubtotal-face
       'estimate-subtotal t
       'read-only      estimate-total-number-read-only
       'rear-nonsticky t
       ))))


;;(estimate-mode-update-total-string 5000)
(defun estimate-mode-calc-and-update-total ()
  (interactive)
  (save-excursion

    (let ((total              0)
          (total-string       nil)
          (inhibit-read-only  t)
          (all-rows           nil)
          (header-content     nil)
          (marker-footer      nil)
          (max-category-width 0)
          (max-item-width     0)
          (max-price-width    0)
          (max-unit-width     0)
          (max-quantity-width 0)
          (max-total-width    0)
          )

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
               (let* ((nquant    (string-to-number quantity))
                      (nprice    (string-to-number price))
                      (nsubtotal (* nquant nprice)))
                 (setq subtotal (format "%d" nsubtotal))
                 (setq total (+ total nsubtotal))
                 (setq all-rows
                       (cons (list (point-marker)
                                   category
                                   item
                                   price
                                   quantity
                                   unit
                                   subtotal)
                             all-rows)))))

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

      (setq total-string (format "%d" total))
      (setq max-total-width (max (string-width total-string)
                                 max-total-width))
      (setq marker-footer (point-marker))


      ;;
      ;; update-buffer
      ;;
      (let* ((row-width (+ max-category-width
                           1
                           max-item-width
                           1
                           max-price-width
                           1
                           max-quantity-width
                           1
                           max-unit-width
                           3
                           max-total-width))
             (rule  (propertize (concat (make-string row-width ?-) "\n")
                                'read-only t
                                'rear-nonsticky t
                                'face 'estimate-rule-face))
             (drule (propertize (concat (make-string row-width ?=) "\n")
                                'read-only t
                                'rear-nonsticky t
                                'face 'estimate-rule-face)))

        ;;
        ;; update-header
        ;;
        (when header-content
          (save-excursion
            (estimate--with-insert-row header-content)
            (next-line)
            (estimate--kill-line)
            (insert rule)
            ))

        ;;
        ;; update-rows
        ;;
        (save-excursion
          (dolist (row all-rows)
            (estimate--with-insert-row row)))

        ;;
        ;; update total
        ;;
        (goto-char marker-footer)
        (beginning-of-line)
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (insert
         rule
         (propertize
          (concat
           (estimate--column (estimate-get-column-name :total)
                             (- row-width max-total-width 3)
                             (estimate-get-column-align :total))
           " : "
           (estimate--column total-string max-total-width 'right))
          'read-only      estimate-total-number-read-only
          'face           'estimate-total-face
          'rear-nonsticky t
          ))
        
        );; end of updating block (let)
      )));; end of procedure environment (let, save-excursion, defun)


;;;
;;; major mode
;;;
(defconst estimate-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c i") 'estimate-insert-item)
    km))

(defun estimate--after-change (beg end old-len)
  (if estimate-rearrange-flag
      (progn
        (setq estimate-rearrange-flag nil)
        (estimate-mode-calc-and-update-total)
        (setq estimate-rearrange-flag t)
        ())
    ()))


(define-derived-mode estimate-mode fundamental-mode "estimate"
  ""
  (make-variable-buffer-local 'estimate-source)
  (make-variable-buffer-local 'estimate-all-items)
  (make-variable-buffer-local 'estimate-default-quantities)
  (make-variable-buffer-local 'estimate-rearrange-flag)
  
  (estimate-load-sources)
  (make-variable-buffer-local 'after-change-functions)
  (add-hook 'after-change-functions 'estimate--after-change)
  (estimate--after-change (point-min) (point-max) 0))


;;(add-to-list 'auto-mode-alist '("\\.estimate\\'" . estimate-mode))

(provide 'estimate-mode)
;;; estimate-mode.el ends here
 
