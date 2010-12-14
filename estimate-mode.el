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
;;; model options (application scope)
;;;
(defvar estimate-source-jsons           nil)
(defvar estimate-total-number-read-only t)
(defvar estimate-consumption-tax-rate   0.05)

;;;
;;; model variables (buffer scope)
;;;
(defvar estimate-all-rows nil)
(defvar estimate-candidate nil)

;;;
;;; controler variables (buffer scope)
;;;

(defconst estimate-rearrange-flag      t)
(defconst estimate-with-tax            t)


;;;
;;; display options
;;;
(defconst estimate-column-spec
  '(
    :category (
               :name "分類"
               :align right
               :h-align center
               )

    :item     (
               :name "品目"
               :align left
               :h-align center
               )

    :price    (
               :name "単価"
               :align right
               :h-align center
               )

    :quantity (
               :name "分量"
               :align right
               :h-align center
               )

    :unit     (
               :name "単位"
               :align left
               :h-align center
               )

    :subtotal (
               :name "小計"
               :align right
               :h-align center
               )

    :total    (
               :name "合計"
               :align right
               :h-align center
               )

    :total-with-tax (
                     :name "税込合計"
                     :align right
                     :h-align center
                     )
    
    ))

(defun estimate-get-column-spec (p1 p2)
  (plist-get (plist-get estimate-column-spec p1) p2))
(defun estimate-get-column-name (p1)
  (estimate-get-column-spec p1 :name))
(defun estimate-get-column-align (p1)
  (estimate-get-column-spec p1 :align))
(defun estimate-get-column-h-align (p1)
  (estimate-get-column-spec p1 :h-align))

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
(defun estimate-set-column-h-align (&rest opt)
  (estimate-set-column-spec :h-align opt))

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
     (:foreground "#463"))
    )
  "")
;;(put 'estimate-total-face 'face-defface-spec nil)

(defface estimate-rule-face
  '(
    (((class color) (min-colors 16) (background light))
     (
      :foreground "#D33"
      :strike-through t
     ))
    )
  "")

(defface estimate-zebra-face
  '(
    (((class color) (min-colors 16) (background light))
     (:background "#EEE"))
    )
  "")

;;;
;;; session global variables
;;;
(defconst estimate-source-loaded     nil)
(defconst estimate-source            nil)
(defconst estimate-all-items          ())
(defconst estimate-default-quantities ())


;;;
;;; row forfmats
;;;
(defconst estimate-number-regexp "[0-9,]+")
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
   "\\([^\000- ]+\\)\\s +"
   "\\([^\000- ]+\\)\\s +"
   "\\([^\000- ]+\\)\\s +"
   "\\([^\000- ]+\\)\\s +"
   "\\([^\000- ]+\\)\\s +"
   "\\(:\\)\\s *"
   "\\([^\000- ]+\\)\\s *$"
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
  (setq estimate-source    nil)
  (setq estimate-all-items nil)
  (setq estimate-default-quantities nil)
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

(defsubst estimate-read-number (str)
  (string-to-number (replace-regexp-in-string "," "" str)))

(defsubst estimate-format-number (num)
  (let ((str (format "%d," num)))
    (while (string-match "[0-9][0-9][0-9][0-9]" str)
      (setq str (replace-regexp-in-string "\\([0-9]\\)\\([0-9][0-9][0-9],\\)"
                                          "\\1,\\2"
                                          str)))
    (substring str 0 (1- (length str)))))

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
(defsubst estimate-on-estimate-row-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward estimate-row-regexp
                       (save-excursion (end-of-line)(point)) t)))


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

(defun estimate-increase-item (step)
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

(defun estimate-decrease-item (step)
  (interactive (list 1))
  (estimate-increase-item (- step)))

(defun estimate-up-item ()
  (interactive)
  (when (and (estimate-on-estimate-row-p)
             (save-excursion
               (previous-line)
               (estimate-on-estimate-row-p)))
    (let ((inhibit-read-only t)
          (estimate-rearrange-flag nil))
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
    (define-key km (kbd "M-<up>")   'estimate-increase-item)
    (define-key km (kbd "M-<down>") 'estimate-decrease-item)
    (define-key km (kbd "C-<up>")   'estimate-up-item)
    (define-key km (kbd "C-<down>") 'estimate-down-item)
    km))

(defun estimate--after-change (beg end old-len)
  (if estimate-rearrange-flag
      (save-excursion
        (let ((buffer-undo-list t))
          (setq estimate-rearrange-flag nil)
          (estimate-mode-calc-and-update-total)
          (setq estimate-rearrange-flag t)
          ()))
    ()))


(define-derived-mode estimate-mode fundamental-mode "estimate"
  ""
  (make-variable-buffer-local 'estimate-rearrange-flag)
  (unless estimate-source-loaded
    (setq estimate-source-loaded t)
    (estimate-load-sources))
  (make-variable-buffer-local 'estimate-all-rows)
  (make-variable-buffer-local 'after-change-functions)
  (add-hook 'after-change-functions 'estimate--after-change)
  (estimate--after-change (point-min) (point-max) 0))

(defun estimate-insert-template ()
  (interactive)
  )


;;(add-to-list 'auto-mode-alist '("\\.estimate\\'" . estimate-mode))

(provide 'estimate-mode)
;;; estimate-mode.el ends here
 
