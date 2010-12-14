;;; estimate-display.el --- 

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


(provide 'estimate-display)
;;; estimate-display.el ends here
