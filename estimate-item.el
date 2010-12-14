;;; estimate-item.el --- 

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
;;; session global variables
;;;
(defconst estimate-candidate         nil)
(defconst estimate-source-loaded     nil)
(defconst estimate-source            nil)
(defconst estimate-all-items          ())
(defconst estimate-default-quantities ())

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



(provide 'estimate-item)
;;; estimate-item.el ends here
