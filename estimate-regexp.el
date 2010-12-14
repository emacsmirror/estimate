;;; estimate-regexp.el --- 

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


(provide 'estimate-regexp)
;;; estimate-regexp.el ends here
