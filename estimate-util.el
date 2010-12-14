;;; estimate-util.el --- 

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


(defsubst estimate-read-number (str)
  (string-to-number (replace-regexp-in-string "," "" str)))

(defsubst estimate-format-number (num)
  (let ((str (format "%d," num)))
    (while (string-match "[0-9][0-9][0-9][0-9]" str)
      (setq str (replace-regexp-in-string "\\([0-9]\\)\\([0-9][0-9][0-9],\\)"
                                          "\\1,\\2"
                                          str)))
    (substring str 0 (1- (length str)))))

;;;
;;;
;;;
(defsubst estimate-on-estimate-row-p ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward estimate-row-regexp
                       (save-excursion (end-of-line)(point)) t)))



(provide 'estimate-util)
;;; estimate-util.el ends here
