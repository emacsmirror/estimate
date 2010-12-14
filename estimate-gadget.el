;;; estimate-gadget.el --- 

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



(defun estimate-edit-source-json (json)
  (interactive (list (case (length estimate-source-jsons)
                       (0 (error "estimate-source-jsons is not defined."))
                       (1 (car estimate-source-jsons))
                       (t
                        (anything :sources
                                  '(anything-c-source-estimate-source-jsons)
                                  :buffer "*estimate-edit-source-json*")
                        nil))))
  (when json
    (find-file json)))


(provide 'estimate-gadget)
;;; estimate-gadget.el ends here
