;;; packages.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  harmony

;; Author: harmony <harmony@hamnix.ca>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

(straight-use-package 'exwm)
(straight-use-package 'xelb)
(straight-use-package 'evil)
(straight-use-package 'sudo-edit)
(straight-use-package 'vterm)
(straight-use-package 'magit)
(straight-use-package 'robe)
(straight-use-package 'inf-ruby)
(straight-use-package 'rspec-mode)
(straight-use-package 'direnv)
(straight-use-package 'no-littering)
(straight-use-package 'projectile)

(provide 'packages)
;;; packages.el ends here
