;;; config.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  harmony

;; Author: harmony <harmony@hamnix.ca>
;; Keywords: 

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

;;; Commentary:

;; 

;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/" rational-config-path))
(require 'rational-use-package)
(require 'packages) ;; all my packages loaded here

;; set up no-littering immediately
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)

(require 'rational-defaults)
(require 'rational-ui)
(require 'rational-completion)
(require 'rational-evil)
;; everything rational loaded before this point


;; ctrl-g often enters `:g` with dual-function settings
;; this allows entering a useful keyboard-quit anyways
(evil-ex-define-cmd "g" #'keyboard-quit)

(require 'ft-leader)

(setq ft-leader-mod-alist
  '((nil . "C-")
    ("m" . "M-")
    ("SPC". "")
    ("<XF86Launch9>" . "")
    ("RET" . "")))

(require 'exwm)
(require 'exwm-randr)
(require 'exwm-config)
(exwm-config-example)

(setq split-height-threshold 60)
(setq split-width-threshold 160)

(defun juri-split-window-sensibly (&optional window)
  "replacement `split-window-sensibly' function which prefers vertical splits"
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             (select-window (with-selected-window window
                              (split-window-right))))
        (and (window-splittable-p window)
             (select-window (with-selected-window window
                              (split-window-below)))))))

(defun juri-shellcommand (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(setq exwm-input-global-keys
      `((,(kbd "s-w") . exwm-workspace-switch)
	(,(kbd "s-f") . exwm-layout-toggle-fullscreen)
	(,(kbd "s-p") . exwm-floating-toggle-floating)
	(,(kbd "s-w") . exwm-workspace-switch)

	(,(kbd "<XF86Launch9>") . ft-leader-mode-exec)

        (,(kbd "s-d") . juri-shellcommand)))

(defun embark-ft-keymap ()
  (interactive)
  (embark-bindings-in-keymap ft-current-keymap))
(define-key ft-fallback-map (kbd "C-h") 'embark-ft-keymap)
(define-key ft-fallback-map (kbd "C-?") 'embark-ft-keymap)
(define-key ft-fallback-map (kbd "C-!") 'embark-ft-keymap)

(define-key ft-main-map (kbd "C-d") 'juri-shellcommand)

(defvar window-map (make-sparse-keymap))
(define-key ft-main-map (kbd "C-w") window-map)
(define-key window-map (kbd "C-u") 'juri-split-window-sensibly)
(define-key window-map (kbd "C-f") 'delete-other-windows)

(define-key window-map (kbd "C-q") 'delete-window)

(define-key window-map (kbd "C-l") 'exwm-workspace-move-window)
(define-key window-map (kbd "C-y") 'exwm-workspace-move)
(define-key window-map (kbd "C-:") 'exwm-workspace-switch)

;; evil window management is very good within workspaces.
;; all the regular C-w bindings work as well, these just override
(define-key window-map (kbd "C-n") 'evil-window-left)
(define-key window-map (kbd "C-e") 'evil-window-down)
(define-key window-map (kbd "C-i") 'evil-window-up)
(define-key window-map (kbd "C-o") 'evil-window-right)
(define-key window-map (kbd "n") 'evil-window-move-far-left)
(define-key window-map (kbd "e") 'evil-window-move-very-bottom)
(define-key window-map (kbd "i") 'evil-window-move-very-top)
(define-key window-map (kbd "o") 'evil-window-move-far-right)

(setq exwm-manage-configurations '((t char-mode t)))
(exwm-enable)
(exwm-randr-enable)
(require 'exwm-systemtray)
(exwm-systemtray-enable)

(require 'vterm)
(dolist (i exwm-input-global-keys)
  (define-key vterm-mode-map (car i) (cdr i)))

(set-face-attribute
 'default nil
 :family "IBMPlexMono" :height 185
 :weight 'normal :width 'normal)

(load-theme 'modus-operandi t)

(projectile-mode +1)
(winner-mode t)

(provide 'config)
;;; config.el ends here
