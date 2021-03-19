;;; exwm-utils.el -*- lexical-binding: t; -*-

;; Author: Lucas Gruss (lucas.gruss@gmail.com)
;; Created: November 26, 2020
;; Keywords: exwm, convenience
;;
;; This file is not a part of GNU Emacs.
;;
;; This is a collection of functions to extend EXWM and change some of its
;; behavior
;;
;; Version 0.10
;;
;; This software is licensed under the GPL version 3.
;;
;; To install:
;;   (require 'exwm-utils)
;;
;; See README for more information

;;; Code:
(require 'exwm)
(require 'exwm-workspace)

(defun exwm-utils-workspace-switch-cyclically ()
  "Cycle through workspaces"
  (interactive)
  (other-frame +1))

(defun exwm-utils-workspace-move-cyclically ()
  "Move the current window to the next exwm workspace"
  (interactive)
  (exwm-workspace-move-window
   (or (nth (+ exwm-workspace-current-index 1) exwm-workspace--list)
       (car exwm-workspace--list)))
  (exwm-utils-workspace-switch-cyclically))

(defun exwm-utils-workspace-switch-to-buffer (buffer-or-name)
  "Extend exwm-workspace-switch-to-buffer to move the window to
  the current window instead of switching to the corresponding
  workspace. This enables x windows to behave like regular windows"
(interactive
   (let ((inhibit-quit t))
     (prog1
         (with-local-quit
           (list (get-buffer (read-buffer-to-switch "Switch to buffer: ")))))))
  (with-current-buffer buffer-or-name
    (when (derived-mode-p 'exwm-mode)
      (exwm-workspace-move-window exwm-workspace-current-index exwm--id))
    (exwm-workspace-switch-to-buffer buffer-or-name)))
