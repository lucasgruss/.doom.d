;;; lisp/lg-centaur-tabs.el -*- lexical-binding: t; -*-

(define-minor-mode centaur-tabs-local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Centaur-Tabs mode is off."
  :group 'centaur-tabs
  :global nil
  (if (centaur-tabs-mode-on-p)
      (progn
;;; ON
        (if centaur-tabs-local-mode
            (if (and (local-variable-p centaur-tabs-display-line-format)
                     (eval centaur-tabs-display-line-format))

                ;; A local header line exists, hide it to show the tab bar.
                (progn
                  ;; Fail in case of an inconsistency because another local
                  ;; header line is already hidden.
                  (when (local-variable-p 'centaur-tabs--local-hlf)
                    (error "Another local header line is already hidden"))
                  (set (make-local-variable 'centaur-tabs--local-hlf)
                       (eval centaur-tabs-display-line-format))
                  (kill-local-variable centaur-tabs-display-line-format))
              ;; Otherwise hide the tab bar in this buffer.
              (set centaur-tabs-display-line-format nil))
;;; OFF
          (if (local-variable-p 'centaur-tabs--local-hlf)
              ;; A local header line is hidden, show it again.
              (progn
                (set centaur-tabs-display-line-format centaur-tabs--local-hlf)
                (kill-local-variable 'centaur-tabs--local-hlf))
            ;; The tab bar is locally hidden, show it again.
            (kill-local-variable centaur-tabs-display-line-format))))
    (message "Centaur-Tabs mode must be enabled")))

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((and (derived-mode-p 'exwm-mode)
          exwm-firefox-evil-mode)
     "Firefox")
    ((or (derived-mode-p 'matlab-mode)
         (derived-mode-p 'matlab-shell-mode)
         (derived-mode-p 'matlab-shell-help-mode)
         (string-match "MATLAB" (or exwm-class-name "")))
     "Matlab")
    ((memq major-mode '(emms-playlist-mode emms-browser-mode))
     "EMMS")
    ((or (derived-mode-p 'exwm-mode)
         (string-equal "*EXWM" (substring (buffer-name) 0 4)))
     "EXWM")
    ((or (derived-mode-p 'smudge-device-select-mode)
         (derived-mode-p 'smudge-track-search-mode)
         (derived-mode-p 'smudge-playlist-search-mode))
     "smudge")
    ((derived-mode-p 'eww-mode)
     "eww")
    ((derived-mode-p 'python-mode)
     "Python")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'vterm-mode)
     "Vterm")
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

(defun centaur-tabs--create-new-tab ()
  "Create a new tab"
  (interactive)
  (cond
   ((string= exwm-class-name "Firefox-esr")
    (exwm-firefox-core-window-new))
   ((eq major-mode 'exwm-mode)
    (call-interactively #'app-launcher-run-app))
   ((eq major-mode 'eshell-mode)
    (eshell t))
   ((eq major-mode 'vterm-mode)
    (vterm))
   ((eq major-mode 'term-mode)
    (ansi-term "/bin/bash"))
   ((derived-mode-p 'eww-mode)
    (let ((current-prefix-arg 4))
      (call-interactively #'eww-browse-with-history)))
   (t
    (call-interactively #'find-file))))

(provide 'lg-centaur-tabs)
