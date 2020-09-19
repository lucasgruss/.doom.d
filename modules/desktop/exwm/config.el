(defun my/volume-down ()
  "Lower volume of the computer"
  (interactive)
  (shell-command "~/scripts/volume.sh down"))

(defun my/volume-up ()
  "Increase volume of the computer"
  (interactive)
  (shell-command "~/scripts/volume.sh up"))

(defun my/brightness-down ()
  "Lower brightness of the screen"
  (interactive)
  (shell-command "xbacklight -dec 10"))

(defun my/brightness-up ()
  "Increase brightness of the screen"
  (interactive)
  (shell-command "xbacklight -inc 10"))

(use-package! exwm-randr
  :config
  ;;(setq! exwm-randr-workspace-output-plist '(1 "VGA1"))
  (setq! exwm-randr-workspace-output-plist '(1 "DP2"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output DP2 --right-of eDP1")))
               ;;"xrandr" nil "xrandr --output VGA1 --left-of LVDS1")))
  (exwm-randr-enable))

(use-package! exwm-systemtray
  :disabled
  :config
  (setq! exwm-systemtray-height 15)
  ;;exwm-systemtray-icon-gap 3)
  ;;exwm-systemtray-background-color 'black)
  (exwm-systemtray-enable))

(use-package! exwm
  :init
  (setq! exwm-firefox-buffer-name "Firefox-esr")
  (map!
   :map exwm-mode-map
   :localleader
   :desc "Toggle mode-line"       "m" #'exwm-layout-toggle-mode-line
   :leader
   (:prefix ("e" . "EXWM")
    :desc "Attach minibuffer"      "a" #'exwm-workspace-attach-minibuffer
    :desc "Detach minibuffer"      "d" #'exwm-workspace-detach-minibuffer
    :desc "Fullscreen"             "f" #'exwm-layout-set-fullscreen
    :desc "Floating hide"          "h" #'exwm-layout-set-fullscreen
    :desc "Release keyboard"       "k" #'exwm-input-release-keyboard
    :desc "Send next key"          "q" #'exwm-input-send-next-key
    :desc "Reset"                  "r" #'exwm-reset
    :desc "Toggle floating layout" "t" #'exwm-floating-toggle-floating
    :desc "Workspace move window"  "w" #'exwm-workspace-move-window))

  :config
  (defun my/exwm-async-run (name)
    "Run a process asynchronously"
    (interactive)
    (start-process name nil name))

  (defun my/invoke-firefox ()
    "If firefox exists, switch to its buffer or else launch it"
    (interactive)
    ;;(let ((firefox-string)))
    (if (string= (buffer-name) exwm-firefox-buffer-name)
        (bury-buffer)
      (if (get-buffer exwm-firefox-buffer-name)
          (progn
            (exwm-workspace-switch-to-buffer exwm-firefox-buffer-name)
            (my/exwm-firefox-force-fullscreen))
        (my/exwm-async-run "firefox"))))

  (defun my/switch-to-firefox-open-new-tab ()
    "Switch to firefox and then open a new tab."
    (interactive)
    (my/invoke-firefox)
    (exwm-firefox-core-tab-new))

  (defun my/invoke-firefox-other-window ()
    "Invoke Firefox in a new window"
    (interactive)
    (split-window-right)
    (other-window 1)
    (my/invoke-firefox))

  (defun my/invoke-spotify ()
    "If spotify exists, switch to its buffer or else launch it"
    (interactive)
    (if (string= (buffer-name) "Spotify")
        (bury-buffer)
      (if (get-buffer "Spotify")
          (exwm-workspace-switch-to-buffer "Spotify")
        (my/exwm-async-run "spotify"))))

  (defun my/invoke-spotify-other-window ()
    "Invoke spotify in a new window"
    (interactive)
    (split-window-right)
    (other-window 1)
    (my/invoke-spotify))

  (defun my/launch-process (command)
    "Launch a process"
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (defun my/toggle-line-char-modes ()
    "If on a EXWM buffer, toggle 'line' or 'char'"
    (interactive)
    (if exwm-window-type
        (if (string= exwm--input-mode "line-mode")
            (call-interactively #'exwm-input-release-keyboard) ; switch to char mode
          (call-interactively #'exwm-input-grab-keyboard)))) ; switch to line mode

  (defun my/toggle-panel ()
    (interactive)
    (shell-command "cd && pgrep xfce4-panel && xfce4-panel -q || xfce4-panel &"))

  (defun my/lock-screen ()
    "Lock screen with slock"
    (interactive)
    (start-process "" nil "/usr/local/bin/slock"))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
                                        ;(add-hook 'exwm-manage-finish-hook 'exwm-layout-hide-mode-line)

  (setq exwm-input-global-keys
        `(;; EXWM
          ([S-s-backspace] . exwm-workspace-delete)
          ([?\s-f] . exwm-layout-set-fullscreen)
          ([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-b] . counsel-switch-buffer)
          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([s-f2]  . my/lock-screen)
          ([?\s-d] . counsel-linux-app)
          ([?\s-i] . my/invoke-firefox)
          ([?\s-I] . my/invoke-firefox-other-window)
          ([?\s-m] . my/toggle-line-char-modes)
          ([?\s-s] . my/invoke-spotify)
          ([s-return] . +term/toggle)
          ([s-backspace] . my/kill-this-buffer)
          ;; Everything window
          ([?\s-q] . evil-window-delete)
          ([s-tab]  . windower-switch-to-last-buffer)
          ([?\s-\\] . windower-toggle-split)
          ([?\s-o]  . windower-toggle-single)
          ([142606440] . windower-move-border-left) ; M-s-h
          ([142606442] . windower-move-border-below); M-s-j
          ([142606443] . windower-move-border-above); M-s-k
          ([142606444] . windower-move-border-right); M-s-l
          ([?\s-h] . windmove-left)  ([?\s-H] . windower-swap-left)
          ([?\s-j] . windmove-down)  ([?\s-J] . windower-swap-below)
          ([?\s-k] . windmove-up)    ([?\s-K] . windower-swap-above)
          ([?\s-l] . windmove-right) ([?\s-L] . windower-swap-right)
          ;; Media keys
          ([XF86MonBrightnessUp]   . my/brightness-up)
          ([XF86MonBrightnessDown] . my/brightness-down)
          ([XF86AudioRaiseVolume]  . my/volume-up)
          ([XF86AudioLowerVolume]  . my/volume-down))
        exwm-workspace-show-all-buffers t
        exwm-workspace-number 1
        exwm-workspace-minibuffer-position nil
        exwm-workspace-display-echo-area-timeout 1)

  (push (aref (kbd "<escape>") 0) exwm-input-prefix-keys)
  ;; (setq! exwm-input-simulation-keys
  ;;   '(([?\M-h] . [left])
  ;;     ([?\M-l] . [right])
  ;;     ([?\M-k] . [up])
  ;;     ([?\M-j] . [down])
  ;;     ;([?\C-a] . [home])
  ;;     ;([?\C-e] . [end])
  ;;     ;([?\M-v] . [prior])
  ;;     ;([?\C-v] . [next])
  ;;     ;([?\C-d] . [delete])
  ;;     ([?\C-k] . [S-end delete])))
  (exwm-enable))

(use-package! exwm-firefox-evil
  :hook ((exwm-manage-finish . exwm-firefox-evil-activate-if-firefox)
         ;; (exwm-manage-finish . my/exwm-firefox-force-fullscreen)
         (exwm-firefox-evil-mode . my/exwm-firefox-hook))
  :config
  (setq exwm-firefox-evil-class-name '("Firefox" "Firefox-esr"))
  (defun exwm-input--on-ButtonPress-line-mode (buffer button-event)
    "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on. BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
    (with-current-buffer buffer
      (let ((read-event (exwm-input--mimic-read-event button-event)))
        (exwm--log "%s" read-event)
        (if (and read-event
                 (exwm-input--event-passthrough-p read-event))
            ;; The event should be forwarded to emacs
            (progn
              (exwm-input--cache-event read-event)
              (exwm-input--unread-event button-event)
              xcb:Allow:ReplayPointer)
          ;; xcb:Allow:SyncPointer)
          ;; The event should be replayed
          xcb:Allow:ReplayPointer))))

  (defun my/exwm-firefox-force-fullscreen ()
    "Send F11 to firefox to always be in full screen.

Whenever you switch to another window and then come back to
firefox, it leaves fullscreen mode."
    (interactive)
    (exwm-input--fake-key 'f11))

  (defun my/exwm-firefox-toggle-tree-tab ()
    "Toggle the tree tab extension"
    (interactive)
    (exwm-input--fake-key 'f1))

  (defun my/exwm-firefox-hook ()
    "Sets firefox how I like it"
    (interactive)
    (when exwm-firefox-evil-mode
      (my/exwm-firefox-toggle-tree-tab)))

  (defun my/exwm-firefox-hint ()
    "Highlights hints on the page."
    (interactive)
    (exwm-input--fake-key 'C-m)
    (exwm-firefox-evil-insert))

  (define-key! 'normal exwm-firefox-evil-mode-map
    "f" #'my/exwm-firefox-hint
    "F" #'my/exwm-firefox-force-fullscreen
    "T" #'my/exwm-firefox-toggle-tree-tab
    "q" #'exwm-input-send-next-key))

(use-package! exwm-firefox
  :after exwm-firefox-evil
  :config
  ;; I have different keybinding in firefox for tabdetach-attach : M-S-t
  (defun my/exwm-firefox-attach ()
    "Attach the current tab into its parent window.

   This requires the tabdetach extension to work."
    (interactive)
    (exwm-input--fake-key ?\M-\S-T))

  (define-key! 'normal exwm-firefox-evil-mode-map
    "A" #'my/exwm-firefox-attach
    "D" #'exwm-firefox-split-detach
    "M" #'exwm-firefox-merge)
  ;; I don't like renaming the name of the firefox window
  (remove-hook 'exwm-update-title-hook 'exwm-firefox--update-title))
