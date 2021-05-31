;;; lisp/lg-elfeed.el -*- lexical-binding: t; -*-

(require 'elfeed)
(require 'emms)

(defun elfeed-view-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (shell-command "killall mpv")
  (setq emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display"))
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             ;;do (emms-play-url it))
             do (start-process "mpv" " *elfeed-mpv*" "mpv" it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-listen-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (shell-command "killall mpv")
  (setq emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--vid=no"))
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (emms-play-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(provide 'lg-elfeed)
