;;; lisp/lg-eww.el -*- lexical-binding: t; -*-

(require 'emms)
(require 'eww)

(defun eww-mpv-video-at-point ()
  "View video in url at point in mpv"
  (interactive)
  (emms-play-url (thing-at-point 'url)))

(defun eww-mpv-video-at-current-url ()
  "View video in url at point in mpv"
  (interactive)
  (emms-play-url (eww-current-url)))

(defun eww-mpv-audio-at-point ()
  (interactive)
  "View video in url at point in mpv"
  (mpv-a-open (thing-at-point 'url)))

(defun eww-browse-with-history (arg)
  (interactive "P")
  (let ((url (completing-read "EWW: " eww-prompt-history)))
    (eww url (if arg 4 nil))))

(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(defun prot-eww-jump-to-url-on-page ()
  "Jump to URL position on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (let ((links))
      (save-excursion
        (goto-char (point-max))
        (while (text-property-search-backward 'shr-url nil nil t)
          (when (and (get-text-property (point) 'shr-url)
                     (not (get-text-property (point) 'eww-form)))
            (push (format "%s  @ %s ~ %d"
                          (button-label (point))
                          (propertize (get-text-property (point) 'shr-url) 'face 'link)
                          (point))
                  links))))
      (let* ((selection (completing-read "Jump to URL on page: " links nil t))
             (position (replace-regexp-in-string ".*~ " "" selection))
             (point (string-to-number position)))
        (goto-char point)
        (prot-pulse-pulse-line)))))

(provide 'lg-eww)
