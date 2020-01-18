;;; privacy-mode.el --- A digital "privacy filter" -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: 2020/01/12
;; Keywords: faces, privacy
;; URL: https://github.com/riscy/privacy-mode
;; Package-Requires: ((emacs "24.4"))
;; Version: 0

;; This file is free software; you can redistribute or modify it under the terms
;; of the GNU General Public License <https://www.gnu.org/licenses/>, version 3
;; or any later version.  This software comes with with NO WARRANTY, not even
;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; This minor mode is like a digital version of the "privacy filter film"
;; you'd put on your screen to make life difficult for shoulder surfers.

;; Type M-x privacy-mode to toggle the minor mode off and on.
;; You can also add the following magic comment to the top of your file:
;;   -*- eval: (privacy-mode 1) -*-
;; ...enabling the filter to turn on automatically.

;;; Code:

(require 'color)
(require 'face-remap)

(defgroup privacy nil
  "A digital 'privacy filter'."
  :group 'text
  :prefix "privacy-"
  :link '(url-link :tag "URL" "https://github.com/riscy/privacy-mode")
  :link '(emacs-commentary-link :tag "Commentary" "privacy-mode.el"))

(defface privacy-face nil
  "A dynamically modified face to hide private text."
  :group 'privacy)

(defcustom privacy-amount 8
  "Percent difference between `privacy-face' and the background.
Setting this to a smaller number makes 'private' text harder to see."
  :type 'integer
  :group 'privacy
  :link '(function-link privacy--update-face))
(put 'privacy-amount 'safe-local-variable 'integerp)

(defvar-local privacy--font-lock-p nil
  "Whether font-lock-mode was on when the privacy filter was activated.")

(defvar-local privacy--cookie nil)

;;;###autoload
(define-minor-mode privacy-mode
  "Toggle privacy-mode on or off."
  :lighter " p"
  (if privacy-mode (privacy--enable) (privacy--disable)))

(defun privacy--enable ()
  "Turn on the privacy filter (disable font-locks, overlays, etc.)."
  (privacy--unfontify)
  (privacy--refontify))

(defun privacy--disable ()
  "Turn off the privacy filter and resume font-locking."
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (face-remap-remove-relative privacy--cookie)
      (when (or privacy--font-lock-p global-font-lock-mode)
        (font-lock-mode 1)))))

(defun privacy--unfontify ()
  "Unfontify the buffer and inhibit overlays by advising `overlay-put'."
  (let ((inhibit-read-only t))
   (with-silent-modifications (set-text-properties (point-min) (point-max) nil)))
  (dolist (o (overlays-in (point-min) (point-max))) (delete-overlay o))
  (advice-add #'overlay-put :around #'privacy--inhibit-overlays)
  (setq-local privacy--font-lock-p font-lock-mode)
  (font-lock-mode -1))

(defun privacy--refontify ()
  "Remap the default face to `privacy-face'.
That face is adjusted to match the background color."
  (let ((background (face-attribute 'default :background)))
    (set-face-foreground 'privacy-face
     (if (eq (frame-parameter nil 'background-mode) 'dark)
         (color-lighten-name background privacy-amount)
       (color-darken-name background privacy-amount))))
  (setq-local privacy--cookie
              (face-remap-add-relative 'default 'privacy-face)))

(defun privacy--inhibit-overlays (overlay-put-function &rest args)
  "Advice to inhibit OVERLAY-PUT-FUNCTION (invoked with ARGS)."
  (unless privacy-mode (apply overlay-put-function args)))

(defun privacy-unload-function ()
  "Pre-cleanup when `unload-feature' is called."
  (advice-remove #'overlay-put #'privacy--inhibit-overlays)
  nil)

(provide 'privacy-mode)
;;; privacy-mode.el ends here
