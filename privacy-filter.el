;;; privacy-filter.el --- A digital "privacy filter" mode -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com)
;; Created: 2020/01/12
;; Keywords: faces, privacy
;; URL: https://github.com/riscy/privacy-filter
;; Package-Requires: ((emacs "24.4"))
;; Version: 0

;; This file is free software; you can redistribute or modify it under the terms
;; of the GNU General Public License <https://www.gnu.org/licenses/>, version 3
;; or any later version.  This software comes with with NO WARRANTY, not even
;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; This minor mode is like a digital version of the "privacy filter film"
;; you'd put on your screen to make life difficult for shoulder surfers.

;; Type M-x privacy-filter-mode to toggle the minor mode off and on.
;; You can also add the following magic comment to the top of your file:
;;   -*- eval: (privacy-filter-mode 1) -*-
;; ...enabling the filter to turn on automatically.

;;; Code:

(require 'color)

(defgroup privacy-filter nil
  "A digital 'privacy filter' mode."
  :prefix "privacy-filter-"
  :group 'text
  :link '(url-link :tag "URL" "https://github.com/riscy/privacy-filter")
  :link '(emacs-commentary-link :tag "Commentary" "privacy-filter.el"))

(defface privacy-filter-face nil
  "To propertize text with a color very close to the background color.
This color is modified dynamically based on the current background."
  :group 'privacy-filter)

(defcustom privacy-filter-amount 8
  "The percent difference between `privacy-filter-face' and the background.
Setting this to a smaller number makes private text harder to see."
  :type 'integer
  :link '(function-link privacy-filter--update-face)
  :group 'privacy-filter)
(put 'privacy-filter-amount 'safe-local-variable 'integerp)

(defvar-local privacy-filter--font-lock-p nil
  "Whether font-lock-mode was on when the privacy filter was activated.")

;;;###autoload
(define-minor-mode privacy-filter-mode
  "Toggle privacy-filter-mode on or off."
  :lighter "p "
  (if privacy-filter-mode (privacy-filter--on) (privacy-filter--off)))

(defun privacy-filter--on ()
  "Turn on the privacy filter (disable font-locks, overlays, etc.)."
  (setq-local privacy-filter--font-lock-p font-lock-mode)
  (font-lock-mode -1)
  (privacy-filter--update-face)
  (privacy-filter--remove-overlays)
  (privacy-filter--propertize-text))

(defun privacy-filter--off ()
  "Turn off the privacy filter and resume font-locking."
  (set-text-properties (point-min) (point-max) nil)
  (when privacy-filter--font-lock-p (font-lock-mode 1)))

(defun privacy-filter--update-face ()
  "Update `privacy-filter-face' relative to the current background color."
  (set-face-foreground
   'privacy-filter-face
   (if (eq (frame-parameter nil 'background-mode) 'dark)
       (color-lighten-name
        (face-attribute 'default :background) privacy-filter-amount)
     (color-darken-name
      (face-attribute 'default :background) privacy-filter-amount))))

(defun privacy-filter--remove-overlays ()
  "Remove additional fontification (i.e. overlays) from the buffer.
Also, prevent overlays from coming back by advising `overlay-put'."
  (dolist (o (overlays-in (point-min) (point-max))) (delete-overlay o))
  (advice-add #'overlay-put :around #'privacy-filter--hide-overlays))

(defun privacy-filter--propertize-text ()
  "Propertize all of the text in the buffer."
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil)
    (add-text-properties (point-min) (point-max) '(face privacy-filter-face))))

(defun privacy-filter--hide-overlays (overlay-put-function &rest args)
  "Advice to block OVERLAY-PUT-FUNCTION (ARGS)."
  (unless privacy-filter-mode (apply overlay-put-function args)))

(defun privacy-filter-unload-function ()
  "Pre-cleanup when `unload-feature' is called."
  (advice-remove #'overlay-put #'privacy-filter--hide-overlays)
  nil)

(provide 'privacy-filter)
;;; privacy-filter.el ends here
