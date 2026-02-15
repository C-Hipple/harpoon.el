;;; harpoon.el --- harpoon.el Allow quick hotkey switching to selected buffers -*- lexical-binding: t; -*-

;; Author: Chris Hipple
;; URL: https://github.com/C-Hipple/harpoon.el
;; Version: 1.0.5
;; Package-Requires: ((emacs "25.1"))

;; SPDX-License-Identifier: GPL-3.0+

;;; Commentary:

;; This package is an emacs port of the neovim plugin "harpoon" written by ThePrimeagen.
;; You can easily link files to a given hotkey and then jump your buffer to that file
;; with that hotkey.  Files can be hotswapped easily.

;;
;; Use Bind keys for harpoon-open-entry harpoon-add-file-or-buffer called with a number argument.
;; Then you can add files with that command, and then jump to them immediately.

;;; Code:

(require 'subr-x)

;;;###autoload
(setq harpoon--file (expand-file-name ".harpoon" user-emacs-directory))

;;;###autoload
(defun harpoon-open-entry (line-number)
  "Reads harpoon--file and opens the file on LINE-NUMBER in the existing buffer if it's a valid file."
  (interactive)
  (let ((harpoon-file (expand-file-name harpoon--file))
        file-to-open)
    (with-temp-buffer
      (insert-file-contents harpoon-file)
      (goto-char (point-min))
      (forward-line (1- line-number))
      (setq file-to-open (string-trim (buffer-substring (line-beginning-position) (line-end-position))))
      (if-let ((harpoon-to-buffer (get-buffer file-to-open)))
          (switch-to-buffer harpoon-to-buffer)
        (if (and (file-exists-p file-to-open)
                 (file-regular-p file-to-open))
            (find-file file-to-open)
          (message "No valid file on line %d of %s" line-number harpoon-file))))))


;;;###autoload
(defun harpoon-add-file-or-buffer (line-number)
  "Adds the current buffer to harpoon--file at LINE-NUMBER, replacing the existing line."
  (interactive)
  (let ((current-identifier (harpoon--get-buffer-identifier))
        (harpoon-file (expand-file-name harpoon--file)))
    (if (not current-identifier)
        (message "Buffer not visiting a file.")
      (let ((harpoon-buffer (find-file-noselect harpoon-file)))
        (with-current-buffer harpoon-buffer
          (goto-char (point-min))
          (forward-line (1- line-number))
          (let ((line-start (point))
                line-end)
            (setq line-end (line-end-position))
            (delete-region line-start line-end)
            (insert current-identifier)
            (save-buffer)
            (kill-buffer harpoon-buffer)
            (message "Replaced line %d of %s with '%s'" line-number harpoon-file current-identifier)))))))

(defun harpoon--get-buffer-identifier ()
  "Returns buffer filename if buffer else buffer name"
  (if (buffer-file-name)
      (buffer-file-name)
    (buffer-name)))

;;;###autoload
(defun harpoon-open-or-create ()
  "Opens (and creates if it doesn't exist) the harpoon--file in a minibuffer and moves the cursor to it."
  (interactive)
  (let ((file-path (expand-file-name harpoon--file)))
    (if (not (file-exists-p file-path))
        (with-current-buffer (find-file-noselect file-path)
          (save-buffer)
          (switch-to-buffer (current-buffer)))
      (find-file file-path))))

(provide 'harpoon)
