;;; harpoon.el --- harpoon.el Allow quick hotkey switching to selected buffers -*- lexical-binding: t; -*-

;; Author: Chris Hipple
;; URL: https://github.com/C-Hipple/harpoon.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; SPDX-License-Identifier: GPL-3.0+

;;; Commentary:

;; This package is an emacs port of the neovim plugin "harpoon" written by ThePrimeagen.
;; You can easily link files to a given hotkey and then jump your buffer to that file
;; with that hotkey.  Files can be hotswapped easily.

;;
;; Use Bind keys for harpoon--open-file harpoon--add-file called with a number argument.
;; Then you can add files with that command, and then jump to them immediately.

;;; Code:
(setq harpoon--file "~/harpoon-el")

(defun harpoon--open-file (line-number)
  "Reads `~/harpoon-el` and opens the file on LINE-NUMBER in the existing buffer if it's a valid file."
  (interactive)
  (let ((harpoon-file (expand-file-name harpoon--file))
        file-to-open)
    (if (file-readable-p harpoon-file)
        (with-temp-buffer
          (insert-file-contents harpoon-file)
          (goto-char (point-min))
          (forward-line (1- line-number))
          (setq file-to-open (string-trim (buffer-substring (line-beginning-position) (line-end-position))))
          (if (and (file-exists-p file-to-open)
                   (file-regular-p file-to-open))
              (find-file file-to-open)
            (message "No valid file on line %d of %s" line-number harpoon-file)))
      (message "%s is not readable." harpoon-file))))


(defun harpoon-add-file (line-number)
  "Adds the current buffer's file path to `~/harpoon-el` at LINE-NUMBER, replacing the existing line."
  (interactive)
  (let ((current-file (buffer-file-name))
        (harpoon-file (expand-file-name harpoon--file)))
    (if (not current-file)
        (message "Buffer not visiting a file.")
      (let ((harpoon-buffer (find-file-noselect harpoon-file)))
        (with-current-buffer harpoon-buffer
          (goto-char (point-min))
          (forward-line (1- line-number))
          (let ((line-start (point))
                line-end)
            (setq line-end (line-end-position))
            (delete-region line-start line-end)
            (insert current-file)
            (save-buffer)
            (kill-buffer harpoon-buffer)
            (message "Replaced line %d of %s with '%s'" line-number harpoon-file current-file)))))))


(defun harpoon-open-or-create ()
  "Opens (and creates if it doesn't exist) the file `~/harpoon-el` in a minibuffer and moves the cursor to it."
  (interactive)
  (let ((file-path (expand-file-name harpoon--file)))
    (if (not (file-exists-p file-path))
        (with-current-buffer (find-file-noselect file-path)
          (save-buffer)
          (switch-to-buffer (current-buffer)))
      (find-file file-path))))


(define-key evil-normal-state-map (kbd "SPC h A") 'harpoon-open-or-create)

(define-key evil-normal-state-map (kbd "SPC h 1") (lambda () (interactive) (harpoon--open-file 1)))
(define-key evil-normal-state-map (kbd "SPC h 2") (lambda () (interactive) (harpoon--open-file 2)))
(define-key evil-normal-state-map (kbd "SPC h 3") (lambda () (interactive) (harpoon--open-file 3)))
(define-key evil-normal-state-map (kbd "SPC h 4") (lambda () (interactive) (harpoon--open-file 4)))

(define-key evil-normal-state-map (kbd "SPC h a 1") (lambda () (interactive) (harpoon-add-file 1)))
(define-key evil-normal-state-map (kbd "SPC h a 2") (lambda () (interactive) (harpoon-add-file 2)))
(define-key evil-normal-state-map (kbd "SPC h a 3") (lambda () (interactive) (harpoon-add-file 3)))
(define-key evil-normal-state-map (kbd "SPC h a 4") (lambda () (interactive) (harpoon-add-file 4)))

(provide 'harpoon)
