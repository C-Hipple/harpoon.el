(defun harpoon--open-file-from-harpoon-el (line-number)
  "Reads `~/harpoon-el` and opens the file on LINE-NUMBER in the existing buffer if it's a valid file."
  (interactive)
  (let ((harpoon-file (expand-file-name "~/harpoon-el"))
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
        (harpoon-file (expand-file-name "~/harpoon-el")))
    (if (not current-file)
        (message "Buffer not visiting a file.")
      (with-current-buffer (find-file-noselect harpoon-file)
        (goto-char (point-min))
        (forward-line (1- line-number))
        (let ((line-start (point))
              line-end)
          (setq line-end (line-end-position))
          (delete-region line-start line-end)
          (insert current-file)
          (save-buffer)
          (switch-to-buffer (current-buffer))
          (message "Replaced line %d of %s with '%s'" line-number harpoon-file current-file))))))


(defun harpoon-open-or-create ()
  "Opens (and creates if it doesn't exist) the file `~/harpoon-el` in a minibuffer and moves the cursor to it."
  (interactive)
  (let ((file-path (expand-file-name "~/harpoon-el")))
    (if (not (file-exists-p file-path))
        (with-current-buffer (find-file-noselect file-path)
          (save-buffer)
          (switch-to-buffer (current-buffer)))
      (find-file file-path))))


(define-key evil-normal-state-map (kbd "SPC h A") 'harpoon-open-or-create)

(define-key evil-normal-state-map (kbd "SPC h 1") (lambda () (interactive) (harpoon--open-file-from-harpoon-el 1)))
(define-key evil-normal-state-map (kbd "SPC h 2") (lambda () (interactive) (harpoon--open-file-from-harpoon-el 2)))
(define-key evil-normal-state-map (kbd "SPC h 3") (lambda () (interactive) (harpoon--open-file-from-harpoon-el 3)))
(define-key evil-normal-state-map (kbd "SPC h 4") (lambda () (interactive) (harpoon--open-file-from-harpoon-el 4)))

(define-key evil-normal-state-map (kbd "SPC h a 1") (lambda () (interactive) (harpoon-add-file 1)))
(define-key evil-normal-state-map (kbd "SPC h a 2") (lambda () (interactive) (harpoon-add-file 2)))
(define-key evil-normal-state-map (kbd "SPC h a 3") (lambda () (interactive) (harpoon-add-file 3)))
(define-key evil-normal-state-map (kbd "SPC h a 4") (lambda () (interactive) (harpoon-add-file 4)))
