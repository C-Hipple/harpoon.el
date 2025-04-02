(defun harpoon-open-or-create ()
  "Opens (and creates if it doesn't exist) the file `~/harpoon-el` in a minibuffer and moves the cursor to it."
  (interactive)
  (let ((file-path (expand-file-name "~/harpoon-el")))
    (if (not (file-exists-p file-path))
        (with-current-buffer (find-file-noselect file-path)
          (insert ";; Harpoon file\n") ; Optional: Initial content
          (save-buffer)
          (switch-to-buffer (current-buffer)))
      (find-file file-path))))



(defun harpoon--open-file-from-harpoon-el (line-number)
  "Reads `~/harpoon-el` and opens the file on the 2nd line in the existing buffer if it's a valid file."
  (let ((harpoon-file (expand-file-name "~/harpoon-el"))
        file-to-open)
    (if (file-readable-p harpoon-file)
        (with-temp-buffer
          (insert-file-contents harpoon-file)
          (goto-char (point-min))
          (forward-line line-number)
          (setq file-to-open (string-trim (buffer-substring (line-beginning-position) (line-end-position))))
          (if (and (file-exists-p file-to-open)
                   (file-regular-p file-to-open))
              (find-file file-to-open)
            (message "No valid file on the second line of %s" harpoon-file)))
      (message "%s is not readable." harpoon-file))))


(defun harpoon-open-first-file ()
  (interactive)
  (harpoon--open-file-from-harpoon-el 1))

(defun harpoon-open-second-file ()
  (interactive)
  (harpoon--open-file-from-harpoon-el 2))

(defun harpoon-open-third-file ()
  (interactive)
  (harpoon--open-file-from-harpoon-el 3))

(defun harpoon-open-fourth-file ()
  (interactive)
  (harpoon--open-file-from-harpoon-el 4))


;; why
(define-key evil-normal-state-map (kbd "SPC H a") 'harpoon-open-or-create)
(define-key evil-normal-state-map (kbd "SPC H 1") 'harpoon-open-first-file)
(define-key evil-normal-state-map (kbd "SPC H 2") 'harpoon-open-second-file)
(define-key evil-normal-state-map (kbd "SPC H 3") 'harpoon-open-third-file)
(define-key evil-normal-state-map (kbd "SPC H 4") 'harpoon-open-fourth-file)
