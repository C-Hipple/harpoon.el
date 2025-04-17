# harpoon.el
A minimalist port of The Primegean's vim plugin Harpoon to emacs (as I remember the functionality from several years ago)

[Documentation](https://harpoonel.readthedocs.io/en/latest/)

## Quickstart

1. Install the package using your favorite emacs package manager

2. Bind the keys as below.

3. Add a file OR buffer to your harpoon using the add command.

4. Naviate to another buffer.

5. Harpoon back to that file using the open-entry function.


## Keybindings

Set bindings for lambdas for open and add file functions as shown below:

I use evil, so this is my setup:

```elisp
  (define-key evil-normal-state-map (kbd "SPC h A") 'harpoon-open-or-create)

  (define-key evil-normal-state-map (kbd "SPC h 1") (lambda () (interactive) (harpoon-open-entry 1)))
  (define-key evil-normal-state-map (kbd "SPC h 2") (lambda () (interactive) (harpoon-open-entry 2)))
  (define-key evil-normal-state-map (kbd "SPC h 3") (lambda () (interactive) (harpoon-open-entry 3)))
  (define-key evil-normal-state-map (kbd "SPC h 4") (lambda () (interactive) (harpoon-open-entry 4)))

  (define-key evil-normal-state-map (kbd "SPC h a 1") (lambda () (interactive) (harpoon-add-file-or-buffer 1)))
  (define-key evil-normal-state-map (kbd "SPC h a 2") (lambda () (interactive) (harpoon-add-file-or-buffer 2)))
  (define-key evil-normal-state-map (kbd "SPC h a 3") (lambda () (interactive) (harpoon-add-file-or-buffer 3)))
  (define-key evil-normal-state-map (kbd "SPC h a 4") (lambda () (interactive) (harpoon-add-file-or-buffer 4)))
```
