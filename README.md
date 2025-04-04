# harpoon.el
A minimalist port of The Primegean's vim plugin Harpoon to emacs (as I remember the functionality from several years ago)

[Documentation](https://harpoonel.readthedocs.io/en/latest/)

## TODO:
Consider using buffers OR files

## Keybindings

Set bindings for lambdas for open and add file functions as shown below:

I use evil, so this is my setup:

```elisp
  (define-key evil-normal-state-map (kbd "SPC h A") 'harpoon-open-or-create)

  (define-key evil-normal-state-map (kbd "SPC h 1") (lambda () (interactive) (harpoon--open-file 1)))
  (define-key evil-normal-state-map (kbd "SPC h 2") (lambda () (interactive) (harpoon--open-file 2)))
  (define-key evil-normal-state-map (kbd "SPC h 3") (lambda () (interactive) (harpoon--open-file 3)))
  (define-key evil-normal-state-map (kbd "SPC h 4") (lambda () (interactive) (harpoon--open-file 4)))

  (define-key evil-normal-state-map (kbd "SPC h a 1") (lambda () (interactive) (harpoon-add-file 1)))
  (define-key evil-normal-state-map (kbd "SPC h a 2") (lambda () (interactive) (harpoon-add-file 2)))
  (define-key evil-normal-state-map (kbd "SPC h a 3") (lambda () (interactive) (harpoon-add-file 3)))
  (define-key evil-normal-state-map (kbd "SPC h a 4") (lambda () (interactive) (harpoon-add-file 4)))
```
