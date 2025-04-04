Configuration
=============

Basic Configuration
-----------------

The main configuration option in harpoon.el is the location of the harpoon list file. By default, it's set to ``~/harpoon-el``, but you can change it by setting the ``harpoon--file`` variable:

.. code-block:: elisp

   (setq harpoon--file "~/.config/emacs/harpoon-list")

Keybindings
----------

You can configure keybindings to suit your workflow. Here are some examples:

For regular Emacs users:

.. code-block:: elisp

   (global-set-key (kbd "C-c h A") 'harpoon-open-or-create)
   (global-set-key (kbd "C-c h 1") (lambda () (interactive) (harpoon--open-file 1)))
   (global-set-key (kbd "C-c h 2") (lambda () (interactive) (harpoon--open-file 2)))
   (global-set-key (kbd "C-c h 3") (lambda () (interactive) (harpoon--open-file 3)))
   (global-set-key (kbd "C-c h 4") (lambda () (interactive) (harpoon--open-file 4)))
   
   (global-set-key (kbd "C-c h a 1") (lambda () (interactive) (harpoon-add-file 1)))
   (global-set-key (kbd "C-c h a 2") (lambda () (interactive) (harpoon-add-file 2)))
   (global-set-key (kbd "C-c h a 3") (lambda () (interactive) (harpoon-add-file 3)))
   (global-set-key (kbd "C-c h a 4") (lambda () (interactive) (harpoon-add-file 4)))

For Evil mode users (as shown in the :doc:`usage` section):

.. code-block:: elisp

   (define-key evil-normal-state-map (kbd "SPC h A") 'harpoon-open-or-create)
   (define-key evil-normal-state-map (kbd "SPC h 1") (lambda () (interactive) (harpoon--open-file 1)))
   ;; ... and so on

Customizing the Harpoon List
--------------------------

The harpoon list is stored in a simple text file, with one file path per line. You can:

1. Edit it manually by opening the file
2. Use the ``harpoon-open-or-create`` function to open it in a buffer
3. Use the ``harpoon-add-file`` function to programmatically add files

The file format is simple: each line contains a full path to a file. Lines are numbered from 1 to 4, corresponding to the position argument used in the functions.

Example harpoon list file:

.. code-block:: text

   /home/user/projects/main.py
   /home/user/docs/notes.org
   /home/user/config/emacs.org
   /home/user/todo.org 