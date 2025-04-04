Usage
=====

Basic Usage
----------

harpoon.el provides a simple way to quickly switch between frequently used files. The package maintains a list of files in ``~/harpoon-el`` and allows you to quickly jump to any of them using numeric hotkeys.

Adding Files
-----------

To add the current file to your harpoon list at position 1:

.. code-block:: elisp

   (harpoon-add-file 1)

You can add files to positions 1-4.

Opening Files
------------

To open a file from your harpoon list:

.. code-block:: elisp

   (harpoon--open-file 1)  ; Opens the file at position 1

Managing Your Harpoon List
-------------------------

To view and edit your harpoon list:

.. code-block:: elisp

   (harpoon-open-or-create)

This will open the ``~/harpoon-el`` file in a buffer, allowing you to manually edit the list of files.

Example Workflow
---------------

1. Open a file you want to add to your harpoon list
2. Add it to a position: ``(harpoon-add-file 1)``
3. Later, quickly switch to it: ``(harpoon--open-file 1)``

For Evil Mode Users
------------------

If you're using Evil mode, you can set up keybindings like this:

.. code-block:: elisp

   (define-key evil-normal-state-map (kbd "SPC h A") 'harpoon-open-or-create)
   
   (define-key evil-normal-state-map (kbd "SPC h 1") (lambda () (interactive) (harpoon--open-file 1)))
   (define-key evil-normal-state-map (kbd "SPC h 2") (lambda () (interactive) (harpoon--open-file 2)))
   (define-key evil-normal-state-map (kbd "SPC h 3") (lambda () (interactive) (harpoon--open-file 3)))
   (define-key evil-normal-state-map (kbd "SPC h 4") (lambda () (interactive) (harpoon--open-file 4)))
   
   (define-key evil-normal-state-map (kbd "SPC h a 1") (lambda () (interactive) (harpoon-add-file 1)))
   (define-key evil-normal-state-map (kbd "SPC h a 2") (lambda () (interactive) (harpoon-add-file 2)))
   (define-key evil-normal-state-map (kbd "SPC h a 3") (lambda () (interactive) (harpoon-add-file 3)))
   (define-key evil-normal-state-map (kbd "SPC h a 4") (lambda () (interactive) (harpoon-add-file 4)))

This setup provides:
- ``SPC h A`` to open the harpoon list
- ``SPC h 1-4`` to jump to files
- ``SPC h a 1-4`` to add files 