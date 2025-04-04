Installation
============

Prerequisites
------------

- Emacs 25.1 or later
- (Optional) Evil mode for vim-like keybindings

Package Installation
------------------

You can install harpoon.el using your preferred package manager:

Using straight.el
~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (straight-use-package
    '(harpoon :type git :host github :repo "C-Hipple/harpoon.el"))

Using use-package
~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (use-package harpoon
     :straight (harpoon :type git :host github :repo "C-Hipple/harpoon.el"))

Manual Installation
-----------------

1. Clone the repository:

   .. code-block:: bash

      git clone https://github.com/C-Hipple/harpoon.el.git

2. Add the following to your Emacs configuration:

   .. code-block:: elisp

      (add-to-list 'load-path "/path/to/harpoon.el")
      (require 'harpoon)

Configuration
------------

After installation, you'll need to configure your keybindings. See the :doc:`configuration` section for details. 