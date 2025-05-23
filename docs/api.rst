API Reference
============

This section provides detailed information about all the functions provided by harpoon.el.

Core Functions
-------------

.. function:: harpoon-open-entry line-number

   Opens the buffer or file at the specified line number in the harpoon list.

   :param line-number: The line number (1-4) in the harpoon list
   :type line-number: integer
   :returns: nil

   This function reads the harpoon list file and opens the buffer or file specified at the given line number. If the file doesn't exist or the line number is invalid, it displays an error message.

.. function:: harpoon-add-file-or-buffer line-number

   Adds the current buffer to the harpoon list at the specified position.  If the buffer is visiting a file, that file is stored, otherwise the buffername is stored.  Therefore you can re-open a file that youv'e closed the buffer for, but you can also add other buffers (such as shells or magit) to harpoon.

   :param line-number: The line number (1-4) where to add the file
   :type line-number: integer
   :returns: nil

   This function adds the current buffer's to the harpoon list at the specified line number, replacing any existing entry.

.. function:: harpoon-open-or-create

   Opens the harpoon list file for editing, creating it if it doesn't exist.

   :returns: nil

   This function opens the harpoon list file in a buffer, creating it if it doesn't exist. This allows you to manually edit the list of files.

Variables
---------

.. variable:: harpoon--file

   The path to the harpoon list file.

   :type: string
   :default: "~/harpoon-el"

   This variable stores the path to the file that contains the list of harpooned files. You can customize this to store the list in a different location.

Example Usage
------------

Here's an example of how to use these functions programmatically:

.. code-block:: elisp

   ;; Add current file to position 1
   (harpoon-add-file-or-buffer 1)

   ;; Open the file at position 1
   (harpoon-open-entry 1)

   ;; Open the harpoon list for editing
   (harpoon-open-or-create)
