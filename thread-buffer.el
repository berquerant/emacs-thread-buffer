;;; thread-buffer.el --- buffers for threads -*- lexical-binding: t -*-

;; Author: berquerant
;; Maintainer: berquerant
;; Created: 5 Sep 2023
;; Version: 0.1.0
;; Keywords: thread buffer
;; URL: https://github.com/berquerant/emacs-thread-buffer

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defun thread-buffer--match-current-buffer-name? (regex)
  "Return buffer name if current buffer name matches with REGEX or nil."
  (let ((name (buffer-name)))
    (when (and regex (string-match-p regex name))
      name)))

(defun thread-buffer--find-minimum-unused-buffer-number (buffer-name-template)
  (let ((buffer-names (mapcar 'buffer-name (buffer-list)))
        (n 1))
    (while (member (format buffer-name-template n) buffer-names)
      (setq n (+ n 1)))
    n))

(defun thread-buffer--get-thread-buffer-name (regex buffer-name-template)
  (let ((name (thread-buffer--match-current-buffer-name? regex)))
    (if name name
      (format buffer-name-template (thread-buffer--find-minimum-unused-buffer-number buffer-name-template)))))

(defun thread-buffer--get-thread-buffer-create (regex buffer-name-template)
  "Create a new buffer for threads or open an existing buffer.
Open the buffer if the current buffer name matches REGEX.
Otherwise create a new buffer."
  (get-buffer-create (thread-buffer--get-thread-buffer-name regex buffer-name-template)))

(defun thread-buffer--switch-to-thread-buffer (thread-buffer-or-name)
  "Switch to THREAD-BUFFER-OR-NAME and move the cursor to the end of the buffer.
If the current buffer is not THREAD-BUFFER-OR-NAME,
`switch-to-buffer-other-window'."
  (with-current-buffer thread-buffer-or-name
    (goto-char (point-max)))
  (unless (get-buffer-window thread-buffer-or-name)
    (switch-to-buffer-other-window thread-buffer-or-name)))

(defun thread-buffer--overwrite-buffer (input buffer)
  "Truncate BUFFER and write INPUT into BUFFER."
  (with-current-buffer buffer
    (erase-buffer)
    (insert input)))

(defun thread-buffer--append-buffer (input buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert input)))

(defun thread-buffer-write
    (input
     buffer-name-template
     &optional
     buffer-name-regex
     append
     no-switch)
  "Overwrite a thread buffer with INPUT and switch to the buffer.

The buffer name is made up of BUFFER-NAME-TEMPLATE and a number,
e.g. template is `*some-thread-%d*' and a number is 1 then `*some-thread-1*'.

If the current buffer is not a thread buffer, create a new thread buffer.
The number is the smallest natural number that is not used
in the thread buffer name,

e.g. template is `*some-thread-%d*',
- no thread buffers then create `*some-thread-1*'.
- `*some-thread-1*' exist then create `*some-thread-2*'
- `*some-thread-2*' exist then create `*some-thread-1*'

If BUFFER-NAME-REGEX is not nil, it is used to check if the current buffer
is a thread buffer.

If APPEND is not nil, append INPUT to the thread buffer instead of overwriting.

If NO-SWITCH is not nil, does not switch to the thread buffer after writing."
  (let ((buffer (thread-buffer--get-thread-buffer-create buffer-name-regex
                                                         buffer-name-template)))
    (if append (thread-buffer--append-buffer input buffer)
      (thread-buffer--overwrite-buffer input buffer))
    (unless no-switch
      (thread-buffer--switch-to-thread-buffer buffer))))

(provide 'thread-buffer)
;;; thread-buffer.el ends here
