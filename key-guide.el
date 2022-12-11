;;; key-guide.el --- Easily add key guides  -*- lexical-binding: t; -*-
;; Copyright (C) 2022  Daniel Laurens Nicolai

;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
;; Version: 0.1
;; Keywords: docs, help, maint, convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/dalanicolai/key-guide.el


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code

(require 'subr-x)

(defvar-local key-guide-string nil)
(defvar-local key-guide-hydra-style nil)
(defvar-local key-guide-show nil)
(defvar-local key-guide-associated-buffer nil)

(defvar key-guide-alist '((default navigation
                                   ("Test" "a" "b"))
                          ((default normal)
                           ("Move"
                            ((evil-forward-char evil-backward-char) "next/previous char")
                            ((evil-next-line evil-previous-line) "next/previous line")
                            ((evil-scroll-page-down evil-scroll-page-up) "scroll page forward/backward")
                            ((evil-backward-word-begin evil-forward-word-begin) "beginning next/previous of word")
                            ((evil-forward-word-end evil-backward-word-end) "end next/previous of word")
                            ((evil-forward-sentence-end evil-backward-sentence-end) "end next/previous of sentence")
                            ((evil-beginning-of-line evil-end-of-line) "beginning/end of line")
                            (evil-first-non-blank "first non blank"))
                           ("Jump"
                            (evil-jump-item "jump to matching character")
                            ((evil-find-char evil-find-char-backward) "jump to next/previous char")
                            ((evil-find-char-to evil-find-char-to-backward) "jump to before/after next/previous char")
                            ((evil-repeat-find-char evil-repeat-find-char-reverse) "repeat f,t,F,T forwards/backwards")
                            ((evil-window-top evil-window-middle evil-window-bottom) "cursor to top/middle/bottom")
                            ((evil-goto-line evil-goto-first-line) "goto (last) line/first line")
                            ("g d" "jump to definition"))
                           ("Editing"
                            ((evil-replace evil-replace-state) "replace single/multiple char(s)")
                            ((evil-join evil-join-whitespace) "join line below/without space")
                            evil-change
                            evil-substitute
                            (evil-undo evil-redo)
                            evil-repeat)
                           ("Cut and paste"
                            (evil-yank "copy")
                            ("p/P" "paste after/before")
                            (evil-delete "delete")
                            ("i/a" "(infix) inner/a (incl. space)")
                            ("w/(/[/..." "(suffix) word/delimiter"))
                           ("Macros/scroll/fold"
                            ("qa" "record macro a")
                            ("q" "stop recording")
                            ("@a" "run macro a")
                            ("@@" "rerun last macro")
                            ("z t/z/b" "scroll to top/middle/bottom")
                            (evil-toggle-fold "toggle fold")
                            ((evil-close-folds evil-open-folds) "open/close folds")))
                          ((default insert)
                           ("Section 1"
                            (forward-char backward-char)
                            electric-newline-and-maybe-indent
                            evil-paste-last-insertion-and-stop-insert
                            evil-paste-last-insertion
                            evil-shift-left-line
                            evil-copy-from-below
                            evil-insert-digraph)
                           ("Section 2"
                            evil-complete-next
                            evil-execute-in-normal-state
                            evil-complete-previous
                            evil-quoted-insert
                            evil-paste-from-register
                            evil-shift-right-line)
                           ("Section 3"
                            evil-quoted-insert
                            evil-delete-backward-word
                            evil-copy-from-above
                            evil-emacs-state
                            delete-char)
                           ("Section4"
                            evil-normal-state
                            evil-replace-state
                            mouse-yank-primary
                            evil-delete-backward-char-and-join
                            hippie-expand
                            evil-complete-next-line
                            evil-complete-previous-line))
                          ((default visual)
                           ("Infix" ("i/a" "inner/a (incl. space)"))
                           ("Suffix" ("o/w" "object/word") ("(/[/..." "delimiter")))
                          (Info-mode
                           ("Navigation"
                            (Info-scroll-down Info-scroll-up)
                            (Info-scroll-up Info-scroll-down))
                           ("Navigation"
                            (Info-forward-node Info-backward-node)
                            (Info-menu)
                            (Info-next-reference Info-prev-reference))
                           ("Help"
                            (Info-summary)
                            (describe-mode)))))



(setq key-guide-sections '((navigation (backward-char forward-char)
                                       (backward-word forward-word)
                                       (previous-line next-line)
                                       (move-beginning-of-line move-end-of-line)
                                       (backward-sentence forward-sentence)
                                       (backward-paragraph forward-paragraph)
                                       (backward-page forward-page)
                                       (backward-sexp forward-sexp)
                                       (beginning-of-defun end-of-defun)
                                       (beginning-of-buffer end-of-buffer)
                                       (scroll-up-command scroll-down-command)
                                       (scroll-left scroll-right)
                                       (recenter-top-bottom)
                                       (goto-line)
                                       (goto-char))
                           (files (counsel-find-file)
                                  (save-buffer)
                                  (save-some-buffers)
                                  (insert-file)
                                  (find-alternate-file)
                                  (write-file)
                                  (read-only-mode))
                           (help (insert-file)
                                 (scroll-other-window)
                                 (counsel-apropos)
                                 (describe-key)
                                 (counsel-describe-function)
                                 (describe-mode))
                           ((navigation normal) "j/k")))

(defun key-guide-quick-create (arg)
  (interactive "P")
  (let ((buf (get-buffer-create "*key-list*")))
    (display-buffer buf)
    (with-current-buffer buf
      (insert "((\""))
    (let ((key (key-description
                (read-key-sequence-vector
                 "Type chars, separate groups by comma, finish with RET: "))))
      (while (not (string= key "RET"))
        (pcase key
          ((or "," ", ,")
           (with-current-buffer buf
             (goto-char (point-max))
             (delete-char -1)
             (insert "\")\n (\"")))
          (_ (with-current-buffer buf
               (goto-char (point-max))
               (insert key "/"))))
        (setq key (key-description
                   (read-key-sequence-vector
                    "Type chars, separate groups by comma, finish with RET: "))))
      (when (string= key "RET")
        (with-current-buffer buf
          (delete-char -1)
          (insert "\"))")
          (when arg
            (let ((key-list (read (buffer-string))))
              (kill-new (pp key-list)))))))))

(defun key-guide-format-keys-to-functions (doc-alist &optional kill)
  (interactive (list (read (read-string "Insert alist: "))))
  (let ((function-style-alist
         (mapcar (lambda (b)
                   (let ((keys (split-string (if (consp b) (car b) b) "/")))
                     (if (consp b)
                         (mapcar (lambda (k)
                                        (key-binding (kbd k)))
                                      keys)
                         (cadr b)
                       (mapcar (lambda (k)
                                      (key-binding (kbd k)))
                                    keys))))
                 doc-alist)))
    (if (or (called-interactively-p 'any) kill)
        (kill-new (pp-to-string function-style-alist))
      function-style-alist)))

(defun key-guide-alist-get ()
  (or (when (and (featurep 'evil)
                 (not (eq evil-state 'emacs)))
        (or (alist-get (list major-mode evil-state) key-guide-alist nil nil #'equal)
            (alist-get (list 'default evil-state) key-guide-alist nil nil #'equal)))
      (alist-get major-mode key-guide-alist)
      (alist-get 'default key-guide-alist)))

(defun key-guide-create-key-doc (key-desc doc &optional hydra)
  (concat (if hydra
              ;; (if (string-match "[^[:space:]]" key-desc)
              (replace-regexp-in-string "[^[:space:]].*$" "[\\&" key-desc)
                ;; (propertize (replace-regexp-in-string "   $" "  nil" key-desc) 'face 'shadow))
            key-desc)
          (if hydra (if (string-match "[^[:space:]]" key-desc)
                        "] "
                      "   ")
            (propertize " → " 'face 'font-lock-comment-face))
          doc))

(defun key-guide-get-key-descs (command &optional type)
  "TYPE can be `all' or `evil'"
  (let ((keys (where-is-internal command)))
    (if keys
      (let* ((keys-select (pcase type
                            ('evil (list (or (seq-find (lambda (k)
                                                      (numberp (aref k 0)))
                                                    keys)
                                             (car keys))))
                            ('all keys)
                            (_ (list (car keys))))))
        (mapcar #'key-description keys-select))
      (list "nil"))))

(defun key-guide-format-key-descs (keys &optional type hydra)
  (apply #'propertize
         (mapconcat (lambda (k)
                      (apply #'propertize
                             (if (and (string= (substring k 0 1) "<")
                                      (> (length k) 1))
                                 (car (split-string k "[<>]" t))
                               k)
                             (when hydra '(face font-lock-comment-face))))
                    keys
                    "/")
         (unless hydra '(face font-lock-function-name-face))))

(defun key-guide-get-commands (key-desc)
  "Get and return list of function symbols from key description."
  (let ((keys (split-string key-desc "/")))
    (mapcar (lambda (k)
              (key-binding (kbd k)))
            keys)))

(defun key-guide-parse-keybinding (definition &optional type hydra)
  (let* ((custom-desc (and (consp definition)
                           (stringp (car (last definition)))))
         (bindings (if (consp definition)
                       (if custom-desc
                           (car definition)
                         definition)
                     definition))
         (keys (cond ((consp bindings) (mapcan (lambda (c)
                                                 (key-guide-get-key-descs c type))
                                               bindings))
                     ((symbolp bindings) (key-guide-get-key-descs bindings type))
                     (t (split-string bindings "/"))))
         (commands (if custom-desc
                       (car (last definition))
                     (cond ((consp bindings) (mapconcat #'symbol-name bindings "/"))
                           ((symbolp bindings) (symbol-name bindings))
                           (t (mapconcat #'symbol-name (key-guide-get-commands bindings) "/"))))))
    (cons (key-guide-format-key-descs keys type hydra) commands)))

(defun key-guide-parse-alist (def-alist &optional type hydra)
  (let ((parsed-sections (mapcar (lambda (s)
                                   (if (symbolp s)
                                       (cons (upcase-initials (symbol-name s))
                                             (alist-get s key-guide-sections))
                                     s))
                                 def-alist)))
    (mapcar (lambda (kb-alist)
              (cons (car kb-alist)
                    (mapcar (lambda (kb)
                              (key-guide-parse-keybinding kb type (or key-guide-hydra-style hydra)))
                            (cdr kb-alist))))
              parsed-sections)))

(defun key-guide-pad-descs (descs-alist)
  (mapcar (lambda (s)
            (let ((keys-length (apply #'max (mapcar (lambda (kb)
                                                      (length (car kb)))
                                                    (cdr s))))
                  (descs-length (apply #'max (mapcar (lambda (kb)
                                                       (length (cdr kb)))
                                                     (cdr s)))))
              (cons (string-pad (car s) (+ keys-length descs-length 3) nil)
                    (mapcar (lambda (d)
                              (cons (string-pad (car d) keys-length nil t)
                                    (string-pad (cdr d) descs-length nil)))
                            (cdr s)))))
          descs-alist))

(defun key-guide-transpose-list (list)
  "Transpose a 2 dimensional nested list."
  ;; determine max number of bindings in categories
  (let ((max-category-bindings (apply #'max (mapcar #'length list)))
        transpose)
    (dotimes (i max-category-bindings)
      (push (mapcar (lambda (b) (nth i b)) list) transpose))
    (reverse transpose)))

(defmacro -> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the second item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
second item in second form, etc."
  (declare (debug (form &rest [&or symbolp (sexp &rest form)])))
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,(car form) ,x ,@(cdr form))
                  (list form x)))
   (:else `(-> (-> ,x ,form) ,@more))))

(defun key-guide-create-string (doc-alist)
  (let ((rows (key-guide-transpose-list doc-alist)))
    (concat "\n"
            (mapconcat (lambda (h) (concat (propertize h 'face 'bold) "  ")) (car rows)) "\n"
            (mapconcat (lambda (h)
                         (concat (make-string (length h)
                                              (if key-guide-hydra-style
                                                  (string-to-char "-")
                                                (string-to-char "─")))
                                 "  "))
                       (car rows))
            "\n"
            (mapconcat (lambda (r)
                         (let ((i 0))
                           (mapconcat (lambda (kb)
                                        (setq i (1+ i))
                                        (if kb
                                            (key-guide-create-key-doc (car kb) (cdr kb) key-guide-hydra-style)
                                          (make-string (length (nth (1- i) (car rows))) (string-to-char " "))))
                                      r "  ")))
                       (cdr rows) "\n"))))

(defun key-guide-show (&optional doc-alist)
  (let ((key-guide-buffer (get-buffer-create "*key-guide*")))
    (if doc-alist
        (setq key-guide-string (key-guide-create-string
                                (key-guide-pad-descs
                                 (key-guide-parse-alist
                                  doc-alist
                                  'evil))))
      (unless (and key-guide-string
                   (not (eq (current-buffer)
                            (buffer-local-value key-guide-associated-buffer
                                                key-guide-buffer))))
        (setq key-guide-string 
              (key-guide-create-string (key-guide-pad-descs (key-guide-parse-alist (key-guide-alist-get) 'evil))))))
    (let ((assoc-buf (current-buffer))
          (key-guide key-guide-string))
      (with-current-buffer key-guide-buffer
        (read-only-mode 0)
        (erase-buffer)
        (setq cursor-type nil)
        (setq key-guide-associated-buffer assoc-buf)
        (unless (eq assoc-buf key-guide-buffer)
          (insert key-guide))
        (setq truncate-lines t)
        (read-only-mode)
        (goto-char (point-min))
        ;; side window is dedicated and does not get included when using 'other
        ;; window action functions' (see info node `(elisp)Displaying Buffers in
        ;; Side Windows')
        (display-buffer-in-side-window (current-buffer)
                                       (list (cons 'window-height (count-lines (point-min) (point-max)))
                                             (cons 'window-parameters '((mode-line-format . none)))))))))


;;;###autoload
(defun key-guide-toggle ()
  (interactive)
  (let ((buf (get-buffer "*key-guide*")))
    (cond (buf (setq key-guide-show nil)
               (kill-buffer buf))
          (t
           (setq key-guide-show t)
           (if (or key-guide-string
                   (key-guide-alist-get))
               (key-guide-show)
             (user-error "No `key-guide-alist' defined in this buffer"))))))

(defun key-guide-hydra-style ()
  (setq key-guide-hydra-style t))

(defun key-guide-window-selection-change-hook-function (&optional arg)
  (print arg #'external-debugging-output)
  (if (and key-guide-show
           (or key-guide-string (key-guide-alist-get)))
      (key-guide-show)
    (when-let (buf (get-buffer-create "*key-guide*"))
      (kill-buffer buf))))

(add-hook 'after-change-major-mode-hook #'key-guide-window-selection-change-hook-function)
(add-hook 'window-selection-change-functions #'key-guide-window-selection-change-hook-function)
(add-hook 'window-configuration-change-hook #'key-guide-window-selection-change-hook-function)

(when (featurep 'evil)
  (defun key-guide-state-change-hook-function (&optional arg)
    (print arg #'external-debugging-output)
    (if (and key-guide-show
             (key-guide-alist-get))
        (key-guide-show (key-guide-alist-get))))

  (add-hook 'evil-normal-state-entry-hook #'key-guide-state-change-hook-function)
  (add-hook 'evil-insert-state-entry-hook #'key-guide-state-change-hook-function)
  (add-hook 'evil-visual-state-entry-hook #'key-guide-state-change-hook-function)

  (evil-global-set-key 'normal (kbd "C-.") #'key-guide-toggle))

(global-set-key (kbd "C-.") #'key-guide-toggle)

(provide 'key-guide)

;;; key-guide.el ends here
