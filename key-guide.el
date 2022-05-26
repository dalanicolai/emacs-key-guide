;;; key-guide.el --- Easily add key guides  -*- lexical-binding: t; -*-
;; Copyright (C) 2020  Daniel Laurens Nicolai

;; Author: Daniel Laurens Nicolai <dalanicolai@gmail.com>
;; Version: 0
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

(require 'subr-x)

(defvar key-guide-alist '((Info-mode ("Navigation"
                                      ((Info-next))
                                      ((Info-prev))
                                      ((Info-scroll-up))
                                      ((Info-scroll-down)))
                                     ("Navigation"
                                      ((Info-forward-node))
                                      ((Info-backward-node))
                                      ((Info-menu))
                                      ((Info-next-reference))
                                      ((Info-prev-reference)))
                                     ("Help"
                                      ((Info-summary))
                                      ((describe-mode))))))
(defvar-local key-guide-string nil)
(defvar-local key-guide-hydra-style nil)
(defvar-local key-guide-show nil)
(defvar-local key-guide-associated-buffer nil)


(setq gobal-navigation-guide '(("Navigation"
                               ((next-line)))))

(defun key-guide-alist-get ()
  (or (when (and (featurep 'evil)
                 (not (eq evil-state 'emacs))))
      (alist-get (cons major-mode evil-state) key-guide-alist)
      (alist-get major-mode key-guide-alist)))

(defun key-guide-format-keys-to-functions (doc-alist &optional kill)
  (interactive (list (read (read-string "Insert alist: "))))
  (let ((function-style-alist
         (mapcar (lambda (c)
                   (cons (car c)
                         (mapcar (lambda (b)
                                   (let ((keys (split-string (if (consp b) (car b) b) "/")))
                                     (if (consp b)
                                         (list (mapcar (lambda (k)
                                                         (key-binding (kbd k)))
                                                       keys)
                                               (cadr b))
                                       (list (mapcar (lambda (k)
                                                       (key-binding (kbd k)))
                                                     keys)))))
                                 (cdr c))))
                 doc-alist)))
    (if (or (called-interactively-p 'any) kill)
        (kill-new (pp-to-string function-style-alist))
      function-style-alist)))

;; (key-guide-format-keys-to-functions ebib-index-key-guide-doc-alist) 

(defun key-guide-format-functions-to-keys (doc-alist &optional kill)
  (let ((keys-style-alist (mapcar (lambda (c)
                                    (cons (car c)
                                          (mapcar (lambda (b)
                                                    (list (mapconcat #'identity
                                                                     (mapcar (lambda (keys)
                                                                               (if (featurep 'evil)
                                                                                   (key-description (seq-find (lambda (k)
                                                                                                                (numberp (aref k 0)))
                                                                                                              keys))
                                                                                 (mapconcat #'key-description (print keys) "/")))
                                                                             (mapcar #'where-is-internal (car b)))
                                                                     "/")
                                                          (if (cdr b)
                                                              (cadr  b)
                                                            (symbol-name (caar b)))))
                                                  (cdr c))))
                                  doc-alist)))
    (if kill
        (kill-new (pp-to-string keys-style-alist))
      keys-style-alist)))

(defun key-guide--create (doc-alist)
  (interactive)
  ;; determine if alist is defined in 'function' style or in 'keys' style
  (unless (or (stringp (cadar doc-alist)) (stringp (caadar doc-alist))
    (setq doc-alist (key-guide-format-functions-to-keys doc-alist))))
  (let* (;; per category create alist with elements (key . action), both strings
         (bindings (mapcar (lambda (c)
                             (mapcar (lambda (k)
                                       (if (consp k)
                                           (cons (car k) (cadr k))
                                         (cons k (symbol-name (key-binding k)))))
                                     (cdr c)))
                           doc-alist))
         ;; now per category determine max string length of keys and functions
         (max-string-lengths (mapcar (lambda(c)
                                       (mapcar (lambda (f)
                                                 (apply #'max (mapcar (lambda (b)
                                                                        (length (funcall f b)))
                                                                      c)))
                                               '(car cdr)))
                                     bindings))
         ;; determine max number of bindings in categories
         (max-category-bindings (1- (apply #'max (mapcar #'length doc-alist))))
         ;; transpose bindings for easy insertion (we must insert per row not per column)
         (bindings-transpose (let (transpose)
                               (dotimes (i max-category-bindings)
                                 (push (mapcar (lambda (b) (nth i b)) bindings) transpose))
                               (reverse transpose)))
         (pad-left (lambda (n &optional x) (concat "%" (number-to-string n) "s")))
         (pad-right (lambda (n &optional x) (concat "%-" (number-to-string n) "s"))))
    (let ((doc ""))
      ;; insert headings
      (dotimes (i (length doc-alist))
        (let ((title (car (nth i doc-alist))))
          (setq doc (concat doc (string-pad title
                                            (max (+ (length title) 2)
                                                 (+ (apply '+ (nth i max-string-lengths)) 5)))))))
      (setq doc (concat doc "\n"))
      ;; insert separators
      (dotimes (i (length doc-alist))
        (let ((title (car (nth i doc-alist))))
          (setq doc (concat doc
                            (propertize 
                             (make-string (max (length title)
                                               (1- (+ (apply '+ (nth i max-string-lengths)) 4)))
                                          (if key-guide-hydra-style
                                              (string-to-char "-")
                                            9472))
                             'face (if key-guide-hydra-style
                                       'default
                                     'shadow))
                            "  "))))
      (setq doc (concat doc "\n"))
      (dotimes (i (length bindings-transpose))
        (let ((b (nth i bindings-transpose)))
          (dotimes (j (length b))
            (let ((title-length (length (car (nth j doc-alist))))
                  (keys-length (+ (car (nth j max-string-lengths)) 3)))
              (setq doc (concat doc
                                (concat (string-pad (when-let (s (car (nth j b)))
                                                      (if key-guide-hydra-style
                                                          (concat "[" (propertize s 'face 'font-lock-comment-face) "] ")
                                                        (concat (propertize s 'face 'font-lock-function-name-face)
                                                                (propertize " → " 'face 'font-lock-comment-face))))
                                                    keys-length
                                                    nil
                                                    (unless key-guide-hydra-style t))
                                        (string-pad (cdr (nth j b))
                                                    (max (- title-length keys-length) (cadr (nth j max-string-lengths))))
                                        "  ")))))
          (setq doc (concat doc "\n"))))
      doc)))

;; (key-guide--create
;;  (key-guide-format-functions-to-keys (key-guide-format-keys-to-functions ebib-index-key-guide-doc-alist)))
(key-guide--create
 (key-guide-format-functions-to-keys gobal-navigation-guide))

(defun key-guide-show (&optional doc-alist)
  (let ((key-guide-buffer (get-buffer-create "*key-guide*")))
    (if doc-alist
        (setq key-guide-string (key-guide--create doc-alist))
      (unless (and key-guide-string
                   (not (eq (current-buffer)
                            (buffer-local-value key-guide-associated-buffer
                                                key-guide-buffer))))
        (setq key-guide-string (key-guide--create (key-guide-alist-get)))))
    (let ((assoc-buf (current-buffer))
          (key-guide key-guide-string))
      (with-current-buffer key-guide-buffer
        (read-only-mode 0)
        (erase-buffer)
        (setq cursor-type nil)
        (setq key-guide-associated-buffer assoc-buf)
        (insert key-guide)
        (setq truncate-lines t)
        (read-only-mode)
        (goto-char (point-min))
        (display-buffer-at-bottom (current-buffer)
                                  (list (cons 'window-height (count-lines (point-min) (point-max)))
                                        (cons 'window-parameters '((mode-line-format . none)
                                                                   (no-other-window . t)))
                                        (cons 'dedicated t)))))))

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

(defun key-guide-set-doc-alist (mode alist)
  (setf (alist-get mode key-guide-alist) alist))

(defun key-guide-hydra-style ()
  (setq key-guide-hydra-style t))

;; (defun key-guide-major-mode-hook-function ()
;;   (when-let (alist (key-guide-alist-get))
;;     (unless key-guide-string
;;       (setq key-guide-string (key-guide--create alist)))))

;; (add-hook 'after-change-major-mode-hook 'key-guide-hydra-style)

(defun key-guide-window-selection-change-hook-function (&optional arg)
  (if (and key-guide-show
           (or key-guide-string (key-guide-alist-get)))
      (key-guide-show)
    (when-let (buf (get-buffer-create "*key-guide*"))
      (kill-buffer buf))))

(add-hook 'after-change-major-mode-hook 'key-guide-window-selection-change-hook-function)
(add-hook 'window-selection-change-functions #'key-guide-window-selection-change-hook-function)
(add-hook 'window-configuration-change-hook #'key-guide-window-selection-change-hook-function)
(global-set-key (kbd "C-.") #'key-guide-toggle)

(provide 'key-guide)

;;; key-guide.el ends here
