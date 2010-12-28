;;; python-ex.el --- selfish plugin for writing python (fully depends on ipython)

;; Copyright (C) 2010  podhmo

;; Author:  podhmo <ababjam61@gmail.com>
;; Keywords: programing, python, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage:

;; In your emacs config,
;; (require 'python-lite)

;; (defun python-lite:setup ()
;;   (let ((key-map '(("\C-c\C-@" . python-lite:eval/compile)
;;                    ("\C-c\C-u" . python-lite:insert-module)
;;                    ([f1] . python-lite:pydoc)
;;                    ([f2] . python-lite:webhelp)
;;                    )))
;;     (loop for (k . fun) in key-map
;;           do (define-key python-mode-map k fun))))

;; (add-hook 'python-mode-hook 'python-lite:setup)

;;;; code ;;;;

(require 'cl)

;;; util macro
(defmacro python-lite:rlet1
  (var val &rest body)
  "imported from gauche"
  (\` (python-lite:let1 (\, var) (\, val) (\,@ body) (\, var))))
(put 'python-lite:rlet1 'lisp-indent-function 2)
(defmacro python-lite:let1
  (var val &rest body)
  "imported from gauche"
  (\` (let (((\, var) (\, val))) (\,@ body))))
(put 'python-lite:let1 'lisp-indent-function 2)
(defmacro python-lite:and-let*
  (bindings &rest body)
  "imported from srfi-2"
  (reduce (function
           (lambda (binding r)
             (let ((head (car binding)))
               (cond ((and (atom head) (symbolp head))
                      (\` (let ((\, binding)) (when (\, head) (\, r)))))
                     ((listp head)
                      (\` (when (\, head) (\, r))))
                     (t (error "and-let*: invalid head %s" head))))))
          bindings
          :from-end
          t
          :initial-value
          (\` (progn (\,@ body)))))
(put 'python-lite:and-let* 'lisp-indent-function 1)

(defvar python-lite:python-command "python")
(defvar python-lite:doc-url "http://docs.python.org/library/")

(defvar python-lite:all-modules nil)
(defun python-lite:all-modules (&optional force-reloadp)
  (when force-reloadp (setq python-lite:all-modules nil))
  (or python-lite:all-modules
      (setq python-lite:all-modules (python-lite:all-modules-1))))

(defun python-lite:all-modules-1 ()
  (let* ((cmd (format "%s -c 'import pydoc; import sys; pydoc.ModuleScanner().run(lambda path,modname,desc : sys.stdout.write(modname+\"\\n\"))' 2>/dev/null | sort -u" 
                      python-lite:python-command))
         (modules-str (shell-command-to-string cmd)))
    (split-string modules-str "\n")))

(defun python-lite:reload-all-modules () (interactive)
  (message "collecting all modules ...")
  (python-lite:all-modules t)
  (message "...done"))


(defun python-lite:complete-module (init)
  (let* ((minibuffer-local-completion-map
         (python-lite:rlet1 kmap (copy-keymap minibuffer-local-completion-map)
           (define-key kmap "\C-c\C-u" 'python-lite:reload-all-modules)))
         (table (all-completions init (python-lite:all-modules))))
    (cond ((= 1 (length table)) (car table))
          (t (completing-read "module:(update C-c C-u) " table nil nil init)))))

;;; interactive 
(defun python-lite:eval/compile () (interactive)
  (python-lite:and-let* ((file (buffer-file-name)))
    (compile
     (format "%s %s" python-lite:python-command file))))
(defun python-lite:insert-module () (interactive)
  (let* ((word (or (thing-at-point 'word) ""))
         (module (python-lite:complete-module 
                  (try-completion word (python-lite:all-modules)))))
    (delete-backward-char (length word))
    (insert (format "import %s" module) "\n")))

(defun python-lite:pydoc () (interactive)
  (let* ((word (or (thing-at-point 'word) ""))
         (module (python-lite:complete-module 
                  (try-completion word (python-lite:all-modules)))))
    (shell-command (format "pydoc %s" module))))

(defun python-lite:webhelp () (interactive)
  (let* ((word (or (thing-at-point 'word) ""))
         (module (python-lite:complete-module 
                  (try-completion word (python-lite:all-modules)))))
    (browse-url (concat python-lite:doc-url module ".html"))))
  
(provide 'python-lite)