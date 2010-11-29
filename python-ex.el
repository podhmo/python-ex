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
;; (require 'python-ex)

;; (defun python-ex:setup ()
;;   (let ((key-map '(("\C-cu" . python-ex:select-modules-with-anything)
;;                   ("\C-cS" . python-ex:run-repl)
;;                   ("\C-c\C-k" . python-ex:kill-repl)
;;                   ("\C-c\C-l" . python-ex:load-file)
;;                   ("\C-c\C-r" . python-ex:send-region)
;;                   ("\C-c\C-b" . python-ex:send-buffer))))
;;     (loop for (k . fun) in key-map
;;           do (define-key python-mode-map k fun))))

;; (add-hook 'python-mode-hook 'python-ex:setup)

;; (defun python-ex:inferior-setup ()
;;   (define-key inferior-python-mode-map (kbd "<tab>")
;;     'python-ex:ipython-complete-with-anything))

;; (add-hook 'inferior-python-mode-hook
;;           'python-ex:inferior-setup)

;; ;; personal setting
;; (setq python-ex:base-dir (expand-file-name "~/myproject/python-ex/"))

;;;; code ;;;;

(eval-when-compile (require 'cl))
(or (require 'anything nil t) (message "anything is good plugin for everyone."))
(require 'python)
(defmacro python-ex:with-anything (&rest actions)
  `(when (fboundp 'anything)
     ,@actions))
;; (while (re-search-forward "python-ex:" nil t 1)(compose-region (match-beginning 0) (match-end 0) ?＠))

;;; util macro
;;(util-macro-install "python-ex:")

(defmacro python-ex:with-lexical-bindings
  (syms &rest body)
  (let ((clauses (loop for sym in syms collect (\` ((\, sym) (\, sym))))))
    (\` (lexical-let ((\,@ clauses)) (\,@ body)))))
(put 'python-ex:with-lexical-bindings 'lisp-indent-function 1)
(defmacro python-ex:with-gensyms
  (syms &rest body)
  (let ((bindings (mapcar (lambda (x) (\` ((\, x) '(\, (gensym))))) syms)))
    (\` (let ((\,@ bindings)) (\,@ body)))))
(put 'python-ex:with-gensyms 'lisp-indent-function 1)
(defmacro python-ex:cute
  (&rest args)
  "impoterd from srfi-28"
  (let* ((partial-args (list))
         (dotted-p nil)
         (pre-eval-sexps (list))
         (args* (mapcar (lambda (x)
                          (cond ((and (listp x) (not (eq 'quote (car x))))
                                 (python-ex:rlet1
                                     g
                                     (gensym)
                                   (push (\` ((\, g) (\, x)))
                                         pre-eval-sexps)))
                                ((eq '<> x)
                                 (python-ex:rlet1
                                     x*
                                     (gensym)
                                   (push x* partial-args)))
                                ((eq '<\.\.\.> x)
                                 (python-ex:rlet1
                                     x*
                                     (gensym)
                                   (setq dotted-p t)
                                   (push '&rest partial-args)
                                   (push x* partial-args)))
                                (t x)))
                        args)))
    (setq partial-args (nreverse partial-args))
    (cond (dotted-p (\` (lexical-let* ((\,@ (nreverse pre-eval-sexps)))
                          (lambda (\, partial-args) (apply (\,@ args*))))))
          (t (\` (lexical-let* ((\,@ (nreverse pre-eval-sexps)))
                   (lambda (\, partial-args) (funcall (\,@ args*)))))))))

(defmacro python-ex:cut
  (&rest args)
  "impoterd from srfi-28"
  (let* ((partial-args (list))
         (dotted-p nil)
         (args* (mapcar (lambda (x)
                          (cond ((eq '<> x)
                                 (python-ex:rlet1
                                     x*
                                     (gensym)
                                   (push x* partial-args)))
                                ((eq '<\.\.\.> x)
                                 (python-ex:rlet1
                                     x*
                                     (gensym)
                                   (setq dotted-p t)
                                   (push '&rest partial-args)
                                   (push x* partial-args)))
                                (t x)))
                        args)))
    (setq partial-args (nreverse partial-args))
    (cond (dotted-p (\` (lambda (\, partial-args) (apply (\,@ args*)))))
          (t (\` (lambda (\, partial-args) (funcall (\,@ args*))))))))

(defmacro python-ex:rlet1
  (var val &rest body)
  "imported from gauche"
  (\` (python-ex:let1 (\, var) (\, val) (\,@ body) (\, var))))
(put 'python-ex:rlet1 'lisp-indent-function 2)
(defmacro python-ex:let1
  (var val &rest body)
  "imported from gauche"
  (\` (let (((\, var) (\, val))) (\,@ body))))
(put 'python-ex:let1 'lisp-indent-function 2)
(defmacro python-ex:and-let*
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
(put 'python-ex:and-let* 'lisp-indent-function 1)
(defmacro python-ex:alambda
  (args &rest body)
  "Anaphoric lambda. enable to self recursion using `self' anaphorar"
  (\` (labels ((self (\, args) (\,@ body))) (function self))))
(put 'python-ex:alambda 'lisp-indent-function 1)
(defmacro python-ex:aand
  (&rest args)
  "Anaphoric and. anaphorar is `it'"
  (cond ((null args)
         t)
        ((null (cdr args))
         (car args))
        (t (\` (python-ex:aif
                   (\, (car args))
                   (python-ex:aand (\,@ (cdr args))))))))

(defmacro python-ex:aif
  (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (\` (let ((it (\, test-form))) (if it (\, then-form) (\,@ else-forms)))))
(put 'python-ex:aif 'lisp-indent-function 2)

;;;; setting
(defvar python-ex:auto-send-import-module-p t)
(defvar python-online-document-url "http://docs.python.org")
(defvar python-ex:eval-type 'external) ;; 'external or 'internal
(defvar python-ex:python-command python-command)
(defvar python-ex:temoprary-directory "/tmp/")
(defvar python-ex:buffer-name "*Python-ex*")
(defvar python-ex:buffer nil)
(defvar python-ex:base-dir default-directory)
(defvar python-ex:prompt-rx "\n+\\[0;32mIn \\[\\[1;32m[0-9]+\\[0;32m\\]: \\[0m") ;; ">>> " classic
(defun python-ex:buffer ()
  (or python-ex:buffer
      (progn (python-ex:proc)
             python-ex:buffer)))

(defvar python-ex:repl-command "ipython")
(defvar python-ex:auto-scroll-p nil) ;;obsolete?

;;; debug
(defvar python-ex:debug-info-p nil)
(defmacro python-ex:debug-info (fmt &rest args)
  (if python-ex:debug-info-p 
      (python-ex:with-gensyms (str)
        `(let* ((,str (format ,fmt ,@args))
                (,str (mapconcat 'identity (split-string ,str "\n") "\n  ")))
           (message (concat "[info]\n  " ,str))))))

(defun python-ex:debug-info-enable (k) (interactive "P")
  (python-ex:let1 val (case k
                        ((1) t)
                        ((-1) nil)
                        (otherwise (not python-ex:debug-info-p)))
    (message "python-ex:debug-info-p is `%s`" val)
    (setq python-ex:debug-info-p val)))

;;; async util
(defvar python-ex:async-error-buffer " *python-ex async error*")
(defmacro python-ex:with-async (args &rest actions)
  (declare (indent 1))  
  `(python-ex:with-async* 0.1 ,args :action (progn ,@actions)))

(defmacro* python-ex:with-async* (secs args &key repeat-p action)
  (declare (indent 2))
  (python-ex:with-gensyms (err)
    `(python-ex:with-lexical-bindings ,args
       (run-with-idle-timer
        ,secs ,repeat-p
        (lambda ()
          (condition-case ,err
              ,action
            (error
             (with-current-buffer (get-buffer-create python-ex:async-error-buffer)
               (insert (format "%s\n" ,err))
               (insert "---KILL ALL TIMERS---\n")
               (python-ex:cancel-all-async-timers))
             (display-buffer python-ex:async-error-buffer))))))))

(defvar python-ex:async-timers-alist nil)

(defun python-ex:cancel-async-timer (timer-name)
  (python-ex:let1 timer (assoc-default timer-name python-ex:async-timers-alist)
    (cancel-timer timer)
    (setq python-ex:async-timers-alist
          (remassoc timer-name python-ex:async-timers-alist))))

(defun python-ex:cancel-all-async-timers () (interactive)
  (loop for (_ . timer) in python-ex:async-timers-alist
        do (progn (message "remove %s timer" timer)
                  (cancel-timer timer)))
  (setq python-ex:async-timers-alist nil))

(defun python-ex:wait-for (dsecs finish-check &optional call-back)
  (lexical-let* ((name (gensym))
                 (timer (python-ex:with-async*
                            dsecs
                            (dsecs finish-check call-back)
                          :repeat-p t
                          :action (progn
                                    (python-ex:debug-info
                                     "python-ex:wait-for -- in wait-for loop [checker %s]"
                                     finish-check)
                                    (when (funcall finish-check)
                                      (when (and call-back (functionp call-back))
                                        (funcall call-back))
                                      (python-ex:cancel-async-timer name))
                                    (python-ex:debug-info "python-ex:wait-for after cancel-timer")))))
    (push (cons name timer) python-ex:async-timers-alist)))


;;;; repl (interactive shell)
(lexical-let ((installed nil))
  (defun python-ex:install () (interactive)
    (unless installed
      (defalias 'run-python-original 'run-python)
      (defalias 'python-proc-original 'python-proc)
      (defalias 'run-python 'python-ex:run-repl)
      (defalias 'python-proc 'python-ex:proc))))

(defun python-ex:command-list (cmd &rest rest)
  (cond ((string-match-p "ipython" cmd) 
         `(,cmd "-nobanner" ,@rest))
        ;; `(,cmd "-cl" "-noreadline" "-nobanner" ,@rest))
        (t (list cmd))))

(defun python-ex:kill-repl (&optional erase-buffer-p) (interactive "P")
  (when (comint-check-proc (python-ex:buffer))
    (python-ex:let1 process (python-ex:proc)
      (set-process-filter process nil)
      (delete-process process))
    (when erase-buffer-p
      (with-current-buffer (python-ex:buffer)
        (erase-buffer)))))

(defun python-ex:run-repl (&optional noshow cmd)
  (interactive (if current-prefix-arg
                   (list nil (read-string "Run Python: " python-ex:repl-command))
                   (list nil python-ex:repl-command)))
  (unless cmd (setq cmd python-ex:repl-command))
  ;;  (python-ex:check-version cmd) not implemented
  (setq python-ex:repl-command cmd) ;; danger

  (unless (comint-check-proc python-ex:buffer)
    (let* ((buf (get-buffer-create python-ex:buffer-name))
           (cmdlist (python-ex:command-list cmd)))
      (with-current-buffer 
          (apply 'make-comint-in-buffer "Python" buf (car cmdlist) nil (cdr cmdlist))
        (when (string-match-p "ipython" cmd)
          (require 'ansi-color)
          (ansi-color-for-comint-mode-on))
        (comint-simple-send 
         (get-buffer-process buf) (python-ex:format/ex-module "")) ;;
        (setq python-ex:buffer (current-buffer))
        ;; (accept-process-output (get-buffer-process (python-ex:buffer)) 0 500)
        (inferior-python-mode))))

  ;;  (if (derived-mode-p 'python-mode)
  (unless noshow (display-buffer (python-ex:buffer)))
  (python-ex:buffer))

(defun python-ex:proc ()
  (unless (comint-check-proc python-ex:buffer)
    (python-ex:run-repl t))
  (get-buffer-process python-ex:buffer))

;;;; eval syncronized
(defun python-ex:eval (code) (interactive "s") 
  (case python-ex:eval-type
    ((internal) (python-ex:eval-internal code))
    ((external) (python-ex:eval-external code))
    (otherwise (python-ex:let1 fmt "python-ex:eval -- invalid eval-type %s"
                 (error (format fmt python-ex:eval-type))))))

(defun python-ex:gensym-name ()
  (concat python-ex:temoprary-directory 
          "pyex" (symbol-name (gensym))))

(defun python-ex:eval-external (code)
  (python-ex:let1 file (python-ex:gensym-name)
    (with-temp-file file
      (insert code))
    (python-ex:aand
     (shell-command-to-string 
      (format "%s %s" python-ex:python-command file))
     (substring-no-properties it 0 -1))))

(defun python-ex:eval-internal (code-or-send-action &optional async call-back filters)
  (lexical-let* ((buf (list))
                 (done-p nil)
                 (call-back call-back)
                 (code-or-send-action code-or-send-action)
                 (send-action (cond ((functionp code-or-send-action) code-or-send-action)
                                    (t (lambda ()
                                         (comint-simple-send 
                                          (python-ex:proc) code-or-send-action)))))
                 (output-interception
                  (lambda (str) 
                    (python-ex:debug-info  "world==== %S" str)
                    (cond ((null python-ex:eval-reading-p)
                           (python-ex:let1 str (replace-regexp-in-string 
                                                python-ex:prompt-rx "" str)
                             (push str buf))
                           (setq buf (nreverse buf) done-p t))
                          (t (push str buf)))
                    "")))
    (python-ex:eval-internal-1 
     :send-action send-action
     :filters (cons output-interception filters)
     :async async
     :call-back (lambda ()
                  (while (null done-p)
                    (python-ex:debug-info "python-ex:eval-internal --  outer-sleep")
                    (sleep-for 0 10))
                  (python-ex:let1 r (mapconcat 'identity buf "")
                    (cond (call-back (funcall call-back r))
                          (t r)))))))

(defvar python-ex:eval-reading-p nil) ;;internal-variable for polling
(defun* python-ex:eval-internal-1 (&key send-action filters call-back async)
  (lexical-let ((end-checker (lambda (str)
                               (python-ex:debug-info "===hello %S" str)
                               (when (string-match-p python-ex:prompt-rx str)
                                 (setq python-ex:eval-reading-p nil))
                               str)))
    (let* ((old-filters comint-preoutput-filter-functions)
           (filters* (cons end-checker filters)))

      (setq python-ex:eval-reading-p t)

      (cond (async
             (setq comint-preoutput-filter-functions filters*)
             (funcall send-action)
             (python-ex:debug-info
              "python-ex:preoutput-filers-cont -- *async* start")
             (python-ex:with-lexical-bindings (call-back old-filters)
               (lexical-let* ((last-call-back
                               (lambda () 
                                 (setq comint-preoutput-filter-functions old-filters)
                                 (funcall call-back))))
                 (python-ex:wait-for 0.5
                                     (lambda () (null python-ex:eval-reading-p))
                                     last-call-back))))
            (t
             (unwind-protect
                 (let ((comint-preoutput-filter-functions filters*)) ;;special variable
                   (funcall send-action)
                   (while python-ex:eval-reading-p ;;polling
                     (python-ex:debug-info "python-ex:eval-internal-1 -- inner-sleep")
                     (sleep-for 0 100))
                   (when call-back (funcall call-back)))
               (setq comint-preoutput-filter-functions old-filters)))))))
;; async main
(defun python-ex:eval-async (code &optional call-back) (interactive "s\na")
  (case python-ex:eval-type
    ((internal) (python-ex:eval-internal-async code call-back))
    ((external) (python-ex:eval-external-async code call-back))
    (otherwise (python-ex:let1 fmt "python-ex:eval -- invalid eval-type %s"
                 (error (format fmt python-ex:eval-type))))))

(defun python-ex:eval-file-async-1 (tmpbuf file args &optional call-back*)
  (python-ex:with-lexical-bindings (call-back* tmpbuf)
    (set-process-sentinel
     (apply 'start-process-shell-command 
            "python-ex:external" tmpbuf python-ex:python-command file args)
     (lambda (status &rest args)
       (cond (call-back* (funcall call-back*))
             (t (display-buffer tmpbuf)))))))

(defun python-ex:eval-file-async (file &optional args call-back) (interactive "ffile:\nsargs:")
  (python-ex:let1 tmpbuf (format "*%s output*" file)
    (python-ex:eval-file-async-1 tmpbuf file args call-back)))

(defun python-ex:eval-external-async (source-code &optional call-back)
    (python-ex:let1 file (python-ex:gensym-name)
      (with-temp-file file
        (insert source-code))
      (lexical-let ((tmpbuf (python-ex:gensym-name)))
        (python-ex:with-lexical-bindings (call-back)
          (python-ex:eval-file-async-1
           tmpbuf file nil
           (lambda ()
             (python-ex:let1 r (with-current-buffer tmpbuf
                                 (buffer-string))
               (kill-buffer tmpbuf)
               (cond (call-back (funcall call-back r))
                     (t (message "pyex-result: %s" r))))))))))
;; ;;
;; ;;; *buggy* ultrasensitive for output from a [i]python shell
;; ;;
(defun python-ex:eval-internal-async (code-or-send-action &optional call-back)
  (python-ex:let1 call-back (or call-back (lambda (r) (message "pyex-result: %s" r)))
    (python-ex:eval-internal code-or-send-action t call-back)))

;;; repl-action
(defmacro python-ex:with-action-repl-buffer (&rest actions)
  (python-ex:with-gensyms (w)
    `(python-ex:and-let* ((,w (find (python-ex:buffer) (window-list) :key 'window-buffer)))
       (with-selected-window ,w
         ,@actions))))

(defun python-ex:auto-scroll-callback (&rest ignore)
  (python-ex:debug-info "buf:%s window:" (current-buffer) (selected-window))
  (python-ex:with-action-repl-buffer
   (goto-char (point-max))))

(defun python-ex:send-string (code &optional call-back)
  (cond ((or call-back python-ex:auto-scroll-p)
         (python-ex:eval-internal-async 
          code 
          (or call-back 'python-ex:auto-scroll-callback)))
        (t (comint-simple-send (python-ex:proc) code))))

(defun python-ex:send-region (beg end &optional call-back) (interactive "r\nP")
  (cond (python-ex:auto-scroll-p
         (python-ex:eval-internal-1
          :async t
          :send-action :send-action (lambda () (comint-send-region (python-ex:proc) beg end))
          :call-back (or call-back 'python-ex:auto-scroll-callback)))
        (t (comint-send-region (python-ex:proc) beg end))))

(defun python-ex:send-buffer () (interactive)
  (python-ex:send-region (point-min) (point-max)))

(defun python-ex:send-defun ()  (interactive)
  (save-excursion
    (python-ex:send-region
     (progn (beginning-of-defun) (point))
     (progn (end-of-defun) (point)))))

(defun python-ex:load-file (file) (interactive "ffile:")
  (let ((module (replace-regexp-in-string "\\..+$" "" (file-name-nondirectory file)))
        (dir (python-ex:aif (file-name-directory file) it default-directory)))
    (python-ex:debug-info 
     "python-ex:load-file --- %s"
     (format "%s = ex.load(%S,%S)" module module dir))
    (python-ex:send-string 
     (format "%s = ex.load(%S,%S)" module module dir))
    (message "python-ex: --- %s is loaded." file)))

;;;; import
(defvar python-ex:all-modules-cache-buffer nil)

(defun python-ex:format/ex-module (fmt &rest args)
  (let ((prepare-string (format "
import sys
sys.path.append(%S)
import ex"
                                python-ex:base-dir)))
    (concat prepare-string "\n" (apply 'format fmt args))))

  
(defun python-ex:all-modules-cache-buffer (&optional force-reloadp asyncp showp)
  (when force-reloadp
    (setq python-ex:all-modules-cache-buffer nil))
  (or python-ex:all-modules-cache-buffer
      (python-ex:let1 code (python-ex:format/ex-module "ex.print_all_modules()")
        (python-ex:with-lexical-bindings (showp)
          (lexical-let* ((buf (get-buffer-create " *python modules*"))
                         (insert-result-to-buffer
                          (lambda (r) 
                            (with-current-buffer buf
                              (erase-buffer)
                              (insert r))
                            (message "python-ex:all-modules-cache-buffer --- ... done")
                            (when showp (display-buffer " *python modules*")))))
            (message "python-ex:all-modules-cache-buffer --- collecting python modules ...")
            (cond (asyncp (python-ex:eval-external-async code insert-result-to-buffer))
                  (t (funcall insert-result-to-buffer (python-ex:eval-external code))))
            (setq python-ex:all-modules-cache-buffer buf))))))


(defun* python-ex:message-with-buffer (call-back &optional (name "*Pyex Help") (reuse-buffer-p nil))
  (python-ex:rlet1 buf
      (cond (reuse-buffer-p (get-buffer-create name))
            (t (generate-new-buffer name)))
    (with-current-buffer buf
      (python-ex:let1 inhibit-read-only nil
        (when reuse-buffer-p
          (erase-buffer))
        (funcall call-back)))))

(defun* python-ex:message-with-other-buffer (call-back &optional (name "*Pyex Help") (reuse-buffer-p nil))
  (python-ex:let1 buf
      (python-ex:message-with-buffer call-back name reuse-buffer-p)
    ;; (with-current-buffer buf
    ;;   (goto-char (point-min)))
    (set-window-start (display-buffer buf) 1)))

(defun python-ex:loaded-modules () (interactive) 
  ;;async-internal is not support, so using sync version
  (lexical-let ((loaded-modules (python-ex:eval-internal
                                 "
import types
for k, e in vars().items():
    if isinstance(e, types.ModuleType):
        print \"%15s : %s\" % (k, e)
"
                                 )))
    (python-ex:message-with-other-buffer
     (lambda () 
       (insert loaded-modules)
       (ansi-color-apply-on-region (point-min) (point-max)))
     "*pyex:loaded-modules*" t)))


;;; url-button
(put 'python-ex:link-button 'face 'link)
(put 'python-ex:link-button 'mouse-face 'highlight)
(put 'python-ex:link-button 'keymap button-map)
(put 'python-ex:link-button 'type 'button)
(put 'python-ex:link-button 'help-echo "mouse-2, RET: Push this button")
(put 'python-ex:link-button 'evaporate t)
(put 'python-ex:link-button 'rear-nonsticky t)
(put 'button 'button-category-symbol 'python-ex:link-button)

(defun python-ex:clickable-url ()
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(f\\|ht\\)tps?://[^)>\s)\n]+" nil t 1)
        (set-text-properties (match-beginning 0)
                             (match-end 0)
                             (lexical-let ((url (match-string-no-properties 0)))
                               `(follow-link t  
                                             action ,(lambda (b) (browse-url url))
                                             category python-ex:link-button
                                             button (t))))))))

;;; anything interface
(python-ex:with-anything
 (defun python-ex-anything:help (c)
   (python-ex:message-with-other-buffer
    (lambda ()
      (insert
       (python-ex:eval-external
        (format "
import %s
help('%s')" 
                c c)))
     (python-ex:clickable-url))))
 
 (defun python-ex-anything:web-help (c)
   (browse-url
    (format "%s/library/%s.html" python-online-document-url c)))

 (defun python-ex-anything:find-module-other-frame (_)
   (let* ((c (anything-get-selection nil t))
          (file (cadr (split-string c " +:"))))
     (find-file-other-frame file)))

 (lexical-let (sys-path)
   (defun python-ex:sys-path (&optional force-reload-p)
     (when force-reload-p (setq sys-path nil))
     (or sys-path
         (let* ((cmd "
D=[d for d in sys.path if not \"bin\" in d]
print ','.join(D)")
                (sys-paths-str (python-ex:eval-internal cmd)))
           (setq sys-path (cdr (split-string sys-paths-str ",")))))))

 (defun python-ex:module->path (module &optional force-reload-p)
   (python-ex:let1 path (replace-regexp-in-string "\\." "/" module)
     (loop for dir in (python-ex:sys-path force-reload-p)
           for d = (concat dir "/" path)
           if (file-exists-p d)
           return d
           else
           for file = (concat d ".py")
           when (file-exists-p file)
           return file)))

 (defun python-ex:modules-cache-add-path (&optional buffer force-rewrite-p)
   (let ((buffer (or buffer (python-ex:all-modules-cache-buffer))))
     (with-current-buffer buffer
       (save-excursion 
         (goto-char (point-min))
         (when (or force-rewrite-p
                   (null (save-excursion (re-search-forward ":" nil t 1))))
           (message "%s: add path (for find-module) ..." buffer)
           (while (re-search-forward "^.+" nil t 1)
             (python-ex:and-let*
                 ((module (match-string-no-properties 0))
                  (new-text (python-ex:module->path module)))
               (delete-region (point-at-bol) (point-at-eol))
               (insert (format "%-35s:%s" module new-text)))))))))


 (defun python-ex-anything:insert-import-module (c)
   (python-ex:let1 cmd (format "import %s" c)
     (when python-ex:auto-send-import-module-p
       (python-ex:send-string cmd)
       (insert cmd))))

 (defvar python-ex:anything-c-source-all-modules
   '((name . "import")
     (init . (lambda ()
               (python-ex:with-async nil
                 (python-ex:modules-cache-add-path))
               (anything-candidate-buffer 
                (python-ex:all-modules-cache-buffer nil t))))
     (candidates-in-buffer)
     (display-to-real . (lambda (c) (car (split-string c " +:"))))
     (action . (("insert" . python-ex-anything:insert-import-module)
                ("find-module-other-frame" . python-ex-anything:find-module-other-frame)
                ("help" . python-ex-anything:help)
                ("web-help" . python-ex-anything:web-help)))
     (persistent-action . python-ex-anything:help)))

   (defvar python-ex:anything-daily-use-modules-file 
     (concat python-ex:base-dir "daily-modules.py"))

   (defvar python-ex:anything-c-source-daily-use-modules
     '((name . "daily modules")
       (candidates-file . python-ex:anything-daily-use-modules-file)
       (display-to-real . (lambda (c) (car (split-string c " +:"))))
       (action . (("insert" . python-ex-anything:insert-import-module)
                  ("find-module-other-frame" . python-ex-anything:find-module-other-frame)
                  ("help" . python-ex-anything:help)
                  ("web-help" . python-ex-anything:web-help)))
       (persistent-action . python-ex-anything:help)))

   (defun python-ex:select-modules-with-anything () (interactive)
     (let ((sources
            '(python-ex:anything-c-source-daily-use-modules
              python-ex:anything-c-source-all-modules))
           (keymap
            (python-ex:rlet1 kmp (copy-keymap anything-map)
              (define-key kmp "\C-c\C-u" (lambda () (interactive) 
                                           (message "recollect modules ...")
                                           (python-ex:all-modules-cache-buffer t t)
                                           (python-ex:with-async nil
                                             (python-ex:modules-cache-add-path)))))))
       (anything :prompt "module(C-c C-u recollect modules) " 
                 :sources sources :buffer " *python import*" :keymap keymap)))

   (defvar python-ex:anything-c-source-input-histories 
     '((name . "input history")
       (candidates
        . (lambda () (split-string (python-ex:eval-internal "%history") "\n")))
       (display-to-real (lambda (c) (cadr (split-string ": " c))))
       (action . (("send-string" . 
                 (lambda (c)
                   (message "send -- %S --" c)
                   (python-ex:send-string c)))
                ("kill-new" . kill-new))))
     "select sentence from all inputed histories ")

   ;; (defvar python-ex:anything-c-source-input-histories
   ;;   '((name . "input history")
   ;;     (init 
   ;;      . (lambda ()
   ;;          (python-ex:let1 histories-array
   ;;              (with-current-buffer (python-ex:buffer)
   ;;                (delete-duplicates (cddr comint-input-ring)
   ;;                                   :test 'string-equal))
   ;;            (with-current-buffer (anything-candidate-buffer 'global)
   ;;              (erase-buffer)
   ;;              (dotimes (i (length histories-array))
   ;;                (python-ex:aand
   ;;                 (aref histories-array i)
   ;;                 (insert it "\n")))))))
   ;;     (candidates-in-buffer)
   ;;     (search-from-end)
   ;;     (action . python-ex:send-string))
   ;; "select sentence from directly inputed histories")
   
   (defun python-ex:input-histories-with-anything () (interactive)
     (anything '(python-ex:anything-c-source-input-histories)))

   (defun* python-ex:anything-candidate-buffer-from-string 
       (string &optional (bufname " *pyex:candidate") (reuse-buffer-p t))
     (anything-candidate-buffer
      (python-ex:message-with-buffer
       (lambda () (insert (python-ex:eval-internal string)))
       bufname reuse-buffer-p)))

   (defvar python-ex:anything-c-source-input-magick-commands
     '((name . "%magick comment")
       (init . (lambda ()
                 (python-ex:let1 buf (python-ex:anything-candidate-buffer-from-string
                                      "%quickref" " *pyexc:quickref")
                   (with-current-buffer buf
                     (goto-char (point-min))
                     (when (re-search-forward "The following magic functions are currently available:" nil t)
                       (delete-region (point-min) (1+ (point))))
                     (while (re-search-forward ":\n" nil t 1)
                       (replace-match ":"))))))
       (display-to-real . (lambda (c) (car (split-string c ":"))))
       (candidates-in-buffer)
       (action . python-ex:send-string)))

   (defun python-ex:input-magick-commands-with-anything () (interactive)
     (anything '(python-ex:anything-c-source-input-magick-commands)))

 ;;; pydoc interface

   (defun python-ex-anything:pydoc-external (keyword)
     (python-ex:with-lexical-bindings (keyword)
       (lexical-let ((bufname "*pydoc*"))
         (python-ex:message-with-buffer
          (lambda () 
            (python-ex:with-lexical-bindings (bufname)
              (set-process-sentinel
               (start-process-shell-command
                "pydoc-keyword" bufname "pydoc" keyword)
               (lambda (&rest _)
                 (set-window-start (display-buffer bufname) 1)))))
          bufname t))))

   (defvar python-ex:anything-c-source-pydoc
     '((name . "pydoc")
       (display-to-real . (lambda (c)
                            (car (split-string c " -"))))
       (candidates . (lambda ()
                       (start-process-shell-command 
                        "pydoc" nil
                        "pydoc" "-k" anything-input)))
       (action . (("pydoc" . (lambda (c) (python-ex-anything:pydoc-external c)))))
       (persistent-action . python-ex-anything-pydoc-external)))

   (defun python-ex:pydoc-with-anything () (interactive)
     (let* ((word (word-at-point))
            (word (if (string-equal "\n" word) "" word)))
       (anything '(python-ex:anything-c-source-pydoc) word)))

   
 ;;; ipython dynamic complete

   (defun python-ex:ipython-complete-with-anything () (interactive)
     (python-ex:and-let*
         ((end (point))
          (str (save-excursion
                 (skip-chars-backward "0-9a-zA-Z._%$")
                 (buffer-substring-no-properties (point) end)))
          (command
           (format "print('\\n'.join(__IP.complete(%S)))" str))
          (source
           `((name . ,(format "ipython complete: %S" str))
             (init . (lambda ()
                       (python-ex:anything-candidate-buffer-from-string
                        ,command " *Pyex:completes*")))
             (candidates-in-buffer)
             (candidate-number-limit . 100000)
             (search-from-end) ;; adhoc fix
             (action . (lambda (c)
                         (delete-backward-char ,(length str))
                         (insert c))))))
          (declare (special anything-execute-action-at-once-if-one))
            (let ((anything-execute-action-at-once-if-one t)
               (keymap (python-ex:rlet1 kmp (copy-keymap anything-map)
                         (define-key kmp (kbd "<tab>") 'anything-next-line)
                         (define-key kmp (kbd "<backtab>") 'anything-previous-line))))
           (anything :sources (list source) :keymap keymap))))

(when (require 'anything-show-completion nil t)
  (dolist (f '(python-ex:ipython-complete-with-anything
               python-ex:select-modules-with-anything
               python-ex:input-histories-with-anything
               python-ex:pydoc-with-anything
               python-ex:input-magick-commands-with-anything))
    (use-anything-show-completion f '(length anything-complete-target))))
   )
 ;; (save-excursion (goto-char (point-min)) (loop while (re-search-forward "(defvar.*c-source" nil t) collect (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

 (provide 'python-ex)
