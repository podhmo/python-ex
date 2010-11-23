(setq python-ex:debug-info-p t)

(require 'cl)
(require 'anything nil t)
(defmacro python-ex:with-anything (&rest actions)
  `(when (fboundp 'anything)
     ,@actions))
;;(util-macro-install "python-ex:")
;; (while (re-search-forward "python-ex:" nil t 1)(compose-region (match-beginning 0) (match-end 0) ?＠))

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
(defvar python-ex:eval-type 'external) ;; 'external or 'internal
(defvar python-ex:python-command python-command)
(defvar python-ex:temoprary-directory "/tmp/")
(defvar python-ex:buffer-name "*Python-ex*")
(defvar python-ex:buffer nil)
(defvar python-ex:repl-command "ipython")
(defvar python-ex:auto-scroll-p t)

;;; debug
(defvar python-ex:debug-info-p nil)
(defmacro python-ex:debug-info (fmt &rest args)
  (if python-ex:debug-info-p 
      (python-ex:with-gensyms (str)
        `(let* ((,str (format ,fmt ,@args))
                (,str (mapconcat 'identity (split-string ,str "\n") "\n  ")))
           (message (concat "[info]\n  " ,str))))))
;;;;
(lexical-let ((installed nil))
  (defun python-ex:install () (interactive)
    (unless installed
      (defalias 'run-python-original 'run-python)
      (defalias 'python-proc-original 'python-proc)
      (defalias 'run-python 'python-ex:run-repl)
      (defalias 'python-proc 'python-ex:proc))))

(defun python-ex:command-list (cmd &rest rest)
  (cond ((string-match-p "ipython" cmd) 
         `(,cmd "-cl" "-noreadline" "-nobanner" ,@rest))
        (t (list cmd))))

(defun python-ex:kill-repl () (interactive)
  (when (comint-check-proc python-ex:buffer)
    (python-ex:let1 process (python-ex:proc)
      (set-process-filter process nil)
      (delete-process process))))

(defun python-ex:run-repl (&optional noshow cmd)
  (interactive (if current-prefix-arg
                   (list (read-string "Run Python: " python-ex:repl-command) nil t)
                   (list python-ex:repl-command)))
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
        (setq python-ex:buffer (current-buffer))
        ;; (accept-process-output (get-buffer-process python-ex:buffer) 0 500)
        (inferior-python-mode))))

  ;;  (if (derived-mode-p 'python-mode)
  (unless noshow (display-buffer python-ex:buffer))
  python-ex:buffer)

(defun python-ex:proc ()
  (unless (comint-check-proc python-ex:buffer)
    (python-ex:run-repl t))
  (get-buffer-process python-ex:buffer))

;;;; eval
(defun python-ex:eval (code) (interactive "s") 
  (case python-ex:eval-type
    ((internal) (python-ex:eval-internal code))
    ((external) (python-ex:eval-external code))
    (otherwise (python-ex:let1 fmt "python-ex:eval -- invalid eval-type %s"
                 (error (format fmt python-ex:eval-type))))))

(defun python-ex:gensym-filename ()
  (concat python-ex:temoprary-directory 
          "pyex" (symbol-name (gensym))))

(defun python-ex:eval-external (code)
  (python-ex:let1 file (python-ex:gensym-filename)
    (with-temp-file file
      (insert code))
    (python-ex:aand
     (shell-command-to-string 
      (format "%s %s" python-ex:python-command file))
     (substring-no-properties it 0 -1))))

(defmacro python-ex:with-preoutput-filters (filters &rest actions)
  (declare (indent 1))
  (python-ex:with-gensyms (orig)
    `(let* ((,orig comint-preoutput-filter-functions)
            (comint-preoutput-filter-functions ,filters))
       (unwind-protect
           (progn ,@actions)
         (setq comint-preoutput-filter-functions ,orig)))))

;; (defmacro python-ex:with-preoutput-filter (filter &rest actions)
;;   (declare (indent 1))
;;   `(python-ex:with-preoutput-filters (list ,filter)
;;      ,@actions))

(defun python-ex:eval-internal (source-code &optional asyncp last-call-back) ;; must not use asyncp and last-call-back directly
  (lexical-let* ((buf (list))
                 (done-p nil)
                 (output-interception
                  (lambda (str) 
                    (python-ex:debug-info  "world==== %S" str)
                    (cond ((null python-ex:eval-reading-p)
                           (python-ex:let1 str (replace-regexp-in-string "\n?>>> " "" str)
                             (push str buf))
                           (setq buf (nreverse buf) done-p t))
                          (t (push str buf)))
                    "")))
    (python-ex:with-lexical-bindings (last-call-back source-code asyncp)
      (python-ex:eval-internal-1 
       :send-action (lambda () (comint-simple-send (python-ex:proc) source-code))
       :filters (list output-interception)
       :async asyncp
       :call-back 
       (lambda ()
         (cond (asyncp
                (python-ex:with-lexical-bindings (last-call-back asyncp done-p buf)
                (python-ex:wait-for 0.01 
                                    (lambda ()
                                      (python-ex:debug-info "python-ex:eval-internal --  async outer-sleep")
                                      (null done-p))
                                    (lambda ()
                                      (python-ex:let1 r (mapconcat 'identity buf "")
                                        (if last-call-back (funcall last-call-back r) r))))))
               (t 
                (while (null done-p)
                  (python-ex:debug-info "python-ex:eval-internal --  outer-sleep")
                  (sleep-for 0 10))
                (python-ex:let1 r (mapconcat 'identity buf "")
                  (if last-call-back (funcall last-call-back r) r)))))))))



(defvar python-ex:eval-reading-p nil) ;;internal-variable for polling
(defun* python-ex:eval-internal-1 (&key send-action filters call-back async)
  (lexical-let ((prompt-rx (format "\n?%s" ">>> ")))
    (let* ((end-check-filter
            (lambda (str)
              (python-ex:debug-info "===hello %S" str)
              (when (string-match-p prompt-rx str)
                (setq python-ex:eval-reading-p nil))
              str))
           (filters* (cons end-check-filter filters)))
      (python-ex:with-preoutput-filters filters*
        (setq python-ex:eval-reading-p t)
        (funcall send-action)
        (cond (async
               (python-ex:debug-info "python-ex:eval-internal-1 -- async start")
               (python-ex:wait-for 1 ;;
                                   (lambda () 
                                     (python-ex:debug-info "python-ex:eval-internal-1 -- reading? %s"
                                                           python-ex:eval-reading-p)
                                     (null python-ex:eval-reading-p))
                                   call-back))
              (t (while python-ex:eval-reading-p ;;polling
                   (python-ex:debug-info "python-ex:eval-internal-1 -- inner-sleep")
                   (sleep-for 0 100))
                 (when call-back (funcall call-back))))))))

;;; async-eval
(defun python-ex:eval-async (code &optional call-back) (interactive "s\na")
  (case python-ex:eval-type
    ((internal) (python-ex:eval-internal-async code call-back))
    ((external) (python-ex:eval-external-async code call-back))
    (otherwise (python-ex:let1 fmt "python-ex:eval -- invalid eval-type %s"
                 (error (format fmt python-ex:eval-type))))))

(defun python-ex:eval-internal-async (source-code &optional call-back)
  (python-ex:with-async* 0.01 (source-code call-back)
    (python-ex:eval-internal 
     source-code t 
     (or call-back (lambda (r) (message "pyex-result: %s" r))))))

(defun python-ex:eval-external-async (source-code &optional call-back)
  (let ((tmpbuf " *python-ex:sentinel*")
        (file (python-ex:gensym-filename)))
    (with-current-buffer (get-buffer-create tmpbuf)
      (erase-buffer))
    (with-temp-file file
      (insert source-code))
    (python-ex:with-lexical-bindings (call-back tmpbuf)
      (set-process-sentinel
       (start-process-shell-command 
        "python-ex:external" tmpbuf python-ex:python-command file)
       (lambda (status &rest args)
         (python-ex:let1 r (with-current-buffer tmpbuf (buffer-string))
           (cond (call-back (funcall call-back r))
                 (t (message "pyex-result: %s" r)))))))))

;;; repl-action
(defvar python-ex:async-error-buffer " *python-ex async error*")

(defmacro python-ex:with-async (args &rest actions)
  `(python-ex:with-async* 0.1 ,args ,@actions))

(defmacro python-ex:with-async* (secs args &rest actions)
  (declare (indent 2))
  (python-ex:with-gensyms (err)
    `(python-ex:with-lexical-bindings ,args
       (run-with-idle-timer
        ,secs nil
        (lambda ()
          (condition-case ,err
              (progn ,@actions)
            (error (with-current-buffer (get-buffer-create python-ex:async-error-buffer)
                     (insert (prin1-to-string ,err)))
                   (display-buffer python-ex:async-error-buffer))))))))

(defun python-ex:wait-for (dsecs finish-check &optional wait-call-back)
  (cond ((funcall finish-check) (when wait-call-back (funcall wait-call-back)))
        (t (python-ex:with-async* dsecs (dsecs finish-check wait-call-back)
             (python-ex:debug-info "python-ex:wait-for -- in wait-for loop")
             (python-ex:wait-for dsecs finish-check wait-call-back)))))

(defmacro python-ex:with-action-repl-buffer (&rest actions)
  (python-ex:with-gensyms (w)
    `(python-ex:and-let* ((,w (find python-ex:buffer (window-list) :key 'window-buffer)))
       (with-selected-window ,w
         ,@actions))))

(defun python-ex:auto-scroll-callback ()
  (python-ex:debug-info "buf:%s window:" current-buffer (selected-window))
  (python-ex:with-action-repl-buffer
   (goto-char (point-max))))

(defun python-ex:send-string (code &optional call-back)
  (cond (python-ex:auto-scroll-p
         (python-ex:with-async (code call-back)
           (python-ex:eval-internal-1
            :send-action (lambda () (comint-simple-send (python-ex:proc) code))
            :call-back (or call-back 'python-ex:auto-scroll-callback))))
        (t (comint-simple-send (python-ex:proc) code))))

(defun python-ex:send-region (beg end &optional call-back) (interactive "r\nP")
  (cond (python-ex:auto-scroll-p
         (python-ex:with-async (beg end call-back)
           (python-ex:eval-internal-1
            :send-action (lambda () (comint-send-region (python-ex:proc) beg end))
            :call-back (or call-back 'python-ex:auto-scroll-callback))))
        (t (comint-send-region (python-ex:proc) beg end))))

(defun python-ex:send-buffer () (interactive)
  (pyton-ex:send-region (point-min) (point-max)))

(defun python-ex:send-defun ()  (interactive)
  (save-excursion
    (python-ex:send-region
     (progn (beginning-of-defun) (point))
     (progn (end-of-defun) (point)))))

;;;; import
(python-ex:with-anything
 (defvar python-ex:all-modules-cache-buffer nil)

 (defun python-ex:all-modules-cache-buffer (&optional force-reloadp asyncp showp)
  (or (and python-ex:all-modules-cache-buffer
           (not force-reloadp))
      (python-ex:let1 code "
import pydoc
import sys
from os.path import dirname

sys.path.remove(dirname(sys.argv[0]))

def all_modules(is_subpath_enable=True):
    r = []
    def callback(path, modname, desc):
        if  is_subpath_enable or modname.find(\".\") < 0:
            r.append(modname)
    pydoc.ModuleScanner().run(callback)
    return sorted(r)

for i in  all_modules():
    print i
"
        (python-ex:with-lexical-bindings (showp)
          (lexical-let ((insert-result-to-buffer
                         (lambda (r) 
                           (python-ex:let1 buf (get-buffer-create " *python modules*")
                             (with-current-buffer buf
                               (erase-buffer)
                               (insert r)
                               (when showp (display-buffer " *python modules*"))
                               (setq python-ex:all-modules-cache-buffer buf))))))
            (cond (asyncp (python-ex:eval-external-async code insert-result-to-buffer))
                  (t (funcall insert-result-to-buffer (python-ex:eval-external code))))))))))
        
;;          (help 
;;           (lambda (c)
;;             (python-ex:message-with-other-buffer
;;              (lambda ()
;;                (python-ex:eval
;;                 (format "
;; import %s
;; help('%s')" 
;;                         c c)))))))
;;      (let ((source
;;             `((name . "import")
;;               (init . (lambda ()
;;                         (with-current-buffer (anything-candidate-buffer 'global)
;;                           (insert (python-ex:eval ,code)))))
;;               (candidates-in-buffer)
;;               (action . (("insert" . (lambda (c) (insert (format "import %s" c))))
;;                          ("help" . ,help)))
;;               (persistent-action . ,help))))
;;        (anything-other-buffer (list source) " *python import*"))))

 (provide 'python-ex)
 