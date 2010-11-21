(provide 'python-ex)
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
(defvar python-ex:eval-type 'external) ;; 'external or 'internal
(defvar python-ex:python-command python-command)
(defvar python-ex:temoprary-directory "/tmp/")
(defvar python-ex:buffer-name "*Python-ex*")
(defvar python-ex:buffer nil)
(defvar python-ex:repl-command "ipython")
(defvar python-ex:auto-scroll-p t)

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
(defun python-ex:eval (code) (interactive "s") ;;output-filterを用意しても良かったかも？
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

(defmacro python-ex:with-preoutput-filter (filter &rest actions)
  (python-ex:with-gensyms (orig preout-func)
    `(let* ((,orig comint-preoutput-filter-functions)
            (,preout-func ,filter)
            (comint-preoutput-filter-functions (list ,preout-func)))
       (unwind-protect
           (progn ,@actions)
         (setq comint-preoutput-filter-functions ,orig)))))

(defun python-ex:eval-internal (code)
  (lexical-let ((running t) buf (prompt ">>> "))
    (comint-simple-send (python-ex:proc) code)
    (python-ex:with-preoutput-filter
     (lambda (str) (cond ((string-match prompt str)
                          (let* ((rx (format "\n?%s" prompt))
                                 (str (replace-regexp-in-string rx "" str)))
                            (push str  buf))
                          (setq buf (nreverse buf))
                          (setq running nil) "")
                         (t (push str buf) "")))
     (while running ;;polling
       (sleep-for 0 100))
     (mapconcat 'identity  buf ""))))

;;; repl-action
(defun python-ex:action-repl-buffer (action &rest args)
  (python-ex:and-let* ((w (find python-ex:buffer (window-list) :key 'window-buffer)))
    (with-selected-window w
      (apply action args))))

(defun python-ex:action-repl-buffer-async (fun &rest args)
  (lexical-let ((fun fun) (args args))
    (apply 'run-with-idle-timer 0.01 nil 
           'python-ex:action-repl-buffer
           fun args)))

(defun python-ex:auto-scroll-preoutput-filter (str)
  (when (string-match ">>> " str)
    (python-ex:action-repl-buffer-async (lambda () (goto-char (point-max)))))
  str)

;;; send
(defun python-ex:send-string (code)
  (cond (python-ex:auto-scroll-p
         (python-ex:with-preoutput-filter 
          'python-ex:auto-scroll-preoutput-filter
          (comint-simple-send (python-ex:proc) code)))
        (t (comint-simple-send (python-ex:proc) code))))

(defun python-ex:send-region (beg end) (interactive "r")
  (cond (python-ex:auto-scroll-p
         (python-ex:with-preoutput-filter
          'python-ex:auto-scroll-preoutput-filter
          (comint-send-region (python-ex:proc) beg end)))
        (t (comint-send-region (python-ex:proc) beg end))))

(defun python-ex:send-buffer () (interactive)
  (pyton-ex:send-region (point-min) (point-max)))

(defun python-ex:send-defun () ;;buggy
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion
    (python-ex:send-region
     (progn (beginning-of-defun) (point))
     (progn (end-of-defun) (point)))))

(provide 'python-ex)
  