(require 'cl)
(require 'anything nil t)
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
(defvar python-online-document-url "http://docs.python.org")
(defvar python-ex:eval-type 'external) ;; 'external or 'internal
(defvar python-ex:python-command python-command)
(defvar python-ex:temoprary-directory "/tmp/")
(defvar python-ex:buffer-name "*Python-ex*")
(defvar python-ex:buffer nil)
(defvar python-ex:prompt-rx "\n+\\[0;32mIn \\[\\[1;32m[0-9]+\\[0;32m\\]: \\[0m") ;; ">>> " classic
(defun python-ex:buffer ()
  (or python-ex:buffer
      (progn (python-ex:proc)
             python-ex:buffer)))

(defvar python-ex:repl-command "ipython")
(defvar python-ex:auto-scroll-p nil)

;;; debug
(defvar python-ex:debug-info-p nil)
(defmacro python-ex:debug-info (fmt &rest args)
  (if python-ex:debug-info-p 
      (python-ex:with-gensyms (str)
        `(let* ((,str (format ,fmt ,@args))
                (,str (mapconcat 'identity (split-string ,str "\n") "\n  ")))
           (message (concat "[info]\n  " ,str))))))

(defun python-ex:debug-info-enable (k) (interactive "p")
  (python-ex:let1 val (case k
                        ((1) t)
                        ((-1) nil)
                        (otherwise (not python-ex:debug-info-p)))
    (message "python-ex:debug-info-p is `%s`" val)
    (setq python-ex:debug-info-p val)))

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
         `(,cmd ,@rest))
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

(defmacro python-ex:with-preoutput-filters (filters &rest actions)
  (declare (indent 1))
  (python-ex:with-gensyms (orig)
    `(let* ((,orig comint-preoutput-filter-functions)
            (comint-preoutput-filter-functions ,filters))
       (unwind-protect
           (progn ,@actions)
         (setq comint-preoutput-filter-functions ,orig)))))

(defun python-ex:eval-internal (source-code)
  (lexical-let* ((buf (list))
                 (done-p nil)
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
     :send-action (lambda () (comint-simple-send (python-ex:proc) source-code))
     :filters (list output-interception))
    (while (null done-p)
      (python-ex:debug-info "python-ex:eval-internal --  outer-sleep")
      (sleep-for 0 10))
    (mapconcat 'identity buf "")))

(defvar python-ex:eval-reading-p nil) ;;internal-variable for polling
(defun* python-ex:eval-internal-1 (&key send-action filters call-back)
  (let* ((end-check-filter
          (lambda (str)
            (python-ex:debug-info "===hello %S" str)
            (when (string-match-p python-ex:prompt-rx str)
              (setq python-ex:eval-reading-p nil))
            str))
         (filters* (cons end-check-filter filters)))
    (python-ex:with-preoutput-filters filters*
      (setq python-ex:eval-reading-p t)
      (funcall send-action)
      (while python-ex:eval-reading-p ;;polling
        (python-ex:debug-info "python-ex:eval-internal-1 -- inner-sleep")
        (sleep-for 0 100))
      (when call-back (funcall call-back)))))

;;; eval ansync

;; async utility
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

;; async main
(defun python-ex:eval-async (code &optional call-back) (interactive "s\na")
  (case python-ex:eval-type
    ((internal) (python-ex:eval-internal-async code call-back))
    ((external) (python-ex:eval-external-async code call-back))
    (otherwise (python-ex:let1 fmt "python-ex:eval -- invalid eval-type %s"
                 (error (format fmt python-ex:eval-type))))))

(defun python-ex:eval-external-async (source-code &optional call-back)
  (let ((tmpbuf (python-ex:gensym-name))
        (file (python-ex:gensym-name)))
    (with-temp-file file 
      (insert source-code))
    (python-ex:with-lexical-bindings (call-back tmpbuf)
      (set-process-sentinel
       (start-process-shell-command 
        "python-ex:external" tmpbuf python-ex:python-command file)
       (lambda (status &rest args)
         (python-ex:let1 r (with-current-buffer
                               tmpbuf (buffer-string))
           (kill-buffer tmpbuf)
           (cond (call-back (funcall call-back r))
                 (t (message "pyex-result: %s" r)))))))))

(defun python-ex:eval-internal-async (source-code &optional call-back)
  (python-ex:with-lexical-bindings (source-code)
    (python-ex:eval-internal-async-1
     :send-action (lambda () (comint-simple-send (python-ex:proc) source-code))
     :call-back call-back)))

(defun* python-ex:eval-internal-async-1 (&key send-action call-back)
  (lexical-let* ((prev-pt (with-current-buffer (python-ex:buffer)
                            (marker-position comint-last-output-start))))
    (funcall send-action)
    (python-ex:with-lexical-bindings (call-back)
      (lexical-let ((last-call-back
                     (lambda ()
                       (python-ex:let1 r
                           (with-current-buffer (python-ex:buffer)
                             (save-excursion
                               (goto-char (marker-position comint-last-output-start))
                               (buffer-substring-no-properties
                                (point) 
                                (next-single-char-property-change (point) 'field))))
                         (if call-back (funcall call-back r) (message "pyex-result: %s" r))))))
        (python-ex:wait-for
         0.1 (lambda ()
               (with-current-buffer (python-ex:buffer)
                 (python-ex:debug-info
                  "python-ex:eval-internal-async --- ps(%d,%d)"
                  prev-pt (marker-position comint-last-output-start))
                 (python-ex:debug-info
                  "python-ex:eval-internal-async --- s:%s"
                  (buffer-substring-no-properties
                   (marker-position comint-last-output-start)
                   (point-max)))
                 (python-ex:let1 pt (marker-position comint-last-output-start)
                   (and (< prev-pt pt)
                        (not (string-equal "" (buffer-substring-no-properties pt (point-max))))))))
         last-call-back)))))

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
  (cond (python-ex:auto-scroll-p 
         (python-ex:eval-internal-async 
          code 
          (or call-back 'python-ex:auto-scroll-callback)))
        (t (comint-simple-send (python-ex:proc) code))))

(defun python-ex:send-region (beg end &optional call-back) (interactive "r\nP")
  (cond (python-ex:auto-scroll-p
         (python-ex:eval-internal-async-1
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
  (let* ((dir (or (python-ex:aand (buffer-file-name) (file-name-directory it))
                  default-directory))
         (prepare-string (format "
import sys
sys.path.append(%S)
import ex"
                                 dir)))
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


(defun* python-ex:message-with-other-buffer (proc &optional (name "*Py Help*") (reuse-buffer-p nil))
  (python-ex:let1 buf
      (cond (reuse-buffer-p (get-buffer-create name))
            (t (generate-new-buffer name)))
    (with-current-buffer buf
      (python-ex:let1 inhibit-read-only nil
        (when reuse-buffer-p
          (erase-buffer))
        (funcall proc))
      (goto-char (point-min)))
    (display-buffer buf)))

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


;;; anything interface
(python-ex:with-anything

 (defun python-ex-anything:help (c)
   (python-ex:message-with-other-buffer
    (lambda ()
      (insert
       (python-ex:eval-internal
        (format "
import %s
help('%s')" 
                c c))))))
 
 (defun python-ex-anything:web-help (c)
   (browse-url
    (format "%s/library/%s.html" python-online-document-url c)))

 (defvar python-ex:anything-c-source-all-modules
   '((name . "import")
     (init . (lambda ()
               (anything-candidate-buffer 
                (python-ex:all-modules-cache-buffer nil t))))
     (candidates-in-buffer)
     (action . (("insert" . (lambda (c) (insert (format "import %s" c))))
                ("help" . python-ex-anything:help)
                ("web-help" . python-ex-anything:web-help)))
     (persistent-action . ,python-ex-anything:help)))

 (defvar python-ex:anything-daily-use-modules-file 
   (concat (file-name-directory (buffer-file-name)) "daily-modules.py"))

 (defvar python-ex:anything-c-source-daily-use-modules
   '((name . "daily modules")
     (candidates-file . python-ex:anything-daily-use-modules-file)
     (action . (("insert" . (lambda (c) (insert (format "import %s" c))))
                ("help" . python-ex-anything:help)
                ("web-help" . python-ex-anything:web-help)))
     (persistent-action . ,python-ex-anything:help)))

 (defun python-ex:select-modules-with-anything () (interactive)
   (python-ex:let1 sources
       '(python-ex:anything-c-source-daily-use-modules
         python-ex:anything-c-source-all-modules)
     (anything-other-buffer sources " *python import*")))

 (defvar python-ex:anything-c-source-input-histories
   '((name . "input history")
     (init 
      . (lambda ()
          (python-ex:let1 histories-array
              (with-current-buffer (python-ex:buffer)
                (delete-duplicates (cddr comint-input-ring)
                                   :test 'string-equal))
            (with-current-buffer (anything-candidate-buffer 'global)
              (erase-buffer)
              (dotimes (i (length histories-array))
                (python-ex:aand
                 (aref histories-array i)
                 (insert it "\n")))))))
     (candidates-in-buffer)
     (search-from-end)
     (action . python-ex:send-string)))

 (defun python-ex:input-histories-with-anything () (interactive)
     (anything '(python-ex:anything-c-source-input-histories)))
)

(provide 'python-ex)
