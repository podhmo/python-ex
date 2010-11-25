(make-variable-buffer-local 'comint-preoutput-filter-functions)

;; (while (re-search-forward "python-ex" nil t 1)
;;   (compose-region (match-beginning 0) (match-end 0) ?＠))

(python-ex:eval-external "print 1") ; => 1
(python-ex:eval-external-async "print 10")
(python-ex:eval-external-async "print 10" 'insert)
(python-ex:eval-internal "print 10") ; => 10
(python-ex:eval-internal "print '10\n10'") ; => 10
;; 10
(python-ex:eval-internal-async "print 10")
(python-ex:eval-internal-async "print 10" 'insert)
(python-ex:eval "print [1]") ; => [1]
(python-ex:run-repl t)
(python-ex:run-repl)
(python-ex:kill-repl)

(setq python-ex:auto-scroll-p t)
(python-ex:send-string
 "for i in range(10):
    print i,i,i" (lambda () (insert "foo")))

(let ((python-ex:auto-scroll-p t))
  (python-ex:send-string
   "import time
print 123
time.sleep(2)
print 321"))


(setq python-ex:debug-info-p t)
(setq xxxx t)
(python-ex:wait-for 
 0.5 (lambda () (null xxxx))
 (lambda () (mesage "hey!")))
(setq xxxx nil)
(python-ex:run-repl)
(python-ex:load-file "a.py")
(python-ex:all-modules-cache-buffer t t t)

;;(string-match "\\[0-9\\]" "9")
;;(string-match python-ex:prompt-rx (car xxxx))

(cancel-timer timer)
