(make-variable-buffer-local 'comint-preoutput-filter-functions)

;; (while (re-search-forward "python-ex" nil t 1)
;;   (compose-region (match-beginning 0) (match-end 0) ?＠))

(python-ex:eval-external "print 1") ; => 1
(python-ex:eval-external-async "print 10")
(python-ex:eval-external-async "print 10" 'insert)
(python-ex:eval-internal "print 10") ; => 10
(python-ex:eval-internal "print '1010\\n10'")
;; (python-ex:eval-internal "print 123\nprint 321\nprint 231") ;;;
(python-ex:eval-internal-async "print 10")
(python-ex:cancel-all-async-timers)
(python-ex:eval-internal-async "print 10" 'insert)
(python-ex:eval "print [1]") ; => [1]1010
(python-ex:run-repl t)
(python-ex:run-repl)
(python-ex:kill-repl)

(setq python-ex:auto-scroll-p t)

(python-ex:eval-internal-async
 "for i in range(10):10
    print i,i,i" (lambda (r) (insert r)))10


(python-ex:eval-internal-async
 "import time
print 123
time.sleep(2)
print 321")
(python-ex:cancel-all-async-timers)

(setq python-ex:debug-info-p t)
(setq xxxx t)
(python-ex:wait-for 
 0.5 (lambda () (null xxxx))
 (lambda () (message "hey!")))
(python-ex:cancel-all-async-timers)
(setq xxxx nil)
(python-ex:run-repl)
(python-ex:load-file "a.py")
(python-ex:all-modules-cache-buffer t t t)

;;(string-match "\\[0-9\\]" "9")
;;(string-match python-ex:prompt-rx (car xxxx))

(cancel-timer timer)

           
    

;;;;
         
