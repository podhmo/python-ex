;; (while (re-search-forward "python-ex" nil t 1)
;;   (compose-region (match-beginning 0) (match-end 0) ?＠))

(python-ex:eval-external "print 1") ; => 1
(python-ex:eval-external-async "print 10")
(python-ex:eval-external-async "print 10" 'insert)
(python-ex:eval-internal "print 10") ; => 10
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

(python-ex:eval-external-async
 "import time
time.sleep(3)
print 10"
 (lambda (r) (insert r)))
;;;
(python-ex:all-modules-cache-buffer t t t)
(setq python-ex:debug-info-p t)
(setq xxxx t)
(python-ex:wait-for 
 0.5 (lambda () (null xxxx))
 (lambda () (mesage "hey!")))
(setq xxxx nil)
                       
;; (while python-ex:eval-reading-p ;;polling
;;   (python-ex:debug-info "inner-sleep")
;;   (sleep-for 0 100))
;;(pythno-ex:wait-for 100 python-ex:eval-reading-p)

(defun python-ex:wait-for (secs pred)
  (python-ex:let1 finishied-p (if (functionp pred) (fucall pred) pred)
    (unless finishied-p
      (run-with-timer
       secs nil
       (python-ex:cut 'python-ex:wait-for secs pred)))))
