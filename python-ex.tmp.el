(make-variable-buffer-local 'comint-preoutput-filter-functions)

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

(python-ex:eval-internal
 "import time
time.sleep(1)
print 300")

;; (progn 
;;   (let1 pt 
;;       (with-current-buffer (python-ex:buffer)
;;         (marker-position comint-last-output-start))
;;     (comint-simple-send (python-ex:proc) "1")
;;     (sleep-for 0 100)
;;     (let1 pt2 
;;         (with-current-buffer (python-ex:buffer)
;;           (marker-position comint-last-output-start))
;;       (values pt pt2))))

(setq python-ex:debug-info-p t)
(setq xxxx t)
(python-ex:wait-for 
 0.5 (lambda () (null xxxx))
 (lambda () (mesage "hey!")))
(setq xxxx nil)
                       
(python-ex:all-modules-cache-buffer)