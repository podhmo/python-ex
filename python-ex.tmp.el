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
    print i,i,i
" (lambda () (insert "foo")))

(python-ex:eval-internal-async
 "print 10"
 (lambda (r) (insert r)))
;;;
