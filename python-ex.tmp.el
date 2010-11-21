(python-ex:eval-external "print 1") ; => 1
(python-ex:eval-internal "print 1") ; => 1
(python-ex:eval "print [1]") ; => [1]
(python-ex:run-repl t)
(python-ex:run-repl)
(python-ex:kill-repl)
(setq python-ex:auto-scroll-p t)
(python-ex:send-string
 "for i in range(1000):
    print i,i,i,i
")
