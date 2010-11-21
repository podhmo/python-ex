(python-ex:eval-external "print 1") ; => 1
(python-ex:eval-internal "print 1") ; => 1
(python-ex:eval "print 1") ; => 1
(python-ex:run-repl t)
(python-ex:run-repl)
(python-ex:kill-repl)