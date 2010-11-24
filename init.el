(add-to-list 'load-path default-directory)
(add-to-list 'load-path (concat default-directory "el-util-macro"))
(require 'python-ex)


(defun python-ex:setup ()
  (let ((key-map '(("\C-cu" . python-ex:select-modules-with-anything)
                  ("\C-cS" . python-ex:run-repl)
                  ("\C-c\C-k" . python-ex:kill-repl)
                  ("\C-c\C-l" . python-ex:load-file)
                  ("\C-c\C-r" . python-ex:send-region)
                  ("\C-c\C-b" . python-ex:send-buffer))))
    (loop for (k . fun) in key-map
          do (define-key python-mode-map k fun))))

(add-hook 'python-mode-hook 'python-ex:setup)
          
