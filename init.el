(add-to-list 'load-path default-directory)
(add-to-list 'load-path (concat default-directory "el-util-macro"))
(require 'python-ex)

(defun python-ex:setup ()
  (let ((key-map `(("\C-cu" . python-ex:select-modules-with-anything)
                  ("\C-cS" . python-ex:run-repl)
                  ("\C-c\C-k" . python-ex:kill-repl)
                  ("\C-c\C-l" . python-ex:load-file)
                  ("\C-c\C-j" . python-ex:ipython-complete-with-anything)
                  ("\C-c\C-c" . ,(if (fboundp 'sp-toggle-file) 'sp-toggle-file))
                  ("\C-c\C-r" . python-ex:send-region)
                  ("\C-cr" . python-ex:input-histories-with-anything)
                  ("\C-c\C-b" . python-ex:send-buffer))))

    (python-ex:install)
    (loop for (k . fun) in key-map
          do (define-key python-mode-map k fun))))

(add-hook 'python-mode-hook 'python-ex:setup)

(defun python-ex:inferior-setup ()
  (let ((key-map `((,(kbd "<tab>") . python-ex:ipython-complete-with-anything)
                   ("\C-c\C-j" . python-ex:ipython-complete-with-anything)
                   ("\C-cr" . python-ex:input-histories-with-anything)
                   ("\C-cu" . python-ex:select-modules-with-anything))))
    (loop for (k . fun) in key-map
          do (define-key inferior-python-mode-map k fun))))

(add-hook 'inferior-python-mode-hook 'python-ex:inferior-setup)

;; personal setting
(setq python-ex:base-dir (expand-file-name "~/myproject/python-ex/"))


;;;

;; (python-ex:modules-cache-add-path 
;;  (find-file-noselect python-ex:anything-daily-use-modules-file))

(defun python-ex:eval-external-output/buffer (code name &optional force-reload call-back)
  (python-ex:rlet1 buf (get-buffer name)
    (when (or force-reload (null buf))
      (setq buf (get-buffer-create name))
      (python-ex:with-lexical-bindings (name call-back)
        (python-ex:eval-external-async 
         code
       (lambda (r) (with-current-buffer name
                     (erase-buffer)
                     (insert r)
                     (when call-back (funcall call-back)))))))))

;; (defun python-ex:help-with-anything (&optional force-reload)
;;   (flet ((with-pydoc-help
;;             (request) (format "
;; import pydoc
;; import sys
;; for path in sys.path:
;;     if \"tmp\" in path:
;;         sys.path.remove(path)
;; pydoc.help(%S)" request))
;;            ;; (reformat-buffer
;;            ;;  () (save-excursion
;;            ;;       (goto-char (point-min))
;;            ;;       (while (re-search-forward " \\{2,\\}" nil t 1)
;;            ;;         (replace-match "\n"))))
;;            (buffer-to-anything-source
;;             (buf) `((name . ,(buffer-name buf))
;;                     (init . (lambda () (anything-candidate-buffer ,buf)))
;;                     (candidates-in-buffer)
;;                     (action . message))))
;;     (let* ((requests '("help" "keywords" "symbols" "topics" "modules"))
;;            (bufs (loop for request in requests
;;                        for bufname in (mapcar (python-ex:cut 'format "*pyex:%s*" <>) requests)
;;                        collect (python-ex:eval-external-output/buffer
;;                                 (with-pydoc-help request)
;;                                 bufname force-reload)));; 'reformat-buffer)))
;;            (sources (loop for buf in bufs
;;                           collect (buffer-to-anything-source buf))))
;;       (anything sources))))
