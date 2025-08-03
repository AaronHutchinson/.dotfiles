(provide 'cmake-commands)

(defvar cmake-export-vars-list '())
(defvar cmake-defines-list '())
(defvar cmake-ctest-args-list '())
(defvar cmake-generator "")
(defvar cmake-source-dir "")
(defvar cmake-build-dir "")
(defvar cmake-config-file "")
(defvar cmake-ctest-parallel-level 1)

(defun cmake-clear-export-vars () (setq cmake-export-vars-list '()))
(defun cmake-get-export-var (key) (cdr (assoc key cmake-export-vars-list)))
(defun cmake-set-export-var (name &optional val)
  (or val (setq val ""))
  (when (> (length val) 0)
    (push (cons name val) cmake-export-vars-list)
  )
)

(defun cmake-clear-defines () (setq cmake-defines-list '()))
(defun cmake-get-define (key) (cdr (assoc key cmake-defines-list)))
(defun cmake-set-define (name &optional val)
  (or val (setq val ""))
  (when (> (length val) 0)
    (push (cons name val) cmake-defines-list)
  )
)

(defun cmake-ctest-clear-args() (setq cmake-ctest-args-list '()))
(defun cmake-ctest-set-arg (&optional val)
  (or val (setq val ""))
  (when (> (length val) 0)
    (push val cmake-ctest-args-list)
  )
)

(defun cmake-set-generator (name) (setq cmake-generator name))
(defun cmake-get-generator () (or cmake-generator))
(defun cmake-set-source-dir (path) (setq cmake-source-dir path))
(defun cmake-get-source-dir () (or cmake-source-dir))
(defun cmake-set-build-dir (path) (setq cmake-build-dir path))
(defun cmake-get-build-dir () (or cmake-build-dir))
(defun cmake-set-ctest-parallel-level (level) (setq cmake-ctest-parallel-level level))
(defun cmake-get-ctest-parallel-level () (or cmake-ctest-parallel-level))

(defun cmake-check-vars ()
  (when (= (length cmake-generator) 0) (error "Error: no CMake generator specified."))
  (when (= (length cmake-source-dir) 0) (error "Error: no CMake source directory set."))
  (when (= (length cmake-build-dir) 0) (error "Error: no CMake build directory set."))
)

(defun cmake-format-env-vars ()
  (setq result "")
  (dolist (elt cmake-export-vars-list result)
    (setq key (car elt))
    (setq val (cdr elt))
    (setq result (concat key "=" val " " result))
  )
)

(defun cmake-format-defines ()
  (setq result "")
  (dolist (elt cmake-defines-list result)
    (setq key (car elt))
    (setq val (cdr elt))
    (setq result (concat " -D" key "=\"" val "\"" result))
  )
)

(defun cmake-format-ctest-args ()
  (setq result "")
  (dolist (elt cmake-ctest-args-list result)
    (setq result (concat " " elt))
  )
)

(defun cmake-set-configuration-file ()
  ;; save environment
  (setq start-insert-default-directory insert-default-directory)

  ;; read-file-name args
  (setq insert-default-directory nil)
  (setq dir (if (> (length cmake-config-file) 0) (file-name-directory cmake-config-file) ""))
  (setq default-filename (if (> (length cmake-config-file) 0) (file-name-nondirectory cmake-config-file) ""))
  (setq default-filename-str (if (> (length default-filename) 0) (concat " (default " default-filename ")") ""))
  (setq prompt (concat "Choose emacs-cmake config file" default-filename-str ": "))

  (setq new-config-file (read-file-name prompt dir default-filename))
  (setq new-config-file (if (> (length new-config-file) 0)
			    (expand-file-name new-config-file)
			  cmake-config-file))
  (if (= (length new-config-file) 0) (error "Invalid configuration file!"))

  (load-file new-config-file)
  (setq cmake-config-file new-config-file)
  (message "CMake configuration file set to: %s" cmake-config-file)

  ;; restore environment
  (setq insert-default-directory start-insert-default-directory)

  (cmake-check-vars)

  (or cmake-config-file)
)

(defun cmake-run-edit-configuration ()
  (interactive)
  (setq config-file (cmake-set-configuration-file))
  (find-file config-file)
)

(defun cmake-run-set-configuration ()
  (interactive)
  (setq config-file (cmake-set-configuration-file))
  (load-file config-file)
)

(defun cmake-load-and-check-config ()
  (if (and (> (length cmake-config-file) 0) (file-exists-p cmake-config-file))
      (load-file cmake-config-file)
    (cmake-run-set-configuration))
)

(defun cmake-configure-command ()
  (cmake-load-and-check-config)
  (concat
    "cd \"" (cmake-get-source-dir) "\""
    " && "
    (cmake-format-env-vars)
    "cmake"
    " -G " (cmake-get-generator)
    " -B \"" (cmake-get-build-dir) "\""
    " --fresh"
    (cmake-format-defines)
  )
)

(defun cmake-build-command ()
  (cmake-load-and-check-config)
  (concat
    "cd \"" (cmake-get-build-dir) "\""
    " && "
    "cmake --build ."
  )
)

(defun cmake-test-command ()
  (cmake-load-and-check-config)
  (concat
    "cd \"" (cmake-get-build-dir) "\""
    " && "
    "ctest"
    (cmake-format-ctest-args)
    " -j " (number-to-string (cmake-get-ctest-parallel-level))
  )
)

(defun cmake-run-configure ()
  (interactive)
  (compile (cmake-configure-command))
)

(defun cmake-run-build ()
  (interactive)
  (compile (cmake-build-command))
)

(defun cmake-run-test ()
  (interactive)
  (compile (cmake-test-command))
)

