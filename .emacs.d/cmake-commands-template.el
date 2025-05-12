(require 'cmake-commands "cmake-commands.el")
(cmake-clear-defines)
(cmake-clear-export-vars)

;; Set CMake -D defines
(cmake-set-define "TEST_DEFINE"
		  "option1"
;;		  "option2"
		  )

(cmake-set-generator "Ninja")
(cmake-set-source-dir ".")
(cmake-set-build-dir (concat (cmake-get-source-dir) "build"))

(cmake-set-ctest-parallel-level 12)
(cmake-set-export-var "PYTHONPYCACHEPREFIX" "\"$HOME/.cache/cpython\"")
(cmake-ctest-set-arg "--output-on-failure")
