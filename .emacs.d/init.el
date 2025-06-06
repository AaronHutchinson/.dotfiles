
;; --------------------------------------- ;;
;;                MY CODE                  ;;
;; --------------------------------------- ;;

;; Get the directory this file is contained in (resolving symlinks) and load the custom init file.
(setq true-filename (file-truename load-file-name))
(setq emacs-settings-dir (file-name-directory true-filename))

(setq custom-init-filename "custom-init.el")
(load-file (concat emacs-settings-dir custom-init-filename))
