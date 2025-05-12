(setq cmake-commands-filename "cmake-commands.el")
(setq keybindings-filename "keybindings.el")

(unless (boundp 'emacs-settings-dir)
    (setq true-filename (file-truename load-file-name))
    (setq emacs-settings-dir (file-name-directory true-filename)))


;; --------------------------------------- ;;
;;            PACKAGE INSTALLS             ;;
;; --------------------------------------- ;;

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; Workaround for "failed to download 'MELPA' archive"

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq my-pkgs '(
		git-gutter
		gnu-elpa-keyring-update
		magit
		yaml-mode
))

(dolist (pkg my-pkgs)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; --------------------------------------- ;;
;;                 OPTIONS                 ;;
;; --------------------------------------- ;;
(setq custom-theme-directory emacs-settings-dir)
(load-theme 'gruber-darker-patched t)

(tool-bar-mode 0)                                           ;; Disable the toolbar
(menu-bar-mode 0)                                           ;; Disable the menu bar
(scroll-bar-mode 0)                                         ;; Disable visible scrollbar
(column-number-mode 1)
(show-paren-mode 1)                                         ;; Highlight matching parens
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq help-window-select t)
(setq compilation-scroll-output 1)
(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq auto-save-no-message 1)                               ;; Disable autosave messages
(setq scroll-preserve-screen-position 1)                    ;; Preserve point position when scrolling
(setq scroll-margin 10)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq ibuffer-default-sorting-mode 'alphabetic)
(setq display-line-numbers-width 3)
(setq compilation-first-error 'first-error)

;; Ensure terminal scrolling does not break on output
(eval-after-load "term" '(progn (setq term-scroll-to-bottom-on-output t)))

;; This apparently *stops* the cursor from blinking?
;; Behavior might be specific to terminal mode and xterm options
(setq visible-cursor nil)

;; Keep default directory as initial value when starting Emacs
(setq initial-default-directory default-directory)
(add-hook 'find-file-hook (lambda () (setq default-directory initial-default-directory)))

;; Kill *Completions* buffer when done
(add-hook 'minibuffer-exit-hook
	  #'(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))


(kill-buffer "*scratch*") ;; Prevent *scratch* buffer from opening by default
(setq minibuffer-message-timeout nil) ;; Prevent minibuffer message from automatically disappearing


(setq text-quoting-style 'grave)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region of the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    ))

(defun display-startup-echo-area-message () (message "")) ;; Hide startup message from minibuffer

;; --------------------------------------- ;;
;;            FILE TYPE SETTINGS           ;;
;; --------------------------------------- ;;
(add-to-list 'auto-mode-alist '("\\.json\\'" . yaml-mode))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



;; --------------------------------------- ;;
;;             PACKAGE OPTIONS             ;;
;; --------------------------------------- ;;

;; CMake

(load-file (concat emacs-settings-dir cmake-commands-filename))

;; git-gutter
(global-git-gutter-mode +1)
(setq git-gutter:always-show-separator 1)
(setq git-gutter:update-interval 1)
(setq git-gutter:modified-sign "*")
(setq git-gutter:added-sign "+")
(setq git-gutter:deleted-sign "-")
(set-face-foreground 'git-gutter:added "Green")
(set-face-foreground 'git-gutter:modified "Purple")
(set-face-foreground 'git-gutter:deleted "Red")


;; --------------------------------------- ;;
;;               KEYBINDINGS               ;;
;; --------------------------------------- ;;
(load-file (concat emacs-settings-dir keybindings-filename))




;; --------------------------------------- ;;
;;                  OTHER                  ;;
;; --------------------------------------- ;;

;; Prevent *Async-native-compile-log* buffer from opening on startup due to bug in Emacs 29.
;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup

(defun fixed-native-compile-async-skip-p
    (native-compile-async-skip-p file load selector)
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file       (replace-regexp-in-string
                          "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (gethash elc-file comp--no-native-compile)
        (funcall native-compile-async-skip-p file load selector))))

(advice-add 'native-compile-async-skip-p
	    :around 'fixed-native-compile-async-skip-p)

