;;; custom-init.el --- Defines my custom Emacs environment.

;;; Commentary:
;; This is here to silence flycheck.

;;; Code:
(defvar custom-file-name "custom-file.el")
(defvar cmake-commands-filename "cmake-commands.el")
(defvar keybindings-filename "keybindings.el")
(defvar local-settings-filename "local.el")

;; Get the path to the emacs settings directory, resolving symlinks.
(defvar true-filename (file-truename load-file-name))
(defvar emacs-settings-dir (file-name-directory true-filename))

;; --------------------------------------- ;;
;;             PACKAGE REFRESH             ;;
;; --------------------------------------- ;;

(defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; Workaround for "failed to download 'MELPA' archive"

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; --------------------------------------- ;;
;;                 OPTIONS                 ;;
;; --------------------------------------- ;;
(setq custom-theme-directory emacs-settings-dir)
(load-theme 'gruber-darker-patched t)

(tool-bar-mode 0)                                           ;; Disable the toolbar
(menu-bar-mode 0)                                           ;; Disable the menu bar
(scroll-bar-mode 0)                                         ;; Disable visible scrollbar
(column-number-mode 1)                                      ;; Add column number to minibuffer
(show-paren-mode 1)                                         ;; Highlight matching parens
(blink-cursor-mode 0)
(global-hl-line-mode 1)                                     ;; Highlight line under point
(delete-selection-mode 1)                                   ;; Typed text replaces any selection
(setq inhibit-startup-message t)                            ;; Hide message when starting Emacs
(setq inhibit-startup-buffer-menu 1)                        ;; Dont display buffer list when opening multiple files
(setq visible-bell t)                                       ;; Prevent annoying beeping
(setq help-window-select t)                                 ;; Select help window when opened
(defvar compilation-scroll-output)
(setq compilation-scroll-output 1)                          ;; Auto scroll compilation window
(setq backup-directory-alist `(("." . "~/.emacs-backups"))) ;; Directory to store autosaves
(setq auto-save-no-message 1)                               ;; Disable autosave messages
(setq scroll-preserve-screen-position 1)                    ;; Preserve point position when scrolling
(setq scroll-margin 10)                                     ;; Scroll window when cursor reaches margin
(setq scroll-conservatively 10000)
(setq scroll-step 1)                                        ;; Scroll this many lines at a time
(setq display-line-numbers-width 3)                         ;; Line number gutter width
(setq-default indent-tabs-mode nil)
(defvar compilation-first-error)
(setq compilation-first-error 'first-error)                 ;; Stop scrolling compilation on first error
(put 'dired-find-alternate-file 'disabled nil)

;; Ensure terminal scrolling does not break on output
(defvar term-scroll-to-bottom-on-output)
(eval-after-load "term" '(progn (setq term-scroll-to-bottom-on-output t)))

;; This apparently *stops* the cursor from blinking?
;; Behavior might be specific to terminal mode and xterm options
(setq visible-cursor nil)

;; Keep default directory as initial value when starting Emacs
;;(setq initial-default-directory default-directory)
;;(add-hook 'find-file-hook (lambda () (setq default-directory initial-default-directory)))

;; Kill *Completions* buffer when done
(add-hook 'minibuffer-exit-hook
	  #'(lambda ()
	      (let ((buffer "*Completions*"))
		(and (get-buffer buffer)
		     (kill-buffer buffer)))))


(kill-buffer "*scratch*") ;; Prevent *scratch* buffer from opening by default
(setq minibuffer-message-timeout nil) ;; Prevent minibuffer message from automatically disappearing

(defun comment-or-uncomment-region-or-line ()
  "Toggle comment on the region of the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    ))

(defun display-startup-echo-area-message ()
  "Hide startup message from minibuffer."
  (message ""))

(setq custom-file (concat emacs-settings-dir custom-file-name))
(load-file (concat emacs-settings-dir custom-file-name))

;; --------------------------------------- ;;
;;                 IBUFFER                 ;;
;; --------------------------------------- ;;
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Code" (or (mode . c-mode)
		     (mode . c++-mode)
		     (mode . emacs-lisp-mode)
		     (mode . python-mode)
		     (mode . sh-mode)))
	 ("CMake" (or (basename . "CMakeLists.txt")
		      (basename . "Makefile")))
	 ("Data" (or (mode . yaml-mode)
		     (file-extension . "json")
		     (file-extension . "csv")))
	 ("Shells" (mode . shell-mode))
	 ("Dired" (or  (derived-mode . dired-mode)
		       (derived-mode . image-mode)))
	 ("Stars" (or  (starred-name)
		       (process)))
	 )))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "home")
	    (setq ibuffer-hidden-filter-groups '("Stars"))
	    ))
(setq ibuffer-show-empty-filter-groups nil)

;; Adjust IBuffer formatting
(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 30 30 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " " filename-and-process)
	(mark " "
	      (name 16 -1)
	      " " filename)))

(defvar ibuffer-default-sorting-mode)
(setq ibuffer-default-sorting-mode 'alphabetic)     ;; Sort Ibuffer by this ordering

;; --------------------------------------- ;;
;;            FILE TYPE SETTINGS           ;;
;; --------------------------------------- ;;
(add-to-list 'auto-mode-alist '("\\.json\\'" . yaml-mode))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



;; --------------------------------------- ;;
;;                 PACKAGES                ;;
;; --------------------------------------- ;;

;; CMake
(load-file (concat emacs-settings-dir cmake-commands-filename))

(use-package apheleia
  :ensure t
  :hook ((c-mode c++-mode) . apheleia-mode)
  )

(use-package cmake-mode
  :ensure t
  )

(use-package company
  :ensure t
  :config (global-company-mode)
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode +1)
  ;; Customize error list display:
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.20)))
  )

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:always-show-separator 1)
  (setq git-gutter:update-interval 1)
  (setq git-gutter:modified-sign "*")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (set-face-foreground 'git-gutter:added "Green")
  (set-face-foreground 'git-gutter:modified "Purple")
  (set-face-foreground 'git-gutter:deleted "Red")
  )

(use-package gnu-elpa-keyring-update
  :ensure t
  )

(use-package lsp-mode
  :ensure t
  :hook ((c-mode c++-mode) . lsp)
  )

(use-package magit
  :ensure t
  )

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 3.0)       ;; Delay in seconds before which-key buffer appears.
  (setq which-key-add-column-padding 2) ;; Padding between column of keys.
  :config
  (which-key-mode)
  )

(use-package yaml-mode
  :ensure t
  )

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


(defvar local-settings-file (concat emacs-settings-dir local-settings-filename))
(if (file-exists-p local-settings-file)
    (load-file local-settings-file))
