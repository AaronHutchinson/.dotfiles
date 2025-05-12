

;; Use *Ibuffer* instead of *Buffer List*
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Custom comment toggling
(global-set-key [remap comment-dwim] 'comment-or-uncomment-region-or-line)

;; Custom cmake functions (requires 'cmake-commands.el')
(global-set-key (kbd "C-c e") 'cmake-run-edit-configuration)
(global-set-key (kbd "C-c r") 'cmake-run-set-configuration)
(global-set-key (kbd "C-c c") 'cmake-run-configure)
(global-set-key (kbd "C-c b") 'cmake-run-build)
(global-set-key (kbd "C-c t") 'cmake-run-test)


