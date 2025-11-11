

;; Use *Ibuffer* instead of *Buffer List*
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Custom cmake functions (requires 'cmake-commands.el')
(global-set-key (kbd "C-c e") 'cmake-run-edit-configuration)
(global-set-key (kbd "C-c r") 'cmake-run-set-configuration)
(global-set-key (kbd "C-c c") 'cmake-run-configure)
(global-set-key (kbd "C-c b") 'cmake-run-build)
(global-set-key (kbd "C-c t") 'cmake-run-test)
(global-set-key (kbd "C-c x") 'cmake-run-executable)
(global-set-key (kbd "C-c a") 'cmake-run-all)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on the current line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(defun smart-beginning-of-line-with-highlight ()
  (interactive)
  (unless mark-active (set-mark (point)))
  (smart-beginning-of-line))

(keymap-global-set "<home>" 'smart-beginning-of-line)
(keymap-global-set "S-<home>" 'smart-beginning-of-line-with-highlight)
