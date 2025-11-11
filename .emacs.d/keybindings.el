

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


(defun move-text-internal (arg)
  "Move region (transient-mark-mode active) or current line.
Moves ARG lines down when ARG > 0, or up otherwise."
  (cond
   ((and mark-active transient-mark-mode)
    (let ((point-was-first (> (point) (mark))))
      (if point-was-first (exchange-point-and-mark))
      (let ((column (current-column))
            (text (delete-and-extract-region (point) (mark))))
        (forward-line arg)
        (move-to-column column t)
        (push-mark (point))
        (insert text)
        )
      (unless point-was-first (exchange-point-and-mark))
      (setq deactivate-mark nil)
      )
    )
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
