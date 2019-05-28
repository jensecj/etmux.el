;;; etmux.el --- Interact with tmux from emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: etmux
;; Package-Version: 20190525
;; Version: 0.2.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (s "1.12.0"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides an interface to interact with tmux from elisp.

;;; Code:

(require 'dash)
(require 's)
(require 'projectile)

(defvar etmux-jackin-key "C-x C-c"
  "Default keybinding for `etmux-jackin's project-wide command.")

(defvar etmux-terminal "termite"
  "Default terminal to use for external commands.")

(defun etmux-tmux-running? ()
  "Return whether `tmux' is running on the system."
  (zerop (process-file "tmux" nil nil nil "has-session")))

(defun etmux-tmux-run-command (&rest args)
  "Run a tmux-command in the running tmux session."
  (with-temp-buffer
    (let ((retval (apply 'process-file "tmux" nil (current-buffer) nil args)))
      (if (zerop retval)
          (buffer-string)
        (error (format "Failed: %s(status = %d)" (mapconcat 'identity (cons "tmux" args) " ") retval))))))

(defun etmux--send-keys (target keys)
  "Send a key combination to TARGET."
  (etmux-tmux-run-command "send-keys" "-t" target keys "C-m"))

(defun etmux-reset-prompt (target)
  "Clears the prompt of the TARGET."
  (etmux-tmux-run-command "send-keys" "-t" target "C-u"))

(defun etmux-clear (target)
  "Clears the screen of the TARGET."
  (etmux-tmux-run-command "send-keys" "-t" target "C-l"))

(defun etmux-C-c (target)
  "Send interrupt signal to TARGET."
  (etmux-tmux-run-command "send-keys" "-t" target "C-c"))

(defun etmux-C-d (target)
  "Send EOF signal to TARGET."
  (etmux-tmux-run-command "send-keys" "-t" target "C-d"))

(defun etmux-C-z (target)
  "Send TSTP signal to TARGET."
  (etmux-tmux-run-command "send-keys" "-t" target "C-z"))

(defun etmux-run-command (target command)
  "Send a command to the TARGET."
  (interactive)
  (when (etmux-tmux-running?)
    (etmux-reset-prompt target)
    (etmux--send-keys target command)))

(defun etmux-list-sessions ()
  "List all running tmux sessions on the system."
  (if (etmux-tmux-running?)
      (let ((result (etmux-tmux-run-command "list-sessions" "-F" "#{session_name}")))
        (s-split "\n" (s-trim result)))
    (message "found no running tmux sessions")))

(defun etmux-list-windows (session)
  "List all windows in SESSION."
  (if (etmux-tmux-running?)
      (let ((result (etmux-tmux-run-command "list-windows" "-t" session "-F" "#{window_id},#{window_name}")))
        (-map (-partial #'s-split ",") (s-split "\n" (s-trim result))))
    (message "found no running tmux sessions")))

(defun etmux-list-all-windows ()
  "List all windows in SESSION."
  (if (etmux-tmux-running?)
      (let ((result (etmux-tmux-run-command "list-windows" "-a" "-F" "#{window_id},#{window_name}")))
        (-map (-partial #'s-split ",") (s-split "\n" (s-trim result))))
    (message "found no running tmux sessions")))

(defun etmux-window-exists? (window)
  "Returns whether WINDOW exists."
  (let* ((windows (etmux-list-all-windows))
         (window-ids (-map #'car windows)))
    (-contains? window-ids window)))

(defun etmux-list-panes (window)
  "List all panes in WINDOW."
  (cond
   ((not (etmux-tmux-running?)) (message "found no running tmux sessions"))
   ((not (etmux-window-exists? window)) (message "window does not exist"))
   (t (let ((result (etmux-tmux-run-command "list-panes" "-t" window "-F" "#{pane_id},#{pane_title}")))
        (-map (-partial #'s-split ",") (s-split "\n" (s-trim result)))))))

(defun etmux-list-all-panes ()
  "List all panes in SESSION."
  (if (etmux-tmux-running?)
      (let ((result (etmux-tmux-run-command "list-panes" "-a" "-F" "#{pane_id},#{pane_title}")))
        (-map (-partial #'s-split ",") (s-split "\n" (s-trim result))))
    (message "found no running tmux sessions")))

(defun etmux-pane-exists? (pane)
  "Returns whether PANE exists."
  (let* ((panes (etmux-list-all-panes))
         (pane-ids (-map #'car panes)))
    (-contains? pane-ids pane)))

(defun etmux-pick-pane ()
  "Pick a tmux pane using completing-read."
  (let* ((session (completing-read "session:" (etmux-list-sessions)))
         (window (completing-read "window: " (etmux-list-windows session)))
         (pane (completing-read "pane: " (etmux-list-panes window))))
    pane))

(defun etmux-jackin ()
  "Creates a project-wide keybinding to call a command in tmux."
  (interactive)
  (let* ((history-file (no-littering-expand-etc-file-name "etmux-history.el"))
         (etmux-command-history (jens/load-from-file history-file))
         (pane (etmux-pick-pane))
         (command (completing-read "command: " etmux-command-history)))

    (unless (member command etmux-command-history)
      (jens/save-to-file (cons command etmux-command-history) history-file))

    (make-variable-buffer-local 'tmux-cmd)
    (make-variable-buffer-local 'tmux-pane)

    (let ((class-sym (gensym)))
      (dir-locals-set-class-variables
       class-sym
       `((nil . ((tmux-cmd . ,command)
                 (tmux-pane . ,pane)))))

      (dir-locals-set-directory-class
       (projectile-project-root) class-sym))

    (defun etmux-jackin-do nil
      (interactive)
      (hack-local-variables)

      (if (and tmux-pane tmux-cmd)
          (progn
            (etmux-C-c tmux-pane)
            (etmux-run-command tmux-pane tmux-cmd))
        (message "etmux is not jacked-in to tmux.")))

    (global-set-key (kbd etmux-jackin-key) #'etmux-jackin-do)))

(defun etmux-spawn-here (session-name)
  "Spawn a new tmux window in an external terminal, in the
current directory."
  (interactive "P")
  (let* ((session-cmd "tmux new-session")
         (session-name (if session-name (completing-read "Session name: " nil)))
         (session-cmd (if (not (s-blank-str-p session-name))
                          (format "%s -s %s" session-cmd session-name)
                        session-cmd))
         (cmd (format "%s -d '%s' -e '%s'" etmux-terminal default-directory session-cmd)))
    (message "etmux command: %s" cmd)
    (start-process-shell-command "etmux" "etmux" cmd)))

(provide 'etmux)
;;; etmux.el ends here
