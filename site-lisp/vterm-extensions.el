;;; https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
;;; I really don't get what this is doing 20211029TJA
(defun my:vterm-run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun my:vterm-run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(defun my:vterm-run-fbp-api ()
  (interactive)
  (with-current-buffer (vterm (concat "* FBP API *"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/")
    (vterm-send-return)
    (vterm-send-string "./local_startup.sh")
    (vterm-send-return)))

(defun my:vterm-run-fbp-web ()
  (interactive)
  (with-current-buffer (vterm (concat "* FBP WEB *"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/FranchiseePortal-Website/")
    (vterm-send-return)
    (vterm-send-string "./local_startup.sh")
    (vterm-send-return)))
