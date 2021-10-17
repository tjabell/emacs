;;; GSI Project functions
(defun my:gsi:rename-aem-vterm-buffer ()
  (interactive)
  (rename-buffer "*vterm* AEM Author Instance"))

(defun my:gsi:rename-content-api-vterm-buffer ()
  (interactive)
  (rename-buffer "*vterm* Content API"))

(defun my:gsi:rename-fp-web-vterm-buffer ()
  (interactive)
  (rename-buffer "*vterm* Franchisee Portal Angular Client"))

(defun my:gsi:rename-fp-api-vterm-buffer ()
  (interactive)
  (rename-buffer "*vterm* Franchisee Portal API"))

(defun my:gsi:rename-faculty-api-vterm-buffer ()
  (interactive)
  (rename-buffer "*vterm* Faculty API"))

;;; WIP
;;; https://github.com/akermu/emacs-libvterm/issues/55
(defun my:gsi:vterm-rename-sensibly ()
  (interactive)
  (let ((folder ()))
    (rename-buffer "*vterm* " folder)))


