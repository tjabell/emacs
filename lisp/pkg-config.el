(add-hook 'scheme-mode-hook 'paredit-mode)

(add-hook 'hs-minor-mode-hook
					(lambda ()   (global-set-key (kbd "C-x C-o") 'hs-toggle-hiding)))

