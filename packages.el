(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(package-refresh-contents)


(defconst ora-packages  
  '(dropdown-list
    yasnippet
    csharp-mode
    paredit
    magit
    edit-server
    edit-server-htmlize
    iedit
    flymake-google-cpplint
    flymake-cursor
    google-c-style
    smart-mode-line
    maxframe
    avy
    ace-window
    pcre2el
    key-chord
    visual-regexp
    visual-regexp-steroids
    projectile
    helm-projectile
    flx-ido
    jedi
    elpy
    multiple-cursors
    iy-go-to-char
    haskell-mode
    skewer-mode
    emmet-mode
    web-mode
    js-comint
    csv-mode
    org-journal
    company
    company-c-headers
    evil
    geiser
    sublime-themes
    tern
    company-tern
    js2-refactor
    json-mode
    paradox
    auto-yasnippet
    exec-path-from-shell)
  "List of packages that I like.")

;; install required
(dolist (package ora-packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))
