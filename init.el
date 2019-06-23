;; Package Config
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; UI Config
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-frame-font "Hack 10" nil t) ; Set font
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width 3)
(setq default-fill-column 80)
(global-hl-line-mode 1)


;; Misc Config
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Keybindings
(use-package general
  :config (general-define-key
  :states '(normal visual insert emacs)
  ;(general-evil-setup t)
  ;(general-define-key "C-'" 'avy-goto-word-1)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
   ;; bind to simple key press
  "bb" 'ivy-switch-buffer
  "/" 'counsel-git-grep
  ;; bind to double key press
  "f" '(:ignore t :which-key "files")
  "ff" 'counsel-find-file
  "fr" 'counsel-recentf
  "p" '(:ignore t :which-key "project")
  "pt" 'counsel-projectile
  "pp" 'counsel-projectile-switch-project
  "pf" 'counsel-projectile-find-file
 ))

;; Theme
(use-package atom-one-dark-theme
  :init(load-theme 'atom-one-dark t))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package avy
  :commands (avy-goto-word-1))

(use-package all-the-icons)

(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
	("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add recentf-mode and bookmarks to ivy-switchibuffer
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d)"))

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(use-package all-the-icons-ivy
  :config (all-the-icons-ivy-setup))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))

(use-package projectile
  :config
  (projectile-mode))
(setq projectile-completion-system 'ivy)


(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit)

(use-package adaptive-wrap
  :config
  (setq-default adaptive-wrap-extra-indent 1)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode +1))

;; Web
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 4

	web-mode-enable-css-coloraization t
	web-mode-enable-auto-pairing t
	web-mode-enable-current-element-highlight t
	)
  )

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))
  
(use-package org-bullets
  :config (progn (add-hook 'org-mode-hook
			   (lambda ()
			     (org-bullets-mode 1)))))

; (general-define-key
 ;; replace default keybindings
 ;"C-s" 'swiper         ; search for string in current buffer
 ;"M-x" 'counsel-M-x    ; replace default M-x with ivy backend
 ;)

