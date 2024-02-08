;; use-package setup
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; packages
(use-package ef-themes
  :ensure t)
(use-package doom-themes
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package expand-region
  :ensure t)
(use-package which-key)
(which-key-mode 1)
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
(use-package denote
  :ensure t)
(setq denote-directory "~/Notes")
(use-package vertico
  :ensure t
  :init (vertico-mode 1))
(use-package all-the-icons
  :if (display-graphic-p))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package frog-jump-buffer :ensure t)
(global-set-key (kbd "C-`") 'frog-jump-buffer)

(global-set-key [f7] 'ibuffer)

;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

;; lsp stuff
  (use-package lsp-mode
    :init
    (setq gc-cons-threshold (* 100 1024 1024)
          read-process-output-max (* 1024 1024)
          company-idle-delay 0.0
          company-minimum-prefix-length 1
          create-lockfiles nil) ;; lock files will kill `npm start'
    :hook (prog-mode . lsp)
    :commands lsp)

  (use-package lsp-ui
    :commands lsp-ui-mode)

  (use-package company
	:ensure t)
  (use-package typescript-mode
    :mode "\\.tsx?\\'"
    :config
    (setq typescript-indent-level 2))


(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

;; basic UI changes
(setq native-comp-async-report-warnings-errors nil)
(tool-bar-mode 0)
(setq-default tab-width 4)
(add-to-list 'exec-path "~/go/bin/")
;;(when (not (display-graphic-p)) (menu-bar-mode 0))
(menu-bar-mode 1)
										;(global-display-line-numbers-mode 0)
(defun my-display-numbers-hook ()
 (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)


(global-tab-line-mode)
(setq inhibit-startup-message t)
(setq scroll-conservatively 100)
;; basic text editing changes
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(delete-selection-mode 1)
(setq make-backup-files nil) ; stop creating ~ files
;; keybindings
(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-<tab>") 'xah-next-user-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'xah-next-user-buffer)
(global-set-key (kbd "<f5>") 'compile)

(if window-system (load-theme 'doom-tomorrow-day t))
(global-visual-line-mode 1)
;; emacs-lisp functions
(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
;; colorscheme and fonts
(when (member "Cascadia Mono" (font-family-list))
  (set-frame-font "Cascadia Mono-12" t t))

(global-set-key (kbd "<home>") 'frog-jump-buffer)
(global-set-key (kbd "<delete>") 'delete-window)

;; wangblows
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "e546768f3da4b394adc8c460106a7d220af130a3a2a0518d265c832d015a4385" "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" default))
 '(package-selected-packages
   '(company magit typescript-mode company-lsp lsp-ui lsp-mode frog-jump-buffer switch-window all-the-icons vertico denote neotree which-key expand-region go-mode ef-themes spinner markdown-mode lv)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
