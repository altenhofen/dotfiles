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
(use-package go-mode
  :ensure t)
(use-package expand-region
  :ensure t)
(use-package which-key)
(which-key-mode 1)
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)

;; basic UI changes
(setq native-comp-async-report-warnings-errors nil)
(tool-bar-mode 0)
(setq-default tab-width 4)
(add-to-list 'exec-path "~/go/bin/")
;;(when (not (display-graphic-p)) (menu-bar-mode 0))
(menu-bar-mode 0)
(global-display-line-numbers-mode 1)
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

(if window-system (load-theme 'modus-operandi t))

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
(when (member "Terminus" (font-family-list))
  (set-frame-font "Terminus-15" t t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(neotree which-key expand-region go-mode ef-themes spinner markdown-mode lv)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
