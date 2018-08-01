(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-rpc-python-command "D:/Anaconda3/pythonw.exe")
 '(global-company-mode t)
 '(magit-git-executable "D:/msys64/usr/bin/git.exe")
 '(package-selected-packages
   (quote
    (window-number use-package sr-speedbar pyenv-mode helm-ag counsel markdown-mode autopair org-ac ob-ipython swiper paredit aggressive-indent company-anaconda elpy smooth-scroll helm helm-ebdb zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens operate-on-number move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring beacon anzu ace-window)))
 '(python-shell-interpreter "D:/Anaconda3/Scripts/ipython.exe")
 '(safe-local-variable-values (quote ((flycheck-disabled-checkers emacs-lisp-checkdoc)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(package-initialize)
(set-frame-size (selected-frame) 180 45)
(set-frame-position (selected-frame) 20 50)
;;functions
(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package window-number
  :config
  (window-number-mode 1))
;; (require 'smooth-scroll)
(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-rpc-backend "jedi"))
(use-package ob-ipython)

(use-package package
  :bind (("C-c p l" . package-list-packages)
         ("C-c p i" . package-install)
         ("C-c p d" . package-delete)
         ("C-c p r" . package-reinstall)))

(use-package ediff
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = c" . compare-windows)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)
         ("C-c = m" . count-matches)
         ("C-c = r" . ediff-revision)
         ("C-c = p" . ediff-patch-file)
         ("C-c = P" . ediff-patch-buffer)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise)))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
;; (smooth-scroll-mode -1)

(tool-bar-mode t)
(scroll-bar-mode -1)
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(set-font   "Microsoft YaHei Mono" "Microsoft YaHei Mono" 30 30)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (python . t)
   ;; other language
   ))
;;don't prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)
;;display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(defun my/toggle-line-numbers(&optional ARG)
  "display line-numbers or not"
  (interactive)
  (if (eq display-line-numbers t)
      (display-line-numbers-mode -1)
    (display-line-numbers-mode t)))
(defun python/scratch ()
  (interactive)
  (let (
        ;; Creates a new buffer object.
        (buf (get-buffer-create "*python-scratch*"))
        )
    ;; Executes functions that would change the current buffer at
    ;; buffer buf
    (with-current-buffer buf
       ;;; Set the new buffer to scratch mode
      (python-mode)
       ;;; Pop to scratch buffer
      (pop-to-buffer buf)
      )))
(defun reload-init-file ()
  "Reload init.el file."
  (interactive)
  (load user-init-file)
  (message "Reloaded init.el OK."))
(defun open-init-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/personal/custom.el")))
(global-set-key (kbd "M-n") 'my/toggle-line-numbers)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)


(use-package ace-jump-mode
  :ensure t
  :bind ("C-," . ace-jump-mode))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)))

(use-package company
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setq company-show-numbers t)

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
     Unless the number is potentially part of the candidate.
     In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))
  (global-company-mode 1))
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (("C-x y d" . yas-load-directory)
         ("C-x y i" . yas-insert-snippet)
         ("C-x y f" . yas-visit-snippet-file)
         ("C-x y n" . yas-new-snippet)
         ("C-x y t" . yas-tryout-snippet)
         ("C-x y l" . yas-describe-tables)
         ("C-x y g" . yas/global-mode)
         ("C-x y m" . yas/minor-mode)
         ("C-x y a" . yas-reload-all)
         ("C-x y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory (emacs-path "snippets"))
  (yas-global-mode 1))

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)
