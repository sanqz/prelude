;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)
(set-frame-size (selected-frame) 130 30)
(set-frame-position (selected-frame) 20 50)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'prelude-macos))

(message "Loading Prelude's modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "Missing modules file %s" prelude-modules-file)
  (message "You can get started by copying the bundled example file from sample/prelude-modules.el"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#\.].*el$")))

(message "Prelude is ready to do thy bidding, Master %s!" current-user)

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
(menu-bar-mode t)
(scroll-bar-mode -1)
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

(set-font   "Microsoft YaHei Mono" "Microsoft YaHei Mono" 35 35)
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

(defun org-insert-image-from-clipboard ()
  (interactive)
  (let* ((the-dir (file-name-directory buffer-file-name))
         (attachments-dir (concat the-dir "attachments"))
         (png-file-name (format-time-string "%a%d%b%Y_%H%M%S.png"))
         (png-path (concat attachments-dir "/" png-file-name))
         (temp-buffer-name "CbImage2File-buffer"))
    (call-process "CbImage2File" nil temp-buffer-name nil png-path)
    (let ((result (with-current-buffer temp-buffer-name (buffer-string))))
      (progn
        (kill-buffer temp-buffer-name) 
        (if (string= result "")
            (progn 
              (insert (concat "[[./attachments/" png-file-name "]]"))
              (org-display-inline-images))
          (insert result))))))

(defun my/clipboard-image-to-file ()
  "use imagemagick to write clipboard image to file "
  (interactive)
  (let* ( (img-dir "~/org/images")
          (filename (concat  (make-temp-name
                              (concat 
                               "CAPT_"
                               (format-time-string "%Y%m%d_%H%M%S_"))) ".png")))
    (unless (file-exists-p img-dir)
      (make-directory img-dir))
    ;;(message filename)
    (call-process "magick" nil nil nil "clipboard: " filename)
    (insert (concat "[[" filename "]]"))) )

(defun my/org-insert-clip-image ()
  "insert clip image into org file"
  (interactive)
  (my/clipboard-image-to-file)
  (org-display-inline-images)
  )
(global-set-key (kbd "C-S-y") 'my/org-insert-clip-image)

;;org-mode set-up here
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))

  :config
  (setq org-default-notes-file "~/org/notes.org")
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
  )

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
  (find-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "M-n") 'my/toggle-line-numbers)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key [M-up] 'move-line-up)
(global-set-key [M-down] 'move-line-down)

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
(yas-minor-mode 1)

(require 'dired)
(defun dired-mouse-find-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (progn
              (select-window window)
              (dired file)))
      (select-window window)
      (find-file-other-window (file-name-sans-versions file t)))))

(define-key dired-mode-map [double-mouse-1] 'dired-mouse-find-file)
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

;;(local-unset-key [mouse-2])
;;(define-key [mouse-2] key nil)

(require 'dired-details-s)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list
        '("λ"
          "✓"
          "✓"
          "✓"
          ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
          ;; Small
          ;; ► • ★ ▸
          )
        ))

;;汉字  
;; (setq utf-translate-cjk-mode t) 
;; (set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8-mac) 
;; (setq locale-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (unless (eq system-type 'windows-nt)
;;   (set-selection-coding-system 'utf-8))
;; (prefer-coding-system 'utf-8)
;; (when (eq system-type 'windows-nt)
;;   (set-selection-coding-system 'utf-16-le))

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))
(server-start)
;;(prelude-eval-after-init
;; greet the use with some useful tip
;;(run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here
