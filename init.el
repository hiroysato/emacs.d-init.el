;; emacs -nw
;; ESC `

;; --------------------------------------------------
;; initialize
;; --------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(global-auto-revert-mode 1)        ; 自動読み込み
(keyboard-translate ?\C-h ?\C-?)   ; Backspace
(fset 'yes-or-no-p 'y-or-n-p)      ; 選択肢を y-n にする
(setq auto-save-default nil)       ; オートセーブファイルを作らない
(setq completion-ignore-case t)    ; file名の補完で大文字小文字を区別しない


;; --------------------------------------------------
;; language
;; --------------------------------------------------
(setenv "LANG"  "ja_JP.UTF-8")
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
;; mnemonic for utf-8 is "U", which is defined in the mule.el
(setq eol-mnemonic-dos ":CRLF")
(setq eol-mnemonic-mac ":CR")
(setq eol-mnemonic-undecided ":?")
(setq eol-mnemonic-unix ":LF")


;; --------------------------------------------------
;; package
;; --------------------------------------------------
(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))


;; --------------------------------------------------
;; use-package
;; --------------------------------------------------
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; --------------------------------------------------
;; auto-compile
;; init.el を変更したらすぐにバイトコンパイル
;; --------------------------------------------------
(use-package auto-compile :no-require t :defer t :ensure t
  :diminish "C"
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))


;; --------------------------------------------------
;; mozc
;; https://miyazakikenji.wordpress.com/2015/08/11/ac-mozc-on-emacs25-in-ubuntu/
;; --------------------------------------------------
(use-package mozc
  :config
  (setq default-input-method "japanese-mozc")
  (bind-key* "C-o" 'toggle-input-method)
  (custom-set-variables '(mozc-leim-title "あ"))
  (add-hook 'input-method-activate-hook '(lambda () (set-cursor-color "DarkGreen")))
  (add-hook 'input-method-inactivate-hook '(lambda () (set-cursor-color "DarkRed")))
  )

(use-package mozc-popup
  :config
  (setq mozc-candidate-style 'popup)
  )


;; --------------------------------------------------
;; elscreen
;; --------------------------------------------------
(use-package elscreen
  :config
  (bind-key* "<C-tab>" 'elscreen-next)
  (bind-key* "<C-S-tab>" 'elscreen-previous)
  (setq elscreen-prefix-key (kbd "C-z"))
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start)
  )


;; --------------------------------------------------
;; whitespace
;; https://uwabami.github.io/cc-env/Emacs.html
;; --------------------------------------------------
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-line-column 72
        whitespace-style '(face              ; faceを使って視覚化する．
                           trailing          ; 行末の空白を対象とする．
                           tabs              ; tab
                           spaces            ; space
                           )
        whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])
                                      (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))
        whitespace-space-regexp "\\(\u3000+\\)"
        whitespace-global-modes '(not
                                  eww-mode
                                  term-mode
                                  eshell-mode
                                  org-agenda-mode
                                  calendar-mode)
        )
  (global-whitespace-mode 1)
  )


;; --------------------------------------------------
;; sql-mysql
;; --------------------------------------------------
(use-package sql
  :config
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  )


;; -----------------------------------------------------------------------------
;; alpha
;; -----------------------------------------------------------------------------
;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))


;; -----------------------------------------------------------------------------
;; increment/decrement region
;; -----------------------------------------------------------------------------
(defun increment-region (&optional beg end arg)
  "Increment all decimal numbers in region between `beg' and
`end' by `arg'. If no prefix arg is given, increment by 1. If the
mark is not active, try to build a region using
`symbol-at-point'."
  (interactive "r\np")
  (or arg (setq arg 1))
  (unless (and mark-active transient-mark-mode)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds (setq beg (car bounds) end (cdr bounds)))))
  (if (< end beg)
      (let ((tmp end))
        (setq beg end end tmp)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "-?[0-9]+" end t)
      (replace-match (number-to-string (+ arg (string-to-number (match-string 0)))))))
  (setq deactivate-mark nil))

(defun decrement-region (&optional beg end arg)
  "Decrement all decimal numbers in region between `beg' and
`end' by `arg'. If no prefix arg is given, increment by 1. If the
mark is not active, try to build a region using
`symbol-at-point'."
  (interactive "r\np")
  (or arg (setq arg 1))
  (unless (and mark-active transient-mark-mode)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if bounds (setq beg (car bounds) end (cdr bounds)))))
  (increment-region beg end (- arg)))

(bind-key* "<M-up>" 'increment-region)
(bind-key* "<M-down>" 'decrement-region)


;; -----------------------------------------------------------------------------
;; hl-line 高速化
;; http://emacs.rubikitch.com/global-hl-line-mode-timer/
;; -----------------------------------------------------------------------------
(require 'hl-line)
;;; hl-lineを無効にするメジャーモードを指定する
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode))
(defun global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)


;; -----------------------------------------------------------------------------
;; org-babel
;; -----------------------------------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
         '((python . t)
           (sql    . t)
           (ruby   . t)))
