;;;; ;; OS によって設定を切り替える例
;;;; (when (eq system-type 'windows-nt) ; Windows、Linux の場合は「x」
;;;;     ;; ←Win用設定を、ここに記述
;;;; )
;;;;
;;;; (when (eq system-type 'darwin) ; Mac OS X
;;;;     ;; ←Mac用設定を、ここに記述
;;;; )
;;;;
;;;; (if (not window-system) (progn
;;;;     ;; ←CUI用設定を、ここに記述
;;;; ))
;;;;
;;;; (if window-system (progn
;;;;     ;; ←GUI用設定を、ここに記述
;;;; ))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; 環境依存文字 文字化け対応
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

(setq eol-mnemonic-dos ":CRLF")
(setq eol-mnemonic-mac ":CR")
(setq eol-mnemonic-undecided ":?")
(setq eol-mnemonic-unix ":LF")


(when (eq system-type 'windows-nt) ; Windows
    ;; ←Win用設定を、ここに記述
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key binding - keyboard                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

  ;; Altキーを使用せずにMetaキーを使用（有効：t、無効：nil）
  (setq w32-alt-is-meta t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - input method                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

  ;; モードラインの表示文字列
  (setq-default w32-ime-mode-line-state-indicator "[Aa] ")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))

  ;; IME初期化
  (w32-ime-initialize)

  ;; デフォルトIME
  (setq default-input-method "W32-IME")
  )

;; IME変更
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; 漢字/変換キー入力時のエラーメッセージ抑止
(global-set-key (kbd "<M-kanji>") 'ignore)
(global-set-key (kbd "<kanji>") 'ignore)



;; フォントサイズ調整
(global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; フォントサイズ リセット
(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))

;; 調整
(setq-default indent-tabs-mode nil)

;; フレーム タイトル
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;; 初期画面の非表示（有効：t、無効：nil）
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)

;; 初期表示のウィンドウサイズ
(add-to-list 'default-frame-alist '(width  . 145))
(add-to-list 'default-frame-alist '(height . 45))


;; フルスクリーン化
(global-set-key (kbd "<M-return>") 'toggle-frame-fullscreen)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 行番号の表示（有効：t、無効：nil）
(line-number-mode t)

;; 列番号の表示（有効：t、無効：nil）
(column-number-mode t)

;; モードライン カスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   w32-ime-mode-line-state-indicator
   " "
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   global-mode-string
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "L%%l/L%d " (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )
(setq mode-line-frame-identification " ")

;; cp932エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


(when (eq system-type 'windows-nt) ; Windows
    ;; ←Win用設定を、ここに記述
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - minibuffer                                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

  ;; minibufferのアクティブ時、IMEを無効化
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (deactivate-input-method)))
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅（有効：1、無効：0）
(blink-cursor-mode 0)

;; 非アクティブウィンドウのカーソル表示（有効：t、無効：nil）
(setq-default cursor-in-non-selected-windows t)

;; IME無効／有効時のカーソルカラー定義
(unless (facep 'cursor-ime-off)
  (make-face 'cursor-ime-off)
  (set-face-attribute 'cursor-ime-off nil
                      :background "DarkRed" :foreground "White")
  )
(unless (facep 'cursor-ime-on)
  (make-face 'cursor-ime-on)
  (set-face-attribute 'cursor-ime-on nil
                      :background "DarkGreen" :foreground "White")
  )

;; IME無効／有効時のカーソルカラー設定
(advice-add 'ime-force-on
            :before (lambda (&rest args)
                      (if (facep 'cursor-ime-on)
                          (let ( (fg (face-attribute 'cursor-ime-on :foreground))
                                 (bg (face-attribute 'cursor-ime-on :background)) )
                            (set-face-attribute 'cursor nil :foreground fg :background bg) )
                        )
                      ))
(advice-add 'ime-force-off
            :before (lambda (&rest args)
                      (if (facep 'cursor-ime-off)
                          (let ( (fg (face-attribute 'cursor-ime-off :foreground))
                                 (bg (face-attribute 'cursor-ime-off :background)) )
                            (set-face-attribute 'cursor nil :foreground fg :background bg) )
                        )
                      ))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'linum)

;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

;; バッファ中の行番号表示の遅延設定
(defvar linum-delay nil)
(setq linum-delay t)
(defadvice linum-schedule (around linum-schedule-around () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示（有効：t、無効：nil）
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.75)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 大文字・小文字を区別しないでサーチ（有効：t、無効：nil）
(setq-default case-fold-search t)

;; インクリメント検索時に縦スクロールを有効化（有効：t、無効：nil）
(setq isearch-allow-scroll nil)

;; C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)

;; C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;; C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda() (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook
 'isearch-mode-hook
 '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
 )
(add-hook
 'isearch-mode-end-hook
 '(lambda() (setq w32-ime-composition-window nil))
 )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - backup                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ファイルオープン時のバックアップ（~）（有効：t、無効：nil）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))

;; 編集中ファイルの自動バックアップ（有効：t、無効：nil）
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除（有効：t、無効：nil）
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ（有効：t、無効：nil）
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルを生成（有効：t、無効：nil）
(setq create-lockfiles nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; スクロール時のカーソル位置を維持（有効：t、無効：nil）
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-marginに影響せず）
(setq scroll-step 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; キー入力中の画面更新を抑止（有効：t、無効：nil）
(setq redisplay-dont-pause t)

;; recenter-top-bottomのポジション
(setq recenter-positions '(middle top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)

;; ;; スクロールダウン
;; (global-set-key (kbd "C-z") 'scroll-down)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        )
    (if (< bgn-num (window-height))
        (goto-char (point-min))
      ad-do-it) ))

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        (end-num nil)
        )
    (save-excursion
      (goto-char (point-max))
      (setq end-num (+ 1 (count-lines (point-min) (point))))
      )
    (if (< (- (- end-num bgn-num) (window-height)) 0)
        (goto-char (point-max))
      ad-do-it) ))


;; --------------------------------------------------
;; initialize
;; --------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(global-auto-revert-mode 1)        ; 自動読み込み
(keyboard-translate ?\C-h ?\C-?)   ; Backspace
(fset 'yes-or-no-p 'y-or-n-p)      ; 選択肢を y-n にする
(setq completion-ignore-case t)    ; file名の補完で大文字小文字を区別しない

(global-set-key (kbd "C-o") 'toggle-input-method)
(global-set-key [kanji] 'toggle-input-method)

;; load-path で ~/.emacs.d とか書かなくてよくなる
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))


;; 起動画面を非表示にする
(setq inhibit-splash-screen t)


;; --------------------------------------------------
;; language
;; --------------------------------------------------
; (setenv "LANG"  "ja_JP.UTF-8")
; (set-language-environment "Japanese")
; (prefer-coding-system 'utf-8-unix)
; (setq-default buffer-file-coding-system 'utf-8-unix)
; (set-default-coding-systems 'utf-8-unix)
; (set-file-name-coding-system 'utf-8-unix)
; (set-terminal-coding-system 'utf-8-unix)
; (set-keyboard-coding-system 'utf-8-unix)
; (set-selection-coding-system 'utf-8-unix)
; (setq default-buffer-file-coding-system 'utf-8-unix)
; (set-buffer-file-coding-system 'utf-8-unix)
; (setq locale-coding-system 'utf-8)
; ;; mnemonic for utf-8 is "U", which is defined in the mule.el
; (setq eol-mnemonic-dos ":CRLF")
; (setq eol-mnemonic-mac ":CR")
; (setq eol-mnemonic-undecided ":?")
; (setq eol-mnemonic-unix ":LF")


;; --------------------------------------------------
;; package
;; --------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)


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
  :ensure t
  :diminish "C"
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

;;;; (if window-system (progn
;;;;     ;; ←GUI用設定を、ここに記述



(eq window-system 'x)
;;;; (if window-system (progn
;;;;     ;; ←GUI用設定を、ここに記述
;;;; ))

(when (eq system-type 'x) ; Linux
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
  )


;; --------------------------------------------------
;; elscreen
;; --------------------------------------------------
(use-package elscreen
  :ensure t
  :config
  (bind-key* "<C-tab>" 'elscreen-next)
  (bind-key* "<C-S-tab>" 'elscreen-previous)
  (setq elscreen-prefix-key (kbd "C-z"))
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start)
  )


;; --------------------------------------------------
;; sql-mysql
;; --------------------------------------------------
(use-package sql
  :ensure t
  :config
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
;  (setq sql-mysql-program "/c/xampp/mysql/bin/mysql")		; gnupack 用
;   (setq sql-mysql-program "c:/xampp/mysql/bin/mysql")
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
;; タブ幅設定用関数
;; https://masutaka.net/chalow/2009-07-10-4.html
;; -----------------------------------------------------------------------------
(defun set-aurora-tab-width (num &optional local redraw)
  "タブ幅をセットします。タブ5とかタブ20も設定できたりします。
localが non-nilの場合は、カレントバッファでのみ有効になります。
redrawが non-nilの場合は、Windowを再描画します。"
  (interactive "nTab Width: ")
  (when local
    (make-local-variable 'tab-width)
    (make-local-variable 'tab-stop-list))
  (setq tab-width num)
  (setq tab-stop-list ())
  (while (<= num 256)
    (setq tab-stop-list `(,@tab-stop-list ,num))
    (setq num (+ num tab-width)))
  (when redraw (redraw-display)) tab-width)

(set-aurora-tab-width (setq default-tab-width (setq-default tab-width 4)))


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
(setenv "PATH"
  (concat
   (getenv "PATH")
;   ":/C/xampp/mysql/bin"           gnupack 用
   ";C:/Users/hiroysato/Documents/App/mysql-5.7.21-winx64/bin"
   ";C:/Program Files/Git/usr/bin"
   ))


;; -------------------------------------------------------
;;                       whitespace
;; -------------------------------------------------------
(use-package whitespace
  :ensure t
  :config
  ;; 色の指定は M-x: list-faces-display から行う
  (setq whitespace-space-regexp "\\([\x0020\x3000]+\\)"
    whitespace-style
        '(face
          tabs
          tab-mark
;          spaces
          space-mark
          newline
          newline-mark
          trailing)
    whitespace-display-mappings
        '(;; (space-mark   ?\    [?\xB7])
          ;; (space-mark   ?\x3000    [?\□])
          (newline-mark ?\n   [?\↲ ?\n] )
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          ;; (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
          )
    whitespace-line-column 85
    whitespace-global-modes '(not
                              eww-mode
                              term-mode
                              eshell-mode
                              org-mode
                              org-agenda-mode
                              calendar-mode)
    )
  (global-whitespace-mode 1)
  )


;; -------------------------------------------------------
;;                       yasnippet
;; -------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )


;; -------------------------------------------------------
;;                       eww
;; -------------------------------------------------------
;;;; N (eww-next-url)
;;;; P (eww-previous-url)
;;;; l (eww-back-url)
;;;; r (eww-forward-url)
;;;; H (eww-list-histories)
;;;; & (eww-browse-with-external-browser)
;;;; b (eww-add-bookmark)
;;;; B (eww-list-bookmarks)
;;;; q (quit-window)
;;;; https://futurismo.biz/archives/2950
(use-package eww
  :config
  (bind-keys :map eww-mode-map
             ("r" . eww-reload)
             ("c 0" eww-copy-page-url)
             ("p" scroll-down)
             ("n" scroll-up))
  (setq eww-search-prefix "https://duckduckgo.com/html/?kl=jp-jp&k1=-1&kc=1&kf=-1&q="))
(defun eww-disable-images ()
  "eww で画像表示させない"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))
(defun eww-enable-images ()
  "eww で画像表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))
;; はじめから非表示
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)


;; -------------------------------------------------------
;;                       org
;; -------------------------------------------------------
;;;; C-c C-x C-i ==> Clock-in
;;;; C-c C-x C-o ==> Clock-out
;;;; C-c C-x ;   ==> Timer-set
;;;; C-c C-x C-d ==> 集計
;;;; http://lioon.net/org-mode-awesome-timer-and-clock
;;;;
;;;; You can configure how Org handles file links by customizing the variable org-file-apps.
;;;; By default Org will check auto-mode-alist and open the file in Emacs if it finds a match. If you would rather use the default system application for .xls files you can add something like this to your init file:
;;;; (add-to-list 'org-file-apps '("\\.xls\\'" . default))
;;;;
;;;; You can also use prefix arguments to choose how to open a link on a case-by-case basis:
;;;;
;;;; C-c C-o will open the link according to your configuration
;;;; C-u C-c C-o will visit the file in Emacs
;;;; C-u C-u C-c C-o will let the system decide how to open the file
;;;; https://emacs.stackexchange.com/questions/2708/relative-path-link-to-xls-file-from-org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (sql    . t)
     (ruby   . t)))
  (add-to-list 'org-file-apps '("\\.xls\\'" . default))
  (add-to-list 'org-file-apps '("\\.xlsx\\'" . default))

  ;; (setq org-directory "~/org/")
  (setq org-directory "D:/Users/xiroh/Documents/org/")
  (setq org-agenda-files (list org-directory))      ; orgs ディレクトリ以下を org-agenda の対象にする
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)"  "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
  (setq org-clock-into-drawer t)                    ; LOGBOOK drawerに時間を格納する
  (setq org-startup-indented t)                     ; 「#+STARTUP: indent」と同じ
  (setq org-indent-mode-turns-on-hiding-stars nil)  ; 「#+STARTUP: showstars」と同じ？
  (setq org-capture-templates
        ;; http://members.optusnet.com.au/~charles57/GTD/datetree.html
        '(("j" "Journal Entry" entry (file+datetree (expand-file-name "Journal.org" org-directory))
           "** %^{Heading}\n%?" :clock-in t :clock-resume t)
          ("t" "Task Entry" entry (file+headline (expand-file-name "Task.org" org-directory) "Tasks")
           "** TODO %^{Description}  %^g\n%?\nAdded: %U" :clock-in t :clock-resume t)
          ("m" "Meeting Entry" entry (file+datetree (expand-file-name "Meeting.org" org-directory))
           "* %^{会議名} %^g\n【目　的】%^{目的}\n【場　所】%^{場所}\n【参加者】%^{参加者}\n** アジェンダ\n%?\n** 決定事項\n** 議事内容\n\n" :prepent t :clock-in t :clock-resume t)
          ))
  (add-hook 'org-timer-done-hook
            (lambda ()
              (save-excursion
                (if (org-clocking-p)   ; org-clockが実行中なら
                    (org-clock-out))))); 完了させる
  (add-hook 'org-clock-out-hook
            (lambda ()
              (if org-timer-countdown-timer
                  (org-timer-cancel-timer))))
  )

;;; 「#+BEGIN:」と「#+END:」の間で C-c C-x C-u (org-dblock-update )
;;; #+BEGIN: clocktable :maxlevel 4 :scope file :tags "" :tstart "2010-12-12" :tend "2010-12-19"
;;; #+END:

;; -------------------------------------------------------
;;                       company
;; -------------------------------------------------------
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)


(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))


;; -------------------------------------------------------
;;                       slime
;; slime-company は use-package で install できなかったので
;; package-list-packages から slime-company を探して手動でインストールした
;; -------------------------------------------------------
(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    (slime-setup '(slime-fancy slime-company))
    (use-package slime-company
      :ensure t)))


;; -------------------------------------------------------
;;                       cl
;; 色々と便利なので Common Lisp マクロパッケージを読み込んでおく
;; -------------------------------------------------------
(require 'cl)



;; -------------------------------------------------------
;;                       skk
;; -------------------------------------------------------
(use-package ddskk
  :ensure t
  :bind (("C-x j" . skk-mode))
  :init (setq skk-egg-like-newline t))
