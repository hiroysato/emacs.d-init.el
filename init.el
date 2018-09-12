;;;; ;; OS によって設定を切り替える例
;;;; (when (eq system-type 'windows-nt) ; Windows
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

;; IME変更
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; 漢字/変換キー入力時のエラーメッセージ抑止
(global-set-key (kbd "<M-kanji>") 'ignore)
(global-set-key (kbd "<kanji>") 'ignore)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 
;; ;; デフォルト フォント
;; ;; (set-face-attribute 'default nil :family "Migu 1M" :height 110)
;; (set-face-font 'default "Migu 1M-11:antialias=standard")
;; 
;; ;; プロポーショナル フォント
;; ;; (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)
;; (set-face-font 'variable-pitch "Migu 1M-11:antialias=standard")
;; 
;; ;; 等幅フォント
;; ;; (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)
;; (set-face-font 'fixed-pitch "Migu 1M-11:antialias=standard")
;; 
;; ;; ツールチップ表示フォント
;; ;; (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)
;; (set-face-font 'tooltip "Migu 1M-9:antialias=standard")
;; 
;;; fontset

;; フォントサイズ調整
(global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; フォントサイズ リセット
(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))

;; 調整
(setq-default indent-tabs-mode nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - frame                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq default-frame-alist
      (append '((width                . 85)  ; フレーム幅
                (height               . 38 ) ; フレーム高
             ;; (left                 . 70 ) ; 配置左位置
             ;; (top                  . 28 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 10 ) ; 左フリンジ幅
                (right-fringe         . 11 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                (tool-bar-lines       . 1  ) ; ツールバー
                (vertical-scroll-bars . 1  ) ; スクロールバー
                (scroll-bar-width     . 17 ) ; スクロールバー幅
                (cursor-type          . box) ; カーソル種別
                (alpha                . 100) ; 透明度
                ) default-frame-alist) )
(setq initial-frame-alist default-frame-alist)

;; フレーム タイトル
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;; 初期画面の非表示（有効：t、無効：nil）
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)

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


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅（有効：1、無効：0）
(blink-cursor-mode 0)

;; 非アクティブウィンドウのカーソル表示（有効：t、無効：nil）
(setq-default cursor-in-non-selected-windows t)

;; evil-mode 側の設定で色を変更するようにしたため
;; （google 日本語入力がポップアップで通知してくれるのでそもそも不要）
;;
;; ;; IME無効／有効時のカーソルカラー定義
;; (unless (facep 'cursor-ime-off)
;;   (make-face 'cursor-ime-off)
;;   (set-face-attribute 'cursor-ime-off nil
;;                       :background "DarkRed" :foreground "White")
;;   )
;; (unless (facep 'cursor-ime-on)
;;   (make-face 'cursor-ime-on)
;;   (set-face-attribute 'cursor-ime-on nil
;;                       :background "DarkGreen" :foreground "White")
;;   )
;;
;; ;; IME無効／有効時のカーソルカラー設定
;; (advice-add 'ime-force-on
;;             :before (lambda (&rest args)
;;                       (if (facep 'cursor-ime-on)
;;                           (let ( (fg (face-attribute 'cursor-ime-on :foreground))
;;                                  (bg (face-attribute 'cursor-ime-on :background)) )
;;                             (set-face-attribute 'cursor nil :foreground fg :background bg) )
;;                         )
;;                       ))
;; (advice-add 'ime-force-off
;;             :before (lambda (&rest args)
;;                       (if (facep 'cursor-ime-off)
;;                           (let ( (fg (face-attribute 'cursor-ime-off :foreground))
;;                                  (bg (face-attribute 'cursor-ime-off :background)) )
;;                             (set-face-attribute 'cursor nil :foreground fg :background bg) )
;;                         )
;;                       ))
;; 
;; ;; バッファ切り替え時の状態引継ぎ設定（有効：t、無効：nil）
;; (setq w32-ime-buffer-switch-p t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
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


;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ screen - tabbar                                               ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;
;; (require 'tabbar)
;;
;; ;; tabbar有効化（有効：t、無効：nil）
;; (call-interactively 'tabbar-mode t)
;;
;; ;; ボタン非表示
;; (dolist (btn '(tabbar-buffer-home-button
;;                tabbar-scroll-left-button
;;                tabbar-scroll-right-button))
;;   (set btn (cons (cons "" nil) (cons "" nil)))
;;   )
;;
;; ;; タブ切替にマウスホイールを使用（有効：0、無効：-1）
;; (call-interactively 'tabbar-mwheel-mode -1)
;; (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
;; (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)
;;
;; ;; タブグループを使用（有効：t、無効：nil）
;; (defvar tabbar-buffer-groups-function nil)
;; (setq tabbar-buffer-groups-function nil)
;;
;; ;; タブの表示間隔
;; (defvar tabbar-separator nil)
;; (setq tabbar-separator '(1.0))
;;
;; ;; タブ切り替え
;; (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
;; (global-set-key (kbd "C-q")     'tabbar-backward-tab)
;;
;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 大文字・小文字を区別しないでサーチ（有効：t、無効：nil）
(setq-default case-fold-search nil)

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


;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ screen - hiwin                                                ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;
;; (require 'hiwin)
;;
;; ;; hiwin-modeを有効化
;; (hiwin-activate)
;;
;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ search - migemo                                               ;;;
;; ;;;   https://github.com/emacs-jp/migemo                            ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;
;; (setq search-default-mode nil)
;; (setq search-default-regexp-mode nil)
;;
;; (require 'migemo)
;;
;; (defvar migemo-command nil)
;; (setq migemo-command "cmigemo")
;;
;; (defvar migemo-options nil)
;; (setq migemo-options '("-q" "--emacs"))
;;
;; (defvar migemo-dictionary nil)
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;;
;; (defvar migemo-user-dictionary nil)
;;
;; (defvar migemo-regex-dictionary nil)
;;
;; (defvar migemo-coding-system nil)
;; (setq migemo-coding-system 'utf-8-unix)
;;
;; (load-library "migemo")
;;
;;
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

;; スクロールダウン
(global-set-key (kbd "C-z") 'scroll-down)

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


;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ shell                                                         ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;
;; (require 'shell)
;; (setq explicit-shell-file-name "bash.exe")
;; (setq shell-command-switch "-c")
;; (setq shell-file-name "bash.exe")
;; ;; (setq explicit-bash.exe-args '("--login" "-i"))
;;
;; ;; (M-! and M-| and compile.el)
;; (setq shell-file-name "bash.exe")
;; (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)
;;
;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ theme                                                         ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;
;; ;; テーマ格納ディレクトリのパス追加
;; (add-to-list 'custom-theme-load-path
;;              (file-name-as-directory (concat user-emacs-directory "theme"))
;;              )
;;
;; ;; テーマ選択
;; ;; (load-theme 'solarized-light t)
;; ;; (load-theme 'solarized-dark t)
;; (load-theme 'gnupack-dark t)
;;
;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ;;; @ server                                                        ;;;
;; ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;
;; ;; emacs-server起動
;; (require 'server)
;; (defun server-ensure-safe-dir (dir) "Noop" t)
;; (setq server-socket-dir "~/.emacs.d")
;; (unless (server-running-p)
;;   (server-start)
;; )
;;
;;; (provide 'init)
;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:


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

(global-set-key (kbd "C-o") 'toggle-input-method)
(global-set-key [kanji] 'toggle-input-method)

;; load-path で ~/.emacs.d とか書かなくてよくなる
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))


;; 検索(全般)時には大文字小文字の区別をしない
(setq case-fold-search t)

;; インクリメンタルサーチ時には大文字小文字の区別をしない
(setq isearch-case-fold-search t)


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
  :ensure t
  :diminish "C"
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode))


;; --------------------------------------------------
;; mozc
;; https://miyazakikenji.wordpress.com/2015/08/11/ac-mozc-on-emacs25-in-ubuntu/
;; --------------------------------------------------
; (use-package mozc
;   :config
;   (setq default-input-method "japanese-mozc")
;   (bind-key* "C-o" 'toggle-input-method)
;   (custom-set-variables '(mozc-leim-title "あ"))
;   (add-hook 'input-method-activate-hook '(lambda () (set-cursor-color "DarkGreen")))
;   (add-hook 'input-method-inactivate-hook '(lambda () (set-cursor-color "DarkRed")))
;   )
;
; (use-package mozc-popup
;   :config
;   (setq mozc-candidate-style 'popup)
;   )
(set-cursor-color "purple")


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
          (space-mark   ?\x3000    [?\□])
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
;;                       hiwin
;; -------------------------------------------------------
; (use-package hiwin
;   :ensure t
;   :config
;   (hiwin-activate)
;   )

;; -------------------------------------------------------
;;                       yasnippet
;; -------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )


;; -------------------------------------------------------
;;                       org
;; -------------------------------------------------------
;;;; C-c C-x C-i ==> Clock-in
;;;; C-c C-x C-o ==> Clock-out
;;;; C-c C-x ;   ==> Timer-set
;;;; C-c C-x C-d ==> 集計
;;;; http://lioon.net/org-mode-awesome-timer-and-clock
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :config
  (setq org-directory "~/org/")
  (setq org-agenda-files (list org-directory))      ; orgs ディレクトリ以下を org-agenda の対象にする
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "NOTE(n)"  "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))
  (setq org-clock-into-drawer t)                    ; LOGBOOK drawerに時間を格納する
  (setq org-startup-indented t)                     ; 「#+STARTUP: indent」と同じ
  (setq org-indent-mode-turns-on-hiding-stars nil)  ; 「#+STARTUP: showstars」と同じ？
  (setq org-capture-templates
        ;; http://members.optusnet.com.au/~charles57/GTD/datetree.html
        '(("j" "Journal Entry" entry (file+datetree (expand-file-name "Journal.org" org-directory))
           "** %^{Heading}" :clock-in t :clock-resume t)
          ("t" "Task Entry" entry (file+headline (expand-file-name "Task.org" org-directory) "Tasks")
           "** TODO %^{Description}  %^g
%?
Added: %U" :prepend t :clock-in t :clock-resume t)))
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
;; -------------------------------------------------------
(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    (slime-setup '(slime-fancy slime-company))
    (use-package slime-company
      :ensure t)))


;;;; -------------------------------------------------------
;;;;                       evil
;;;; -------------------------------------------------------
;;(use-package evil
;;  :ensure t
;;  :init
;;  (setq evil-insert-state-cursor '(bar "yellow"))
;;  (setq evil-normal-state-cursor '(box "#E74C3C"))
;;  (setq evil-emacs-state-cursor  '(box "purple"))
;;  ;; modes to map to different default states
;;  (dolist (mode-map '((ag-mode . emacs)
;;                      (cider-repl-mode . emacs)
;;                      (comint-mode . emacs)
;;                      (eshell-mode . emacs)
;;                      (fundamental-mode . emacs)
;;                      (git-commit-mode . insert)
;;                      (git-rebase-mode . emacs)
;;                      (help-mode . emacs)
;;                      (paradox-menu-mode . emacs)
;;                      (term-mode . emacs)))
;;    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
;;  :config
;;  (evil-mode))
