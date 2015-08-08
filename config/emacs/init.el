;;;; ロードパスの設定
(setq load-path (append (list
                         (expand-file-name "~/.emacs.d/elisp")
			 (expand-file-name "~/.emacs.d/elisp/ess-12.09/lisp")
			 )
                        load-path))

;;;; パッケージ管理に melpa を追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(require 'cl)
(defvar my-packages
  '(flycheck js2-mode php-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
	(when (not (package-installed-p p))
	  (package-install p))))



;;;; 自動コードチェック
;;;;     対応言語（確認済み）: C, C++, elisp, HTML, Perl, Python, Ruby
;;;;     対応言語（未確認）: CSS, JS, PHP, ShelScripts, Tex, XML
;;;;     あれ、対応してなくない？: CSS, JS, PHP, Tex
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; メニューバーを非表示
;;;; not express menu bar
(menu-bar-mode -1)

;;;; 色などの初期設定
;;;; Initial color
(setq default-frame-alist
      (append (list
	       '(foreground-color . "snow") ;; 文字色
	       '(background-color . "black") ;; 背景色
	       ;'(border-color     . "yellow") ;; 縁の色
	       ;'(mouse-color      . "white")
	       ;'(cursor-color     . "white") ; 効いてない
	       ;'(cursor-type      . box) ; 効いてない
	       ;'(cursor-height    . 4) ; 効いてない

	       ;'(menu-bar-lines . 1)
	       ;'(font . "my-fontset")
	       ;'(vertical-scroll-bars . nil) ;;スクロールバーを消す
	       ;'(width . 100) ;; ウィンドウ幅
	       ;'(height . 40) ;; ウィンドウの高さ
	       ;'(top . 60)   ;; 左上隅のy座標
	       ;'(left . 140) ;; 左上隅のx座標
	       )
	      default-frame-alist))

;;;; 背景透過度
;;;; 数値は0(透明) - 100(非透明), (アクティブ時 非アクティブ時)
;(when (featurep 'carbon-emacs-package)
;  (add-to-list 'default-frame-alist '(alpha . (100 50)))
;)

;;;; 選択領域の色
;;;; color of selected region
(set-face-background 'region "white")
(set-face-foreground 'region "black")
;;;; コメントの色
;;;; color of comments
(set-face-foreground 'font-lock-comment-face "red")

;; tab ではなく space を使う
;;(setq-default indent-tabs-mode nil)
;; tab 幅を 4 に設定
(setq-default tab-width 4)
;; バッファの最後の行で next-line しても新しい行を作らない
;;(setq next-line-add-newlines nil)
;; narrowing を禁止
;;(put 'narrow-to-region 'disabled nil)

;;;; 現在の行を目立たせる
(global-hl-line-mode)
(custom-set-faces'(hl-line ((t (:background "blue")))))
;;;; 列番号を表示
(column-number-mode t)

;;;; カーソル位置を保存
(require 'saveplace)
(setq-default save-place t)

;;;; テキスト入力中に補完候補を表示
(require 'auto-complete)
(global-auto-complete-mode t)

;;;; 左側に行番号を表示
;;;; display line number at leftside
(require 'linum)
(global-linum-mode t)
;; linum の表示フォーマット
;; 記号なども表示できる
(setq linum-format "%4d ")
;; linum の背景色
(custom-set-faces
 '(linum ((t (:inherit (shadow default) :background "Gray23")))))


;; 括弧の対応表示
(show-paren-mode t)
(setq show-paren-style 'mixed)
;(setq show-paren-style 'expression)     ;;常に括弧内に色を付けたいときはこっち
(set-face-background 'show-paren-match-face "yellow")
(set-face-foreground 'show-paren-match-face "black")
(set-face-background 'show-paren-mismatch-face "red")
(set-face-foreground 'show-paren-mismatch-face "white")



;;******************************************************************
;;*********************     プログラミング用     *********************
;;******************************************************************


;;C言語
(add-hook
 'c-mode-common-hook
 (lambda ()

   ;; 文法チェック(flymake.el)
   ;(flymake-mode t)

   ;; 行末のスペース・タブに色付けして警告
   (setq show-trailing-whitespace t)
   
   ;; BSDスタイルをベースにする
   ;(c-set-style "bsd")
   ;; スペースでインデントをする
   ;(setq indent-tabs-mode nil)
   ;; インデント幅を設定
   (setq c-basic-offset 4)
   ;; RET キーで自動改行+インデント
   ;(define-key c-mode-base-map "\C-m" 'newline-and-indent)
   ;;自動改行(";"入力時)と、連続する空白の一括削除を有効にする
   ;(c-toggle-auto-hungry-style 1)
   ))


;;C-c c で compile コマンドを呼び出す
;;呼び出し後はgcc *** とすればよい.
(define-key mode-specific-map "c" 'compile)


;;自動でスペルミスチェック
;;(setq-default flyspell-mode t)
;;(setq ispell-dictionary "american")


;;テキストの追加属性をペースト時に無視
;(setq yank-excluded-properties t)

;;Shift + カーソルキーで領域選択
;(setq pc-select-selection-keys-only t)
;(pc-selection-mode 1)




;;プログラムの文法チェック
;;flymake (for C/C++)
;(require 'flymake nil t)

;(defun flymake-get-make-cmdline (source base-dir)
;  "redefinition to remove 'check-syntax' target"
;  (list "make"
;        (list "-s" "-C"
;              base-dir
;              (concat "CHK_SOURCES=" source)
;              "SYNTAX_CHECK_MODE=1"
;              )))

;(defun flymake-simple-make-or-generic-init (cmd &optional opts)
;  "force to check syntax of C/C++ without Makefile"
;  (if (file-exists-p "Makefile")
;      (flymake-simple-make-init) ;; flymake built-in
;    (flymake-simple-generic-init cmd opts)))

;(defun flymake-simple-generic-init (cmd &optional opts)
;  "Makefileがないときのコードチェック用関数"
;  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
;                      'flymake-create-temp-inplace))
;         (local-file (file-relative-name
;                      temp-file
;                      (file-name-directory buffer-file-name))))
;    (list cmd (append opts (list local-file)))))
;; syntax checkが異常終了しても無視する
;(defadvice flymake-post-syntax-check
;  (before flymake-force-check-was-interrupted activate)
;  (setq flymake-check-was-interrupted t))

;; C
;(defun flymake-c-init ()
;  (flymake-simple-make-or-generic-init
;   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only" "$CPPFLAGS")))
;; C++
;(defun flymake-cc-init ()
;  (flymake-simple-make-or-generic-init
;   "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only" "$CPPFLAGS")))

;(push '("\\.[cCh]\\'" flymake-c-init) flymake-allowed-file-name-masks)
;(push '("\\.\\(?:cc\|cpp\|CC\|CPP\\)\\'" flymake-cc-init) flymake-allowed-file-name-masks)

;;警告、エラー時の背景色を設定
;(custom-set-faces
; '(flymake-errline ((((class color)) (:background "red"))))
; '(flymake-warnline ((((class color)) (:background "deepskyblue"))))
; )

;(add-hook 'c-mode-common-hook
;          '(lambda ()
;             (flymake-mode t)))

;;次のエラー部分へ移動	C-c n
;;前のエラー部分へ移動	C-c p
;(define-key c++-mode-map "\C-cn" 'flymake-goto-next-error)
;(define-key c++-mode-map "\C-cp" 'flymake-goto-prev-error)





;;************************************************
;;*****************  R言語  ***********************
;;************************************************
;(require 'ess-site)
;(setq ess-ask-for-ess-directory nil)
;(setq ess-pre-run-hook
;'((lambda ()
;(setq default-process-coding-system '(sjis . sjis))
;)))


;(defun ess:format-window-1 ()
;(split-window-horizontally)
;(other-window 1)
;(split-window)
;(other-window 1))
;(add-hook 'ess-pre-run-hook 'ess:format-window-1)

;(setq default-frame-alist
;(append (list '(foreground-color . "azure3")
;'(background-color . "black")
;'(border-color . "black")
;'(mouse-color . "white")
;'(cursor-color . "white")
;)
;default-frame-alist))






;;***********************************************
;;****************  TeX mode  *******************
;;***********************************************
;;;; YaTeX (野鳥)----------------------------------
;; yatex-mode を起動させる設定
;(setq auto-mode-alist 
;      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; 野鳥が置いてある directry の load-path 設定
;; default で load-path が通っている場合は必要ありません
;;(setq load-path
;;      (cons (expand-file-name
;;	     "/Applications/Emacs.app/Contents/Resources/site-lisp/yatex") load-path))

;; 文章作成時の漢字コードの設定
;; 1 = Shift_JIS, 2 = ISO-2022-JP, 3 = EUC-JP, 4 = UTF-8
;; default は 2
;(setq YaTeX-kanji-code 4) ; euc-jp

;LaTeXコマンドの設定
;(setq tex-command "platex")
;YaTeXでのプレビューアコマンドを設定する
;(setq dvi2-command "xdvi")
;AMS-LaTeX を使用する
;;(setq YaTeX-use-AMS-LaTeX t)

;YaTeXでコメントアウト、解除を割り当てる
;;(add-hook 'yatex-mode-hook
;	  '(lambda ()
;	     (local-set-key "\C-c\C-c" 'comment-region)
;	     (local-set-key "\C-c\C-u" 'uncomment-region) ))


; RefTeXをYaTeXで使えるようにする
;(add-hook 'yatex-mode-hook '(lambda () (reftex-mode t)))
; RefTeXで使うbibファイルの位置を指定する
;(setq reftex-default-bibliography '("~/Library/TeX/bib/papers.bib"))

;;RefTeXに関する設定
;(setq reftex-enable-partial-scans t)
;(setq reftex-save-parse-info t)
;(setq reftex-use-multiple-selection-buffers t)
;;RefTeXにおいて数式の引用を\eqrefにする
;(setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))

;---------------------------------(YaTeXここまで)--------




;;; init.el ends here
