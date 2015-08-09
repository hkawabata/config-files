;;; init.el --- initial setting for emacs
;;; Commentary:


;;; Code:

;;; ロードパスの設定
;(setq load-path (append (list
;                         (expand-file-name "~/.emacs.d/elisp")
;						 (expand-file-name "~/.emacs.d/elisp/ess-12.09/lisp")
;						 )
;                        load-path))

;;;********** パッケージ管理に melpa, marmalade を追加 **********
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;********** パッケージの自動インストール **********
(require 'cl)
(defvar my-packages
  '(flycheck
    auto-complete
    git-gutter
    col-highlight
    js2-mode php-mode
    ;; gnuplot-mode markdown-mode scala-mode
    ;; このリストにあるパッケージがインストールされる
    )
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


;;;********** スクロールの設定 **********
(setq scroll-conservatively 35
      scroll-margin 3  ; スクロールするときの上下のマージン
      scroll-step 1)   ; 1行ずつスクロール


;;********** オートインデントでスペースを使う **********
 (setq-default indent-tabs-mode nil)


;;;********** git で管理しているファイルについて、変更・追加・削除箇所を表示 **********
;;; 【要】 git-gutter
(global-git-gutter-mode t)
(git-gutter:linum-setup)


;;; テキスト入力中に補完候補を表示
;;; 【要】auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)


;;;********** 自動コードチェック **********
;;; 【要】flycheck
;;;;     対応言語（確認済み）: C, C++, elisp, HTML, Perl, Python, Ruby, JS
;;;;     対応言語（未確認）: CSS, PHP, ShelScripts, Tex, XML
;;;;     あれ、対応してなくない？: CSS, PHP, Tex
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;********** メニューバーを非表示 **********
(menu-bar-mode -1)


;;;********** 色などの初期設定 **********
(setq default-frame-alist
      (append (list
               '(foreground-color . "snow") ;; 文字色
               '(background-color . "black") ;; 背景色
               )
              default-frame-alist))


;;;********** 選択領域の色 **********
(set-face-background 'region "white")
(set-face-foreground 'region "black")

;;;********** コメントの色 **********
(set-face-foreground 'font-lock-comment-face "red")

;;********** tab 幅を 4 に設定 **********
(setq-default tab-width 4)
;; narrowing を禁止
;(put 'narrow-to-region 'disabled nil)


;;;********** 現在の列をハイライト **********
(require 'col-highlight)
;; 常時ハイライト
(column-highlight-mode 1)
;; 1秒間何もしないとハイライト
;; (toggle-highlight-column-when-idle 1)
;; (col-highlight-set-interval 1)
;; ハイライトの色を指定
(custom-set-faces
 '(col-highlight((t (:background "blue")))))

;;;********** 現在の行をハイライト **********
(global-hl-line-mode)
(custom-set-faces'(hl-line ((t (:background "blue")))))


;;;********** 列番号を表示 **********
(column-number-mode t)

;;;********** カーソル位置を保存 **********
(require 'saveplace)
(setq-default save-place t)


;;;********** 左側に行番号を表示 **********
(require 'linum)
(global-linum-mode t)
;; linum の表示フォーマット
;; 記号なども表示できる
(setq linum-format "%4d ")
;; linum の背景色
(custom-set-faces
 '(linum ((t (:inherit (shadow default) :background "Gray23")))))


;;********** 括弧の対応表示 **********
(show-paren-mode t)
(setq show-paren-style 'mixed)
(set-face-background 'show-paren-match-face "yellow")
(set-face-foreground 'show-paren-match-face "black")
(set-face-background 'show-paren-mismatch-face "red")
(set-face-foreground 'show-paren-mismatch-face "white")
;;常に括弧内に色を付けたいとき
;; (setq show-paren-style 'expression)


;;******************************************************************
;;*********************     プログラミング用     *********************
;;******************************************************************


;;C言語
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; 行末のスペース・タブに色付けして警告
   (setq show-trailing-whitespace t)   
   ;; BSDスタイルをベースにする
   ;;(c-set-style "bsd")
   ;; インデント幅を設定
   (setq c-basic-offset 4)
   ))

;;C-c c で compile コマンドを呼び出す
;;呼び出し後はgcc *** とすればよい.
(define-key mode-specific-map "c" 'compile)


;;; init.el ends here
