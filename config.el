;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq mac-right-command-modifier 'hyper)
  (setq mac-right-option-modifier 'hyper)
  (global-set-key (kbd "s-<backspace>") (kbd "M-DEL")) ;; Karbiner maps M-Del to s-Del and vice vesa
  )

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Prasoon Shukla"
      user-mail-address "prasoon.d.shukla@gmail.com")


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'semi-light))
(setq doom-leader-alt-key "H-k")
(setq doom-localleader-alt-key "H-l")
(setq doom-themes-treemacs-theme "doom-colors")
(setq doom-themes-treemacs-enable-variable-pitch nil)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(global-subword-mode 1)
(setq windmove-wrap-around t)
(delete-selection-mode 1)
(setq tab-always-indent t)
(mouse-wheel-mode t)
(global-hl-line-mode)
(column-number-mode 1) ;; Show column-number in the mode line
(menu-bar-mode -1)
(show-paren-mode 1)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1))
(setq ring-bell-function 'ignore)
(setq-default x-stretch-cursor t) ;; When on a tab, make the cursor the tab length.

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(smartscan-mode 1)
(setq ag-reuse-buffers 't)
(setq ag-highlight-search t)
(setq scroll-preserve-screen-position t)
(setenv "WORKON_HOME" "/Users/prasoon/.pyenv/versions")


;; Page scoll stuff
(defun zz-scroll-half-page (direction)
  "Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)  ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))  ;; Current line becomes first
    (move-to-window-line opos)))  ;; Restore cursor/point position

(defun zz-scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (zz-scroll-half-page nil))

(defun zz-scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (zz-scroll-half-page t))

(use-package! dired
  :bind (:map dired-mode-map ("." . hydra-dired/body))
  :preface
  ;; dired hydra
  (defhydra hydra-dired (:hint nil :color pink)
    "
  _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
  _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
  _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
  _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
  _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
  _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
  _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
  _z_ compress-file  _A_ find regexp
  _Z_ compress       _Q_ repl regexp

  T - tag prefix
  "
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
)

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _q_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("q" nil))


(use-package! lsp-ui
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c u" . lsp-ui-imenu)
         ("C-c d" . lsp-describe-thing-at-point))
  :config
  ;; (setq lsp-ui-sideline-delay 1
  ;;       lsp-ui-doc-delay 3
  ;;       lsp-ui-doc-enable t
  ;;       lsp-ui-sideline-show-hover t
  ;;       lsp-ui-flycheck-enable t
  ;;       lsp-ui-flycheck-list-position 'right
  ;;       lsp-ui-flycheck-live-reporting t
  ;;       lsp-ui-peek-enable t
  ;;       lsp-ui-peek-list-width 60
  ;;       lsp-ui-peek-peek-height 25
  ;;       lsp-ui-doc-header t
  ;;       lsp-ui-doc-include-signature t
  ;;       lsp-ui-doc-position 'bottom-edge
  ;;       lsp-ui-doc-use-childframe nil
  ;;       lsp-ui-doc-use-webkit nil
  ;;       ;; lsp-ui-doc-border (face-foreground 'default)
  ;;       lsp-ui-sideline-enable t
  ;;       lsp-ui-sideline-ignore-duplicate t
  ;;       lsp-ui-sideline-show-code-actions t
  ;;       lsp-auto-guess-root nil)
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  ;; Reset `lsp-ui-doc-background' after loading theme
  ;; (add-hook 'after-load-theme-hook
  ;;      (lambda ()
  ;;        (setq lsp-ui-doc-border (face-foreground 'default))
  ;;        (set-face-background 'lsp-ui-doc-background
  ;;                             (face-background 'tooltip))))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate) (setq mode-line-format nil)))

(add-hook 'dart-mode-hook
          (lambda ()
            (set-ligatures! 'dart-mode nil)
            (setq hover-hot-reload-on-save t)
            (map! :map dart-mode-map
                  "C-M-x" #'flutter-run-or-hot-reload
                  "C-M-z" #'hover-run-or-hot-reload)))


(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

\(fn arg char)"
  'interactive)

(use-package! gptel
 :config
 (setq! gptel-api-key "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"))

(map! :after treemacs
      "M-0" 'treemacs-select-window)

(global-set-key (kbd "C-x r s") 'hydra-rectangle/body)


(fset 'switch-default-buffer
      [?\s-b return])

(fset 'delete-whitespace-around-point
      "\334 ")
(fset 'kill-buffer-and-close-window
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([8388715 24 48] 0 "%d")) arg)))

;;;;;;;;;;;;;;; Key Bindings ;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-s-SPC") `(lambda () (interactive) (push-mark)))
(global-set-key (kbd "C-S-g") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "M-]") 'complete-tag)
(global-set-key (kbd "C-M-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-K") 'kill-buffer-and-close-window)
(global-set-key (kbd "s-b") '+vertico/switch-workspace-buffer)
(global-set-key (kbd "C-S-<right>") 'shift-right)
(global-set-key (kbd "C-S-<left>") 'shift-left)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-r") 'er/expand-region)
(global-set-key (kbd "M-r") 'er/expand-region)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(define-key smartparens-mode-map (kbd "<M-S-backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "<M-backspace>") nil)
(global-set-key (kbd "C-s-b") 'switch-default-buffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-M-;") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-c c b") 'copy-file-name-to-clipboard)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-,") 'embrace-commander)
(global-set-key (kbd "C-s-s") 'phi-search)
(global-set-key (kbd "C-s-r") 'phi-search-backward)
;; (global-set-key (kbd "C-s") 'swiper) ;; use ivy for showing results
;; (define-key ivy-mode-map (kbd "C-w") 'ivy-yank-word) ;; not working; fix. use M-j for now
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "s-s") '+vertico/project-search)
(global-set-key (kbd "C-q") 'consult-recent-file)
(define-key projectile-mode-map (kbd "C-o") 'projectile-find-file)
