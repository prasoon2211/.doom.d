;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq mac-right-command-modifier 'hyper)
  (global-set-key (kbd "s-<backspace>") (kbd "M-DEL")) ;; Karbiner maps M-Del to s-Del and vice vesa
  )
(setenv "PATH" (concat "/Users/prasoon.shukla/anaconda3/bin:" (getenv "PATH")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append '("/Users/prasoon.shukla/anaconda3/bin") exec-path))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(delete-selection-mode 1)

(setq default-buffer-file-coding-system 'utf-8)
(set-face-attribute 'default nil :height 140)
;; (setq tab-always-indent t)
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)
;; (setq c-basic-offset 4)

(mouse-wheel-mode t)
;; Show line-number in the mode line
(line-number-mode 1)
(global-hl-line-mode)
;; Show column-number in the mode line
(column-number-mode 1)
(setq-default fill-column 79)
(if window-system
 (tool-bar-mode -1))
(menu-bar-mode -1)
(show-paren-mode 1)
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(setq ring-bell-function 'ignore)
;; When on a tab, make the cursor the tab length.
(setq-default x-stretch-cursor t)
(defvar base-prettify-symbols-alist '(("<=" . ?≤)
                                      (">=" . ?≥)
                                      ("<-" . ?←)
                                      ("->" . ?→)
                                      ("<=" . ?⇐)
                                      ("=>" . ?⇒)
                                      ("lambda" . ?λ)
                                      ))
(defun my-lisp-prettify-symbols-hook ()
  "Set pretty symbols for lisp modes."
  (setq prettify-symbols-alist base-prettify-symbols-alist))
(add-hook 'prog-mode-hook 'my-lisp-prettify-symbols-hook)
(doom-themes-org-config)
(doom-modeline-mode)

;; ======== Block Indentation ========== ;;
;; Shift the selected region right if distance is postive, left if
;; negative
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
  (shift-region (- 1)))

(defun shift-right-amount (amount)
  (interactive)
  (shift-region amount))

(defun shift-left-amount (amount)
  (interactive)
  (shift-region (- amount)))

;; FIXME
(setq-hook! 'after-init-hook company-backends '((company-files ; files & directory
                                                 company-keywords ; keywords
                                                 company-capf)    ; completion-at-point-functions
                                                (company-dabbrev-code company-gtags company-etags)
                                                (company-abbrev company-dabbrev)
                                                ))
;; (setq company-tooltip-align-annotations t)

(company-statistics-mode)
(company-quickhelp-mode)

(use-package! helm
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :hook
  ;; Save current position to mark ring when jumping to a different place
  (helm-goto-line-before . helm-save-current-pos-to-mark-ring)
  :init
  ;; (setq helm-command-prefix-key "C-c c h")
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-fuzzy-match t)
  (setq helm-idle-delay 0.0
        helm-input-idle-delay 0.01
        helm-quick-update t
        helm-M-x-requires-pattern nil)
  (setq completion-styles '(flex))

  :bind (("M-x" . helm-M-x)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos)
         ("s-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("C-q" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-t" . helm-for-files)
         ("s-s" . helm-projectile-ag)
         ("s-S" . helm-projectile-rg)
         :map helm-map
         (("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
          ("C-i" . helm-execute-persistent-action)
          ("C-z" . helm-select-action)
          ("<escape>" . hydra-helm/body))
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history))
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (helm-gtags-mode t)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c c h") 'helm-command-prefix)
  ;; (global-unset-key (kbd "C-x c"))
  (setq helm-split-window-inside-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                                        ; Note: Use C-o to change sources.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-buffer-not-found))
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  (run-at-time nil (* 10 60) 'recentf-save-list)
  (helm-autoresize-mode t)
  (setq helm-autoresize-min-height 20)
  (setq helm-completion-in-region-fuzzy-match t)
  ;; (setq helm-mode-fuzzy-match t)
  (setq helm-boring-buffer-regexp-list (list (rx "*")))
  (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level)
  (with-eval-after-load 'helm-gtags
     (define-key helm-gtags-mode-map (kbd "s-.") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "s->") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "C-s-.") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "s-,") 'helm-gtags-pop-stack))
  :preface
  ;; Helm hydra
  (defhydra hydra-helm (:hint nil :color pink)
    "
                                                                        ╭──────┐
 Navigation   Other  Sources     Mark             Do             Help   │ Helm │
╭───────────────────────────────────────────────────────────────────────┴──────╯
      ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
      ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
  _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
      ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
      ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
--------------------------------------------------------------------------------
        "
    ("<tab>" helm-keyboard-quit "back" :exit t)
    ("<escape>" nil "quit")
    ("\\" (insert "\\") "\\" :color blue)
    ("h" helm-beginning-of-buffer)
    ("j" helm-next-line)
    ("k" helm-previous-line)
    ("l" helm-end-of-buffer)
    ("g" helm-beginning-of-buffer)
    ("G" helm-end-of-buffer)
    ("n" helm-next-source)
    ("p" helm-previous-source)
    ("K" helm-scroll-other-window-down)
    ("J" helm-scroll-other-window)
    ("c" helm-recenter-top-bottom-other-window)
    ("m" helm-toggle-visible-mark)
    ("t" helm-toggle-all-marks)
    ("u" helm-unmark-all)
    ("H" helm-help)
    ("s" helm-buffer-help)
    ("v" helm-execute-persistent-action)
    ("d" helm-persistent-delete-marked)
    ("y" helm-yank-selection)
    ("w" helm-toggle-resplit-and-swap-windows)
    ("f" helm-follow-mode)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Prasoon Shukla"
      user-mail-address "prasoon.d.shukla@gmail.com")

(setq windmove-wrap-around t)


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Org
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; https://lepisma.xyz/2017/10/28/ricing-org-mode/
(use-package! org
  :init
  (setq org-directory "~/Documents/org")
  (require 'f)
  (require 'ox-md)
  :bind (
         :map org-mode-map
         (("s-e" . org-emphasize)
          ;; ("M-'" . ispell-word)
          ;; ("C-M-'" . ispell-complete-word)
          ))
  :hook
  (org-mode . writeroom-mode)
  (org-mode . endless/org-ispell)
  (org-mode . init-org-prettify-syntax)
  (org-mode . (lambda ()
                (visual-line-mode)
                ;; (variable-pitch-mode 1)
                ;; (org-bullets-mode 1)
                ;; (org-indent-mode)
                (emojify-mode)
                (linum-mode 0)
                (setq org-startup-indented t
                      org-indent-indentation-per-level 1
                      ;; org-hide-leading-stars t
                      org-hide-emphasis-markers t
                      org-ellipsis " ⤵ "
                      show-trailing-whitespace nil
                      org-startup-with-inline-images t
                      )
                (setq org-edit-src-content-indentation 0)
                (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                (set-face-background 'linum (face-attribute 'default :background))
                (set (make-local-variable 'company-backends)
                     ;; (cons 'company-capf company-backends)
                     '(company-capf)
                     )
                                        ; (setq org-latex-create-formula-image-program 'imagemagick); install imagemagick and pdflatex
                (setq org-latex-create-formula-image-program 'dvisvgm) ; See Org-Mode Zettel under Latex
                (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
                (setq org-special-ctrl-a/e t)
                (setq org-capture-templates
                      `(("t" "Todo" entry (file+headline ,(f-join org-directory "tasks.org") "Captured Tasks")
                         "* TODO %?\n%U" :empty-lines 1)
                        ("T" "Todo from clipboard" entry (file+headline ,(f-join org-directory "tasks.org") "Captured Tasks")
                         "* TODO %?\n%U  %c" :empty-lines 1)
                        ("j" "Journal" entry (file+datetree ,(f-join org-directory "journal.org"))
                         "* %U %?" :empty-lines 1)
                        ("n" "Note" entry (file+headline ,(f-join org-directory "inbox.org") "Notes")
                         "* NOTE %U %?" :empty-lines 1)
                        ("N" "Note with Clipboard" entry (file+headline ,(f-join org-directory "inbox.org") "Notes")
                         "* NOTE %U %?\n %c" :empty-lines 1)
                        ("w" "Work Item" plain (file ,(f-join org-directory "work" "notes.org"))
                         "- [ ] %?")))
                (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
                                        ;(setq org-image-actual-width (/ (display-pixel-width) 3))
                (setq org-image-actual-width nil)
                (setq org-bullets-bullet-list '("▹"))
                ;; (setq org-bullets-bullet-list '(" "))
                (setq header-line-format " ")
                (setq line-spacing 0.1)
                (setq org-list-demote-modify-bullet
                      (quote (("+" . "-")
                              ("-" . "+")
                              ("*" . "-")
                              ("1." . "-")
                              ("1)" . "-")
                              ("A)" . "-")
                              ("B)" . "-")
                              ("a)" . "-")
                              ("b)" . "-")
                              ("A." . "-")
                              ("B." . "-")
                              ("a." . "-")
                              ("b." . "-"))))
                (font-lock-add-keywords 'org-mode
                                        '(("^ *\\([-]\\) "
                                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
                (font-lock-add-keywords 'org-mode
                                        '(("^ *\\([+]\\) "
                                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))
                (setq-local company-idle-delay 0
                            company-minimum-prefix-length 0)))
  :preface
  (defun init-org-prettify-syntax ()
    "Prettify syntax with symbols."
    (dolist (symbol '(("#+title:" . ?⋮)
                      ("#+TITLE:" . ?⋮)
                      ("#+subtitle:" . ?⋮)
                      ("#+SUBTITLE:" . ?⋮)
                      ("#+begin_src" . ?λ)
                      ("#+BEGIN_SRC" . ?λ)
                      ("#+end_src" . ?≋)
                      ("#+END_SRC" . ?≋)
                      ("#+begin_quote" . ?“)
                      ("#+BEGIN_QUOTE" . ?“)
                      ("#+end_quote" . ?”)
                      ("#+END_QUOTE" . ?”)))
      (cl-pushnew symbol prettify-symbols-alist :test #'equal))
    (dolist (n (number-sequence 1 8 1))
      (let ((symbol (cons (concat (make-string n ?*) " ") (-interpose '(Br . Bl) (make-list (+ 1 n) ?\s)))))
        (cl-pushnew symbol prettify-symbols-alist :test #'equal)))
    (prettify-symbols-mode 1))

  (defun endless/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))))

(use-package ox-clip
  :ensure t
  :bind (:map org-mode-map (("H-k" . ox-clip-formatted-copy))))

(use-package org-variable-pitch
  :ensure t
  :diminish org-variable-pitch-minor-mode
  :hook (org-mode . org-variable-pitch-minor-mode)
  :custom
  (org-variable-pitch-fixed-font "Menlo")
  (org-variable-pitch-fontify-headline-prefix t))


(use-package org-preview-html
  :ensure t)


(use-package org-fragtog
  :ensure t
  :hook
  (org-mode . org-fragtog-mode))


;; Org roam
(use-package org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :config
  (setq org-roam-completion-system 'helm)
  (require 'org-roam-protocol)
  (setq org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox-bin")
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
          :file-name "%<%Y%m%d%H%M%S>-${slug}"
          :head "#+title: ${title}\n
* Metadata
- Tags:
- Resources:

* /Notes/\n"
          :unnarrowed t)))
  :custom
  (org-roam-directory (f-join org-directory "zettelkasten"))
  :bind (:map org-roam-mode-map
              (("s-o o" . org-roam)
               ("s-o f" . org-roam-find-file)
               ("s-o r" . org-roam-find-ref)
               ("s-o d" . org-roam-find-directory)
               ("s-o b" . org-roam-switch-to-buffer)
               ("s-o g" . org-roam-graph-show)
               ("s-o l" . org-cliplink)
               ("C-c a" . org-agenda)
               ("C-c c" . org-capture))
              :map org-mode-map
              (("s-o I" . org-roam-insert)
               ("s-o i" . org-roam-insert-immediate)
               ("C-c l" . org-store-link))))


(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; org-download
(use-package org-download
  :ensure t
  :hook
  (dired-mode . org-download-enable)
  :init
  (setq-default org-download-image-dir (f-join org-directory "zettelkasten"))
  (setq-default org-download-method 'drestivo/org-download-method)

  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(defun drestivo/org-download-method (link)
  "This is an helper function for org-download.
It creates an \"./image\" folder within the same directory of the org file.
Images are separated inside that image folder by additional folders one per
org file.
More info can be found here: https://github.com/abo-abo/org-download/issues/40.
See the commit message for an example:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (concat
              (file-name-directory (buffer-file-name))
              (format "%s/%s"
                      "images"
                      (file-name-base (buffer-file-name))))))
    (progn
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Check if directory exists otherwise creates it
      (unless (file-exists-p dir)
        (make-directory dir t))
      (message (format "Image: %s saved!" (expand-file-name filename-with-timestamp dir)))
      (expand-file-name filename-with-timestamp dir))))


(defun my/org-download-attach-file (link)
  "Save file at address LINK to attachments."
  (interactive "sUrl or Filepath: ")
  (require 'url)
  (require 'f)
  (let* ((copy-to-dir (f-join (f-expand "resources") (f-base (buffer-file-name))))
         (copy-to-path (f-join copy-to-dir (f-filename link))))
    (if (not (f-exists? copy-to-dir))
        (f-mkdir "resources" (f-base (buffer-file-name))))

    (cond ((f-exists? (f-expand link))
           (let ((copy-from-path (f-expand link)))
             (f-copy copy-from-path copy-to-path)))
          ((url-host (url-generic-parse-url link))
           (url-copy-file link copy-to-path 1)))

    (if (f-file? copy-to-path)
        (progn
          (message "File created or already exisited")
          (save-excursion
            (insert
             (format
              "[[file:%s][%s]]"
              (f-relative copy-to-path default-directory)
              (f-filename link)))))
      (message "File could not be created")))
  )

(defun my/org-download-file-from-ring ()
  "Gets URL at point and it it begins with http/https or is a valid filepath
then the file is moved to resources/filename/<file>"
  (my/org-download-attach-file
   (replace-regexp-in-string "\n+$" "" (current-kill 0))))

(defun ll/org/link-file-path-at-point ()
  "Get the path of the file referred to by the link at point."
  (let* ((org-element (org-element-context))
         (is-subscript-p (equal (org-element-type org-element) 'subscript))
         (is-link-p (equal (org-element-type org-element) 'link))
         (is-file-p (equal (org-element-property :type org-element) "file")))
    (when is-subscript-p
      (user-error "Org thinks you're in a subscript. Move the point and try again."))
    (unless (and is-link-p is-file-p)
      (user-error "Not on file link"))
    (expand-file-name (org-element-property :path org-element))))

(defun ll/org/resize-image-at-point (&optional arg)
  (interactive)
  (let ((img (ll/org/link-file-path-at-point))
        (percent (read-number "Resize to what percentage of current size? ")))
    (start-process "mogrify" nil "magick"
                   "mogrify"
                   "-resize"
                   (format "%s%%" percent)
                   img)
    (org-remove-inline-images)
    (org-display-inline-images)))

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


(popwin-mode 1)
(defun popwin-with-mode-lines (modelines)
  (mapc (lambda (modeline)
          (push `(,modeline :height 15 :noselect t)
                popwin:special-display-config))
        modelines))
(popwin-with-mode-lines
 '("*org headlines*" "*Warnings*" "*Process List*" "*Messages*"
   "*Backtrace*" "*Compile-Log*" "*Remember*" "*undo-tree*" "*All*"
   "*cider-error*" "*cider-doc*"
   "*Helm Swoop*"))

(use-package! ibuffer
  :preface
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
  ^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
  _m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
  _u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
  _*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
  _t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
  "
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("t" ibuffer-toggle-marks)

    ("D" ibuffer-do-delete)
    ("s" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("S" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("H" describe-mode :color blue)

    ("h" ibuffer-backward-filter-group)
    ("k" ibuffer-backward-line)
    ("l" ibuffer-forward-filter-group)
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)

    ("TAB" ibuffer-toggle-filter-group)

    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" quit-window :color blue)
    ("." nil :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                  :after-exit
                                  (if (eq major-mode 'ibuffer-mode)
                                      (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  :hook
  (ibuffer . hydra-ibuffer-main/body)
  :bind (:map ibuffer-mode-map ("." . hydra-ibuffer-main/body))
)
;; Keeps focus on *Occur* window, even when when target is visited via RETURN key.
;; See hydra-occur-dwim for more options.
(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))
;; Occur Hydra
(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(add-hook! 'occur-hook (lambda () (other-window 1)))

(defun reattach-occur ()
  (if (get-buffer "*Occur*")
    (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body)))

;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
;; focus on the *Occur* window and hides upon request in case needed later.
(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "Start occur-dwim" :color red)
  ("j" occur-next "Next" :color red)
  ("k" occur-prev "Prev":color red)
  ("h" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red))
(global-set-key (kbd "C-x o") 'hydra-occur-dwim/body)

;; Diff hydra
(defhydra hydra-ediff (:color blue :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))
(global-set-key (kbd "C-c C-x d") 'hydra-ediff/body)


(use-package! ace-window
  :config
  (global-set-key (kbd "C-c w") 'hydra-window/body)
  :preface
  (defhydra hydra-window ()
    "
Movement^^        ^Split^       ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_w_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_e_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_r_ X↑
_l_ →        	_Z_ reset      	_s_wap		_t_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_q_ cancel	_o_nly this   	_d_elete
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("w" hydra-move-splitter-left)
    ("e" hydra-move-splitter-down)
    ("r" hydra-move-splitter-up)
    ("t" hydra-move-splitter-right)
    ("b" helm-mini)
    ("f" helm-find-files)
    ("F" follow-mode)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("o" delete-other-windows)
    ("i" ace-maximize-window)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("Z" winner-redo)
    ("q" nil))
)

;; helm hydras
(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   helm-projectile-find-dir)
  ("s-f" helm-projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("p"   helm-projectile-switch-project "switch project")
  ("s"   helm-projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

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

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(add-hook! 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd ".") 'hydra-org-agenda/body)))

;; rectangle commands hydra
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
(global-set-key (kbd "C-x r s") 'hydra-rectangle/body)

(semantic-mode t)

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))

(setq flymd-browser-open-function 'my-flymd-browser-function)

(put 'dired-find-alternate-file 'disabled nil)


(use-package! ein
  :preface
  (defhydra ein/hydra (:hint nil :color red)
    "
 Operations on Cells^^^^^^            Other
 ----------------------------^^^^^^   ----------------------------------^^^^
 [_k_/_j_]^^     select prev/next     [_t_]^^         toggle output
 [_K_/_J_]^^     move up/down         [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^^ merge above/below    [_C-o_]^^       open console
 [_O_/_o_]^^     insert above/below   [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_]   copy/paste           [_x_]^^         close notebook
 [_u_]^^^^       change type          [_q_]^^         quit
 [_RET_]^^^^     execute
"
    ("q" nil :exit t)
    ("h" ein:notebook-worksheet-open-prev-or-last)
    ("j" ein:worksheet-goto-next-input)
    ("k" ein:worksheet-goto-prev-input)
    ("l" ein:notebook-worksheet-open-next-or-first)
    ("H" ein:notebook-worksheet-move-prev)
    ("J" ein:worksheet-move-cell-down)
    ("K" ein:worksheet-move-cell-up)
    ("L" ein:notebook-worksheet-move-next)
    ("t" ein:worksheet-toggle-output)
    ("d" ein:worksheet-kill-cell)
    ("R" ein:worksheet-rename-sheet)
    ("y" ein:worksheet-copy-cell)
    ("p" ein:worksheet-yank-cell)
    ("o" ein:worksheet-insert-cell-below)
    ("O" ein:worksheet-insert-cell-above)
    ("u" ein:worksheet-change-cell-type)
    ("RET" ein:worksheet-execute-cell-and-goto-next)
    ;; Output
    ("C-l" ein:worksheet-clear-output)
    ("C-S-l" ein:worksheet-clear-all-output)
    ;;Console
    ("C-o" ein:console-open :exit t)
    ;; Merge and split cells
    ("C-k" ein:worksheet-merge-cell)
    ("C-j" spacemacs/ein:worksheet-merge-cell-next)
    ("s" ein:worksheet-split-cell-at-point)
    ;; Notebook
    ("C-s" ein:notebook-save-notebook-command)
    ("C-r" ein:notebook-rename-command)
    ("x" ein:notebook-close :exit t)))


(add-hook! 'python-mode-hook
  (lambda ()
    (setenv "WORKON_HOME" "/Users/prasoon.shukla/anaconda3/envs")
    (pyvenv-mode 1)
    (pyvenv-workon "kids")
    (defun conda-venv (venv-name)
      "Activate a conda venv."
      (interactive "sVirtual env name: ")
      (pyvenv-activate (expand-file-name (concat "~/anaconda3/envs/" venv-name))))

    (elpy-enable)
    (setq elpy-rpc-virtualenv-path 'default)
    (setq python-shell-interpreter "jupyter-console"
          python-shell-interpreter-args "--simple-prompt"
          python-shell-prompt-detect-failure-warning nil
          elpy-shell-echo-output nil)
    (defun annotate-pdb ()
      (interactive)
      (highlight-lines-matching-regexp "import ipdb")
      (highlight-lines-matching-regexp "ipdb.set_trace()"))

    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter")
    (define-key python-mode-map (kbd "C-c d") 'insert-ipdb-macro)
    (setq blacken-line-length 120)
    (add-hook 'python-mode-hook 'annotate-pdb)
    (add-hook 'python-mode-hook 'blacken-mode)

    ;; (setq flycheck-python-pylint-executable "/Users/prasoon.shukla/anaconda3/bin/pylint")
    ;; (setq flycheck-pylintrc "/Users/prasoon.shukla/.pylintrc")
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook! 'elpy-mode-hook 'flycheck-mode))))

(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here
(defun create-tags (dir-name)
    "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "find %s -type f -iname \"*\" | etags -" path-to-ctags (directory-file-name dir-name)))
)

(setenv "GTAGSLABEL" "pygments")

(use-package! sublimity
  :hook
  (after-init . (sublimity-global-mode)))

(global-subword-mode 1)
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

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

(smartscan-mode 1)
(setq ag-reuse-buffers 't)
(setq ag-highlight-search t)
(setq helm-projectile-fuzzy-match t)

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

(setq scroll-preserve-screen-position t)

(use-package! auto-compile
  :hook
  (emacs-lisp-mode . auto-compile-on-load-mode)
  (emacs-lisp-mode . auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-use-mode-line nil))


(setq magit-last-seen-setup-instructions "1.4.0")

(fset 'switch-default-buffer
   [?\C-x ?b return])

(fset 'delete-whitespace-around-point
   "\334 ")

(fset 'insert-ipdb-macro
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote
          ([5 10 tab 105 109 112 company-dummy-event 111 114
              116 32 105 112 100 98 59 105 112 100 98 46 115 101
              116 95 company-dummy-event 116 114 97 99 101 40 41] 0 "%d")) arg)))

(fset 'kill-buffer-and-close-window
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([8388715 24 48] 0 "%d")) arg)))


;;;;;;;;;;;;;;; Key Bindings ;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-s-SPC") '(lambda () (interactive) (push-mark)))
(global-set-key (kbd "C-S-g") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "M-]") 'complete-tag)
(global-set-key (kbd "C-M-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-K") 'kill-buffer-and-close-window)
;; (global-set-key (kbd "s-.") 'helm-etags-select)
(define-key projectile-mode-map (kbd "C-o") 'helm-projectile-find-file)
(define-key projectile-mode-map (kbd "s-p") 'hydra-projectile/body)
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
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-M-;") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-p") 'ace-window)
;; (global-set-key (kbd "s-c") 'connect-to-ipynb)
(global-set-key (kbd "s-s") 'helm-projectile-ag)
(global-set-key (kbd "s-S") 'helm-projectile-rg)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-c b") 'copy-file-name-to-clipboard)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-v") 'zz-scroll-half-page-up)
(global-set-key (kbd "C-v") 'zz-scroll-half-page-down)
(global-set-key (kbd "C-,") 'embrace-commander)
(global-set-key (kbd "M-<right>") 'cycbuf-switch-to-next-buffer)
(global-set-key (kbd "M-<left>") 'cycbuf-switch-to-previous-buffer)
(global-set-key (kbd "M-S-<right>") 'cycbuf-switch-to-next-buffer-no-timeout)
(global-set-key (kbd "M-S-<left>") 'cycbuf-switch-to-previous-buffer-no-timeout)
(global-set-key (kbd "C-s-s") 'phi-search)
(global-set-key (kbd "C-s-r") 'phi-search-backward)
(global-set-key (kbd "C-s") 'swiper) ;; use ivy for showing results
;; (define-key ivy-mode-map (kbd "C-w") 'ivy-yank-word) ;; not working; fix. use M-j for now
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-s-y") 'my/org-download-file-from-ring)
