(visual-line-mode 1)
(org-indent-mode)
(variable-pitch-mode 1)

(setq org-hide-emphasis-markers t)

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
 
(defun rc/org-font-setup ()
  
  (let* ((variable-tuple
         (cond ((x-list-fonts "Ubuntu Mono")         '(:font "Ubuntu Mono"))
                (nil (warn "Ubuntu Mono font didnt find.")))) 
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (defface org-checkbox-todo-text
    '((t (:inherit org-todo)))
    "Face for the text part of an unchecked org-mode checkbox.")

  (font-lock-add-keywords
    'org-mode
    `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text   prepend))
     'append)

  (defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
    'org-mode
    `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
    'append)

  (custom-theme-set-faces
    'user
    '(variable-pitch ((t (:family "Ubuntu Mono" :height 180 :weight thin))))
    '(fixed-pitch ((t ( :family "Ubuntu Mono" :height 160)))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)

  (add-hook 'org-mode-hook 'visual-line-mode))

(use-package org
  :hook (org-mode . rc/org-mode-setup)
  :config
  (setq org-ellipsis " v")
  (rc/org-font-setup))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-agenda-files (list "~/Documents/Agenda/"))

(setq org-export-backends '(md))

(defun rc/org-increment-move-counter ()
  (interactive)

  (defun default (x d)
    (if x x d))

  (let* ((point (point))
         (move-counter-name "MOVE_COUNTER")
         (move-counter-value (-> (org-entry-get point move-counter-name)
                                 (default "0")
                                 (string-to-number)
                                 (1+))))
    (org-entry-put point move-counter-name
                   (number-to-string move-counter-value)))
  nil)

(defun rc/org-get-heading-name ()
  (nth 4 (org-heading-component)))

(defun rc/org-kill-heading-name-save ()
  (interactive)
  (let ((heading-name (rc/org-get-heading-name)))
    (kill-new heading-name)
    (message "Kill \"%s\"" heading-name)))

(setq org-agenda-custom-commands
      '(("u" "Unscheduled" tags "+personal-SCHEDULED={.+}-DEADLINE={.+}/!+TODO"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("p" "Personal" ((agenda "" ((org-agenda-tag-filter-preset (list "+personal"))))))
        ("w" "Work" ((agenda "" ((org-agenda-tag-filter-preset (list "+work"))))))
        ))

;;; org-cliplink 

(rc/require 'org-cliplink)


(defun rc/cliplink-task ()
  (interactive)
  (org-cliplink-retrieve-title
   (substring-no-properties (current-kill 0))
   '(lambda (url title)
      (insert (if title
                  (concat "* TODO " title
                          "\n  [[" url "][" title "]]")
                (concat "* TODO " url
                        "\n  [[" url "]]"))))))

;;; org-capture

(setq org-capture-templates
      '(("p" "Capture task" entry (file "~/Documents/Agenda/Tasks.org")
         "* TODO %?\n  SCHEDULED: %t\n")
        ("K" "Cliplink capture task" entry (file "~/Documents/Agenda/Tasks.org")
         "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))
(define-key global-map "\C-cc" 'org-capture)



; KEY BINDINGS:
(global-set-key (kbd "C-x p i") 'org-cliplink)

(global-set-key (kbd "C-x p w") 'rc/org-kill-heading-name-save)

(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-c C-x j") #'org-clock-jump-to-current-clock)
