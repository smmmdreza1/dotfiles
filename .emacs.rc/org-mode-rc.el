(rc/require 'org)
(require 'org)


;;; defined functions
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
  (nth 4 (org-heading-components)))

(defun rc/org-kill-heading-name-save ()
  (interactive)
  (let ((heading-name (rc/org-get-heading-name)))
    (kill-new heading-name)
    (message "Kill \"%s\"" heading-name)))


;;; ===============================================================================
;;; org appearance customization
(setq org-hide-emphasis-markers t)

(setq org-startup-folded t)

(setq org-startup-indented t)
(setq org-startup-with-inline-images t)

(org-indent-mode)
(variable-pitch-mode 1)
(visual-line-mode 1)

(font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple
          (cond ((x-list-fonts "Ubuntu Mono")         '(:font "Ubuntu Mono"))
                (nil (warn "Ubuntu mono font cannot found!"))))
         (headline           `(:weight bold)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Ubuntu Mono" :height 180 :weight thin))))
 '(fixed-pitch ((t ( :family "Ubuntu Mono" :height 160)))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Ubuntu Mono" :height 160))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:weight bold :font "Ubuntu Mono" :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :slant normal :weight bold :height 1.5 :width normal :foundry "outline-1" :family "Ubuntu Mono"))))
 '(org-level-2 ((t (:inherit outline-3 :extend nil :slant normal :weight bold :height 1.25 :width normal :foundry "outline-3" :family "Ubuntu Mono"))))
 '(org-level-3 ((t (:inherit outline-1 :extend nil :slant normal :weight bold :height 1.1 :width normal :foundry "outline-1" :family "Ubuntu Mono"))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :slant normal :weight bold :height 1 :width normal :foundry "outline-4" :family "Ubuntu Mono"))))
 '(org-level-5 ((t (:inherit outline-8 :extend nil :slant normal :weight bold :height 98 :width normal :foundry "outline-8" :family "Ubuntu Mono"))))
 '(org-level-6 ((t (:inherit outline-7 :extend nil :slant normal :weight bold :height 98 :width normal :foundry "outline-7" :family "Ubuntu Mono"))))
 '(org-level-7 ((t (:inherit outline-5 :extend nil :slant normal :weight bold :height 98 :width normal :foundry "outline-5" :family "Ubuntu Mono"))))
 '(org-level-8 ((t (:weight bold :font "Ubuntu Mono"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Ubuntu Mono" :height 180 :weight thin)))))

(defun rc/org-mode-visual-fill ()
  (setq visual-fill-column-width 170
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . rc/org-mode-visual-fill))

;;; ===============================================================================
;;; org-agenda setup
(custom-set-variables
     '(org-agenda-files '("~/org/"
                          "~/Documents/Agenda/")))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "PROJ(p)" "BACKLOG(b)" "ACTIVE(a)" "WAIT(w@/!)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("personal" . ?P)
       ("note" . ?n)
       ("goal" . ?G)
       ("idea" . ?i)))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda ""
                  ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "goal/TODO"
                     ((org-agenda-overriding-header "This Mounth Goals:")))
          (tags-todo "agenda/ACTIVE"
                     ((org-agenda-overriding-header "Active Projects"))))
         nil)

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("p" "Personal" ((agenda "" ((org-agenda-tag-filter-preset (list "+personal"))))))

        ("w" "Work" ((agenda "" ((org-agenda-tag-filter-preset (list "+work"))))))

        ("P" "Projects"
         ((todo "PROJ"
                ((org-agenda-overriding-header "Your Projects")))))

        ("u" "Unscheduled" tags "+personal-SCHEDULED={.+}-DEADLINE={.+}/!+TODO"
         ((org-agenda-sorting-strategy '(priority-down))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ("m" "Workflow Status"
         ( (todo "BACKLOG"
                 ((org-agenda-overriding-header "Project Backlog")
                  (org-agenda-todo-list-sublevels nil)
                  (org-agenda-files org-agenda-files)))
           (todo "ACTIVE"
                 ((org-agenda-overriding-header "Active Projects")
                  (org-agenda-files org-agenda-files)))
           (todo "COMPLETED"
                 ((org-agenda-overriding-header "Completed Projects")
                  (org-agenda-files org-agenda-files)))
           (todo "CANC"
                 ((org-agenda-overriding-header "Cancelled Projects")
                  (org-agenda-files org-agenda-files)))))))


;;; ===============================================================================
;;; org-habbit
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)


;;; ===============================================================================
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


;;; ===============================================================================
;;; org-capture 
(setq org-capture-templates
      '(("p" "Capture task" entry
         (file+olp "~/Documents/Agenda/Tasks.org" "Inbox")
         "* TODO %?\n SCHEDULED: %t\n" :empty-lines 1)
        
        ("P" "Capture new Project" entry
         (file+olp "~/Documents/Agenda/Progects.org" "Inbox")
         "* PROJ %?\n SCHEDULED: %t\n" :empty-lines 1)

        ("w" "New word" entry
         (file "~/Documents/Org/Words.org")
         "*** - %?" :empty-lines 0)

        ("K" "Cliplink capture task" entry
         (file "~/Documents/Agenda/Tasks.org")
         "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)

        ("j" "Journal" entry
         (file+olp+datetree "~/Documents/Agenda/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)))

(define-key global-map "\C-cc" 'org-capture)

(define-key global-map (kbd "C-c j")
            (lambda () (interactive) (org-capture nil "j")))

(setq org-export-backends '(md))

(setq org-bookmark-names-plist
      '(:last-capture nil))
       

;;; ===============================================================================
;;; org-feed
       (setq org-feed-alist
             '(("Hacker News"
                "https://news.ycombinator.com/rss" ;
                "~/org/feeds.org" "Hacker News Entries")
               ("SZ"
                "http://rss.sueddeutsche.de/app/service/rss/alles/index.rss?output=rss" ;
                "~/org/feeds.org" "SZ Entries")))


;;; ===============================================================================
;;; org-drill
(use-package org-drill
  :ensure t
  :config
  (setq org-drill-add-random-noise-to-intervals-p nil))


;;; ===============================================================================
                                        ; KEY BINDINGS:
(global-set-key (kbd "C-x p i") 'org-cliplink)

(global-set-key (kbd "C-x p w") 'rc/org-kill-heading-name-save)

(global-set-key (kbd "C-x a") 'org-agenda)

(global-set-key (kbd "C-c C-x j") #'org-clock-jump-to-current-clock)
