;;;
;;; GTD Org-mode Setup <https://www.labri.fr/perso/nrougier/GTD/index.html>
;;;

(require 'org)

;;; Configure Org-mode
(setq org-directory "~/Documents/Org")

;;; Org capture templates
(setq
  org-capture-templates `(
    ("i" "Inbox" entry  (file "inbox.org")
     ,(concat "* TODO %?\n"
              "/Entered on/ %U"))))
(defun gtd-org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

;;; Org agenda
(setq
  org-agenda-files (list "inbox.org" "projects.org")
  org-agenda-hide-tags-regexp "."
  org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                             (todo   . " ")
                             (tags   . " %i %-12:c")
                             (search . " %i %-12:c"))
  org-agenda-custom-commands '(
    ("g" "Get Things Done (GTD)"
     ((agenda "" ((org-deadline-warning-days 0)))
      (todo "NEXT"
            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
             (org-agenda-prefix-format "  %i %-12:c [%e] ")
             (org-agenda-overriding-header "\nTasks\n")))
      (tags-todo "inbox"
                 ((org-agenda-prefix-format "  %?-12t% s")
                  (org-agenda-overriding-header "\nInbox\n")))
      (tags "CLOSED>=\"<today>\""
           ((org-agenda-overriding-header "\nCompleted today\n")))))))
(add-hook 'org-agenda-mode-hook 'delete-other-windows)

;;; Org refiling
(setq
  org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
  org-refile-use-outline-path 'file
  org-outline-path-complete-in-steps nil)

;;; Org todo
(setq
  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)"))
  org-log-done 'time)
(defun gtd-log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'gtd-log-todo-next-creation-date)

;;; Key bindings
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c i") 'gtd-org-capture-inbox)
(define-key global-map (kbd "C-c a") 'org-agenda)
