;;; review-counts-test.el --- Review count snapshots -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Compare count snapshots with real review views using synthetic Org data.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-review-counts)
(require 'org-gtd-command-center)
(e-unit-initialize)

(defvar native-comp-enable-subr-trampolines)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (let ((org-gtd-review-show-counts t)
          (org-id-locations-file "/mock:/gtd/id-locations")
          (org-id-extra-files nil)
          (org-id-search-archives nil)
          (native-comp-enable-subr-trampolines nil)
          (org-agenda-sticky nil))
      (funcall proceed context))))

(defun ogt-count-test--source (text)
  "Put TEXT in an unsaved agenda source buffer."
  (with-current-buffer (find-file-noselect "/mock:/gtd/org-gtd-tasks.org")
    (erase-buffer)
    (insert text)
    (org-mode)
    (current-buffer)))

(defun ogt-count-test--count (spec)
  "Return the snapshot count for SPEC."
  (cdr (assoc spec (org-gtd-review-counts--snapshot (list spec)))))

(defun ogt-count-test--view-count (spec)
  "Count actual agenda entry lines in the view for SPEC."
  (org-gtd-view-show spec)
  (let ((count 0))
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'org-hd-marker)
        (cl-incf count))
      (forward-line 1))
    count))

(deftest review-counts/native-todo-matcher ()
  "Delegated counts require the configured WAIT state, not just the skip filter."
  (let ((org-todo-keywords '((sequence "OPEN" "READY" "HOLD" "|" "FINISHED")))
        (org-gtd-keyword-mapping
         '((todo . "OPEN") (next . "READY") (wait . "HOLD")
           (done . "FINISHED") (canceled . "FINISHED"))))
    (ogt-count-test--source
     "* HOLD Correct state\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* OPEN Wrong state\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* FINISHED Finished\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n")
    (let ((spec (car org-gtd-reflect-missed-engagements-view-specs)))
      (assert-equal 1 (ogt-count-test--count spec))
      (assert-equal 1 (ogt-count-test--view-count spec)))))

(deftest review-counts/all-review-specs-match-views ()
  "Every counted review specification agrees with its actual agenda view."
  (ogt-count-test--source
   "* TODO Unready\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* NEXT Ready\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* DONE Finished action\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* TODO Missing date\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n* DONE Done missing date\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n* TODO Old calendar\n:PROPERTIES:\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* WAIT Old delegation\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:DELEGATED_TO: Example\n:END:\n* WAIT Future delegation\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2099-01-01 Thu>\n:DELEGATED_TO: Example\n:END:\n* TODO Invalid habit\n:PROPERTIES:\n:ORG_GTD: Habit\n:END:\n* TODO Invalid tickler\n:PROPERTIES:\n:ORG_GTD: Tickler\n:END:\n* Maybe\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n* TODO Archived :ARCHIVE:\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n** TODO Archived child\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* COMMENT Hidden\n** TODO Comment child\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n")
  (dolist (spec (append org-gtd-reflect-stuck-items-view-specs
                       org-gtd-reflect-missed-engagements-view-specs
                       (list org-gtd-reflect-upcoming-delegated-view-spec
                             org-gtd-reflect-someday-maybe-view-spec
                             org-gtd-reflect-completed-projects-view-spec)))
    (let ((count (ogt-count-test--count spec)))
      (assert-equal (ogt-count-test--view-count spec) count))))

(deftest review-counts/projects-use-graph-not-child-count ()
  "One project with two blocked graph tasks counts once, even across files."
  (let ((source (ogt-count-test--source
                 "* TODO Project\nDEADLINE: <2000-01-01 Sat> SCHEDULED: <2000-01-01 Sat>\n:PROPERTIES:\n:ID: count-project\n:ORG_GTD: Projects\n:ORG_GTD_FIRST_TASKS: count-task-a\n:END:\n")))
    (with-current-buffer (find-file-noselect "/mock:/gtd/inbox.org")
      (insert "* TODO Task A\n:PROPERTIES:\n:ID: count-task-a\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: count-project\n:ORG_GTD_BLOCKS: count-task-b\n:END:\n* TODO Task B\n:PROPERTIES:\n:ID: count-task-b\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: count-project\n:END:\n")
      (org-mode))
    (org-id-add-location "count-project" (buffer-file-name source))
    (org-id-add-location "count-task-a" "/mock:/gtd/inbox.org")
    (org-id-add-location "count-task-b" "/mock:/gtd/inbox.org")
    (let ((spec (org-gtd-reflect--stuck-view-spec 'stuck-project)))
      (assert-equal 1 (ogt-count-test--count spec))
      (assert-equal 1 (ogt-count-test--view-count spec)))
    (assert-equal 0 (ogt-count-test--count org-gtd-reflect-completed-projects-view-spec))
    ;; An overdue and late-starting project occurs in two view blocks.
    (assert-equal 2
                  (apply #'+
                         (mapcar #'ogt-count-test--count
                                 (org-gtd-command-center--count-specs 'missed-projects))))))

(deftest review-counts/inheritance-and-skip-settings ()
  "Counts use native inheritance, archive/comment and global skip settings."
  (ogt-count-test--source
   "* Parent\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n** Child\n* Archived :ARCHIVE:\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n* COMMENT Hidden\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n")
  (dolist (inherit '(nil t))
    (dolist (archives '(nil t))
      (let ((org-use-property-inheritance inherit)
            (org-agenda-archives-mode archives)
            (spec org-gtd-reflect-someday-maybe-view-spec))
        (assert-equal (ogt-count-test--view-count spec)
                      (ogt-count-test--count spec)))))
  (let ((org-agenda-skip-function-global
         (lambda () (org-entry-end-position))))
    (assert-equal 0
                  (ogt-count-test--count org-gtd-reflect-someday-maybe-view-spec))))

(deftest review-counts/no-ui-data-or-id-cache-side-effects ()
  "Counting respects unsaved data and leaves point, narrowing and IDs unchanged."
  (let* ((source (ogt-count-test--source
                  "* TODO Project\n:PROPERTIES:\n:ID: unsaved-project\n:ORG_GTD: Projects\n:ORG_GTD_FIRST_TASKS: unsaved-task missing-task\n:END:\n* TODO Task\n:PROPERTIES:\n:ID: unsaved-task\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: unsaved-project\n:END:\n"))
         (ids org-id-locations)
         (buffers (buffer-list))
         (window (selected-window))
         (spec (org-gtd-reflect--stuck-view-spec 'stuck-project)))
    (with-current-buffer source
      (goto-char (point-max))
      (narrow-to-region (line-beginning-position 0) (point-max))
      (let ((position (point))
            (start (point-min))
            (end (point-max))
            (tick (buffer-chars-modified-tick)))
        (cl-letf (((symbol-function 'org-agenda)
                   (lambda (&rest _) (error "Opened agenda while counting")))
                  ((symbol-function 'write-region)
                   (lambda (&rest _) (error "Wrote a file while counting")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (error "Prompted while counting")))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (error "Prompted while counting"))))
          (assert-equal 1 (ogt-count-test--count spec)))
        (assert-equal position (point))
        (assert-equal start (point-min))
        (assert-equal end (point-max))
        (assert-equal tick (buffer-chars-modified-tick))
        (assert-true (buffer-modified-p))))
    (assert-true (eq ids org-id-locations))
    (assert-equal 0 (hash-table-count ids))
    (assert-true (eq window (selected-window)))
    (assert-equal (sort (mapcar #'buffer-name buffers) #'string<)
                  (sort (mapcar #'buffer-name (buffer-list)) #'string<))))

(deftest review-counts/snapshot-deduplicates-views ()
  "Overlapping menu entries compute each constituent view only once."
  (let ((calls 0)
        (spec org-gtd-reflect-someday-maybe-view-spec))
    (cl-letf (((symbol-function 'org-gtd-review-counts--count)
               (lambda (&rest _) (cl-incf calls) 0)))
      (assert-equal (list (cons spec 0))
                    (org-gtd-review-counts--snapshot (list spec spec))))
    (assert-equal 1 calls)))

(deftest review-counts/reopen-refreshes-unsaved-changes ()
  "Reopening recomputes zero/nonzero counts without a TTL."
  (let ((source (ogt-count-test--source "* Candidate\n"))
        scopes)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (_prefix _layout _edit &rest params)
                 (push (plist-get params :scope) scopes))))
      (org-gtd-command-center--setup 'org-gtd-command-center '(someday))
      (with-current-buffer source
        (goto-char (point-min))
        (org-entry-put nil "ORG_GTD" "Someday"))
      (org-gtd-command-center--setup 'org-gtd-command-center '(someday)))
    (assert-equal '(((someday . 1)) ((someday . 0))) scopes)))

(deftest review-counts/disabled-does-not-scan ()
  "The opt-out keeps the menu available without any file scans."
  (let ((org-gtd-review-show-counts nil)
        called)
    (cl-letf (((symbol-function 'org-gtd-review-counts--snapshot)
               (lambda (&rest _) (error "Unexpected count")))
              ((symbol-function 'transient-setup)
               (lambda (&rest _) (setq called t))))
      (org-gtd-command-center--setup 'org-gtd-command-center '(someday)))
    (assert-true called)))

(deftest review-counts/errors-are-not-zero ()
  "A failed snapshot warns and leaves unknown counts, not false zeroes."
  (let (warning scope)
    (cl-letf (((symbol-function 'org-gtd-review-counts--snapshot)
               (lambda (&rest _) (error "Unreadable source")))
              ((symbol-function 'display-warning)
               (lambda (_type message &rest _) (setq warning message)))
              ((symbol-function 'transient-setup)
               (lambda (_prefix _layout _edit &rest params)
                 (setq scope (plist-get params :scope)))))
      (org-gtd-command-center--setup 'org-gtd-command-center '(someday)))
    (assert-match "Unreadable source" warning)
    (assert-nil scope)))

(deftest review-counts/label-formats-zero-positive-and-unavailable ()
  "Only zero loses its count suffix; positive and unknown counts stay visible."
  (let ((suffix (org-gtd-command-center--counted-suffix
                 :description "Review" :command 'ignore :count-key 'someday))
        (transient--prefix (transient-prefix :scope '((someday . 0)))))
    (assert-equal "Review" (transient-format-description suffix))
    (oset transient--prefix scope '((someday . 3)))
    (assert-equal "Review (3)" (transient-format-description suffix))
    (oset transient--prefix scope nil)
    (assert-equal "Review (?)" (transient-format-description suffix))
    (let ((org-gtd-review-show-counts nil))
      (assert-equal "Review" (transient-format-description suffix)))))

(deftest review-counts/unopened-files-do-not-run-user-hooks ()
  "A snapshot reads unopened sources without user hooks and releases its buffers."
  (with-temp-file "/mock:/gtd/org-gtd-tasks.org"
    (insert "* Maybe\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n"))
  (let ((org-mode-hook (list (lambda () (error "Ran Org mode hook"))))
        (find-file-hook (list (lambda () (error "Ran find-file hook")))))
    (assert-equal 1 (ogt-count-test--count org-gtd-reflect-someday-maybe-view-spec)))
  (assert-nil (org-find-base-buffer-visiting "/mock:/gtd/org-gtd-tasks.org")))

(deftest review-counts/error-cleans-up-and-restores-id-lookup ()
  "A failed scan releases its buffers and restores native ID refresh."
  (let ((lookup (symbol-function 'org-id-update-id-locations))
        (buffers (buffer-list))
        caught)
    (cl-letf (((symbol-function 'org-gtd-review-counts--count)
               (lambda (&rest _) (error "Broken skip function"))))
      (condition-case err
          (ogt-count-test--count org-gtd-reflect-someday-maybe-view-spec)
        (error (setq caught (error-message-string err)))))
    (assert-equal "Broken skip function" caught)
    (assert-true (eq lookup (symbol-function 'org-id-update-id-locations)))
    (assert-equal (sort (mapcar #'buffer-name buffers) #'string<)
                  (sort (mapcar #'buffer-name (buffer-list)) #'string<))))

(deftest review-counts/inactive-and-unresolved-projects ()
  "Native predicates exclude inactive projects and keep unresolved references."
  (ogt-count-test--source
   "* CNCL Canceled project\n:PROPERTIES:\n:ID: canceled-project\n:ORG_GTD: Projects\n:END:\n* TODO Inactive task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: canceled-project\n:END:\n* TODO Unresolved task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: unknown-project\n:END:\n")
  (assert-equal 1 (ogt-count-test--count
                   (org-gtd-reflect--stuck-view-spec 'stuck-next-action)))
  (assert-equal 0 (ogt-count-test--count
                   (org-gtd-reflect--stuck-view-spec 'stuck-project)))
  (assert-equal 1 (ogt-count-test--count org-gtd-reflect-completed-projects-view-spec)))

(deftest review-counts/habits-use-scheduled ()
  "A habit's scheduled timestamp, not ORG_GTD_TIMESTAMP, determines stuckness."
  (ogt-count-test--source
   "* TODO Valid\nSCHEDULED: <2099-01-01 Thu +1d>\n:PROPERTIES:\n:ORG_GTD: Habit\n:END:\n* TODO Wrong property\n:PROPERTIES:\n:ORG_GTD: Habit\n:ORG_GTD_TIMESTAMP: <2099-01-01 Thu +1d>\n:END:\n")
  (let ((spec (org-gtd-reflect--stuck-view-spec 'stuck-habit)))
    (assert-equal 1 (ogt-count-test--count spec))
    (assert-equal 1 (ogt-count-test--view-count spec))))

(deftest review-counts/delegated-day-boundaries ()
  "Future means after today's midnight, as in the underlying delegated view."
  (ogt-count-test--source
   (mapconcat
    (lambda (timestamp)
      (format "* WAIT Follow up\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: %s\n:END:\n"
              timestamp))
    (list (format-time-string "<%Y-%m-%d %a>"
                              (time-subtract (current-time) (days-to-time 1)))
          (format-time-string "<%Y-%m-%d %a>")
          (format-time-string "<%Y-%m-%d %a 12:00>")
          (format-time-string "<%Y-%m-%d %a>"
                              (time-add (current-time) (days-to-time 1))))
    ""))
  (assert-equal 1 (ogt-count-test--count
                   (car org-gtd-reflect-missed-engagements-view-specs)))
  (assert-equal 2 (ogt-count-test--count org-gtd-reflect-upcoming-delegated-view-spec))
  (assert-equal 2 (ogt-count-test--view-count org-gtd-reflect-upcoming-delegated-view-spec)))

(deftest review-counts/native-sublevel-option ()
  "Native sublevel suppression applies to counts as well as review lists."
  (ogt-count-test--source
   "* Parent\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n** Child\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n")
  (dolist (sublevels '(nil t))
    (let ((org-tags-match-list-sublevels sublevels)
          (spec org-gtd-reflect-someday-maybe-view-spec))
      (assert-equal (if sublevels 2 1) (ogt-count-test--count spec))
      (assert-equal (ogt-count-test--view-count spec)
                    (ogt-count-test--count spec)))))

(deftest review-counts/native-todo-ignore-options ()
  "The native tags-todo scanner honors configured scheduling exclusions."
  (ogt-count-test--source
   "* WAIT Included\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* WAIT Scheduled\nSCHEDULED: <2099-01-01 Thu>\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n")
  (let ((org-agenda-tags-todo-honor-ignore-options t)
        (org-agenda-todo-ignore-scheduled 'future)
        (spec (car org-gtd-reflect-missed-engagements-view-specs)))
    (assert-equal 1 (ogt-count-test--count spec))
    (assert-equal 1 (ogt-count-test--view-count spec))))

;;; review-counts-test.el ends here
