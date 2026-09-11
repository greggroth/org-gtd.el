;;; org-gtd-review-counts.el --- Counts for review menus -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Read-only snapshots of the native tags views used by review menus.
;; Rendering a transient description never scans agenda files.

;;; Code:

(require 'org-id)
(require 'org-gtd-view-language)

(defcustom org-gtd-review-show-counts t
  "Whether review menus show counts of matching entries.
Counts are computed when opening a menu, not on redisplay.  Set this to
nil to avoid scanning agenda files and project-ID sources for menu counts."
  :group 'org-gtd
  :type 'boolean)

(defvar org-gtd-review-counts--new-buffers nil
  "File buffers opened during the current count snapshot.")

(defun org-gtd-review-counts--buffer (file)
  "Return an Org buffer for FILE without running user hooks or local eval.
Remember newly opened buffers for cleanup after counting."
  (or (org-find-base-buffer-visiting file)
      (progn
        (unless (file-readable-p file)
          (error "Cannot read review count source: %s" file))
        (let ((org-inhibit-startup t)
              (org-agenda-file-menu-enabled nil)
              (non-essential t)
              (large-file-warning-threshold nil)
              (enable-local-eval nil)
              (enable-local-variables :safe)
              (find-file-hook nil)
              (org-mode-hook nil)
              (change-major-mode-hook nil)
              (after-change-major-mode-hook nil))
          (let ((buffer (find-file-noselect file)))
            (push buffer org-gtd-review-counts--new-buffers)
            buffer)))))

(defun org-gtd-review-counts--id-sources ()
  "Return the sources Org searches when resolving project IDs.
Use the same source set as `org-id-update-id-locations'."
  (delete-dups
   (mapcar
    #'file-truename
    (cl-remove-if-not
     #'stringp
     (append (org-agenda-files t org-id-search-archives)
             (if (symbolp org-id-extra-files)
                 (symbol-value org-id-extra-files)
               org-id-extra-files)
             org-id-files
             (mapcar #'buffer-file-name (org-buffer-list 'files t)))))))

(defun org-gtd-review-counts--index-ids (files)
  "Build a private ID index from FILES, including unsaved buffer contents."
  (let ((locations (make-hash-table :test #'equal)))
    (dolist (file files)
      ;; Org's ID discovery also ignores missing historical ID sources.
      (when (or (org-find-base-buffer-visiting file) (file-exists-p file))
        (with-current-buffer (org-gtd-review-counts--buffer file)
          (unless (derived-mode-p 'org-mode)
            (error "Review count source is not in Org mode: %s" file))
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (when-let* ((id (org-entry-get nil "ID")))
               (puthash id file locations)))))))
    locations))

(defun org-gtd-review-counts--count (spec files)
  "Count matches for a single review SPEC in FILES.
Use both the native tags matcher and the translated skip function."
  (let* ((block (org-gtd-view-lang--create-agenda-block
                 (org-gtd-view-lang--normalize-view-spec spec)))
         (org--matcher-tags-todo-only (eq (car block) 'tags-todo))
         (org-tags-match-list-sublevels org-tags-match-list-sublevels)
         (org-agenda-skip-function
          (eval (cadr (assq 'org-agenda-skip-function (nth 2 block))) t))
         (matcher (cdr (org-make-tags-matcher (nth 1 block))))
         (count 0))
    (unless (memq (car block) '(tags tags-todo))
      (error "Review counts require a tags view: %S" spec))
    (dolist (file files)
      (with-current-buffer (org-gtd-review-counts--buffer file)
        (unless (derived-mode-p 'org-mode)
          (error "Review count source is not in Org mode: %s" file))
        (org-with-wide-buffer
         (when (eq (current-buffer) org-agenda-restrict)
           (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end))
         (org-scan-tags (lambda () (cl-incf count))
                        matcher org--matcher-tags-todo-only))))
    count))

(defun org-gtd-review-counts--snapshot (specs)
  "Return an alist of unique SPECS and their counts for this menu opening.
Reuse native Org ID lookup with a complete, private, in-memory index.
No agenda buffer is created, and no ID cache is saved or refreshed on disk.
Errors propagate to the menu, which can display unavailable counts."
  (let ((non-essential t)
        (org-gtd-review-counts--new-buffers nil)
        (org-id-locations org-id-locations)
        (org-id-files org-id-files)
        (org-agenda-new-buffers nil))
    (save-match-data
      (save-current-buffer
        (unwind-protect
            (let ((files (org-agenda-files nil 'ifmode)))
              (unless org-id-locations
                (org-id-locations-load))
              (setq org-id-locations
                    (org-gtd-review-counts--index-ids
                     (org-gtd-review-counts--id-sources)))
              ;; A missing ID must not trigger Org's disk-writing refresh.
              ;; All of its discovery sources were indexed above.
              (cl-letf (((symbol-function 'org-id-update-id-locations)
                         (lambda (&optional _files _silent)
                           (unless org-id-track-globally
                             (error "Please turn on `org-id-track-globally' if you want to track IDs"))
                           org-id-locations)))
                (mapcar (lambda (spec)
                          (cons spec (org-gtd-review-counts--count spec files)))
                        (delete-dups (copy-sequence specs)))))
          (dolist (buffer org-gtd-review-counts--new-buffers)
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(provide 'org-gtd-review-counts)
;;; org-gtd-review-counts.el ends here
