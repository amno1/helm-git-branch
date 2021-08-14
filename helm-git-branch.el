;;; helm-git-branch.el --- Switch git branch with Helm interface.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'helm-ls-git)

(defvar helm-source-git-local-branches nil
  "This source will built at runtime.
It can be build explicitly with function
`helm-git-build-local-branches-source'.")

(defvar helm-source-git-remote-branches nil
  "This source will built at runtime.
It can be build explicitly with function
`helm-git-build-remote-branches-source'.")

(defvar helm-source-git-remote-all-branches nil
  "This source will built at runtime.
It can be build explicitly with function
`helm-build-remote-all-branches-source'.")

(defface helm-git-branch-current-clean-face
    '((t :foreground "#2aa198"))
  "Files which contain rebase/merge conflicts."
  :group 'helm-ls-git)

(defface helm-git-branch-current-dirty-face
    '((t :foreground "#dc322f"))
  "Files which contain rebase/merge conflicts."
  :group 'helm-ls-git)

;;; Custom

(defgroup helm-git-branch nil
  "Fast git branch switching with Helm."
  :group 'helm)

(defcustom helm-git-branch-changes-action 'stash
  "The default action upon changing a branch with uncommited changes.

If this option is set to 'stash the changes will be automatically stashed.

If this option is set to 'commit the changes will automatically commited."
  :type 'symbol
  :group 'helm)

(defgroup helm-git-branch nil
  "Fast git branch switching with Helm."
  :group 'helm)

(defcustom helm-git-branch-stash-prefix "helm-git-branch--"
  "The prefix to prepend to stash names when autostashing a branch."
  :type 'string
  :group 'helm)

(defcustom helm-git-branch-fuzzy-match nil
  "Enable fuzzy matching in `helm-git-*-branch'."
  :group 'helm-branch-git
  :set (lambda (var val)
         (set var val)
         (setq helm-source-git-local-branches nil
               helm-source-git-remote-branches nil
               helm-source-git-remote-all-branches nil))
  :type 'boolean)


(defmacro replace-all (from to &optional buffer)
  `(with-current-buffer (or ,buffer (current-buffer))
     (goto-char (point-min))
     (while (search-forward ,from nil t)
       (replace-match ,to))))

(defun git-make-cmd (git-args &rest cmd-args)
  (let ((cmd
         (seq-concatenate
          'list
          '(call-process "git" nil t nil) (split-string git-args) cmd-args)))
    `(lambda () ,cmd)))

(defun git-call (git-args)
  (funcall (git-make-cmd git-args)))

(defmacro with-helm-git (git-cmd &rest body)
  `(nbutlast
     (split-string
      (helm-aif (helm-ls-git-root-dir)
          (with-helm-default-directory it
            (with-output-to-string
              (with-current-buffer standard-output
                (insert (format "%s" (git-call ,git-cmd)))
                ,@body
                (buffer-string))))) "\n" t "[\s\t]*")))

(defvar helm-git-branch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    map))

(defvar helm-git-branch-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-buffer-map)
    map))


;;; Sources
(defun helm-git--local-branches ()
  (with-helm-git "branch"))

(defvar-local helm-git--cache-remote-heads nil)
(defvar-local helm-git--cache-remote-tracked nil)

(defun helm-git--remote-branches ()
  (unless helm-git--cache-remote-tracked
    (setq-local helm-git--cache-remote-tracked
                (with-helm-git
                  "branch -r"
                  (replace-all "origin/" ""))))
  (cdr helm-git--cache-remote-tracked))

(defun helm-git--remote-all-branches ()
  (unless helm-git--cache-remote-heads
    (setq-local helm-git--cache-remote-heads
                (with-helm-git
                  "ls-remote --heads"
                  (replace-all "refs/heads/" ""))))
  (let (accum temp)
    (dolist (head (cdr helm-git--cache-remote-heads))
      (setq temp (split-string head))
      (setq temp (or (cdr temp) (car temp)))
      (push (car temp) accum))
    (nreverse accum)))

(defun helm-git-branch-local-transformer (candidates _source)
  (cl-loop with root = (helm-ls-git-root-dir)
        for i in candidates
        collect
        (cond ((string-match "^\\(*\\)\\(.*\\)" i) ; current branch
               (if (helm-git-branch--dirty-p)
                   (cons (propertize i 'face 'helm-git-branch-current-dirty-face)
                         (expand-file-name (match-string 2 i) root))
                 (cons (propertize i 'face 'helm-git-branch-current-clean-face)
                       (expand-file-name (match-string 2 i) root))))
              (t i))))

(defun helm-git-branch-local-action-transformer (actions _candidate)
  (let ((disp (helm-get-selection nil t))
        (action 
         (when (= 1 (length (helm-marked-candidates)))
           (helm-make-actions "Checkout branch"
                              (lambda (_candidate)
                                (let ((marked (helm-marked-candidates)))
                                  (cl-loop for f in marked do
                                           (progn
                                             (vc-git-revert f)
                                             (helm-aif (get-file-buffer f)
                                                 (with-current-buffer it
                                                   (revert-buffer t t)))))))))))
    ;; Unregistered files
    (cond ((string-match "^[?]\\{2\\}" disp)
           (append actions
                   (list '("Add file(s)"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (vc-call-backend 'Git 'register marked))))
                         '("Delete file(s)" . helm-ff-delete-files)
                         '("Copy bnames to .gitignore"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (with-current-buffer (find-file-noselect
                                                       (expand-file-name
                                                        ".gitignore"
                                                        (helm-ls-git-root-dir)))
                                   (goto-char (point-max))
                                   (cl-loop with last-bname 
                                         for f in marked
                                         for bname = (helm-basename f)
                                         unless (string= bname last-bname)
                                         do (insert (concat bname "\n"))
                                         do (setq last-bname bname))
                                   (save-buffer))))))))
          ((string-match "^A " disp)
           (append actions '(("Commit staged file(s)"
                              . helm-ls-git-commit)
                             ("Extend commit"
                              . helm-ls-git-extend-commit)
                             ("Amend commit"
                              . helm-ls-git-amend-commit)
                             ("Unstage file(s)"
                              . helm-ls-git-unstage-files))))
          ;; Modified but not staged
          ((string-match "^ M+ *" disp)
           (append actions (helm-append-at-nth
                            action
                            '(("Stage file(s)"
                               . helm-ls-git-stage-files)
                              ("Stage marked file(s) and commit"
                               . helm-ls-git-stage-marked-and-commit)
                              ("Stage marked file(s) and extend commit"
                               . helm-ls-git-stage-marked-and-extend-commit)
                              ("Stage marked file(s) and amend commit"
                               . helm-ls-git-stage-marked-and-amend-commit))
                            1)))
          ;; Modified and staged
          ((string-match "^M+ *" disp)
           (append actions (helm-append-at-nth
                            action
                            '(("Commit staged file(s)"
                               . helm-ls-git-commit)
                              ("Extend commit"
                               . helm-ls-git-extend-commit)
                              ("Amend commit"
                               . helm-ls-git-amend-commit)
                              ("Unstage file(s)"
                               . helm-ls-git-unstage-files))
                            1)))
          ;; Deleted
          ((string-match "^ D " disp)
           (append actions (list '("Git delete" . vc-git-delete-file)
                                 '("Stage file(s)"
                                   . helm-ls-git-stage-files))))
          (t actions))))

(defclass helm-git-local-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-git--local-branches))))
   (keymap :initform 'helm-git-branch-map)
   (action :initform (helm-make-actions "Git checkout"
                                        (lambda (_candidate)
                                          (helm-git-branch--checkout _candidate))))
   (filtered-candidate-transformer :initform 'helm-git-branch-local-transformer)
   (action-transformer :initform 'helm-git-branch-local-action-transformer)))

(defclass helm-git-remote-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-git--remote-branches))))
   (keymap :initform 'helm-git-branch-map)
   ;;(filtered-candidate-transformer :initform 'helm-ls-git-branches-transformer)
   ;;(action-transformer :initform 'helm-ls-git-branches-action-transformer)
   ))

(defclass helm-git-remote-all-source (helm-source-in-buffer)
  ((header-name :initform 'helm-ls-git-header-name)
   (init :initform
         (lambda ()
           (helm-init-candidates-in-buffer 'global
             (helm-git--remote-all-branches))))
   (keymap :initform 'helm-git-branch-map)
   ;;(filtered-candidate-transformer :initform 'helm-ls-git-branches-transformer)
   ;;(action-transformer :initform 'helm-ls-git-branches-action-transformer)
   ))

(defun helm-git-build-local-branches-source ()
  ;;(and (memq 'helm-source-ls-git-branches helm-ls-git-default-sources)
  (helm-make-source "Local branches" 'helm-git-local-source
    :fuzzy-match helm-git-branch-fuzzy-match
    :group 'helm
    :keymap helm-git-branch-map))

(defun helm-git-build-remote-branches-source ()
  ;;(and (memq 'helm-source-ls-git-branches helm-ls-git-default-sources)
  (helm-make-source "Tracked remote branches" 'helm-git-remote-source
    :fuzzy-match helm-git-branch-fuzzy-match
    :group 'helm))

(defun helm-git-build-remote-all-branches-source ()
  ;;(and (memq 'helm-source-ls-git-branches helm-ls-git-default-sources)
  (helm-make-source "All remote branches" 'helm-git-remote-all-source
    :fuzzy-match helm-git-branch-fuzzy-match
    :group 'helm))



(defun helm-git-branch--dirty-p ()
  (not (string-blank-p (helm-ls-git-status))))

(defun helm-git-branch--stash ()
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (insert (call-process "git" nil t nil "stash" "save"
                                  (concat helm-git-branch-stash-prefix
                                  (helm-ls-git--branch)))))))))

(defun helm-git-branch--unstash ()
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (insert (call-process "git" nil t nil "stash" "list"))
            (goto-char (point-min))
            (when (search-forward (concat helm-git-branch-stash-prefix
                                          (helm-ls-git--branch)) nil t)
              (goto-char (line-beginning-position))
              (search-forward "}" (line-end-position))
              (let ((stash (buffer-substring-no-properties
                            (line-beginning-position) (point))))
                (call-process "git" nil t nil "stash" "apply" stash)
                (call-process "git" nil t nil "stash" "drop" stash))))))))

(defun helm-git-branch--checkout (branch)
  (helm-aif (helm-ls-git-root-dir)
      (with-helm-default-directory it
        (with-output-to-string
          (with-current-buffer standard-output
            (when (helm-git-branch--dirty-p)
              (helm-git-branch--stash))
            (insert (call-process "git" nil t nil "checkout" branch))
            (helm-git-branch--unstash))))))


;;; Commands
;;;###autoload
(defun helm-git-local-branches (&optional arg)
  (interactive "p")
  (let ((helm-ff-default-directory
         (or helm-ff-default-directory
             default-directory)))
    (when (and arg (not (helm-ls-git-root-dir)))
      (error "Not inside a Git repository"))
    (setq helm-source-git-local-branches
          (helm-git-build-local-branches-source))
    (helm-set-local-variable
     'helm-ls-git--current-branch (helm-ls-git--branch))
    (helm :sources helm-source-git-local-branches
          :ff-transformer-show-only-basename nil
          :truncate-lines helm-buffers-truncate-lines
          :buffer "*helm branches*")))

;;;###autoload
(defun helm-git-remote-branches (&optional arg)
  (interactive "p")
  (let ((helm-ff-default-directory
         (or helm-ff-default-directory
             default-directory)))
    (when (and arg (not (helm-ls-git-root-dir)))
      (error "Not inside a Git repository"))
    (setq helm-source-git-remote-branches
          (helm-git-build-remote-branches-source))
    (helm-set-local-variable
     'helm-ls-git--current-branch (helm-ls-git--branch))
    (helm :sources helm-source-git-remote-branches
          :ff-transformer-show-only-basename nil
          :truncate-lines helm-buffers-truncate-lines
          :buffer "*helm branches*")))

;;;###autoload
(defun helm-git-remote-all-branches (&optional arg)
  (interactive "p")
  (let ((helm-ff-default-directory
         (or helm-ff-default-directory
             default-directory)))
    (when (and arg (not (helm-ls-git-root-dir)))
      (error "Not inside a Git repository"))
    (helm-set-local-variable
     'helm-ls-git--current-branch (helm-ls-git--branch))
    (setq helm-source-git-remote-all-branches
          (helm-git-build-remote-all-branches-source))
    (helm :sources helm-source-git-remote-all-branches
          :ff-transformer-show-only-basename nil
          :truncate-lines helm-buffers-truncate-lines
          :buffer "*helm branches*")))

(provide 'helm-git-branch)
;;; helm-git-branch.el ends here
