;;; user-keys-abstract.el --- Make binding conventions configurable -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author: Psionik K <73710933+psionic-k@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/positron-solutions/user-keys-abstract

;;; License notice:

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
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

;; This module is a proof of concept for properly implementing conventions such
;; as C-n and C-p for list and line navigation.  Because lists are the most
;; prevalent concept in Lisp, and Emacs Lisp, the keys for navigating lists are
;; among the first that the user should have a choice in rebinding.

;;; Code:

(require 'dash)

(defun user-keys-abstract--strip-kwargs (form)
  "Return the stripped FORM and keyword-args as a plist.
FORM must be a list.  Keyword args can be out of order but each
value is assumed to follow the keyword."
  (let* ((kwargs)
         (key)
         (stripped))
    (--each form
      (if key (progn
                (push it kwargs)
                (push key kwargs)
                (setq key nil))
        (if (and (symbolp it)
                 ;; look for colon at beginning of symbol name
                 (equal ?\: (aref (symbol-name it) 0)))
            (setq key it)
          (push it stripped))))
    (list (reverse stripped) kwargs)))

(defun user-keys-abstract--expand-shadow-remaps (form)
  "Expand the shadow-remaps contained in FORM and flatten.
FORM is a `(remap-definition shadow-remap ... shadow-remap)'
declaration accepted by `user-keys-abstract-define-remap'."

  (pcase-let* ((`(,remap-def _)
                (user-keys-abstract--strip-kwargs (car form)))
               (`(,abstract-command-name ,orig-sequence _)
                remap-def))
    (let* ((shadow-remaps (cdr form))
           ;; push feature to each shadow-remap and normalize each output to
           ;; `((feature shadow-remap) (feature shadow-remap))'
           (shadow-remaps
            (-reduce #'append
                     (--map (let ((feature (car it))
                                  (remaps (cdr it)))
                              (--map (cons feature it)
                                     (if (listp (car remaps))
                                         remaps
                                       (list remaps))))
                            shadow-remaps))))
      (->> shadow-remaps
           (-map
            (lambda (shadow-remap)
              (pcase-let ((`(,feature ,map ,shadow-command) shadow-remap))
                (list
                 ;; unbind the existing sequence
                 (if (version< "29" emacs-version)
                     `(,feature (define-key ,map (kbd ,orig-sequence) nil t))
                   ;; emacs 28 cannot remove bindings entirely
                   `(,feature (define-key ,map (kbd ,orig-sequence) nil)))
                 ;; remap-bind to the abstract command
                 `(,feature (define-key
                             ,map
                             (vector 'remap #',abstract-command-name)
                             #',shadow-command))))))
           ;; flatten each pair of `define-key' expressions
           (-reduce #'append)))))

(defmacro user-keys-abstract-define-remap (&rest forms)
  "Re-map multiple bindings onto new abstract commands.
An abstract command is one that is usually bound to a sequence.
Real commands that would normally shadow the sequence directly
are instead bound via command remapping.  Thus, if the abstract
command is rebound to another sequence, all of the shadows move
with it.  This converts ad-hoc sequence conventions like `C-n'
into first-class conventions that the user can manage more
effectively.

To illustrate, after abstracting all of the `C-n' commands to an
abstract command such as `user-keys-abstract-next', you can
rebind `user-keys-abstract-next' to `C-j' and all of the remapped
commands will move with it, meaning commands like `next-line'
move to `C-j' as well.  You can even remove these keys by
providing no binding to the abstract command.

Regarding the implementation and usage, at a high level, this
macro returns expressions that define abstract commands, binds
them to a sequence, and then uses command remapping to re-direct
the abstract command to a concrete command in a number of
keymaps.  Because these are lazily loaded, `eval-after-load' is
used and we need to know the feature name or elisp file.  When
making sequences abstract, the sequence binding in each map is
removed so that the map only contains the command remap.  This
enables rebinding of the abstract command's sequence to affect
all abstracted sequences.

Each form in FORMS has the following structure:

`(remap-definition shadow-remap ... shadow-remap)'

Each REMAP-DEFINITION form is the following:

`(abstract-command-name sequence-string original-command &rest args)'

ARGS should be plist style pairs of :keyword value, with the following support:

- :map for any map other than the `global-map'.
- :target-sequence if you want to bind commands to a new sequence other than
  SEQUENCE-STRING.  This is used to both define the abstract command and place
  it on a different binding.
- :no-bind if you want to define the abstract command but not the key sequence.
  Without a key sequence, any REMAP-DEFINITION forms using the abstract command
  will not have an active binding, just a remap to a command that is not itself
  bound.

Each SHADOW-REMAP form is one of the following:

`(feature1 map shadow-command)'
`(feature2 (map1 shadow-command)
           (map2 shadow-command))'

FEATURE can be any expression suitable for `eval-after-load' and
usually you will use a feature symbol, unquoted, or a string that
points to a file.  In the case of subr.el, where many of the most
fundamental maps are defined, there is no corresponding feature,
so just use the elisp file name.

SHADOW-COMMAND is the command that shadows the SEQUENCE-STRING in
the REMAP-DEFINITION.  Two expressions will be generated.  One to
unbind SHADOW-COMMAND from SEQUENCE-STRING and another to remap
ABSTRACT-COMMAND-NAME to SHADOW-COMMAND so that SHADOW-COMMAND is
active whenever that map has precedence.

TODO shadow-remap forms need plist support.  If the command is
being moved from a sequence other than SEQUENCE-STRING, this
needs to be expressed per shadow-command.

TODO When a :target-sequence is expressed, it may need unbinding
in many, many maps so that the abstract command is not shadowed
after being moved.  At a minimum, unbind in maps with remaps.

TODO Moving sequences needs to be coordinated over all forms so
that unbinding and rebinding don't clobber each other in the
global map."

  (let* ((remap-defs (-map #'car forms))
         ;; define abstract commands for each remap-def
         (ac-def-forms
          (->> remap-defs
               (-map #'car)
               (-map (lambda (abstract-command-name)
                       `(defun ,abstract-command-name '()
                          ,(concat "Abstract command target.\n"
                                   "This is a global map abstract command.")
                          (interactive)
                          (undefined))))))
         ;; define global map binding & remap for each remap-def
         (global-remap-forms
          (->> remap-defs
               (-map
                (lambda (remap-def)
                  (pcase-let* ((`(,remap-def ,remap-kwargs)
                                (user-keys-abstract--strip-kwargs remap-def))
                               (`(,abstract-command-name
                                  ,sequence-string
                                  ,original-command)
                                remap-def))
                    (let ((no-bind (plist-get remap-kwargs :no-bind))
                          (target-sequence
                           (plist-get remap-kwargs :target-sequence)))
                      (list
                       ;; unbind the original command from the original sequence
                       (when target-sequence
                         (if (version< "29" emacs-version)
                             `(define-key global-map (kbd ,sequence-string) nil t)
                           ;; emacs 28 cannot remove bindings entirely
                           `(define-key global-map (kbd ,sequence-string) nil)))

                       ;; bind the abstract command to the target sequence
                       (unless no-bind
                         `(define-key
                           global-map
                           (kbd ,(or target-sequence sequence-string))
                           #',abstract-command-name))

                       ;; remap the original command to the abstract command
                       `(define-key
                         global-map
                         (vector 'remap #',abstract-command-name)
                         #',original-command))))))

                (-reduce #'append)
                (-non-nil)))

         ;; for each remap-def, expand its shadow-remap forms over their feature,
         ;; then gather all shadow-remaps over features and generate
         ;; `eval-after-load' expressions.
         (eval-after-load-forms
          (->>
           forms
           ;; expand shadow-remap forms over the remap-definition
           (-map #'user-keys-abstract--expand-shadow-remaps)
           (-reduce #'append)          ; flatten over each remap-def
           (-group-by #'car)            ; group shadow-remaps by feature
           (-map
            (lambda (feature-group)
              ;; feature-group is (feature ((feature rebind) (feature rebind)))
              ;; with all same feature
              (let ((feature (car feature-group))
                    (shadow-remaps (-map #'cadr (cdr feature-group))))
                `(eval-after-load ',feature (progn ,@shadow-remaps))))))))
    ;; splice
    `(progn ,@ac-def-forms
            ,@global-remap-forms
            ,@eval-after-load-forms)))

(defun user-keys-abstract-list-navigation ()
  "WARNING!  YOU BETTER KNOW WHAT YOU ARE DOING!
Okay, so you found this pre-alpha backage and it says it can make
your bindings abstract, allowing you to move around `C-n' and
`C-p' etc.  This function will do it, but it's basically a demo
and not intended to be run in your daily driving.  This is why I
did not bind it in a command."
  (user-keys-abstract-define-remap
       ((abstract-next "C-n" next-line)
        ("subr.el" esc-map backward-list)
        (calendar-mode calendar-mode-map calendar-backward-week)
        (comint-mode comint-repeat-map comint-previous-prompt)
        (company-mode (company-active-map company-select-previous-or-abort)
                      (company-search-map company-select-previous-or-abort))
        ;; (doc-view doc-view-mode-map doc-view-previous-line-or-previous-page)
        (gnus gnus-summary-goto-map gnus-summary-prev-same-subject)
        (kmacro kmacro-keymap kmacro-cycle-ring-previous)
        (org (org-agenda-keymap org-agenda-previous-line)
             (org-agenda-mode-map org-agenda-previous-line)
             (org-babel-map org-babel-previous-src-block))
        (outline (outline-mode-prefix-map outline-previous-visible-heading)
                 (outline-navigation-repeat-map outline-previous-visible-heading))
        (popup popup-menu-keymap popup-previous)
        (quail quail-simple-translation-keymap quail-other-command)
        (quail quail-translation-keymap quail-prev-translation-block))
        ;; (menu-bar tty-menu-navigation-map tty-menu-prev-item)
        ;; (widget widget-global-map previous-line))

       ((abstract-previous "C-p" previous-line)

        ("subr.el" esc-map forward-list)
        (calendar-mode calendar-mode-map calendar-forward-week)
        (comint-mode comint-repeat-map comint-next-prompt)
        (company-mode (company-active-map company-select-next-or-abort)
                      (company-search-map company-select-next-or-abort))
        ;; (doc-view doc-view-mode-map doc-view-next-line-or-next-page)
        (gnus gnus-summary-goto-map gnus-summary-prev-same-subject)
        (kmacro kmacro-keymap kmacro-cycle-ring-next)
        (org (org-agenda-keymap org-agenda-next-line)
             (org-agenda-mode-map org-agenda-next-line)
             (org-babel-map org-babel-next-src-block))
        (outline (outline-mode-prefix-map outline-next-visible-heading)
                 (outline-navigation-repeat-map outline-next-visible-heading))
        (popup popup-menu-keymap popup-next)
        (quail quail-simple-translation-keymap quail-other-command)
        (quail quail-translation-keymap quail-prev-translation-block))))
        ;; (menu-bar tty-menu-navigation-map tty-menu-prev-item)
        ;; (widget widget-global-map next-line))))


(provide 'user-keys-abstract)
;;; user-keys-abstract.el ends here
