;;; user-keys.el --- clean and protect your keymaps from pollution. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author: Psionik K <73710933+psionic-k@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (keymap-utils "4.0.0"))
;; Homepage: http://github.com/positron-solutions/user-keys

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

;; User-keys contains tools to diagnose and remove poor key bindings
;; and to free up key sequences that users actually want.

;;; Code:

(require 'derived)
(require 'dash)
(require 'keymap-utils)

;; implementation state and constants

(defvar user-keys-sequence nil "Current sequence being analyzed.")

(defvar user-keys-operation nil "Current analysis operation.")

(defvar user-keys-target-buffer nil "Current buffer for inspecting bindings.")

(defconst user-keys--fkey-events
  (mapcar
   (lambda (c) (make-symbol (concat "f" (int-to-string c))))
   (number-sequence 1 20))
  "List of function key events, through F20.")

(defvar user-keys--stupid-events
  '('insert
    'pause
    'print
    'again
    'begin
    ;; 'find
    ;; 'help
    'insertchar
    'insertline
    'prior
    )
  "Sequences should almost never end in these events.")

(defvar user-keys--exception-sequences
  '([f1 f11])
  "Sequences to except from usual unbinding.
Some key sequences are from desktop environment idioms rather
than traditional Emacs design errors.  While these desktop
environments are also mostly wrong, the cost to benefit is in favor
of keeping them bound.")

(defvar user-keys--available-string (propertize "available" 'face 'success)
  "Nobody has spammed their keymap ads on this key sequence.")

;; customize

(defgroup user-keys nil "User keys.")

(defcustom user-keys-stupid-modifiers
  '("S-" "H-" "s-" "C-M-" "M-S-" "C-M-S-")
  "Sequences should almost never require these modifier combinations.
Modifiers are in A-C-H-M-S-s and specified in this order for valid keys"
  :type '(repeat (repeat string))
  :group 'user-keys)

(defcustom user-keys-buffer-name "*User-Keys*"
  "Where should we display interactive output?"
  :type 'string
  :group 'user-keys)

(defcustom user-keys-shifted-keys
  (string-to-list "~!@#$%^&*()_+{}|:\"<>?")
  "Keys that have an implied shift modifier."
  :type '(repeat 'character)
  :group 'user-keys)

(defcustom user-keys-preferred-sequences
  "Sequences to look report in `user-keys-report-preferred'.
Set to an expression or function that returns a list of sequence
vectors.  By default, all top-level single-modifier keys are
considered preferred sequences."
  (--map (vector it)
         (append (number-sequence ?\C-a ?\C-a)
                 (number-sequence ?\M-a ?\M-z))))

;; implementation functions

(defun user-keys--get-buffer ()
  "Obtain the report buffer."
  (let ((buffer (get-buffer user-keys-buffer-name)))
    (or buffer
        (let ((buffer (get-buffer-create user-keys-buffer-name)))
          (set-buffer buffer)
          (user-keys-mode)
          buffer))))

(defmacro user-keys--with-buffer (buffer-symbol form)
  "Prepare and bind report buffer to BUFFER-SYMBOL and evaluate FORM."
  `(let ((,buffer-symbol (user-keys--get-buffer)))
     (progn (read-only-mode -1)
            (set-buffer buffer-symbol)
            ,form
            (read-only-mode 1))))

(defun user-keys--pop-to-buffer ()
  "Pop to the reporting buffer."
  (pop-to-buffer (user-keys--get-buffer)))

(defsubst user-keys--button (symbol)
  "Format SYMBOL as a button that displays its documentation."
  (propertize (if (symbolp symbol)
                  (symbol-name symbol) symbol)
              'face 'button
              'button '(t)
              'category 'default-button))

(defsubst user-keys--section-header (text)
  "Make a dividing section header for TEXT for pretty printing results."
  format "\n%s\n%s\n" text (make-string (length text) ?-))

(defun user-keys--major-mode-keymaps ()
  "Return a list of major modes map symbols."
  (let* ((major-mode-keymaps '())
         (_ (mapatoms
             (lambda (a) (when (get a 'derived-mode-parent)
                      (let ((mode-map-name (derived-mode-map-name a)))
                        (when (and (boundp mode-map-name)
                                   (keymapp  (symbol-value mode-map-name)))
                          (push mode-map-name
                                major-mode-keymaps))))))))
    major-mode-keymaps))

(defun user-keys--other-maps(keymaps)
  "Return a list of all keymaps not in KEYMAPS.
KEYMAP-LISTS is a list of lists of map symbols."
  (let ((other-keymaps (make-hash-table :test 'eq))
        (known-keymaps (make-hash-table :test 'eq)))
    (mapc
     (lambda (map)
       (puthash map map known-keymaps))
     keymaps)

    ;; TODO keymapp on symbols is annoying
    (mapatoms (lambda (a) (when (or (keymapp a)
                               (and (boundp a) (keymapp (symbol-value a))))
                       (when (equal a 'emacs-lisp-mode-map)
                         (message "Emacs lisp mode map: %s" a)
                         (message "Hash contains: %s" (gethash a known-keymaps)))
                       (unless (gethash a known-keymaps)
                         (puthash a a other-keymaps)))))
    (hash-table-keys other-keymaps)))

(defsubst user-keys--normalize-sequence (sequence)
  "Round trip the SEQUENCE to eliminate common prefix effect.
Within a keymap, having a common prefix seems to result in sequences
with multiple keys even though the simple `kbd' result is just one key."
  (let ((reconverted (kbd (key-description sequence))))
    (if (stringp reconverted) (vconcat reconverted)
      reconverted)))

(defsubst user-keys--maybe-unroll (sequence)
  "Unrolls SEQUENCE if it is a range, returning first sequence."
  (vconcat (--map
            (if (and (consp it) (atom (cdr it)))
                (car it)
              it)
            sequence)))

(defun user-keys--find (keymap seq-predicates &optional exclude-predicates)
  "Find all sequences in KEYMAP matched by SEQ-PREDICATES.

Do not attempt to match sequences that are excluded by
EXCLUDE-PREDICATES.

Each predicate is called with SEQUENCE and DEFINITION.

The only potential modification to SEQUENCE before evaluating
predicates is that some sequences representing ranges will be
unrolled into just the first sequence in the range.  This means
sequences like `C-0..C-9' will instead be called with just `C-0' and
the result will apply to every key in the range.  You can exclude
ranges in EXCLUDE-PREDICATES when this creates a problem.

Within each predicate, you may need to map over the individual keys in
the sequence.  If you need modifiers for a key, call
`event-modifiers'.  For the basic key or event, call
`event-basic-type'.  These functions return a list of modifiers and a
list of basic events respectively.

Each predicate can return a reason, and multiple reasons will be
output into the buffer during analysis of bad keys.  This is for
easier design and debugging of rules."
  (let (matches match-excludes)
    (kmu-map-keymap
     (lambda (sequence definition)
       (let ((orig-sequence sequence)
             (sequence (user-keys--normalize-sequence
                        (user-keys--maybe-unroll sequence)))
             (exclusions (-non-nil (--map
                                    (funcall it sequence definition)
                                    exclude-predicates))))
         (if exclusions
             (push (list orig-sequence definition exclusions) match-excludes)
           (when-let ((reasons (-non-nil (--map (funcall it sequence definition)
                                                seq-predicates))))
             (push (list orig-sequence definition reasons) matches)))))
     keymap)
    (list matches match-excludes)))

(defun user-keys-report-preferred ()
  "Show each of the user's preferred sequences in the current buffer."
  (interactive)
  (let (predicate (user-keys-sequences-predicate
                   user-keys-preferred-sequences t)))

  ;; global
  ;; major mode
  ;; local map
  ;; emulating
  ;; overriding
  (undefined))

(defun user-keys-report-shadows (key)
  "Show all keymaps that potentially could shadow KEY."
  (interactive)

  ;; global
  ;; major modes
  ;; other maps
  (undefined))

(defun user-keys-report-stupid ()
  "Show all of the stupid key sequences that are currently bound."
  (interactive)
  ;; global
  ;; major modes
  ;; other maps
  (undefined))

(defun user-keys-generate-unbinds (output-type)
  "Generate an unbinding expression for OUTPUT-TYPE."
  (interactive)

  (undefined))

;;; TODO what is common to all reports and how can we make each one useful?

;; functions useful for those extending user-keys, part of external API

(defun user-keys-key-predicate (key reason)
  "Return a predicate that will match KEY.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (when (seq-contains-p sequence key)
      reason)))

(defun user-keys-basic-events-predicate (basic-events reason)
  "Return predicate matching sequences containing any of BASIC-EVENTS.
The REASON will be returned for reporters.

Basic events are a list, and for mouse events for example, can contain
multiple elements."
  (lambda (sequence _)
    (when (-non-nil
           (--map (seq-contains-p basic-events (event-basic-type it))
                  sequence))
      reason)))

(defun user-keys-sequences-predicate (sequences reason)
  "Return a predicate that matches any sequence from SEQUENCES.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (message "sequence: %s" sequence)
    (when (member sequence sequences) reason)))

(defun user-keys-multiple-modifiers-predicate (reason)
  "Return predicate matching keys with multiple modifiers.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (when (-non-nil
           (--map (> (length (event-modifiers it)) 1) sequence))
      reason)))

(defun user-keys-modified-basic-events-predicate (basic-events reason)
  "Return a predicate matching any modified use of BASIC-EVENTS.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (when (-non-nil
           (--map (and (event-modifiers it)
                       (seq-contains-p basic-events (event-basic-type it)))
                  sequence))
      reason)))

(defun user-keys-one-mod-events-predicate (modifiers
                                           basic-events reason)
  "Return a predicate matching single MODIFIERS and BASIC-EVENTS.
This is useful for looking at top-level, simply modified sequences.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (when (-non-nil
           (--map
            (let ((sequence-mods (event-modifiers it)))
              (and (= (length sequence-mods) 1)
                   (seq-contains-p modifiers
                                   (car sequence-mods))
                   (seq-contains-p basic-events
                                   (event-basic-type it))))
            sequence))
      reason)))

(defun user-keys-commands-predicate (commands reason)
  "Return a predicate matching COMMANDS.
The REASON will be returned for reporters."
  (lambda (_ description)
    (message "description %s" description)
    (when (member description commands)
      reason)))

;; user commands and modes

(defun user-keys--describe-current-sequence ()
  "Format's the current sequence for display as a group description."
  (format "Current key: %s" (if user-keys-sequence
                                (propertize (key-description
                                             user-keys-sequence)
                                            'face 'success)
                              (propertize "none" 'face 'shadow))))

(defun user-keys--describe-target-buffer ()
  "Format's the current buffer for display as a group description."
  (format "Target buffer: %s" (if user-keys-target-buffer
                                (propertize
                                 (buffer-name (get-buffer
                                               user-keys-target-buffer))
                                 'face 'success)
                              (propertize "none" 'face 'shadow))))

(defun user-keys-set-target-buffer (buffer)
  "Set the BUFFER  used for inspecting bindings.
See `user-keys-target-buffer'."
  (interactive "bTarget buffer for binding inspection: ")
  (setq user-keys-target-buffer buffer))

(defun user-keys-set-sequence-key (key)
  "Set `user-keys-sequence' by inputting just one KEY.
The key does not need to be bound in any active maps."
  (interactive (list (read-key "Enter a key: ")))
  (setq user-keys-sequence (vector key)))

(defun user-keys-set-sequence (sequence)
  "Set the SEQUENCE to analyze for buffer or mode maps.
The sequence needs to be bound.  Incomplete sequences will
continue reading.  TODO This seems to behave differently when
called within transient versus directly.  The transient menu has
different maps active."
  (interactive (list (read-key-sequence-vector "Bound key sequence: ")))
  (setq user-keys-sequence sequence)
  ;; there's no feedback when a sequence terminates
  (message "Sequence set!"))

(defun user-keys-set-sequence-string (sequence)
  "Set the SEQUENCE, but use a string input.
This can be useful when `read-key-sequence' will not terminate input
because the input in the active maps is still a prefix."
  (interactive
   (list (let (sequence)
           (while (not sequence)
             (let ((input (read-string "Key description (for `kbd'): ")))
               (if (with-demoted-errors (kbd input))
                   (setq sequence (kbd input))
                 (message "Input must be valid argument to call `kbd' function.")
                 (sit-for 3))))
           sequence)))
  (setq user-keys-sequence sequence))

(defun user-keys-refresh ()
  "Refresh the results buffer."
  (interactive)
  (undefined))

;;;###autoload
(transient-define-prefix user-keys-dispatch ()
  "Controls for user-keys package."
  [["Generate Report"
    ("s" "sequence shadows report" user-keys-report-shadows)
    ("p" "preferred sequences report" user-keys-report-preferred)
    ("t" "stupid sequences report" user-keys-report-stupid)
    ("T" "unbinds" user-keys-generate-unbinds)]
   ["Controls"
    ("h" "toggle menu" transient-quit-one)
    ("g" "refresh" user-keys-refresh)]]
  ["Options"
   [:description user-keys--describe-current-sequence
    ""
    ("k" "set sequence (key)" user-keys-set-sequence-key :transient t)
    ;; setting the sequence with `kbd' doesn't depend on active maps.
    ("S" "set sequence (string)" user-keys-set-sequence-string
     :transient t)
    ("K" "set sequence" user-keys-set-sequence :transient t)]
   [:description user-keys--describe-target-buffer
    ""
    ("b" "active buffer" user-keys-set-buffer :transient t)]])

(defvar user-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map
     (make-composed-keymap '(button-buffer-map special-mode-map)))
    (define-key map "g" 'user-keys-refresh)
    (define-key map "h" 'user-keys-dispatch)
    (define-key map "?" 'user-keys-dispatch)
    (define-key map "q" 'bury-buffer)

    (define-key map "k" 'user-keys-set-sequence-key)
    (define-key map "S" 'user-keys-set-sequence-string)
    (define-key map "K" 'user-keys-set-sequence)

    (define-key map "s" 'user-keys-report-shadows)
    (define-key map "p" 'user-keys-report-preferred)
    (define-key map "t" 'user-keys-report-stupid)
    (define-key map "T" 'user-keys-report-unbinds)
    map))

(define-derived-mode user-keys-mode special-mode
  "user-keys-mode"
  :interactive nil
  :group 'user-keys
  (use-local-map user-keys-mode-map))

;;;###autoload
(defun user-keys-start ()
  "An application to inspect bindings and generate re-bindings."
  (interactive)
  (pop-to-buffer (user-keys--get-buffer))
  (call-interactively #'user-keys-dispatch))

(provide 'user-keys)
;;; user-keys.el ends here
