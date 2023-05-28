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

;; TODO exception sequences not implemented in stupid keys report
(defvar user-keys--exception-sequences
  '([f1] [f11])
  "Sequences to except from usual unbinding.
Some key sequences are from desktop environment idioms rather
than traditional Emacs design errors.  While these desktop
environments are also mostly wrong, the cost to benefit is in favor
of keeping them bound.")

(defvar user-keys--available-string (propertize "available" 'face 'success)
  "Nobody has spammed their keymap ads on this key sequence.")

;; helper functions for custom

(defun user-keys-meta-ize (key-letters)
  "Return key-sequence encoded forms for meta modified KEY-LETTERS.
KEY-LETERS should be a list of chars or integers."
  (--map (vconcat (kbd (format "M-%c" it))) key-letters))

(defun user-keys-ctl-fy (key-letters)
  "Return key-sequence encoded forms for ctl modified KEY-LETTERS.
KEY-LETERS should be a list of chars or integers."
  (--map (vconcat (kbd (format "C-%c" it))) key-letters))

;; customize

(defgroup user-keys nil "User keys.")

(defcustom user-keys-stupid-modifiers
  '("S-" "H-" "s-" "C-M-" "M-S-" "C-M-S-")
  "Sequences should almost never require these modifier combinations.
Modifiers are in A-C-H-M-S-s and specified in this order for valid keys"
  :type '(repeat (repeat string))
  :group 'user-keys)

(defcustom user-keys-buffer-name "*user-keys*"
  "Where should we display interactive output?"
  :type 'string
  :group 'user-keys)

(defcustom user-keys-shifted-keys
  (string-to-list "~!@#$%^&*()_+{}|:\"<>?")
  "Keys that have an implied shift modifier."
  :type '(repeat 'character)
  :group 'user-keys)

(defcustom user-keys-preferred-sequences
  (let ((home-row (string-to-list "asdfjkl;"))
        (other-meta (string-to-list "qwertyuiop[]\\gh'zxcvbnm,./"))
        ;; C-m is RET, C-i is TAB...
        ;; Control is just a good preferred modifier...
        (other-ctl (string-to-list "qwertyuop\\gh'zxcvbn,./")))
    `((,(propertize "home meta keys\n" 'face 'success)
       ,(user-keys-meta-ize home-row))
      (,(propertize "home ctl keys\n" 'face 'font-lock-builtin-face)
       ,(user-keys-ctl-fy home-row))
      (,(propertize "meta keys\n" 'face 'font-lock-keyword-face)
       ,(user-keys-meta-ize other-meta))
      (,(propertize "ctl keys\n" 'face 'font-lock-string-face)
       ,(user-keys-ctl-fy other-ctl))))
  "Sequences to look report in `user-keys-report-preferred'.
Set to an expression or function that returns a list of elements.
Each element is a string and a list of key sequence vectors or a
function that will return such a list.

The string will be used to generate the match reasons, which are
shown beside each match.  There can be overlap in the sequences,
in which case all match reasons will be shown.  Sequences are
sorted before display.

By default, some top-level single-modifier keys on an English US
keyboard are considered preferred sequences.  Home row and non-home
row are divided into two groups.

You are welcome to contribute a function that will respect the
user's keyboard layout and language settings.

Here's an example expression that just has two sections,

  '((\"control keys\"
     ,(--map (vector it (number-sequence ?\C-a ?\C-z))))
    (\"home left meta keys\"
     '([134217825] [134217843] [134217828] [134217830])))

Key sequences have a lot of variety.  Too much.  Use `kbd' and
`vconcat' in ielm to output sequences and `key-description' to see
what the outputs mean in Emacs style key sequence notation."

  :type '(repeat (list string (choice function (list key-sequence))))
  :group 'user-keys)

;; implementation functions

(defun user-keys--get-buffer ()
  "Obtain the report buffer."
  (let ((buffer (get-buffer user-keys-buffer-name)))
    (or buffer
        (let ((buffer (get-buffer-create user-keys-buffer-name)))
          (set-buffer buffer)
          (user-keys-mode)
          buffer))))

;; TODO tests for this macro
(defmacro user-keys--with-buffer (&rest body)
  "Wrap the evaluation of BODY forms with buffer handling.
We don't want to get read-only at the wrong time or execute in the
wrong buffer's context."
  `(progn
     (set-buffer (user-keys--get-buffer))
     (setq buffer-read-only nil)
     (condition-case form-value
         (progn ,@body)
       ((debug error)
        (setq buffer-read-only t)
        ;; propagate errors from BODY
        (signal (car form-value) (cadr form-value)))
       (:success
        (setq buffer-read-only t)
        form-value))))

(defun user-keys--pop-to-buffer ()
  "Pop to the reporting buffer."
  (pop-to-buffer (user-keys--get-buffer)))

(defsubst user-keys--maybe-button (text-or-symbol)
  "Format TEXT-OR-SYMBOL as a button that displays its documentation."
  (if (symbolp text-or-symbol)
      (propertize (symbol-name text-or-symbol)
                  'face 'button
                  'button '(t)
                  'category 'default-button)
    text-or-symbol))

;; TODO make click actions appropriate for types.  Keys should show shadows.
;; Maps should show preferred and stupid.  Modes should show active maps etc.
(defsubst user-keys--section-header (text-or-symbol indentation)
  "Make a dividing section header for TEXT-OR-SYMBOL.
Indent by INDENTATION.  If not indented, add an underline since
this is a proper section.  Make the symbol a button if it
probably should be clickable."
  (let ((header-text (user-keys--maybe-button text-or-symbol)))
    (if (> indentation 0)
        (format "\n%s%s\n" (make-string indentation ?\s) header-text)
      (format "\n%s\n%s\n\n" header-text
              (make-string (length header-text) ?\-)))))

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

(defun user-keys--minor-mode-keymaps (&optional active-only)
  "Return a list of all minor mode keymaps.
Optional ACTIVE-ONLY argument will control if only active maps
are returned."
  (-non-nil
   (--map (let* ((mode-name (car it))
                 (mode-map-name (derived-mode-map-name mode-name)))
            (if (and (or (not active-only) (symbol-value mode-name))
                     (boundp mode-map-name)
                     (keymapp (symbol-value mode-map-name)))
                mode-map-name
              nil))
          minor-mode-map-alist)))

(defun user-keys--other-maps(keymaps)
  "Return a list of all keymaps not in KEYMAPS.
KEYMAP-LISTS is a list of lists of map symbols."
  (let ((other-keymaps (make-hash-table :test 'eq))
        (known-keymaps (make-hash-table :test 'eq)))
    (mapc
     (lambda (map)
       (puthash map map known-keymaps))
     keymaps)

    ;; TODO keymapp on symbols that have symbol-function is annoying
    (mapatoms (lambda (a) (when (or (keymapp a)
                               (and (boundp a) (keymapp (symbol-value a))))
                       (unless (gethash a known-keymaps)
                         (puthash a a other-keymaps)))))
    (hash-table-keys other-keymaps)))

(defun user-keys--maps-to-symbols (maps symbols)
  "Find which SYMBOLS refer to keymaps in MAPS.
Also return any maps that didn't match.

This is useful when trying to reconstruct outputs of
`current-active-maps'.

It would be nice to get a list of these objects as symbols in
order for inferencing keymaps.  This is the hard way."
  (let ((found)
        (maps (--remove (equal it '(keymap)) maps)))
    (--map
     (let ((m (if (boundp it)
                  (symbol-value it)
                (symbol-function it))))
       (when (seq-contains-p maps m #'eq) ; only match objects
         (setq maps (delete m maps))
         (push it found)))
     symbols)
    `(,found ,maps)))

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

(defun user-keys--render-report (report)
  "Generic output function for similar-ishly structured reports.
REPORT is a plist.
:title - This header is rendered first.  Any string supported.
:data - A list of elements.  Each element is a plist with :header
  :col-labels and :rows.

Each :data element will be rendered as a section and formatted so
that :col-labels, if present, will be printed and aligned above
data in :rows."
  (user-keys--with-buffer
   (erase-buffer)
   ;; TODO title is less prominent than section headers
   (insert (plist-get report :title) "\n")
   (user-keys--insert-rows (plist-get report :data) 0)))

(defun user-keys--insert-rows (rows indentation)
  "Recursively insert ROWS into the buffer.
Begin each row with INDENTATION spaces.  Descend if ROWS is a
plist containing :header, then print :rows.  This can handle
recursive plists."
  (when rows ; TODO filter empty sections during generation
    (let ((header (plist-get rows :header)))
      (when header
        (insert (user-keys--section-header header indentation))))

    (let* ((rows (or (plist-get rows :rows) rows))
           (recursive (plist-get (car rows) :rows)))
      (if recursive
          ;; if each row contains embedded rows, recurse into each
          ;; iteration.  If not, just render each row.
          (--each rows (user-keys--insert-rows it (+ 2 indentation)))

        ;; TODO add col-labels to the header row after the widths
        ;; are known, just before inserting rows.  Probably
        ;; requires backup up in the buffer and popping a mark.

        ;; column information needs to be extracted prior to
        ;; rendering any rows.
        (let* ((ncols (-max (-map #'length rows)))
               (widths (-map
                        (lambda (n)
                          (1+ (-max (--map (length (nth n it)) rows))))
                        (number-sequence 0 (1- ncols)))))
          (--each rows
            (apply #'insert
                   `(,(make-string indentation ?\s)
                     ,@(-interpose
                        " "
                        (--map-indexed
                         (let* ((width (nth it-index widths))
                                (str (if (stringp it) it (format "%s" it)))
                                (short (- width (length str))))
                           (concat str (make-string short ?\s)))
                         it))
                     "\n"))))))))

(defun user-keys-report-preferred ()
  "Show each of the user's preferred sequences in the current buffer."
  (interactive)
  ;; TODO normalize to ask for target buffer when unassigned

  ;; expand the preferred sequences if they contained functions
  ;; TODO test with functions
  (let* ((preferred (--map
                     (let ((seqs (cadr it)))
                       `(,(car it) ,(if (functionp seqs)
                                        (funcall seqs)
                                      seqs)))
                     user-keys-preferred-sequences))

         ;; First we want to see what bindings will be calculated.
         ;; This lookup doesn't tell us which map or why, but it does
         ;; tell us what the result will be.
         (local-lookups (with-current-buffer user-keys-target-buffer
                          (--map
                           (let* ((sequences (cadr it))
                                  (lookups
                                   (--map
                                    (list (key-description it)
                                          (or (when-let ((description (key-binding it t)))
                                                (cond
                                                 ((or (symbolp description)
                                                      (stringp description))
                                                  (user-keys--button description))
                                                 ((keymapp description)
                                                  "<keymap>")
                                                 ((t)
                                                  (format "Bound to: %.20S..." description))))
                                              user-keys--available-string))
                                    sequences)))
                             (list :header (car it)
                                   :rows lookups))
                           preferred)))

         ;; scan active maps with predicates, combining results with
         ;; map data to augment the key-binding pairs earlier
         (predicates (--map (user-keys-sequences-predicate
                             (cadr it) (car it))
                            preferred))

         ;; TODO perhaps abstract some of these functions and behaviors to a higher level.
         ;; TODO this section was being written to work on active
         ;; maps.  It's not clear what the use case is or how it
         ;; should fit with other use cases.  I left this section
         ;; commented in case someone wants to play around.
         ;; (active-map-symbols
         ;;  (let ((results (user-keys--maps-to-symbols
         ;;                  (current-active-maps) (user-keys--other-maps '()))))
         ;;    (when (cadr results)
         ;;      ;; this shouldn't ruin anyone's day, but it is weird.
         ;;      (message "user-keys: Some maps could not be resolved to symbols."))
         ;;    (car results)))

         ;; ;; then report on active maps (which may save time by revealing shadows)
         ;; (scanned-lookups (--map
         ;;                   (list :header it
         ;;                         :rows (user-keys--find it predicates))
         ;;                   active-map-symbols))

         (report `(:title ,(format "Preferred Sequences in: %s" user-keys-target-buffer)
                          :data ,local-lookups)))
    (user-keys--render-report report)))

(defun user-keys-report-shadows (sequence)
  "Show all keymaps that potentially could shadow SEQUENCE."
  (interactive (list (or user-keys-sequence
                         (call-interactively
                          #'user-keys-set-sequence-key))))
  (let* ((basic-mode-maps '(fundamental-mode-map
                            special-mode-map
                            text-mode-map
                            prog-mode-map))
         ;; `override-global-map' appears as an emulation map.
         (high-precedence-maps '(override-global-map))
         (major-mode-maps (user-keys--major-mode-keymaps))
         (minor-mode-maps (user-keys--minor-mode-keymaps))
         (most-maps (append '(global-map)
                            high-precedence-maps
                            basic-mode-maps
                            major-mode-maps
                            minor-mode-maps))
         (other-maps (user-keys--other-maps most-maps))

         ;; `keymap-lookup' uses string input.  `lookup-key' doc string
         ;; says to prefer `keymap-lookup'.
         (key-str (key-description sequence))

         (lookups (->>
                   `(,'(global-map)
                     ,high-precedence-maps
                     ,basic-mode-maps
                     ,major-mode-maps
                     ,minor-mode-maps
                     ,other-maps)
                   (--map (-sort #'string< it))
                   (--map ; it is a list of maps
                    (-non-nil
                     (--map ; it is a single map
                      (let ((binding (with-demoted-errors
                                         (keymap-lookup
                                          (if (boundp it)
                                              (symbol-value it)
                                            (symbol-function it)) ; quirk!
                                          key-str))))
                        (when (and binding (symbolp binding))
                          (list it binding)))
                      it)))))
         (sections '("Global Map"
                     "High Precedence Maps"
                     "Basic Mode Maps"
                     "Major Mode Maps"
                     "Minor Mode Maps"
                     ;; TODO support overriding maps,
                     ;; support showing maps in lookup order.
                     "Other Maps"))
         (data (->>
                (-zip sections lookups)
                (--map (when (cdr it) (list
                                       :header (car it)
                                       :rows (cdr it))))
                (-non-nil)))
         (report `(:title
                   ,(format
                     "Shadows for %s"
                     (propertize key-str 'face 'success))
                   :data ,data)))
    (user-keys--render-report report)))

(defun user-keys-report-stupid ()
  "Show all of the stupid key sequences that are currently bound."
  (interactive)
  ;; scan all keymaps and present each one as a section.
  ;; TODO it may be more user friendly to look at a single keymap or
  ;; buffer to see the preferred and stupid bindings.
  (let* ((basic-mode-maps '(fundamental-mode-map
                            special-mode-map
                            text-mode-map
                            prog-mode-map))
         ;; `override-global-map' appears as an emulation map.
         (high-precedence-maps '(override-global-map))
         (major-mode-maps (user-keys--major-mode-keymaps))
         (minor-mode-maps (user-keys--minor-mode-keymaps))
         (most-maps (append '(global-map)
                            high-precedence-maps
                            basic-mode-maps
                            major-mode-maps
                            minor-mode-maps))
         (other-maps (user-keys--other-maps most-maps))

         (predicates (list (user-keys-multiple-modifiers-predicate
                            "multiple modifiers")
                           (user-keys-modified-basic-events-predicate
                            user-keys--fkey-events "modified function keys")
                           ;; TODO add predicate for stupid modifiers like hyper
                           (user-keys-modified-basic-events-predicate
                            user-keys-shifted-keys "modified shift keys")))

         ;; Scan each map with predicates and amend results by
         ;; appending the map name to each value.
         (lookups (->>
                   `((global-map)
                     ,high-precedence-maps
                     ,basic-mode-maps
                     ,major-mode-maps
                     ,minor-mode-maps
                     ,other-maps)
                   (--map (-sort #'string< it))
                   (--map               ; it is a list of map symbols
                    (-non-nil
                     (--map             ; it is a single map symbol
                      ;; TODO add sub-title support
                      (let ((map (if (boundp it)
                                     (symbol-value it)
                                   (symbol-function it)))) ; quirk
                        (list :title (symbol-name it)
                              :rows (user-keys--find map predicates)))
                      it)))))

         (sections '("Global Map"
                     "High Precedence Maps"
                     "Basic Mode Maps"
                     "Major Mode Maps"
                     "Minor Mode Maps"
                     "Other Maps"))
         (data (->>
                (-zip sections lookups)
                (--map (when (cdr it) (list
                                       :header (car it)
                                       :rows (cdr it))))
                (-non-nil)))
         (report `(:title "Stupid Keys - bindings that should just not"
                   :data ,data)))
    (user-keys--render-report report)))

(defun user-keys-generate-unbinds (output-type)
  "Generate an unbinding expression for OUTPUT-TYPE."
  (interactive)

  (undefined))

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
