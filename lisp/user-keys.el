;;; user-keys.el --- Clean, manage, and inspect your keymaps -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'loadhist)

(eval-when-compile (require 'subr-x))

;; implementation state and constants

(defvar user-keys-sequence nil "Current sequence being analyzed.")

(defvar user-keys-operation nil "Current analysis operation.")

(defvar user-keys-target-buffer nil "Current buffer for inspecting bindings.")

;; TODO f-keys are not currently part of a predicate
(defconst user-keys--fkey-events
  (mapcar
   (lambda (c) (make-symbol (concat "f" (int-to-string c))))
   (number-sequence 1 20))
  "List of function key events, through F20.")

;; TODO stupid events not currently used to create a predicate
(defvar user-keys--stupid-events
  '('insert
    'pause
    'print
    'again
    'begin
    'insertchar
    'insertline
    'prior)
  "Set this to events you can't input on your keyboard.")

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

(defgroup user-keys nil "Clean up, inspect, manage keymaps."
  :group 'convenience)

(defcustom user-keys-buffer-name "*user-keys*"
  "Where should we display interactive output?"
  :type 'string
  :group 'user-keys)

(defcustom user-keys-stupid-modifiers
  '(hyper super)
  "Sequences should almost never require these modifiers.
Valid modifiers are in A-C-H-M-S-s order.  Use `kbd', `vconcat' or
`string-to-vector' and `event-modifiers' to translate Emacs notation to actual
events and modifiers."
  :type '(repeat (repeat symbol))
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

(defcustom user-keys-ignore-maps '(yank-menu
                                   xterm-function-map
                                   key-translation-map
                                   custom-tool-bar-map ;; `keymapp', but not very keymap-ish
                                   ;; handled via special case of global map.
                                   ;; See `user-keys--find'.  Direct lookups in
                                   ;; the global map see a meta offset of the
                                   ;; `esc-map'.
                                   esc-map
                                   function-key-map
                                   ;; TODO widget-global-map is buffer local,
                                   ;; usually a clone of the global map.  We
                                   ;; will need to optionally filter maps when
                                   ;; looking at a buffer
                                   widget-global-map)
  "Some maps are simultaneously very weird and not very useful.
In particular, maps that cause errors because of, for example,
failing the `keymapp' test after autoloaded but not before,
should just be ignored."
  :type '(repeat symbol)
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
     (setq-local truncate-lines t)
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

(defun user-keys--describe-binding (binding)
  "Return a formatted representation of what the BINDING for a key is.
This function handles the numerous possible types of values
that can be returned from `key-binding' and during
`kmu-map-keymap'.  Outputs must be strings."
  (cond
   ((eq nil binding)
    "nil")
   ((or (symbolp binding)
        (stringp binding))
    (user-keys--maybe-button binding))
   ((kmu-prefix-command-p binding)
    (user-keys--maybe-button (kmu-prefix-command-p binding)))
   ((keymapp binding)
    (if (kmu-keymap-variable binding)
        (user-keys--maybe-button (kmu-prefix-command-p binding))
      "<anonymous keymap>"))
   ((vectorp binding) ; maps to another sequence
    (key-description binding))
   ((numberp binding) ; sequence too long
    (format "sequence %s keys too long for prefix" binding))
   ((kmu-menu-binding-p binding)
    (format "menu-item - %s"
            ;; Some menus are... weird.  Because.
            (if (proper-list-p binding)
                (nth 2 binding)
              (if (consp binding)
                  (car binding)
                "%.20S..." binding))))
   ;; TODO we can sometimes get the name out.
   ;; This was originally intended to fix up a weird menu item.
   ((compiled-function-p binding)
    "<compiled-function>")
   ((functionp binding)
    "<function>")
   (t
    (let* ((str-binding (format "%S" binding)))
      (if (> (length str-binding) 20)
          (format "Bound to: %.20s..." str-binding)
        (format "Bound to: %s" str-binding))))))

(defun user-keys--major-mode-keymaps ()
  "Return a list of major modes map symbols."
  (let* ((major-mode-keymaps '())
         (_ (mapatoms
             (lambda (a) (when (get a 'derived-mode-parent)
                      (let ((mode-map-name (derived-mode-map-name a)))
                        (with-demoted-errors "user-keys major mode lookup: %S"
                          (when (keymapp
                                 (user-keys--symbol-to-map mode-map-name))
                            (push mode-map-name major-mode-keymaps)))))))))
    major-mode-keymaps))

(defun user-keys--minor-mode-keymaps (&optional active-only)
  "Return a list of all minor mode keymaps.
Optional ACTIVE-ONLY argument will control if only active maps
are returned."
  (-non-nil
   (--map (let* ((mode-name (car it))
                 (mode-map-name (derived-mode-map-name mode-name)))
            (if (and (or (not active-only) (symbol-value mode-name))
                     (keymapp (user-keys--symbol-to-map mode-map-name)))
                mode-map-name
              nil))
          minor-mode-map-alist)))

(defun user-keys--prefix-keymaps ()
  "Return all prefix commands.
These are functions which return keymaps.  Binding to them
implements prefix maps."
  (kmu-prefix-command-list))

(defun user-keys--emulation-keymaps (&optional active-only)
  "Return a list of all minor mode keymaps.
Optional ACTIVE-ONLY argument will control if only active maps
are returned."
  (-flatten
   (-non-nil
    (--map
     (-non-nil
      (--map (kmu-keymap-variable (cdr it))
             (if (symbolp it) (symbol-value it) it)))
     emulation-mode-map-alists))))

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
    ;; See `user-keys-symbol-to-map' for the reverse operation.
    (mapatoms (lambda (a) (when (or (keymapp a)
                               (and (boundp a) (keymapp (symbol-value a)))
                               (and (fboundp a) (keymapp (symbol-function a))))
                       ;; TODO the local variable logic needs to only remove
                       ;; symbols that point to duplicate values.
                       (unless (or (local-variable-if-set-p a) ; filters some duplicate maps
                                   (indirect-function a)       ; obsolete symbol
                                   (gethash a known-keymaps))
                         (puthash a a other-keymaps)))))
    (--remove (member it user-keys-ignore-maps) (hash-table-keys other-keymaps))))

(defun user-keys--check-intersections ()
  "Diagnostic function for debugging map lists.
Should return a list of empty lists.  If not check the
permutation and find out why the classification overlapped."
  (let* ((map-funcs '(user-keys--major-mode-keymaps
                      user-keys--minor-mode-keymaps
                      user-keys--prefix-keymaps
                      user-keys--emulation-keymaps))
         (most-maps (-flatten (-map #'funcall map-funcs)))
         (other-maps-func (lambda () (user-keys--other-maps most-maps)))
         (map-funcs (append map-funcs (list other-maps-func))))
    (--map
     (cons (list (car it) (cdr it))
           (length (-intersection (funcall (car it)) (funcall (cdr it)))))
     (-flatten
      (--map
       (let ((outer it))
         (pop map-funcs)
         (--map
          (cons outer it)
          map-funcs))
       (seq-copy map-funcs))))))

(defun user-keys--default-maps ()
  "Expand and return user-keys-default-maps.
This is a list of elements, each a list of a heading string and a
list of maps."
  (let* ((basic-mode-maps '(special-mode-map
                            text-mode-map
                            prog-mode-map))
         ;; `override-global-map' appears as an emulation map.
         (major-mode-maps (user-keys--major-mode-keymaps))
         (minor-mode-maps (user-keys--minor-mode-keymaps))
         (prefix-maps (user-keys--prefix-keymaps))
         (emulation-maps (user-keys--emulation-keymaps))
         (most-maps (append '(global-map)
                            basic-mode-maps
                            major-mode-maps
                            minor-mode-maps
                            prefix-maps
                            emulation-maps))
         (other-maps (user-keys--other-maps most-maps)))
    `(("Gobal Map" (global-map))
      ("Basic Mode Maps" ,basic-mode-maps)
      ("Major Mode Maps" ,major-mode-maps)
      ("Minor Mode Maps" ,minor-mode-maps)
      ("Prefix Maps" ,prefix-maps)
      ("Emulation Maps" ,emulation-maps)
      ;; TODO support overriding maps,
      ;; support showing maps in lookup order.
      ("Other Maps" ,other-maps))))

(defun user-keys--maps-to-symbols (maps symbols)
  "Find which SYMBOLS refer to keymaps in MAPS.
Also return any maps that didn't match.

This is useful when trying to reconstruct outputs of
`current-active-maps'.

It would be nice to get a list of these objects as symbols in
order for inferencing keymaps.  This is the hard way."
  (let ((found)
        ;; warns on mapcar, just some dash noise
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

(defun user-keys--symbol-to-map (symbol)
  "Get an actual keymap for SYMBOL.
`keymapp' will return t for all kinds of values.  Keymaps,
autoloads, symbols whose function definitions are keymaps.  This
function encapsulates these little quirks."
  (condition-case
      keymap-error
      (progn
        (let ((value (or (and (boundp symbol) (symbol-value symbol))
                         (and (fboundp symbol) (symbol-function symbol)))))
          ;; Go ahead and eagerly load all keymaps so that we can inspect them.
          ;; Sorry, user memory.
          (when (autoloadp value)
            (autoload-do-load value)
            (setq value (or (and (boundp symbol) (symbol-value symbol))
                            (and (fboundp symbol)(symbol-function symbol)))))
          (unless (keymapp value)
            (error "Values was not a keymap: %s" symbol))
          value))
    (error (message "No keymap could be obtained from symbol: %s" symbol))))

(defun user-keys--symbol-to-feature (symbol &optional ask prompt)
  "Return the feature that will define SYMBOL.
If ASK is non-nil, ask the user, with optional PROMPT.  This
function attempts definitive answers first before using a
heuristic approach and then finally asks the user to handle
degenerate cases."
  (or (when (featurep symbol) symbol)
      (alist-get 'provide (assoc-string (symbol-file symbol) load-history))
      (let ((found) ; attempt to find longest matching feature name.
            (words (string-split (symbol-name symbol) "-")))
        (while (and words (not found))
          (setq words (-butlast words))
          (setq found (locate-library (string-join words "-"))))
        (when words (intern (string-join words "-")))) ; could create new symbols
      (when ask
        (read-feature
         (or prompt
             "Feature loading inference failed.  Please select correct feature:")))))

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

(defun user-keys--sequence-mouse-p (sequence)
  "Return non-nil if SEQUENCE has mouse events."
  (or (member (aref sequence 0) '(menu-bar))
      (-non-nil (--map (-intersection
                        '(click down drag)
                        (event-modifiers it))
                       sequence))))

(defun user-keys--remove-mouse-mods (modifiers)
  "Remove mouse modifiers from MODIFIERS."
  (--remove (member it '(click drag down)) modifiers))

(defun user-keys--real-kbd-mods (key)
  "Remove extraneous modifiers from KEY.
This is for counting the real number of modifiers requiring the
user to press a modifier key."
  (let* ((mods (user-keys--remove-mouse-mods (event-modifiers key)))
         (basic-type (event-basic-type key))
         (phantom-ctl (and (member basic-type '(91 93 105 109))
                           (member 'control mods))))
    (if phantom-ctl (remove 'control mods)
      mods)))

(defun user-keys--esc-offset-event (event)
  "Apply meta to the EVENT."
  (event-apply-modifier event 'meta 27 "M-"))

(defun user-keys--esc-offset-sequence (sequence)
  "Add a meta prefix to the SEQUENCE.
Predicates expect to work on the full sequence.  The `kmu'
package could use some help here to support the special case of
the `esc-map' effectively existing as modified sequences in the
global map.  To emulate this, we apply a meta modifier to the
first event in the sequence unless it already has a meta
modifier."
  (condition-case error
      ;; don't handle degenerate empty sequence
      (let* ((first (aref sequence 0))
             (unroll (and (consp first) (atom (cdr first))))
             (has-meta (member 'meta (event-modifiers
                                      (if unroll (car first) first)))))
        (if has-meta
            sequence
          (let ((sequence sequence))
            (aset sequence 0
                  (if unroll
                      (cons (user-keys--esc-offset-event (car first))
                            (user-keys--esc-offset-event (cdr first)))
                    (user-keys--esc-offset-event first)))
            sequence)))
    (error
     (message "user-keys: offset sequence with meta failed: %s" sequence)
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
  (let (matches
        match-excludes
        (esc-mode (eq keymap esc-map)))
    (kmu-map-keymap
     (lambda (sequence definition)
       (let ((sequence (if esc-mode (user-keys--esc-offset-sequence sequence)
                         sequence))
             (orig-sequence sequence)
             (sequence
              (condition-case error
                  (user-keys--normalize-sequence
                   (user-keys--maybe-unroll sequence))
                (error
                 (message "user-keys: sequence processed raw: %s" sequence)
                 sequence)))
             (exclusions (-non-nil (--map
                                    (funcall it sequence definition)
                                    exclude-predicates))))
         (if exclusions
             (push (list orig-sequence definition exclusions) match-excludes)
           (when-let ((reasons (-non-nil (--map (funcall it sequence definition)
                                                seq-predicates))))
             (push (list orig-sequence definition reasons) matches)))))
     keymap)

    ;; Special case, treat all esc-map entries as their meta-key offset
    ;; sequences in the global map.  Skip the esc-map entirely via
    ;; `user-keys-ignore-maps'.
    (if (eq keymap global-map)
        (pcase-let ((`(,esc-matches ,esc-match-excludes)
                     (user-keys--find esc-map seq-predicates exclude-predicates)))
          (list (append matches esc-matches)
                (append match-excludes esc-match-excludes)))
      (list matches match-excludes))))

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
   (user-keys--insert-rows (plist-get report :data) 0)
   (goto-char 0)))

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

        ;; TODO let's unmix formatting and the structural aspects of rendering
        ;; and data plumbing.  To support rendering unbinds, the core rendering
        ;; logic needs to be factored out.

        ;; column information needs to be extracted prior to
        ;; rendering any rows.
        (let* ((ncols (-max (-map #'length rows)))
               (widths (-map
                        (lambda (n)
                          (1+ (-max (--map
                                     (let ((item (nth n it)))
                                       (length item))
                                     rows))))
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
         (target-buffer (or user-keys-target-buffer
                            (current-buffer)))

         (local-lookups
          (with-current-buffer target-buffer
            (--map
             (let* ((sequences (cadr it))
                    (lookups
                     (--map
                      (list
                       (key-description it)
                       (or (when-let ((description (key-binding it t)))
                             (user-keys--describe-binding description))
                           user-keys--available-string))
                      sequences)))
               (list :header (car it)
                     :rows lookups))
             preferred)))

         ;; TODO this section was being written to work on active
         ;; scan active maps with predicates, combining results with
         ;; map data to augment the key-binding pairs earlier
         ;; maps.  It's not clear what the use case is or how it
         ;; should fit with other use cases.  I left this section
         ;; commented in case someone wants to play around.
         ;; (predicates (--map (user-keys-sequences-predicate
         ;;                     (cadr it) (car it))
         ;;                    preferred))

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

         (report `(:title ,(format "Preferred Sequences in: %s" target-buffer)
                          :data ,local-lookups)))
    (user-keys--render-report report)))

(defun user-keys-report-shadows (sequence &optional maps)
  "Show all keymaps that potentially could shadow SEQUENCE.
MAPS is a list of `(SECTION MAP)' forms.  See
`user-keys--default-maps'."
  (interactive (list (or user-keys-sequence
                         (call-interactively
                          #'user-keys-set-sequence-key))))

  (let* (;; `keymap-lookup' uses string input.  `lookup-key' doc string
         ;; says to prefer `keymap-lookup'.
         (key-str (key-description sequence))
         (data
          (->>
           (or maps (user-keys--default-maps))
           (--map
            (-when-let*
                (((section maps) it)
                 (maps (-sort (lambda (l r) (string< (symbol-name l)
                                                (symbol-name r)))
                              maps))
                 (lookups
                  (->> maps
                       (--map ; it is a single map symbol
                        (let ((binding (with-demoted-errors
                                           "couldn't get keymap for symbol %s"
                                         (keymap-lookup
                                          (user-keys--symbol-to-map it)
                                          key-str))))
                          (when binding
                            (list (user-keys--maybe-button it)
                                  (user-keys--describe-binding binding)))))
                       (-non-nil))))
              (list
               :header section
               :rows lookups)))
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
  ;; TODO esc-map actually defines a lot of double modifiers.  This is because
  ;; of some esoteric implementation detail probably, but it means we can't scan
  ;; the esc-map with the same

  (user-keys--render-report
   `(:title
     "Stupid Keys - bindings that should just not"
     :data
     ,(->>
       (user-keys--default-maps)
       (-map
        (-lambda ((section maps))
          (when-let
              ((data (->>
                      maps
                      (--map
                       (when-let
                           ((map (user-keys--symbol-to-map it))
                            (scanned (car (user-keys--find
                                           map
                                           user-keys-stupid-predicates
                                           user-keys-stupid-exception-predicates)))
                            (display (--map
                                      (list
                                       (key-description (nth 0 it))
                                       (user-keys--describe-binding (nth 1 it))
                                       (mapconcat #'identity (nth 2 it) ", "))
                                      scanned)))
                         (when display (list :header it
                                             :rows display))))
                      (-non-nil))))
            (list
             :header section
             :rows data))))))))

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

(defun user-keys-sequence-too-long-predicate (max-length reason)
  "Return predicate matching sequences with too many keys.
MAX-LENGTH is the longest length that is not stupid.  The REASON
will be returned for reporters."
  (lambda (sequence _)
    (when (and (not (user-keys--sequence-mouse-p sequence))
               (> (length sequence) max-length))
      reason)))

(defun user-keys-multiple-modifiers-predicate (reason)
  "Return predicate matching keys with multiple modifiers.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (when
        (-non-nil
         (--map (> (length (user-keys--real-kbd-mods it)) 1)
                sequence))
      reason)))

(defun user-keys-expanding-modifiers-predicate (reason)
  "Return predicate matching sequences with expanding modifiers.
Expanding modifiers means that the current key uses modifiers
that the previous key did not.  Entering these sequences requires
a change or increase of modifiers.  A decrease of modifiers is
allowed.  The REASON will be returned for reporters."
  (lambda (sequence _)
    (when
        (and (not (user-keys--sequence-mouse-p sequence))
             (let ((mods (--map (user-keys--real-kbd-mods it) sequence)))
               (cadr (--reduce-r-from
                      (list it (append (cadr acc)
                                       (-difference (car acc) it)))
                      (list (-last-item mods) nil) (-butlast mods)))))
      reason)))

(defun user-keys-modifiers-predicate (modifiers reason)
  "Return predicate matching keys with one of MODIFIERS.
The REASON will be returned for reporters."
  (lambda (sequence _)
    (when (-non-nil
           (--map
            (when-let ((event-mods (user-keys--remove-mouse-mods
                                    (event-modifiers it))))
              (-non-nil (--map (member it modifiers) event-mods)))
            sequence))
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

(defun user-keys-all-esc-predicate (sequence _)
  "Match if all keys in the SEQUENCE some variant of the ESC key."
  (unless (seq-find (lambda (k) (not (member k '(27 escape)))) sequence)
    "All escape."))

(defun user-keys-tool-bar-predicate (sequence _)
  "Match if SEQUENCE begins with a mouse event."
  (when (member (event-basic-type (aref sequence 0)) '(tool-bar))
    "Mouse event."))

(defun user-keys-commands-predicate (commands reason)
  "Return a predicate matching COMMANDS.
The REASON will be returned for reporters."
  (lambda (_ description)
    (message "description %s" description)
    (when (member description commands)
      reason)))

;; Had to move this after predicates for loading order
(defcustom user-keys-stupid-predicates
  (list (user-keys-multiple-modifiers-predicate
         "multiple modifiers")
        (user-keys-modified-basic-events-predicate
         user-keys--fkey-events "modified function keys")
        (user-keys-modifiers-predicate user-keys-stupid-modifiers
                                       "difficult modifiers")
        (user-keys-sequence-too-long-predicate 2 "sequence too long")
        (user-keys-expanding-modifiers-predicate
         "expanding modifiers")
        (user-keys-modified-basic-events-predicate
         user-keys-shifted-keys "modified shift keys"))
  "Predicates that match and report stupid bindings."
  :type '(repeat function)
  :group 'user-keys)

(defcustom user-keys-stupid-exception-predicates
  (list #'user-keys-all-esc-predicate
        #'user-keys-tool-bar-predicate)
  "Predicates that except bindings from the stupid dragnet.
Use this to avoid writing stupid predicates with excessively
intelligent over-design.  intelligently."
  :type '(repeat function)
  :group 'user-keys)

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
               (if (with-demoted-errors
                       "`kbd' failed for input: %s"
                       (kbd input))
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
   [:description
    user-keys--describe-current-sequence
    ""
    ("k" "set sequence (key)" user-keys-set-sequence-key :transient t)
    ;; setting the sequence with `kbd' doesn't depend on active maps.
    ("S" "set sequence (string)" user-keys-set-sequence-string
     :transient t)
    ("K" "set sequence" user-keys-set-sequence :transient t)]
   [:description
    user-keys--describe-target-buffer
    ""
    ("b" "active buffer" user-keys-set-target-buffer :transient t)]])

(declare-function helpful-at-point "helpful" ())
(defun user-keys--push-button ()
  "Open help for symbol at point.
Use `helpful' package if loaded."
  (interactive)
  (if (featurep 'helpful)
      (helpful-at-point)
    (describe-symbol (symbol-at-point))))

(defvar user-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map
     (make-composed-keymap '(button-buffer-map special-mode-map)))
    (define-key map (kbd "g") 'user-keys-refresh)
    (define-key map (kbd "h") 'user-keys-dispatch)
    (define-key map (kbd "?") 'user-keys-dispatch)

    (define-key map (kbd "k") 'user-keys-set-sequence-key)
    (define-key map (kbd "S") 'user-keys-set-sequence-string)
    (define-key map (kbd "K") 'user-keys-set-sequence)
    (define-key map (kbd "b") 'user-keys-set-target-buffer)

    (define-key map (kbd "s") 'user-keys-report-shadows)
    (define-key map (kbd "p") 'user-keys-report-preferred)
    (define-key map (kbd "t") 'user-keys-report-stupid)
    (define-key map (kbd "T") 'user-keys-report-unbinds)

    ;; TODO different kinds of values could exist.  Function
    ;; keymaps are an example.
    (define-key map [remap push-button] #'user-keys--push-button)
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
