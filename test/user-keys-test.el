;;; user-keys-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  <author>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Run the batch tests from root directory:
;; nix shell .github#emacsGit --quick --script .github/run-shim.el -- test
;; Test dependencies can be provided to the Emacsen declared inside the root
;; flake.nix.

;; For local development, dependencies should be installed by the user.  Tests
;; can be run from one of the project files using the `erk-ert-project'
;; command.

;;; Code:

(require 'ert)
(require 'user-keys)

(ert-deftest user-keys--maybe-button-test ()
  (user-keys--maybe-button 'switch-frame))

(ert-deftest user-keys--keymaps-test ()
  (should (equal (user-keys--maybe-unroll [1 2 3]) [1 2 3]))
  (should (equal (user-keys--maybe-unroll [1 2 (4 . 3)]) [1 2 4])))

(ert-deftest user-keys--major-mode-maps-test ()
  (should (member 'emacs-lisp-mode-map (user-keys--major-mode-keymaps)))
  (should (not (member 'prog-mode-map (user-keys--major-mode-keymaps))))
  (should (not (member 'global-map (user-keys--major-mode-keymaps)))))

(ert-deftest user-keys--minor-mode-maps-test ()
  (should (member 'context-menu-mode-map (user-keys--minor-mode-keymaps)))
  (should (not (member 'context-menu-mode-map (user-keys--minor-mode-keymaps t))))
  (should (not (member 'prog-mode-map (user-keys--minor-mode-keymaps)))))

(ert-deftest user-keys--other-mode-maps-test ()
  (should (not (member 'emacs-lisp-mode-map
                       (user-keys--other-maps
                        (user-keys--major-mode-keymaps)))))
  (should (member 'global-map (user-keys--other-maps
                               (user-keys--major-mode-keymaps)))))

(ert-deftest user-keys--maps-to-symbols-test ()
  (should (equal '((global-map) nil)
                 (user-keys--maps-to-symbols
                  (list (current-global-map)) '(global-map))))
  (should (equal `(nil (,global-map))
                 (user-keys--maps-to-symbols
                  (list (current-global-map)) '()))))

(ert-deftest user-keys--symbol-to-map-test ()
  ;; sanity check on the number of maps in the test, about 600 with packages
  ;; installed
  (should (> (length (user-keys--other-maps '())) 50))
  (should (let ((bad-maps '()))
            (mapc (lambda (km)
                    (unless (keymapp (user-keys--symbol-to-map km))
                      (push km bad-maps)))
                  (user-keys--other-maps '()))
            (not bad-maps))))

(ert-deftest user-keys--find-test ()
  ;; no predicates, empty list
  (should (equal (user-keys--find global-map nil) '(nil nil)))

  ;; predicate matching all sequences results in nil matches and full excludes
  (pcase-let ((`(,matches ,excludes) (user-keys--find global-map
                                                      (list (lambda (_s _d) "because")))))
    (should (> (length matches) 10))
    (should (not excludes)))

  ;; predicate matching all sequences but also all excludes results in
  ;; empty matches and full excludes
  (pcase-let ((`(,matches ,excludes) (user-keys--find global-map
                                                      (list (lambda (_s _d) "because"))
                                                      (list (lambda (_s _d) "oh-no")))))
    (should (not matches))
    (should (> (length excludes) 10))))

(ert-deftest user-keys-key-predicate-test ()
  (let ((predicate (user-keys-key-predicate ?\C-x "reason"))
        (map (make-sparse-keymap)))

    ;; target key not bound, don't match
    (define-key map (kbd "C-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; match after key is bound
    (define-key map (kbd "C-x") #'forward-word)
    (should (equal (user-keys--find map (list predicate))
                   '((([24] forward-word ("reason"))) nil)))))

(ert-deftest user-keys-sequences-predicate-test ()
  (let ((predicate (user-keys-sequences-predicate '([134217825]) "i like these"))
        (map (make-sparse-keymap)))

    ;; target key not bound, don't match
    (define-key map (kbd "C-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; match after a correct sequence is bound
    (define-key map (kbd "M-a") #'forward-word)
    (should (equal (user-keys--find map (list predicate))
                   '((([27 97] forward-word ("i like these"))) nil)))))

(ert-deftest user-keys-sequence-too-long-predicate-test ()
  (let ((predicate (user-keys-sequence-too-long-predicate 2 "don't have all day"))
        (map (make-sparse-keymap)))

    ;; target key with a sequence not too long
    (define-key map (kbd "C-a C-c") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; match after a correct sequence is bound
    (define-key map (kbd "C-s C-t u") #'forward-word)
    (should (equal (user-keys--find map (list predicate))
                   '((([19 20 117] forward-word ("don't have all day"))) nil)))))


(ert-deftest user-keys-basic-events-predicate-test ()
  (let ((predicate (user-keys-basic-events-predicate (string-to-list "a") "a reason"))
        (map (make-sparse-keymap)))

    ;; bind another basic event, match nothing
    (define-key map (kbd "C-e") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; match after matching event is bound
    (define-key map (kbd "C-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([1] forward-char ("a reason"))) nil)))))

(ert-deftest user-keys-multiple-modifiers-predicate-test ()
  (let ((predicate (user-keys-multiple-modifiers-predicate "broken fingers"))
        (map (make-sparse-keymap)))

    ;; bind a single modified key and no matches result
    (define-key map (kbd "C-e") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind a key with multiple modifiers and get a match

    ;; note: view [27 1] with `key-description' as it is not normalized
    ;; and depends on the local state of map.
    (define-key map (kbd "C-M-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([27 1] forward-char ("broken fingers"))) nil)))))

(ert-deftest user-keys-modifiers-predicate-test ()
  (let ((predicate (user-keys-modifiers-predicate '(hyper) "broken fingers"))
        (map (make-sparse-keymap)))

    ;; bind a modified event not in the list and no matches result
    (define-key map (kbd "s-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind a key with a matching modifier and get a result

    ;; note: view [27 1] with `key-description' as it is not normalized
    ;; and depends on the local state of map.
    (define-key map (kbd "H-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([16777313] forward-char ("broken fingers"))) nil)))))

(ert-deftest user-keys-modified-basic-events-predicate-test ()
  (let ((predicate (user-keys-modified-basic-events-predicate
                    '(f1)
                    "broken fingers"))
        (map (make-sparse-keymap)))

    ;; bind a single modified key and no matches result
    (define-key map (kbd "<f1>") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind a key with multiple modifiers and get a match
    (define-key map (kbd "C-<f1>") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([C-f1] forward-char ("broken fingers"))) nil)))))

(ert-deftest user-keys-one-mod-events-predicate-test ()
  (let ((predicate (user-keys-one-mod-events-predicate
                    '(meta)
                    '(97 98 99) ; a b c
                    "my favorites"))
        (map (make-sparse-keymap)))

    ;; bind an unmodified key and see no matches
    (define-key map (kbd "a") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind an a key with wrong modifier, get no matches
    (define-key map (kbd "C-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; write modifier, wrong key
    (define-key map (kbd "M-d") #'forward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind a key with correct modifier and correct events, get a match

    ;; note:  view [27 97] with `key-description' as it is not normalized
    ;; and depends on the local state of map.
    (define-key map (kbd "M-a") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([27 97] forward-char ("my favorites"))) nil)))))

(ert-deftest user-keys-commands-predicate-test ()
   (let ((predicate (user-keys-commands-predicate
                    '(forward-char)
                    "too forward thinking"))
         (map (make-sparse-keymap)))

    ;; bind wrong command, get no results
    (define-key map (kbd "a") #'backward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind correct command, get results
    (define-key map (kbd "a") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([97] forward-char ("too forward thinking"))) nil)))))

(ert-deftest user-keys-expanding-modifiers-predicate-test ()
  (let ((predicate (user-keys-expanding-modifiers-predicate
                    "amputated my fingers"))
        (map (make-sparse-keymap)))

    ;; bind decreasing modifiers, no problem
    (define-key map (kbd "A-C-H-M-s-f A-C-H-M-c C-M-d q") #'backward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind no modifiers, no problem
    (define-key map (kbd "n") #'backward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind phantom modifier, no problem (using C-m or RET)
    (define-key map (kbd "M-q C-M-m") #'backward-char)
    (should (equal (user-keys--find map (list predicate)) '(nil nil)))

    ;; bind increasingly weird modifiers, problem
    (define-key map (kbd "C-f C-M-q C-H-M-z") #'forward-char)
    (should (equal (user-keys--find map (list predicate))
                   '((([6 27 17 27 16777242]
                       forward-char ("amputated my fingers")))
                     nil)))))

(ert-deftest user-keys--default-maps-test ()
  (should (equal (cadr (car (user-keys--default-maps))) '(global-map))))

(ert-deftest user-keys-report-shadows-test ()
    (user-keys-report-shadows [24]))

(ert-deftest user-keys--symbol-to-feature-test ()
  (should
   (eq (user-keys--symbol-to-feature 'org-really-long-fake-symbol) 'org))
  (should
   (eq (user-keys--symbol-to-feature 'backquote) 'backquote))
  (should
   (eq (user-keys--symbol-to-feature 'backquote-fake-symbol) 'backquote))
  (should
   (eq (user-keys--symbol-to-feature 'this-symbol-does-not-exist) nil)))

(ert-deftest user-keys--esc-offset-sequence ()
  (should
   (equal (user-keys--esc-offset-sequence [paste]) [M-paste]))
  (should
   (equal (user-keys--esc-offset-sequence [97]) [134217825]))
  (should
   (equal (user-keys--esc-offset-sequence [(97 . 98)]) [(134217825 . 134217826)])))

(ert-deftest user-keys--real-kbd-mods-test ()
  (should
   ;; strip phantom mod and obtain nothing
   (equal (user-keys--real-kbd-mods (kbd "C-m")) nil))
  (should
   ;; return correct modifiers after stripping phantom mod
   (equal (user-keys--real-kbd-mods (kbd "C-M-[") '(meta))))
  (should
   ;; no modifiers, no problem
   (equal (user-keys--real-kbd-mods (kbd "m")) nil))
  (should
   ;; don't remove non-phantom control
   (equal (user-keys--real-kbd-mods (kbd "C-j")) '(control))))

(provide 'user-keys-test)
;;; user-keys-test.el ends here.

;; Local Variables:
;; jinx-local-words: #("Pcase" 0 5 (jinx--group "Accept and save word" jinx--suffix #(" [File]" 0 7 (face jinx-annotation))))
;; End:
