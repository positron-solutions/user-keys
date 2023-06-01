;;; user-keys-abstract-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

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
(require 'user-keys-abstract)

(ert-deftest user-keys-abstract--expand-shadow-remaps-test ()
  (should
   (equal
    ;; test with a single inline shadow-remap
    (user-keys-abstract--expand-shadow-remaps
     '((abstract-forward "C-n" forward-word)
       (org org-mode-map org-forward-word)))

    '((org
       (define-key org-mode-map
                   (kbd "C-n")
                   nil t))
      (org
       (define-key org-mode-map
                   (vector 'remap #'abstract-forward)
                   #'org-forward-word)))))

  (should
   (equal
    ;; test expansion of multiple shadow maps and inline.
    ;; result should be flat (feature define-key) forms
    (user-keys-abstract--expand-shadow-remaps
     '((abstract-forward "C-n" forward-word)
       (foo foo-mode-map foo-forward-word)
       (bar (bar-one-mode-map bar-reverse)
            (bar-two-mode-map bar-double))))
    '((foo
       (define-key foo-mode-map
                   (kbd "C-n")
                   nil t))
      (foo
       (define-key foo-mode-map
                   (vector 'remap #'abstract-forward)
                   #'foo-forward-word))
      (bar
       (define-key bar-one-mode-map
                   (kbd "C-n")
                   nil t))
      (bar
       (define-key bar-one-mode-map
                   (vector 'remap #'abstract-forward)
                   #'bar-reverse))
      (bar
       (define-key bar-two-mode-map
                   (kbd "C-n")
                   nil t))
      (bar
       (define-key bar-two-mode-map
                   (vector 'remap #'abstract-forward)
                   #'bar-double))))))

(ert-deftest user-keys-abstract-define-remap-test ()
  (should
   (equal
    (macroexpand
     '(user-keys-abstract-define-remap
       ((abstract-forward "C-n" forward-word)
        (org org-mode-map org-forward-word))))
    '(progn
       (defun abstract-forward 'nil "Abstract command target.
This is a global map abstract command."
              (interactive)
              (undefined))
       (define-key global-map
                   (kbd "C-n")
                   #'abstract-forward)
       (define-key global-map
                   (vector 'remap #'abstract-forward)
                   #'forward-word)
       (eval-after-load org
         (define-key org-mode-map
                     (kbd "C-n")
                     nil t)
         (define-key org-mode-map
                     (vector 'remap #'abstract-forward)
                     #'org-forward-word)))))
  (should
   (equal
    (macroexpand
     `(user-keys-abstract-define-remap
       ((abstract-forward "C-n" forward-word)
        (bar (bar-one-mode-map bar-reverse)
             (bar-two-mode-map bar-double)))))
    '(progn
       (defun abstract-forward 'nil "Abstract command target.
This is a global map abstract command."
              (interactive)
              (undefined))
       (define-key global-map
                   (kbd "C-n")
                   #'abstract-forward)
       (define-key global-map
                   (vector 'remap #'abstract-forward)
                   #'forward-word)
       (eval-after-load bar
         (define-key bar-one-mode-map
                     (kbd "C-n")
                     nil t)
         (define-key bar-one-mode-map
                     (vector 'remap #'abstract-forward)
                     #'bar-reverse)
         (define-key bar-two-mode-map
                     (kbd "C-n")
                     nil t)
         (define-key bar-two-mode-map
                     (vector 'remap #'abstract-forward)
                     #'bar-double)))))

  (should
   (equal
    (macroexpand
     `(user-keys-abstract-define-remap
       ((abstract-quit "C-g" keyboard-quit)
        (foo foo-mode-map foo-quit))
       ((abstract-forward "C-n" forward-word)
        (bar (bar-one-mode-map bar-reverse)
             (bar-two-mode-map bar-double)))))
    '(progn
       (defun abstract-quit 'nil "Abstract command target.
This is a global map abstract command."
              (interactive)
              (undefined))
       (defun abstract-forward 'nil "Abstract command target.
This is a global map abstract command."
              (interactive)
              (undefined))
       (define-key global-map
                   (kbd "C-g")
                   #'abstract-quit)
       (define-key global-map
                   (vector 'remap #'abstract-quit)
                   #'keyboard-quit)
       (define-key global-map
                   (kbd "C-n")
                   #'abstract-forward)
       (define-key global-map
                   (vector 'remap #'abstract-forward)
                   #'forward-word)
       (eval-after-load foo
         (define-key foo-mode-map
                     (kbd "C-g")
                     nil t)
         (define-key foo-mode-map
                     (vector 'remap #'abstract-quit)
                     #'foo-quit))
       (eval-after-load bar
         (define-key bar-one-mode-map
                     (kbd "C-n")
                     nil t)
         (define-key bar-one-mode-map
                     (vector 'remap #'abstract-forward)
                     #'bar-reverse)
         (define-key bar-two-mode-map
                     (kbd "C-n")
                     nil t)
         (define-key bar-two-mode-map
                     (vector 'remap #'abstract-forward)
                     #'bar-double))))))

(provide 'user-keys-abstract-test)
;;; user-keys-abstract-test.el ends here.
