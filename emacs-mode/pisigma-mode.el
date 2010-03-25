;;; pisigma-mode.el --- PiSigma major mode

;; Author: DarinMorrison
;; Keywords: extensions

;; Copyright (c) 2009, Darin Morrison
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.

;; 3. Neither the name of the author nor the names of his contributors
;; may be used to endorse or promote products derived from this
;; software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; 

;;; History:
;; 

;;; Code:

(require 'eri)
(require 'pisigma-input)

;; Associate .pi files with pisigma-mode
(add-to-list 'auto-mode-alist '("\\.pi\\'" . pisigma-mode))
(modify-coding-system-alist 'file "\\.pi$" 'utf-8)

(defgroup pisigma nil
  "Major mode for editing PiSigma programs."
  :group 'languages)

(defcustom pisigma-mode-hook nil
  "Hook run after entering PiSigma mode."
  :type  'hook
  :group 'pisigma)

(defvar pisigma-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for `pisigma-mode'.")

(defvar pisigma-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\  " "      syntax-table)
    (modify-syntax-entry ?\t " "      syntax-table)
    (modify-syntax-entry ?\' "\'"     syntax-table)
    (modify-syntax-entry ?_  "w"      syntax-table)
    (modify-syntax-entry ?\( "()"     syntax-table)
    (modify-syntax-entry ?\) ")("     syntax-table)
    (modify-syntax-entry ?\[ "(]"     syntax-table)
    (modify-syntax-entry ?\] ")["     syntax-table)
    (modify-syntax-entry ?{  "(}1n"   syntax-table)
    (modify-syntax-entry ?}  "){4n"   syntax-table)
    (modify-syntax-entry ?-  "w 123b" syntax-table)
    (modify-syntax-entry ?\n "> b"    syntax-table)
    syntax-table)
  "Syntax table for `pisigma-mode'.")

;; TODO: 1. matcher functions for type signatures and variables.
;;       2. multiline font-lock for case?
(defvar pisigma-font-lock-keywords
  '(("^\\(:l\\)\\s-*\\([^ \t\n]+\\)"
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))
    ("^\\([[:alpha:]][[:alnum:]_']*\\)[[:space:]]*:"
     (1 font-lock-function-name-face))
    ("^\\([[:alpha:]][[:alnum:]_']*\\)[[:space:]]*="
     (1 font-lock-function-name-face))
    ("\\<Type\\>" . font-lock-type-face)
    ("\\<\\(case\\|in\\|let\\|of\\|split\\|with\\)\\>" . font-lock-keyword-face)
    ("\\[\\|\\]\\|♯\\|!\\|♭\\|\\^\\|∞" . font-lock-warning-face)
    ("\\('\\)\\([[:alpha:]][[:alnum:]_']*\\)"
     (1 font-lock-warning-face)
     (2 font-lock-constant-face)))
  "Keyword highlighting specification for `pisigma-mode'.")

(defvar pisigma-imenu-generic-expression
  nil)

(defvar pisigma-outline-regexp
  nil)

;;; Indentation

(defun pisigma-indent-line ()
  "This is what happens when TAB is pressed."
  (interactive)
  (eri-indent))

(defun agda2-indent-line-reverse ()
  "This is what happens when S-TAB is pressed."
  (interactive)
  (eri-indent-reverse))

;;;###autoload
(define-derived-mode pisigma-mode fundamental-mode "PiSigma"
  "A major mode for editing PiSigma files."
  :syntax-table pisigma-mode-syntax-table
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'font-lock-defaults) '(pisigma-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'pisigma-indent-line)
  (set (make-local-variable 'imenu-generic-expression) pisigma-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) pisigma-outline-regexp)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Protect global value of default-input-method from set-input-method.
  (make-local-variable 'default-input-method)
  (set (make-local-variable 'ident-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 8)
  (set-input-method "PiSigma"))

(provide 'pisigma-mode)
;;; pisigma-mode.el ends here
