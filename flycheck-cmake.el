;;; flycheck-cmake.el --- flycheck checker based on cmake

;; Copyright (C) 2016 William Xu

;; Authors: William Xu <william.xwl@gmail.com>

;; URL: https://github.com/xwl/flycheck-cmake
;; Version: 0.1
;; Package-Requires: ((cmake-compile-commands "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; By analysing cmake compile_commands.json, it defines a precise
;; flycheck checker (c/c++-cmake) for each cmake project, with exact
;; compile flags, include paths, etc.

;; If you are like me, editing in Mac OS X, but compiling inside a
;; virtual machine, then the other remote cmake checker
;; (c/c++-cmake-remote) defined here will be very helpful.  What it
;; does is simply running flycheck over ssh.

;;; Code:

(require 'cl)

(require 'cmake-compile-commands)

(defgroup flycheck-cmake nil
  "Config for flycheck-cmake."
  :group 'tools)

(defcustom flycheck-cmake-remote-host ""
  "Remote host argument for connecting via ssh.
e.g., USER@HOST"
  :group 'flycheck-cmake
  :type 'string)

(defcustom flycheck-cmake-remote-shell ""
  "Remote shell wrapper for running compile command.
Optional, can be empty.  e.g., \"sh -c\""
  :group 'flycheck-cmake
  :type 'string)

(flycheck-define-checker c/c++-cmake
  "A C/C++ syntax checker using compile_commands.json generated by cmake.

See `cmake-compile-commands-json-files' for how to config."
  :command
  ("c++"
   (eval (cmake-compile-commands-args))

   "-iquote" (eval (cmake-compile-commands-source-directory))

   "-x" (eval (pcase major-mode
                (`c++-mode "c++")
                (`c-mode "c")))
   ;; GCC performs full checking only when actually compiling, so
   ;; `-fsyntax-only' is not enough. Just let it generate assembly
   ;; code.
   "-S" "-o" null-device
   ;; Read from standard input
   "-")

  :standard-input t

  :error-patterns
  ((error line-start
          (message "In file included from") " " (or "<stdin>" (file-name))
          ":" line ":" column ":" line-end)
   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))

  :error-filter
  (lambda (errors)
    (flycheck-fold-include-levels (flycheck-sanitize-errors errors)
                                  "In file included from"))

  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(flycheck-define-checker c/c++-cmake-remote
  "A C/C++ syntax checker similar to c/c++-cmake, but running on a remote machine via ssh."
  :command
  ("ssh"
   (eval flycheck-cmake-remote-host)
   (eval flycheck-cmake-remote-shell)
   "\""

   (eval (cmake-compile-commands-compiler))
   (eval (cmake-compile-commands-args))
   "-iquote" (eval (cmake-compile-commands-source-directory))
   "-x" (eval (pcase major-mode
                (`c++-mode "c++")
                (`c-mode "c")))
   ;; GCC performs full checking only when actually compiling, so
   ;; `-fsyntax-only' is not enough. Just let it generate assembly
   ;; code.
   "-S" "-o" null-device
   ;; Read from standard input
   "-"

   "\"")

  :standard-input t

  :error-patterns
  ((error line-start
          (message "In file included from") " " (or "<stdin>" (file-name))
          ":" line ":" column ":" line-end)
   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))

  :error-filter
  (lambda (errors)
    (flycheck-fold-include-levels (flycheck-sanitize-errors errors)
                                  "In file included from"))

  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

(add-to-list 'flycheck-checkers 'c/c++-cmake 'append)
(add-to-list 'flycheck-checkers 'c/c++-cmake-remote 'append)

(add-to-list 'flycheck-before-syntax-check-hook
             (lambda ()
               (when (and (eq flycheck-checker 'c/c++-cmake)
                          (null flycheck-c/c++-cmake-executable))
                 (setq-local flycheck-c/c++-cmake-executable
                             (cmake-compile-commands-compiler)))))

(provide 'flycheck-cmake)
;;; flycheck-cmake.el ends here
