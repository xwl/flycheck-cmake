flycheck-cmake
==============

Define below two checkers:

- c/c++-cmake
- c/c++-cmake-remote

By analysing cmake compile_commands.json, it defines a precise
flycheck checker (c/c++-cmake) for each cmake project, with exact
compile flags, include paths, etc.

If you are like me, editing in Mac OS X, but compiling inside a
virtual machine, then the other remote cmake checker
(c/c++-cmake-remote) defined here will be very helpful.  What it
does is simply running flycheck over ssh.

## Note

At present, flycheck has to be patched for setting compiler dynamically.

```lisp
diff --git a/flycheck.el b/flycheck.el
index 17e6c3a..f1da106 100644
--- a/flycheck.el
+++ b/flycheck.el
@@ -4360,9 +4360,9 @@ default `:verify' function of command checkers."

     (unless command
       (error "Missing :command in syntax checker %s" symbol))
-    (unless (stringp (car command))
-      (error "Command executable for syntax checker %s must be a string: %S"
-             symbol (car command)))
+    ;; (unless (stringp (car command))
+    ;;   (error "Command executable for syntax checker %s must be a string: %S"
+    ;;          symbol (car command)))
     (dolist (arg (cdr command))
       (unless (flycheck-command-argument-p arg)
         (error "Invalid command argument %S in syntax checker %s" arg symbol)))
@@ -4426,7 +4426,7 @@ the syntax checker definition, if the variable is nil."

 Return the full absolute path to the executable of CHECKER, or
 nil if the executable does not exist."
-  (funcall flycheck-executable-find (flycheck-checker-executable checker)))
+  (funcall flycheck-executable-find (eval (flycheck-checker-executable checker))))

 (defun flycheck-checker-arguments (checker)
   "Get the command arguments of CHECKER."
```

## Usage

See [cmake-compile-commands](https://github.com/xwl/cmake-compile-commands)
