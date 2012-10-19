;;; ob-scala.el --- org-babel functions for Scala evaluation

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Andrzej Lichnerowicz
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Currently only supports the external execution.  No session support yet.

;;; Requirements:
;; - Scala language :: http://www.scala-lang.org/
;; - Scala major mode :: Can be installed from Scala sources
;;  https://github.com/scala/scala-dist/blob/master/tool-support/src/emacs/scala-mode.el

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function scala-run-scala "ext:scala-mode" (cmd))

(defvar org-babel-tangle-lang-exts) ;; Autoloaded
(add-to-list 'org-babel-tangle-lang-exts '("scala" . "scala"))
(defvar org-babel-default-header-args:scala '())
(defvar org-babel-scala-command "scala"
  "Name of the command to use for executing Scala code.")

(defvar org-babel-scala-eoe-indicator ":org_babel_scala_eoe"
  "String to indicate that evaluation has completed.")
(defvar org-babel-scala-f-write
  "File.open('%s','w'){|f| f.write((_.class == String) ? _ : _.inspect)}")
(defvar org-babel-scala-pp-f-write
  "File.open('%s','w'){|f| $stdout = f; pp(results); $stdout = orig_out}")

(defun org-babel-variable-assignments:scala (params)
  "Return list of scala statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-scala-var-to-scala (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-scala-var-to-scala (var)
  "Convert VAR into a scala variable.
Convert an elisp value into a string of scala source code
specifying a variable of the same value."
  (if (listp var)
      (concat "List(" (mapconcat #'org-babel-scala-var-to-scala var ", ") ")")
    (format "%S" var)))


(defun org-babel-execute:scala (body params)
  "Execute a block of Scala code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Scala source code block")
  (let* ((session (org-babel-scala-initiate-session
		   (cdr (assoc :session params))))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
	 (full-body (org-babel-expand-body:generic
                     body params))
         (result (org-babel-scala-evaluate
                  session full-body result-type result-params)))

    (org-babel-reassemble-table
     result
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))


(defun org-babel-scala-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results))


(defvar org-babel-scala-wrapper-method

"var str_result :String = null;

Console.withOut(new java.io.OutputStream() {def write(b: Int){
}}) {
  str_result = {
%s
  }.toString
}

print(str_result)
")


(defun org-babel-scala-evaluate
  (session body &optional result-type result-params)
  "Evaluate BODY as Scala code."
  (if session
      (org-babel-scala-evaluate-session
   session body result-type result-params)
    (org-babel-scala-evaluate-external-process
   body result-type result-params)))

(defun org-babel-scala-evaluate-session
  (session body &optional result-type result-params)
  "Evaluate BODY to the Scala process in SESSION.
If RESULT-TYPE equals 'output then return standard output as a string.
If RESULT-TYPE equals 'value then return the value of the last statement
in BODY as elisp."
  (let* ((send-wait (lambda () (comint-send-input nil t) (sleep-for 0 5)))
	 (dump-last-value
	  (lambda
	    (tmp-file pp)
	    (mapc
	     (lambda (statement) (insert statement) (funcall send-wait))
	     (if pp
		 (error "no pp support")
		 ;; (list
		 ;;  "import pprint"
		 ;;  (format "open('%s', 'w').write(pprint.pformat(_))"
		 ;; 	  (org-babel-process-file-name tmp-file 'noquote)))
	       (list (format org-babel-scala-f-write
			     (org-babel-process-file-name tmp-file 'noquote)))))))
	 (input-body (lambda (body)
		       (mapc (lambda (line) (insert line) (funcall send-wait))
			     (split-string body "[\r\n]"))
		       (funcall send-wait))))
    ((lambda (results)
       (unless (string= (substring org-babel-scala-eoe-indicator 1 -1) results)
	 (if (or (member "code" result-params)
		 (member "pp" result-params)
		 (and (member "output" result-params)
		      (not (member "table" result-params))))
	     results
	   (org-babel-scala-table-or-string results))))
     (case result-type
       (output
	(mapconcat
	 #'org-babel-trim
	 (butlast
	  (org-babel-comint-with-output
	      (session org-babel-scala-eoe-indicator t body)
	    (funcall input-body body)
	    (funcall send-wait) (funcall send-wait)
	    (insert org-babel-scala-eoe-indicator)
	    (funcall send-wait))
	  2) "\n"))
       (value
	(let ((tmp-file (org-babel-temp-file "scala-")))
	  (org-babel-comint-with-output
	      (session org-babel-scala-eoe-indicator nil body)
	    (let ((comint-process-echoes nil))
	      (funcall input-body body)
	      (funcall dump-last-value tmp-file (member "pp" result-params))
	      (funcall send-wait) (funcall send-wait)
	      (insert org-babel-scala-eoe-indicator)
	      (funcall send-wait)))
	  (org-babel-eval-read-file tmp-file)))))))



(defun org-babel-scala-evaluate-external-process
  (body &optional result-type result-params)
  "Evaluate BODY in external Scala process.
If RESULT-TYPE equals 'output then return standard output as a string.
If RESULT-TYPE equals 'value then return the value of the last statement
in BODY as elisp."
  (case result-type
    (output
     (let ((src-file (org-babel-temp-file "scala-")))
       (progn (with-temp-file src-file (insert body))
              (org-babel-eval
               (concat org-babel-scala-command " " src-file) ""))))
    (value
     (let* ((src-file (org-babel-temp-file "scala-"))
            (wrapper (format org-babel-scala-wrapper-method body)))
       (with-temp-file src-file (insert wrapper))
       ((lambda (raw)
          (if (member "code" result-params)
              raw
            (org-babel-scala-table-or-string raw)))
        (org-babel-eval
         (concat org-babel-scala-command " " src-file) ""))))))


(defun org-babel-prep-session:scala (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-scala-initiate-session session))
         (var-lines (org-babel-variable-assignments:scala params)))
    (org-babel-comint-in-buffer session
      (sit-for .5) (goto-char (point-max))
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))
    session))

(defun org-babel-load-session:scala (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:scala session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))



(defun org-babel-scala-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (require 'scala-mode-inf)
    ;; XXXasrail: ignoring sessions for now. It is easier to add session support on scala-mode-inf
    (let ((session-buffer (save-window-excursion
			    (scala-run-scala "scala") (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
	  (progn (sit-for .25) session-buffer)
        (sit-for .5)
        ;; (org-babel-scala-initiate-session session)
	))))

(provide 'ob-scala)



;;; ob-scala.el ends here
