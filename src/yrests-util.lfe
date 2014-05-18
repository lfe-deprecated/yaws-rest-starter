(defmodule yrests-util
  (export all))

(defun meta-out (arg-data router)
  "This function can be called by all other out functions, as it handles the
  method name parsing. YAWS cannot use this function directly."
  (let ((method-name (lfest:get-http-method arg-data))
        (path-info (lfest:parse-path arg-data)))
    (funcall router path-info method-name arg-data)))
