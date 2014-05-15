(defmodule yrests-util
  (export all))

(include-lib "yaws/include/yaws_api.hrl")


(defun parse-path (arg-data)
  ""
  (arg-pathinfo arg-data))

(defun method (arg-data)
  "Use the LFE record macros to parse data from the records defined in
  yaws_api.hrl."
  (let ((record (arg-req arg-data)))
    (http_request-method record)))

(defun meta-out (arg-data router)
  "This function can be called by all other out functions, as it handles the
  method name parsing. YAWS cannot use this function directly."
  (let ((method-name (method arg-data))
        (path-info (parse-path arg-data)))
    (funcall router path-info method-name arg-data)))
