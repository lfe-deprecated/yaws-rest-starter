(defmodule yrests-util
  (export all))

(include-lib "yaws/include/yaws_api.hrl")

(defun make-json-response (data)
  "Simple function used for handing off data to YAWS."
  (tuple 'content
         '"application/json"
         data))

(defun make-json-data-response (data)
  "Simple function used for handing off data to YAWS."
  (make-json-response (++ '"{\"data\": \"" data '"\"}")))

(defun make-json-error-response (error)
  "Simple function used for handing off data to YAWS."
  (make-json-response (++ '"{\"error\": \"" error '"\"}")))

(defun parse-path
  (('undefined)
   '())
  ((arg-data)
   (string:tokens arg-data "/")))

(defun get-http-method (arg-data)
  "Use the LFE record macros to parse data from the records defined in
  yaws_api.hrl."
  (let ((record (arg-req arg-data)))
    (http_request-method record)))

(defun meta-out (arg-data router)
  "This function can be called by all other out functions, as it handles the
  method name parsing. YAWS cannot use this function directly."
  (let ((method-name (get-http-method arg-data))
        (path-info (parse-path (arg-pathinfo arg-data))))
    (funcall router path-info method-name arg-data)))
