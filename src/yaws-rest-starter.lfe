(defmodule yaws-rest-starter
  (export all))

(include-lib "yaws/include/yaws_api.hrl")

(defrecord arg2
  req
  pathinfo)

(defun method (arg-data)
  (let ((record (arg-req arg-data)))
    (http_request-method record)))

(defun handle
  (('GET arg)
   (: io format '"Handling GET arg: ~p" (list arg))
     #(html "<html><body>Yay!</body></html>"))
  (('POST arg)
   (: io format '"Handling POST arg: ~p" (list arg)))
  (('PUT arg)
   (: io format '"Handling PUT arg: ~p" (list arg)))
  (('DELETE arg)
   (: io format '"Handling DELETE arg: ~p" (list arg)))
  (('OPTIONS arg)
   (: io format '"Handling OPTIONS arg: ~p" (list arg)))
  (('PATCH arg)
   (: io format '"Handling PATCH arg: ~p" (list arg)))
  ((method arg)
   (: io format '"Unknown method: ~p" (list method))))

(defun out (arg)
  (let ((method-name (method arg)))
    (: io format '"method: ~p arg: ~p~n" (list method-name arg))
    (handle method-name arg)))
