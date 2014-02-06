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
     #(content
       "application/json"
       "{\"data\": \"Here, hazsomeGETdataz!\"}"))
  (('POST arg)
   (: io format '"Handling POST arg: ~p" (list arg))
     #(content
       "application/json"
       "{\"data\": \"YOU madesomePOSTdataz!\"}"))
  (('PUT arg)
   (: io format '"Handling PUT arg: ~p" (list arg))
     #(content
       "application/json"
       "{\"data\": \"YOU madesomePUTdataz!\"}"))
  (('DELETE arg)
   (: io format '"Handling DELETE arg: ~p" (list arg))
     #(content
       "application/json"
       "{\"data\": \"OHNOEZ! You byebyeddatazwithDELETE!\"}"))
  (('OPTIONS arg)
   (: io format '"Handling OPTIONS arg: ~p" (list arg))
     #(content
       "application/json"
       "{\"data\": \"Here, hazsomeOPTIONSdataz!\"}"))
  (('PATCH arg)
   (: io format '"Handling PATCH arg: ~p" (list arg))
     #(content
       "application/json"
       "{\"data\": \"YOU madesomePATCHdataz!\"}"))
  ((method arg)
   (: io format '"WTF?: ~p" (list method))))

(defun out (arg)
  (let ((method-name (method arg)))
    (: io format '"method: ~p arg: ~p~n" (list method-name arg))
    (handle method-name arg)))
