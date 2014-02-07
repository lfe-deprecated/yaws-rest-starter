(defmodule yaws-rest-starter
  (export all))

(include-lib "yaws/include/yaws_api.hrl")


(defun method (arg-data)
  "Use the LFE record macros to parse data from the records defined in
  yaws_api.hrl."
  (let ((record (arg-req arg-data)))
    (http_request-method record)))

(defun handle
  "If you'd like to see the the arg parameter printed out for any given
  dispatch below, simply comment it out. The lines that print to stdout
  are commented below in order to better assess perfmance when benchmarking."
  (('GET arg)
   ;(: io format '"Handling GET arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"Here, hazsomeGETdataz!\"}"))
  (('POST arg)
   ;(: io format '"Handling POST arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomePOSTdataz!\"}"))
  (('PUT arg)
   ;(: io format '"Handling PUT arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomePUTdataz!\"}"))
  (('DELETE arg)
   ;(: io format '"Handling DELETE arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"OHNOEZ! You byebyeddatazwithDELETE!\"}"))
  (('OPTIONS arg)
   ;(: io format '"Handling OPTIONS arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"Here, hazsomeOPTIONSdataz!\"}"))
  (('HEAD arg)
   ;(: io format '"Handling HEAD arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"Here, hazsomeHEADdataz!\"}"))
  (('PATCH arg)
   ;(: io format '"Handling PATCH arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomePATCHdataz!\"}"))
  (('CONNECT arg)
   ;(: io format '"Handling CONNECT arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomeCONNECTtyunz!\"}"))
(('TRACE arg)
   ;(: io format '"Handling TRACE arg: ~p" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomeTRACEuhz!\"}"))
  ((method arg)
   (: io format '"WTF?: ~p" (list method))))

(defun out (arg)
  "This is called by YAWS when the requested URL matches the URL specified in
  the YAWS config (see ./etc/yaws.conf) with the 'appmods' directive for the
  virtual host in question."
  (let ((method-name (method arg)))
    ;(: io format '"method: ~p arg: ~p~n" (list method-name arg))
    (handle method-name arg)))
