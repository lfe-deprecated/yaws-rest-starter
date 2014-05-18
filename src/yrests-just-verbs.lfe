(defmodule yrests-just-verbs
  (export all))

(defun get-data
  "If you'd like to see the the arg parameter printed out for any given
  dispatch below, simply comment it out. The lines that print to stdout
  are commented below in order to better assess perfmance when benchmarking."
  (('GET arg)
   ;(io:format "Handling GET arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"Here, hazsomeGETdatuhz!\"}"))
  (('POST arg)
   ;(io:format "Handling POST arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomePOSTdatuhz!\"}"))
  (('PUT arg)
   ;(io:format "Handling PUT arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomePUTdatuhz!\"}"))
  (('DELETE arg)
   ;(io:format "Handling DELETE arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"OHNOEZ! You byebyeddatuhzwithDELETE!\"}"))
  (('OPTIONS arg)
   ;(io:format "Handling OPTIONS arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"Here, hazsomeOPTIONSdatuhz!\"}"))
  (('HEAD arg)
   ;(io:format "Handling HEAD arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"Here, hazsomeHEADdatuhz!\"}"))
  (('PATCH arg)
   ;(io:format "Handling PATCH arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomePATCHdatuhz!\"}"))
  (('CONNECT arg)
   ;(io:format "Handling CONNECT arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomeCONNECTtyunz!\"}"))
  (('TRACE arg)
   ;(io:format "Handling TRACE arg: ~p~n" (list arg))
   #(content
     "application/json"
     "{\"data\": \"YOU madesomeTRACEuhz!\"}"))
  ((method _)
   (io:format "WTF is this verb?! ~p~n" (list method))
   #(content
      "application/json"
      "{\"error\": \"Y U NO GIVE GOOD VERB?!?!!\"}")))

(defun routes
  "REST API Routes"
  (('() method arg-data)
    (get-data method arg-data))
  ; XXX add more routes here for your application
  ;(((list "another" "path") method arg-data)
  ; (your-app:your-func method arg-data))
  ;
  ; When nothing matches, do this
  ((path method arg)
    ; (io:format
    ;   "Unmatched route!~nPath-info: ~p~nmethod: ~p~narg-data: ~p~n~n"
    ;   (list path method arg))
    #(content
      "application/json"
      "{\"error\": \"Unmatched route.\"}")))

(defun out (arg-data)
  "This is called by YAWS when the requested URL matches the URL specified in
  the YAWS config (see ./etc/yaws.conf) with the 'appmods' directive for the
  virtual host in question.

  In particular, this function is intended to handle all v1 traffic for this
  REST API."
  (yrests-util:meta-out arg-data #'routes/3))
