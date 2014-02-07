(defmodule yaws-rest-starter-routes
  (export all))


(defun routes
  "REST API Routes"
  (('"/demo" method arg-data)
    (: yaws-rest-starter-demo get-data method arg-data))
  ((path method arg)
    (: io format
      '"path-info: ~p method: ~p arg-data: ~p~n"
      (list path method arg))
    #(content
      "application/json"
      "{\"error\": \"Unmatched route.\"}")))

(defun out (arg-data)
  "This is called by YAWS when the requested URL matches the URL specified in
  the YAWS config (see ./etc/yaws.conf) with the 'appmods' directive for the
  virtual host in question.

  In particular, this function is intended to handle all v1 traffic for this
  REST API."
  (: yaws-rest-starter-util meta-out arg-data #'routes/3))
