(defmodule yrests-store
  (export all))

;; REST API functions
(defun routes
  "Routes for the Volvoshop REST API."
  ;; /order
  (((list "order") method arg-data)
   (order-api method arg-data))
  ;; /order/:id
  (((list "order" order-id) method arg-data)
   (order-api method order-id arg-data))
  ;; /orders
  (((list "orders") method arg-data)
   (orders-api method arg-data))
  ;; /payment/order/:id
  (((list "payment" "order" order-id) method arg-data)
   (payment-api method order-id arg-data))
  ;; When nothing matches, do this
  ((path method arg)
    (io:format
      "Unmatched route!~nPath-info: ~p~nmethod: ~p~narg-data: ~p~n~n"
      (list path method arg))
    (lfest-json-resp:not-found "Unmatched route.")))

(defun order-api
  "The order API for methods without an order id."
  (('POST arg-data)
   (lfest-json-resp:created "You made a new order."))
  ;; When nothing matches, do this
  ((verb arg-data)
    (io:format
      "Unsupported method!~nVerb: ~p~narg-data: ~p~n~n"
      (list verb arg-data))
    (lfest-json-resp:method-not-allowed "Unsupported method.")))

(defun order-api
  "The order API for methods with an order id."
  (('GET order-id arg-data)
   (lfest-json-resp:ok
     (++ "You got the status for order " order-id '".")))
  (('PUT order-id arg-data)
   (lfest-json-resp:updated
     (++ "You updated order " order-id ".")))
  (('DELETE order-id arg-data)
   (lfest-json-resp:deleted
     (++ "You deleted order " order-id ".")))
  ;; When nothing matches, do this
  ((verb order-id arg-data)
    (io:format
      "Unsupported method!~nVerb: ~p~norder-id: ~p~narg-data: ~p~n~n"
      (list verb order-id arg-data))
    (lfest-json-resp:method-not-allowed "Unsupported method.")))

(defun orders-api
  "The orders API."
  (('GET arg-data)
   (lfest-json-resp:ok "You got a list of orders."))
  ;; When nothing matches, do this
  ((verb arg-data)
    (io:format
      "Unsupported method!~nVerb: ~p~n~narg-data: ~p~n~n"
      (list verb arg-data))
    (lfest-json-resp:method-not-allowed "Unsupported method.")))

(defun payment-api
  "The payment API."
  (('GET order-id arg-data)
   (lfest-json-resp:ok
     "You got the payment status of an order."))
  (('PUT order-id arg-data)
   (lfest-json-resp:updated "You paid for an order."))
  ;; When nothing matches, do this
  ((verb order-id arg-data)
    (io:format
      "Unsupported method!~nVerb: ~p~norder-id: ~p~narg-data: ~p~n~n"
      (list verb order-id arg-data))
    (lfest-json-resp:method-not-allowed "Unsupported method.")))

(defun out (arg-data)
  "This is called by YAWS when the requested URL matches the URL specified in
  the YAWS config (see ./etc/yaws.conf) with the 'appmods' directive for the
  virtual host in question.

  In particular, this function is intended to handle all v1 traffic for this
  REST API."
  (yrests-util:meta-out arg-data #'routes/3))
