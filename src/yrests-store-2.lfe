(defmodule yrests-store-2
  (export all))

(include-lib "deps/lfest/include/macros.lfe")

(defun routes
  ;; top-level
  (('GET "/" arg-data)
   (lfest-html-resp:ok "Welcome to the Volvo Store!"))
  ;; single order operations
  (('POST "/order" arg-data)
   (create-order (lfest:get-data arg-data)))
  (('GET "/order/124" arg-data)
   (get-order 124))
  (('PUT "/order/124" arg-data)
   (update-order 124 (lfest:get-data arg-data)))
  (('DELETE "/order/124" arg-data)
   (delete-order 124))
  ;; order collection operations
  (('GET "/orders" arg-data)
   (get-orders))
  ;; payment operations
  (('GET "/payment/order/124" arg-data)
   (get-payment-status 124))
  (('PUT "/payment/order/124" arg-data)
   (make-payment 124 (lfest:get-data arg-data)))
  ;; error conditions
  ((method _ _) (when (not-in method ('GET 'POST 'PUT 'DELETE)))
   (io:format
     "Method not allowed.~nVerb: ~p~n"
     (list method))
     (lfest-json-resp:method-not-allowed))
  ((method path arg-data)
   (io:format
     "Unmatched data.~nVerb: ~p~nPath: ~p~nArg data: ~p~n~n"
     (list method path arg-data))
   (lfest-json-resp:not-found "Bad path: invalid operation.")))

;;; Operations on single orders
(defun create-order (data)
  (io:format "Got POST with payload: ~p~n" (list data))
  (lfest-json-resp:created '"{\"order-id\": 124}"))

(defun get-order (order-id)
  (io:format "Got GET for order ~p~n" (list order-id))
  (lfest-json-resp:ok
    (++ "You got the status for order " (integer_to_list order-id) ".")))

(defun update-order (order-id data)
  (io:format "Got PUT for order ~p with payload: ~p~n" (list order-id data))
  (lfest-json-resp:updated
    (++ "You updated order " (integer_to_list order-id) ".")))

(defun delete-order (order-id)
  (io:format "Got DELETE for order ~p~n" (list order-id))
  (lfest-json-resp:deleted
     (++ "You deleted order " (integer_to_list order-id) ".")))

;;; Operations on the collection of orders
(defun get-orders ()
  (lfest-json-resp:ok "You got a list of orders."))

;;; Operations having to do with payments
(defun get-payment-status (order-id)
  (lfest-json-resp:ok
    "You got the payment status of an order."))

(defun make-payment (order-id data)
  (io:format "Got PUT with payload: ~p~n" (list data))
  (lfest-json-resp:created "You paid for an order."))

(defun out (arg-data)
  "This is called by YAWS when the requested URL matches the URL specified in
  the YAWS config (see ./etc/yaws.conf) with the 'appmods' directive for the
  virtual host in question.

  In particular, this function is intended to handle all v1 traffic for this
  REST API."
  (let ((method-name (lfest:get-http-method arg-data))
        (path-info (lfest:get-path-info arg-data)))
    (routes method-name path-info arg-data)))
