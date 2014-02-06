(defmodule yaws-rest-starter
  (export all))

(include-lib "yaws/include/yaws_api.hrl")


(defun method (arg)
  (list arg))

(defun handle ('GET arg)
  (: io format '"Handling GET arg: ~p" (list arg)))

(defun handle ('POST arg)
  (: io format '"Handling POST arg: ~p" (list arg)))

(defun handle ('PUT arg)
  (: io format '"Handling PUT arg: ~p" (list arg)))

(defun handle ('DELETE arg)
  (: io format '"Handling DELETE arg: ~p" (list arg)))

(defun handle ('OPTIONS arg)
  (: io format '"Handling OPTIONS arg: ~p" (list arg)))

(defun handle ('PATCH arg)
  (: io format '"Handling PATCH arg: ~p" (list arg)))

(defun handle (method arg)
  (: io format '"Unknown method: ~p" (list method)))

(defun out (arg)
  (: io format '"~p:~p ~p Request ~n" (list (method arg)))
  (handle (method arg) arg))
