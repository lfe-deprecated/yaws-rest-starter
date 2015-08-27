#################
yaws-rest-starter
#################


Introduction
============

This is a project the demonstrates what a RESTful service in `LFE`_ looks
like when run on top of the `YAWS`_ (`Erlang`_) web server. This may be
forked and used as the starter codebase for your own project.


Dependencies
------------

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``.

If you are running Ubuntu, you will need to install the following:

.. code:: bash

    $ sudo apt-get install erlang libpam0g-dev

The latter is needed to compile YAWS.

This project depends upon the following, which is automatically installed by
rebar to the ``deps`` directory of this project when you run ``make deps``
(also done implicitely by ``make compile``):

* `LFE`_ (Lisp Flavored Erlang; needed only to compile)
* `ltest`_ (needed only to run the unit tests)
* `YAWS`_ (The granddaddy of Erlang web servers)


Installation
============

Just clone this puppy and jump in:

.. code:: bash

    $ git clone https://github.com/lfex/yaws-rest-starter.git yrests
    $ cd yrests
    $ make compile

This will install all the dependencies and compile everything you need.


Troubleshooting
---------------

If your compile process fails, you may need to run ``make get-deps``
explicitly and then re-run ``make compile``.


Starting and Stopping
---------------------

To start the YAWS server + demo REST services in development mode, with any
printing (e.g., ``(io:format ...)``) sent to sdout, just do this:

.. code:: bash

    $ make dev

To run the daemon, do:

.. code:: bash

    $ make run

To stop the server once in daemon mode, do:

.. code:: bash

    $ make stop


Demo REST Services
==================


Demo #1: Simple HTTP Verbs
--------------------------

You can make calls to and example the responses from the demo REST server
with curl. Not that in this simple demo, all status codes are 200.

Here's a ``GET``:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demos/verbs
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 04:57:58 GMT
    Content-Length: 34
    Content-Type: application/json

    {"data": "Here, hazsomeGETdataz!"}

And a ``POST``:

.. code:: bash

    $ curl -D- -X POST http://localhost:8000/demos/verbs
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 04:58:38 GMT
    Content-Length: 34
    Content-Type: application/json

    {"data": "YOU madesomePOSTdataz!"}

One more: a Here's a ``GET``:

.. code:: bash

    $ curl -D- -X OPTIONS http://localhost:8000/demos/verbs
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 04:59:44 GMT
    Content-Length: 38
    Content-Type: application/json

    {"data": "Here, hazsomeOPTIONSdataz!"}

Here's what happens when you hit a URL that doesn't have a defined route:

.. code:: bash

    $ curl -D- -X OPTIONS http://localhost:8000/demos/verbs/bad-resource
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 16:23:51 GMT
    Content-Length: 29
    Content-Type: application/json

    {"error": "Unmatched route."}


Demo #2: Volvo Shop
-------------------

This demo was originally made for the LFE presentation given at Erlang
Factory San Francisco, 2014. It was taken from the `cloudy`_ repository
created specifically for that presentation.

In this demo, the correct HTTP status codes are returned.


Order a new car:

.. code:: bash

    $ curl -D- -X POST http://localhost:8000/demos/store/order \
        -d '{"Make": "Volvo", "Model": "P1800"}'
    HTTP/1.1 201 Created
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 06:39:41 GMT
    Content-Length: 33
    Content-Type: application/json

    {"result": "You made a new order."}

Get a list of pending orders:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demos/store/orders
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 06:53:30 GMT
    Content-Length: 37
    Content-Type: application/json

    {"result": "You got a list of orders."}

Get an order's status:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demos/store/order/124
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 06:57:58 GMT
    Content-Length: 46
    Content-Type: application/json

    {"result": "You got the status for order 124."}

Update an order:

.. code:: bash

    $ curl -D- -X PUT http://localhost:8000/demos/store/order/124 \
        -d '{"Model": "2014 P1800"}'
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 06:56:41 GMT
    Content-Length: 34
    Content-Type: application/json

    {"result": "You updated order 124."}

Delete an order:

.. code:: bash

    $ curl -D- -X DELETE http://localhost:8000/demos/store/order/124
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 07:00:54 GMT
    Content-Length: 37
    Content-Type: application/json

    {"result": "You deleted order 124."}

Get the payment status of a car order:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demos/store/payment/order/124
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 06:59:11 GMT
    Content-Length: 51
    Content-Type: application/json

    {"result": "You got the payment status of an order."}

Pay for your car:

.. code:: bash

    $ curl -D- -X PUT http://localhost:8000/demos/store/payment/order/124 \
        -d '{"Payment": "1000000kr"}'
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Thu, 15 May 2014 06:55:19 GMT
    Content-Length: 34
    Content-Type: application/json

    {"result": "You paid for an order."}

Hit a bad URL:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demos/store/jalopies
    HTTP/1.1 404 Not Found
    Server: Yaws 1.98
    Date: Sun, 18 May 2014 01:00:48 GMT
    Content-Length: 41
    Content-Type: application/json

    {"result": {"error": "Unmatched route."}}


Demo #3: Volvo Shop, Reloaded
-----------------------------

This demo offers the same functionality as Demo #2, but differs in the
implementation:

#. The organization of routes and route functions is more like what one
   sees in other web frameworks.

#. It returns proper HTTP status codes.

#. The results are more structured JSON data.

#. It uses some of the functionality offered by the lfest project.

#. It can handle a front page.

This demo offers a front page. View the base URL:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demos/store2/
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Sun, 18 May 2014 00:32:42 GMT
    Content-Length: 27
    Content-Type: text/html

    Welcome to the Volvo Store!

You can test it exactly as Demo #2, but remember to change the the URL to
point to the right demo:

.. code:: bash

  $ curl -X POST http://localhost:8000/demos/store2/order \
      -d '{"Make": "Volvo", "Model": "P1800"}'


Demo #4: Volvo Shop, Revolutions
--------------------------------

This demo offers the same functionality as Demo #3, but differs in the
implementation: it uses the routing macro from the lfest project,
considerably reducing code boiler plate.

You can test it exactly as Demo #3, but remember to change the the URL to
point to the right demo:

.. code:: bash

  $ curl -X POST http://localhost:8000/demos/store3/order \
      -d '{"Make": "Volvo", "Model": "P1800"}'


Benchmarks
==========

Benchmarks are a lie. Okay, now that we've gotten that out of the way, on
with the lies!

Running ``httperf`` and ``ab`` against the demo REST service on a 2012
MacBook Pro laptop with tons of other crap running on it gives **reqs/s** in
the **14,000** to **18,000** range.

Here's an example ``ab`` command that was used:

.. code:: bash

    $ ab -k -c 100 -n 20000 http://localhost:8000/demos/verbs

And one for ``httperf``:

.. code:: bash

    $ httperf --hog \
      --server localhost --port 8000 --uri /demos/verbs \
      --timeout 5 --rate 100 \
      --num-calls 10000 --num-conns 10


Development
===========

Routes are defined in the appropriately-named ``routes`` function in the
service definition files:

.. code:: lisp

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
        (io:format
          "Unmatched route!~nPath-info: ~p~nmethod: ~p~narg-data: ~p~n~n"
          (list path method arg))
        #(content
          "application/json"
          "{\"error\": \"Unmatched route.\"}")))

For a simple REST service, you might only need to replace the code for each
HTTP verb in ``src/yrests-just-verbs.lfe``. For more involved work, you
should take a look at the "store" demo.


Additional Info
===============

* `Learn more about YAWS`_


.. Links
.. -----

.. _LFE: https://github.com/rvirding/lfe
.. _YAWS: https://github.com/klacke/yaws
.. _Erlang: http://www.erlang.org/
.. _rebar: https://github.com/rebar/rebar
.. _ltest: https://github.com/lfex/ltest
.. _Learn more about YAWS: http://www.scribd.com/doc/16212424/Building-RESTful-Web-Services-with-Erlang-and-Yaws
.. _cloudy: https://github.com/oubiwann/cloudy
