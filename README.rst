#################
yaws-rest-starter
#################


Introduction
============

This is a project the demonstrates what a RESTful service in `LFE`_ looks like
when run on top of the `YAWS`_ (`Erlang`_) web server. This may be forked and
used as the starter codebase for your own project.


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
* `lfeunit`_ (needed only to run the unit tests)
* `YAWS`_ (The granddaddy of Erlang web servers)


Installation
============

Just clone this puppy and jump in:

.. code:: bash

    $ git clone https://github.com/lfe/yaws-rest-starter.git
    $ cd yaws-rest-starter
    $ make compile

This will install all the dependencies and compile everything you need.


Troubleshooting
---------------

If your compile process fails, you may need to run ``make get-deps`` explicitly
and then re-run ``make compile``.


The Demo Server
===============


Starting and Stopping
---------------------

To start the YAWS server + demo REST service in development mode, with any
printing (e.g., ``(: io format ...)``) sent to sdout, just do this:

.. code:: bash

    $ make dev

To run the daemon, do:

.. code:: bash

    $ make run

To stop the server once in daemon mode, do:

.. code:: bash

    $ make stop


Checking the HTTP Verbs
-----------------------

You can make calls to and example the responses from the demo REST server
with curl.

Here's a ``GET``:

.. code:: bash

    $ curl -D- -X GET http://localhost:8000/demo
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 04:57:58 GMT
    Content-Length: 34
    Content-Type: application/json

    {"data": "Here, hazsomeGETdataz!"}

And a ``POST``:

.. code:: bash

    $ curl -D- -X POST http://localhost:8000/demo
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 04:58:38 GMT
    Content-Length: 34
    Content-Type: application/json

    {"data": "YOU madesomePOSTdataz!"}

One more: a Here's a ``GET``:

.. code:: bash

    $ curl -D- -X OPTIONS http://localhost:8000/demo
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 04:59:44 GMT
    Content-Length: 38
    Content-Type: application/json

    {"data": "Here, hazsomeOPTIONSdataz!"}

Here's what happens when you hit a URL that doesn't have a defined route:

.. code::

    $ curl -D- -X OPTIONS http://localhost:8000/bad-path
    HTTP/1.1 200 OK
    Server: Yaws 1.98
    Date: Fri, 07 Feb 2014 16:23:51 GMT
    Content-Length: 29
    Content-Type: application/json

    {"error": "Unmatched route."}


Benchmarks
==========

Benchmarks are a lie. Okay, now that we've gotten that out of the way, on
with the lies!

Running ``httperf`` and ``ab`` against the demo REST service on a 2012 MacBook
Pro laptop with tons of other crap running on it gives **reqs/s** in the
**14,000** to **18,000** range.

Here's an example ``ab`` command that was used:

.. code:: bash

    $ ab -k -c 100 -n 20000 http://localhost:8000/

And one for ``httperf``:

.. code:: bash

    $ httperf --hog \
      --server localhost --port 8000 --uri / \
      --timeout 5 --rate 100 \
      --num-calls 10000 --num-conns 10


Development
===========

Routes are defined in the appropriately-named
``./src/yaws-rest-starter-routes.lfe``:

.. code:: lisp

    (defun routes
      "REST API Routes"
      (('"/demo" method arg-data)
        (: yaws-rest-starter-demo get-data method arg-data))
      ; XXX add more routes here for your application
      ;(('"/another/path" method arg-data)
      ; (: your-app your-func method arg-data))
      ;
      ; When nothing matches, do this
      ((path method arg)
        (: io format
          '"Unmatched route!~n Path-info: ~p~n method: ~p~n arg-data: ~p~n~n"
          (list path method arg))
        #(content
          "application/json"
          "{\"error\": \"Unmatched route.\"}")))

For a simple REST service, you might only need to replace the code for each
HTTP verb in ``src/yaws-rest-starter-demo.lfe``. For more involved work, you could
split each of those out in to separate functions, e.g.:

.. code:: lisp

    (defun handle
      (('GET arg)
       (handle-get arg))
      (('POST arg)
       (handle-post arg))
       ...
       )

    (defun handle-get
      "Lots of complicated logic, possibly with intricate pattern matching
      of the arg parameter."
      (( ...
       )))

One could take this a step further for even more complicated projects with
larger codebases, and move the dispatched functions into their own modules.
For instance, in ``./src/your-project.lfe``:

.. code:: lisp

    (defun handle
      (('GET arg)
       (: your-project-gets handle arg))
       ...
       )

And then have a ``src/your-project-gets.lfe`` file for this code that defines
``handle``:

.. code:: lisp

    (defun handle
      "Lots of complicated logic, possibly with intricate pattern matching
      of the arg parameter, with each pattern dispatching to other code in
      the module."
      (( ...
       )))


Additional Info
===============

* `Learn more about YAWS`_

.. Links
.. -----
.. _LFE: https://github.com/rvirding/lfe
.. _YAWS: https://github.com/klacke/yaws
.. _Erlang: http://www.erlang.org/
.. _rebar: https://github.com/rebar/rebar
.. _lfeunit: https://github.com/lfe/lfeunit
.. _Learn more about YAWS: http://www.scribd.com/doc/16212424/Building-RESTful-Web-Services-with-Erlang-and-Yaws
