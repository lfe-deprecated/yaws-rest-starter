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

This project depends upon the following, which installed to the ``deps``
directory of this project when you run ``make deps``:

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
with curl:

.. code:: bash

    $


Benchmarks
==========

Benchmarks are a lie. Okay, now that we've gotten that out of the way, on
with the lies!

Running ``httperf`` and ``ab`` against the demo REST service on a 2012 MacBook
Pro laptop with tons of other crap running on it gives reqs/s in the **14,000**
to **18,000** range.

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

For a simple REST service, you might only need to replace the code for each
HTTP verb in ``src/yaws-rest-starter.lfe``. For more involved work, you could
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


.. Links
.. -----
.. _LFE: https://github.com/rvirding/lfe
.. _YAWS: https://github.com/klacke/yaws
.. _Erlang: http://www.erlang.org/
.. _rebar: https://github.com/rebar/rebar
.. _lfeunit: https://github.com/lfe/lfeunit
