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


Running the Server
==================

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


Benchmarks
==========

Benchmarks are a lie. Okay, now that we've gotten that out of the way, on
with the lies!



Usage
=====

For a simple REST service, you might only need to replace the code for each
HTTP verb in ``src/yaws-rest-starter.lfe``. For more involved work, you could
split each of those out in to separate functions, e.g.:

.. code:: lisp

    (defun handle
      (('GET arg)
       (handle-get arg))
       ...
       )

    (defun handle-get
      "Lots of complicated logic, possibly with intricate pattern matching
      of arg parameter."
      (( ...
       )))

One could take this curther for even more complicated projects with larger
code bases, and move the dispatched functions into their own modules. For
instance:

.. code:: lisp

    (defun handle
      (('GET arg)
       (: your-project-get-handler get arg))
       ...
       )

And then have a ``src/your-project-get-handler.lfe`` file for this code that
defines ``get``:

    (defun get
      "Lots of complicated logic, possibly with intricate pattern matching
      of arg parameter, with each pattern dispatching to other code in the
      module."
      (( ...
       )))


.. Links
.. -----
.. _LFE: https://github.com/rvirding/lfe
.. _YAWS: https://github.com/klacke/yaws
.. _Erlang: http://www.erlang.org/
.. _rebar: https://github.com/rebar/rebar
.. _lfeunit: https://github.com/lfe/lfeunit
