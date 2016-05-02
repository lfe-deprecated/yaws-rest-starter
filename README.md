# yaws-rest-starter [![Build Status][travis-badge]][travis] [![img][tag-badge]][tag] [![img][erl-badge]][erl]

#### Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [Installation](#installation-)
* [Starting and Stopping](#starting-and-stopping-)
* [Demo REST Services](#demo-rest-services-)
  * [Demo #1: Simple HTTP Verbs](#demo-1-simple-http-verbs-)
  * [Demo #2: Volvo Shop](#demo-2-volvo-shop-)
  * [Demo #3: Volvo Shop, Reloaded](#demo-3-volvo-shop--reloaded-)
  * [Demo #4: Volvo Shop, Revolutions](#demo-4-volvo-shop--revolutions-)
* [Benchmarks](#benchmarks-)
* [Development](#development-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

This is a project the demonstrates what a RESTful service in LFE looks
like when run on top of the YAWS web server. This may be
forked and used as the starter codebase for your own project.

Note that Demo #4 is the currently recommended way to develop REST services in LFE on YAWS.

In the future, lmug will support Compojure-like route composiition, and that will supplant existing tools. It will also be web-server agnostic, supporting OTP inets, YAWS, elli, Cowboy, and others.


## Dependencies [&#x219F;](#contents)

This project assumes that you have Erlang and ``rebar3`` installed somwhere in
your ``$PATH``.

If you are running Ubuntu, you will need to install the following:

```bash
$ sudo apt-get install erlang libpam0g-dev
```
This is needed to compile YAWS.

Erlang and LFE library dependencies (including YAWS) are declared in the
``rebar.config`` file and installed (locally, in ``_build``) automatically by
``rebar3`` when the project is compiled.


## Installation [&#x219F;](#contents)

Just clone this puppy and jump in:

```bash
$ git clone https://github.com/lfex/yaws-rest-starter.git yrests
$ cd yrests
$ make
```

This will install all the dependencies and compile everything you need.


## Starting and Stopping [&#x219F;](#contents)

To start the YAWS server + demo REST services in development mode, with any
logging (e.g., ``(logjam:debug ...)``) sent to sdout, just do this:

```bash
$ make run
```

To run the daemon, do:

```bash
$ make daemon
```

To stop the server once in daemon mode, do:

```bash
$ make stop
```

## Demo REST Services [&#x219F;](#contents)


### Demo #1: Simple HTTP Verbs [&#x219F;](#contents)

You can make calls to and example the responses from the demo REST server
with curl. Not that in this simple demo, all status codes are 200.

Here's a ``GET``:

```bash
$ curl -D- -X GET http://localhost:8000/demos/verbs
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Fri, 07 Feb 2014 04:57:58 GMT
Content-Length: 34
Content-Type: application/json

{"data": "Here, hazsomeGETdataz!"}
```

And a ``POST``:

```bash
$ curl -D- -X POST http://localhost:8000/demos/verbs
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Fri, 07 Feb 2014 04:58:38 GMT
Content-Length: 34
Content-Type: application/json

{"data": "YOU madesomePOSTdataz!"}
```

One more: a Here's a ``GET``:

```bash
$ curl -D- -X OPTIONS http://localhost:8000/demos/verbs
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Fri, 07 Feb 2014 04:59:44 GMT
Content-Length: 38
Content-Type: application/json

{"data": "Here, hazsomeOPTIONSdataz!"}
```

Here's what happens when you hit a URL that doesn't have a defined route:

```bash
$ curl -D- -X OPTIONS http://localhost:8000/demos/verbs/bad-resource
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Fri, 07 Feb 2014 16:23:51 GMT
Content-Length: 29
Content-Type: application/json

{"error": "Unmatched route."}
```

### Demo #2: Volvo Shop [&#x219F;](#contents)

This demo was originally made for the LFE presentation given at Erlang
Factory San Francisco, 2014. It was taken from the repository
created specifically for that presentation.

In this demo, the correct HTTP status codes are returned.


Order a new car:

```bash
$ curl -D- -X POST http://localhost:8000/demos/store/order \
    -d '{"Make": "Volvo", "Model": "P1800"}'
HTTP/1.1 201 Created
Server: Yaws 1.98
Date: Thu, 15 May 2014 06:39:41 GMT
Content-Length: 33
Content-Type: application/json

{"result": "You made a new order."}
```

Get a list of pending orders:

```bash
$ curl -D- -X GET http://localhost:8000/demos/store/orders
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Thu, 15 May 2014 06:53:30 GMT
Content-Length: 37
Content-Type: application/json

{"result": "You got a list of orders."}
```

Get an order's status:

```bash
$ curl -D- -X GET http://localhost:8000/demos/store/order/124
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Thu, 15 May 2014 06:57:58 GMT
Content-Length: 46
Content-Type: application/json

{"result": "You got the status for order 124."}
```

Update an order:

```bash
$ curl -D- -X PUT http://localhost:8000/demos/store/order/124 \
    -d '{"Model": "2014 P1800"}'
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Thu, 15 May 2014 06:56:41 GMT
Content-Length: 34
Content-Type: application/json

{"result": "You updated order 124."}
```

Delete an order:

```bash
$ curl -D- -X DELETE http://localhost:8000/demos/store/order/124
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Thu, 15 May 2014 07:00:54 GMT
Content-Length: 37
Content-Type: application/json

{"result": "You deleted order 124."}
```

Get the payment status of a car order:

```bash
$ curl -D- -X GET http://localhost:8000/demos/store/payment/order/124
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Thu, 15 May 2014 06:59:11 GMT
Content-Length: 51
Content-Type: application/json

{"result": "You got the payment status of an order."}
```

Pay for your car:

```bash
$ curl -D- -X PUT http://localhost:8000/demos/store/payment/order/124 \
    -d '{"Payment": "1000000kr"}'
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Thu, 15 May 2014 06:55:19 GMT
Content-Length: 34
Content-Type: application/json

{"result": "You paid for an order."}
```

Hit a bad URL:

```bash
$ curl -D- -X GET http://localhost:8000/demos/store/jalopies
HTTP/1.1 404 Not Found
Server: Yaws 1.98
Date: Sun, 18 May 2014 01:00:48 GMT
Content-Length: 41
Content-Type: application/json

{"result": {"error": "Unmatched route."}}
```

### Demo #3: Volvo Shop, Reloaded [&#x219F;](#contents)

This demo offers the same functionality as Demo #2, but differs in the
implementation:

1. The organization of routes and route functions is more like what one
   sees in other web frameworks.
1. It returns proper HTTP status codes.
1. The results are more structured JSON data.
1. It uses some of the functionality offered by the lfest project.
1. It can handle a front page.

This demo offers a front page. View the base URL:

```bash
$ curl -D- -X GET http://localhost:8000/demos/store2/
HTTP/1.1 200 OK
Server: Yaws 1.98
Date: Sun, 18 May 2014 00:32:42 GMT
Content-Length: 27
Content-Type: text/html

Welcome to the Volvo Store!
```

You can test it exactly as Demo #2, but remember to change the the URL to
point to the right demo:

```bash
$ curl -X POST http://localhost:8000/demos/store2/order \
    -d '{"Make": "Volvo", "Model": "P1800"}'
```

### Demo #4: Volvo Shop, Revolutions [&#x219F;](#contents)

This demo offers the same functionality as Demo #3, but differs in the
implementation: it uses the routing macro from the lfest project,
considerably reducing code boiler plate.

You can test it exactly as Demo #3, but remember to change the the URL to
point to the right demo:

```bash
$ curl -X POST http://localhost:8000/demos/store3/order \
    -d '{"Make": "Volvo", "Model": "P1800"}'
```

## Benchmarks [&#x219F;](#contents)

Benchmarks are a lie. Okay, now that we've gotten that out of the way, on
with the lies!

(Also, note that while fast, YAWS strength lies in number of *concurrent*
connections, not total requests per second. Benchmarks in 2006 showed YAWS
handling 80,000 concurrent connections, while in 2012 WhatsApp showed how they
got 2 million concurrent connections -- Erlang in general, not YAWS-specific --
on a tuned FreeBSD box.)

Running ``httperf`` and ``ab`` against the demo REST service on a 2012
MacBook Pro laptop with tons of other crap running on it gives **reqs/s** in
the **14,000** to **18,000** range.

Here's an example ``ab`` command that was used:

```bash
$ ab -k -c 100 -n 20000 http://localhost:8000/demos/verbs
```

And one for ``httperf``:

```bash
$ httperf --hog \
  --server localhost --port 8000 --uri /demos/verbs \
  --timeout 5 --rate 100 \
  --num-calls 10000 --num-conns 10
```

## Development [&#x219F;](#contents)

Routes are defined in the appropriately-named ``routes`` function in the
service definition files:

```lisp
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
    (logjam:debug (MODULE) 'routes
      "Unmatched route!~nPath-info: ~p~nmethod: ~p~narg-data: ~p~n~n"
      (list path method arg))
    #(content
      "application/json"
      "{\"error\": \"Unmatched route.\"}")))
```

For a simple REST service, you might only need to replace the code for each
HTTP verb in ``src/yrests-just-verbs.lfe``. For more involved work, you
should take a look at the various "store" demos in ``src``.


## License [&#x219F;](#contents)

```
Copyright © 2014-2016 Duncan McGreggor

Distributed under the Apache License, Version 2.0.
```

<!-- Named page links below: /-->

[travis-badge]: https://travis-ci.org/lfex/yaws-rest-starter.png?branch=master
[travis]: https://travis-ci.org/lfex/yaws-rest-starter
[tag-badge]: https://img.shields.io/github/tag/lfex/yaws-rest-starter.svg
[tag]: https://github.com/lfex/yaws-rest-starter/releases/latest
[erl-badge]: https://img.shields.io/badge/erlang-%E2%89%A5R15B03-red.svg
[erl]: http://www.erlang.org/downloads)

