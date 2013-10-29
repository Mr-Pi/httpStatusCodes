[![Build Status](https://travis-ci.org/Mr-Pi/httpStatusCodes.png?branch=master)](https://travis-ci.org/Mr-Pi/httpStatusCodes)
httpStatusCodes
===============
Erlang http status code library

This library provides simple methods to get the description or reference to an http status code.
The list is taken from:	[www.iana.org](http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml)

Types
-----
```erlang
-type httpStatusCode() :: 100..599.
-type httpStatusDescription() :: bitstring().
-type httpStatusReference() :: bitstring().
```

Usage
-----
* `getDescription(httpStatusCode())` returns a httpStatusDescription() to an httpStatusCode()
* `getReference(httpStatusCode())` returns a httpStatusReference() to an httpStatusCode()
* `getHttpStatus(httpStatusCode())` return a tuple, which contains the httpStatusCode(), the httpStatusDescription() and the httpStatusReference() to an httpStatusCode()
