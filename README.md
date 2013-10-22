[![Build Status](https://travis-ci.org/Mr-Pi/httpStatusCodes.png?branch=master)](https://travis-ci.org/Mr-Pi/httpStatusCodes)
httpStatusCodes
===============

a simple erlang application, to get the description and reference, based on a http status code.


Usage
=====
to get a description simple use:

    -spec httpStatusCode() -> integer().
    httpStatusCodes_app:getDescription(httpStatusCode()).
or

    httpStatusCodes_app:getReference(httpStatusCode()).`
to get a reference.
