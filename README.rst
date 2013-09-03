==========
Erlang-re2
==========

The goal of this project is to provide a fast, stable and functional wrapper for
a fast C++ regular expression library re2_.


Overview
--------

Erlang-re2 is implemented as a NIF module.


Status
------
Erlang-re2 is at the very early stage of development, and is more like a proof
of concent at the moment.

It's not recommended for general use, but the early results look very promising.


How promising exactly?
----------------------

When compared to a standard Erlang re library, re2 is a bit faster on simple regular expressions like::

 "^h.*o$"

2-6 times faster on more complex regular expressions like::

  "^((a*)|(b*))((c*)|(d*))$"


And 35-640 times faster on monster regular expressions like this (by the way,
it's a regular expression for urls)::

  "^(http|https|ftp)\://([a-zA-Z0-9\.\-]+(\:[a-zA-Z0-9\.&amp;%\$\-"
  "]+)*@)*((25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|"
  "[1-9])\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|"
  "[1-9]|0)\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|"
  "[1-9]{1}[0-9]{1}|[1-9]|0)\.(25[0-5]|2[0-4][0-9]|"
  "[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])|localhost|"
  "([a-zA-Z0-9\-]+\.)*[a-zA-Z0-9\-]+\.(com|edu|gov|int|mil|net|org"
  "|biz|arpa|info|name|pro|aero|coop|museum|[a-zA-Z]{2}))"
  "(\:[0-9]+)*(/($|[a-zA-Z0-9\.\,\?\'\\\+&amp;%\$#\=~_\-]+))*$"

It is especially much faster if a regular expression doesn't match an input string.

Seeing it in action
-------------------

Erlang-re2 has been tested on Erlang R15B03-1, but I suppose it should run
wihtout problems on R16 as well.

re2 uses rebar for compilation and test running::

  $ ./rebar compile
  $ ./rebar ct

To run performance tests::

  $ ./rebar compile shell
  1> application:start(re2).
  2> re2_perf:run_tests().

.. _re2: http://code.google.com/p/re2/
