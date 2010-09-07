Dependences
===========

* web.py 0.31
* jinja2 2.1

Usage
=====

Make sure you can run ledger with having to use the -f|--file switch.  This can
be done by setting the LEDGER_FILE environment variable to the path of your
ledger file.

Next, run
    $ python plot.py
and point your browser to http://127.0.0.1:8080/

Bugs
====

This program only works with American currency. At least one function uses the
dollar sign when parsing.

The graphs were only tested in Firefox and Opera. Internet Explorer may or may
not work.

