efene
=====

Alternative syntax for the Erlang Programming Language focusing on simplicity, ease of use and programmer UX.

visit `efene.org <http://efene.org>`_ for documentation and `quickstart <http://efene.org/quickstart.html>`_

Build
-----

::

    make

Use
---

For users we provide a `rebar3 plugin <http://efene.org/rebar-plugin.html>`_
if you are developing there's a simple shell script to use efene while developing::

    fn.sh beam file.fn
    fn.sh rawlex file.fn
    fn.sh lex file.fn
    fn.sh ast file.fn
    fn.sh mod file.fn
    fn.sh erl file.fn
    fn.sh erlast file.fn
    fn.sh pprint file.fn


License
-------

`APL 2.0 <https://www.apache.org/licenses/LICENSE-2.0.html>`_, see LICENSE file for details
