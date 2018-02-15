efene |BuildLink|_ |CoverageLink|_
=====

.. |BuildLink| image:: https://travis-ci.org/lauramcastro/efene.svg?branch=master
.. _BuildLink: https://travis-ci.org/lauramcastro/efene

.. |CoverageLink| image:: https://coveralls.io/repos/github/lauramcastro/efene/badge.svg?branch=master
.. _CoverageLink: https://coveralls.io/github/lauramcastro/efene?branch=master

Alternative syntax for the Erlang Programming Language focusing on simplicity,
ease of use and programmer UX.

Visit `efene.org <http://efene.org>`_ for documentation and `quickstart <http://efene.org/quickstart.html>`_

Build
-----

::

    rebar3 compile

Run tests
---------

::

    rebar3 ct


Use
---

For users we provide a `rebar3 plugin <http://efene.org/rebar-plugin.html>`_
if you are developing there's a simple escript to use efene while developing::

    rebar3 escriptize

    ./_build/default/bin/efene beam file.fn
    ./_build/default/bin/efene rawlex file.fn
    ./_build/default/bin/efene lex file.fn
    ./_build/default/bin/efene ast file.fn
    ./_build/default/bin/efene mod file.fn
    ./_build/default/bin/efene erl file.fn
    ./_build/default/bin/efene erlast file.fn
    ./_build/default/bin/efene pprint file.fn

Test
----

The test suite is located under the test folder using the `Common Test <http://erlang.org/doc/man/common_test.html>`_ format.

To execute the suite run the following command from the root folder of the repository:

::

    rebar3 ct

License
-------

`APL 2.0 <https://www.apache.org/licenses/LICENSE-2.0.html>`_, see LICENSE file for details
