=================
 cl-info
=================

.. insert-your badges like that:

.. image:: https://com-40ants-github-actions.herokuapp.com/40ants/cl-info/matrix.svg
    :target: https://github.com/40ants/cl-info/actions

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from


.. image:: https://img.shields.io/badge/rollup-%5E0.67.3-blue


This is a small utility, useful to display information about you Common
Lisp environment.

Usage from Common Lisp
======================

It's main call is ``(cl-info:get-cl-info)``, it returns an object with
customized ``print-object`` method. You can use it to output debug
information in your programs:

.. code:: common-lisp

   CL-USER> (cl-info:get-cl-info)
   OS:   Darwin 15.6.0
   Lisp: SBCL 1.4.8
   ASDF: 3.3.1.1
   QL:   ceramic github-e0d905187946f8f2358f7b05e1ce87b302e34312
         cl-prevalence github-c163c227ed85d430b82cb1e3502f72d4f88e3cfa
         log4cl-json github-c4f786e252d89a45372186aaf32fb8e8736b444b
         log4cl github-6a857b0b41c030a8a3b04096205e221baaa1755f
         quicklisp 2018-04-30
         slynk github-3314cf8c3021cb758e0e30fe3ece54accf1dcf3d
         weblocks-lass github-1b043afbf2f3e84e495dfeae5e63fe67a435019f
         weblocks-parenscript github-8ef4ca2f837403a05c4e9b92dcf1c41771d16f17
         weblocks-ui github-5afdf238534d70edc2447d0bc8bc63da8e35999f
         weblocks-websocket github-b098db7f179dce3bfb045afd4e35e7cc868893f0
         weblocks github-282483f97d6ca351265ebfebb017867c295d01ad
         websocket-driver github-a3046b11dfb9803ac3bff7734dd017390c2b54bb
   CL-USER>

Also, you can gather information about separate systems:

.. code:: common-lisp

   CL-USER> (cl-info:get-system-info :hamcrest)
   System: HAMCREST 0.4.2
          /Users/art/common-lisp/cl-hamcrest/src/


Usage from command-line
=======================

Also, you can use ``cl-info`` as a command-line utility. It is useful to
output information about common lisp environment running on CI server,
for example.

Here how to do it:

.. code:: bash

   # Here we use a Roswell, to install utility
   [art@art-osx:~]% ros install 40ants/cl-info
   
   # And now request information about lisp and some systems
   [art@art-osx:~]% cl-info weblocks clack jonathan some-other-system
   OS:   Darwin 15.6.0
   Lisp: Clozure Common Lisp Version 1.11.5/v1.11.5  (DarwinX8664)
   ASDF: 3.3.1.1
   QL:   org.borodust.bodge 20180214223017
         quicklisp 2017-10-23
   System: weblocks 0.31.1
           /Users/art/common-lisp/weblocks/src/
   System: clack 2.0.0
           /Users/art/common-lisp/clack/
   System: jonathan 0.1
           /Users/art/.roswell/lisp/quicklisp/dists/quicklisp/software/jonathan-20170630-git/
   System: some-other-system is not available


.. Everything after this comment will be omitted from HTML docs.
.. include-to
