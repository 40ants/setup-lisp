(defpackage #:cl-info
   (:nicknames #:cl-info/core)
   (:use #:cl)
   (:import-from #:mgl-pax-minimal
                 #:defsection
                 #:reader)
   (:export #:cl-info
            #:get-cl-info
            #:get-system-info))
(in-package cl-info/core)


(defsection @index (:title "CL-INFO - Common Lisp Environment Reporter")
  "
[![](https://github-actions.40ants.com/40ants/cl-info/matrix.svg)](https://github.com/40ants/cl-info/actions)

This is a small utility, useful to display information about you Common
Lisp environment. You might want to call it in the CI pipeline or
to use it when rendering a crash report in some client applications.

Usage from Common Lisp
======================

It's main entry point is CL-INFO:GET-CL-INFO function. It returns an object with
customized PRINT-OBJECT method. You can use it to output debug
information in your programs.

CL-INFO collects inforrmation about OS, Lisp Implementation, ASDF, installed
Quicklisp distributions:

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

Also, you can gather information about separate systems using CL-INFO:GET-SYSTEM-INFO
function:

    CL-USER> (cl-info:get-system-info :hamcrest)
    System: HAMCREST 0.4.2
           /Users/art/common-lisp/cl-hamcrest/src/


Usage From Command-line
=======================

Also, you can use CL-INFO as a command-line utility. It is useful to
output information about common lisp environment running on CI server.

Here is how to do it:

```shell
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
```

API Reference
=============
"
  (get-cl-info function)
  (get-system-info function)
  
  (cl-info class)
  (get-asdf-version (reader cl-info))
  
  (system-info class))


(defclass cl-info ()
  ((asdf-version :initform (asdf:asdf-version)
                 :reader get-asdf-version
                 :documentation "Returns ASDF version.")
   (lisp-type :initform (lisp-implementation-type)
              :reader get-lisp-type)
   (lisp-version :initform (lisp-implementation-version)
                 :reader get-lisp-version)
   (software-type :initform (software-type)
                  :reader get-software-type)
   (software-version :initform (software-version)
                     :reader get-software-version)
   #+quicklisp
   (ql-dists :initform (ql-dist:all-dists)
             :reader get-ql-dists)))


#-quicklisp
(defun get-ql-dists (obj)
  nil)


(defclass system-info ()
  ((name :initarg :name
         :reader get-name)
   (version :initarg :version
            :reader get-version)
   (path :initarg :path
         :reader get-path)
   (absent :initarg :absent
           :initform nil
           :reader absent-p)))


(defmethod print-object ((info cl-info) stream)
  (format stream
          "OS:   ~A ~A
Lisp: ~A ~A
ASDF: ~A
"
          (get-software-type info)
          (get-software-version info)
          (get-lisp-type info)
          (get-lisp-version info)
          (get-asdf-version info))
  #-quicklisp
  (format stream
          "QL:   is not available~%")
  #+quicklisp
  (format stream
          "QL:   ~{~A~^~%~}~%"
          (loop for dist in (get-ql-dists info)
                for idx upfrom 0
                collect (format nil "~:[~;      ~]~A ~A"
                                (> idx 0)
                                (ql-dist:name dist)
                                (ql-dist:version dist)))))


(defmethod print-object ((info system-info) stream)
  (if (absent-p info)
      (format stream
              "System: ~A is not available~%"
              (get-name info))
      (format stream
              "System: ~A ~A
        ~A~%"
              (get-name info)
              (get-version info)
              (get-path info))))


(defun get-cl-info ()
  "Returns information about lisp implementation, asdf and quicklisp."
  (make-instance 'cl-info))


(defun get-system-info (system-name)
  (let ((system (block search-for-system
                  (handler-bind ((asdf:missing-component
                                  (lambda (c)
                                    (declare (ignorable c))
                                    (return-from search-for-system nil)))
                                 (asdf:system-out-of-date
                                  (lambda (c)
                                    (declare (ignorable c))
                                    (invoke-restart 'continue))))
                    (asdf:find-system system-name)))))
    
    (if system
        (make-instance 'system-info
                       :name system-name
                       :version (asdf:component-version system)
                       :path (asdf:component-pathname system))
      (make-instance 'system-info
                     :name system-name
                     :absent t
                     :version nil
                     :path nil))))
