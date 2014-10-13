AFFTA - Another Framework for Functional Test Application 
=========================================================

Alternately, Another Framework for Functional Test Automation (AFFTA)
in a Common Lisp Platform

## Summary

_(TBD. API is in revision, towards AFFTA 1.3+)_

## Dependencies

AFFTA 1.3 depends on the following systems:

* [info.metacommunity.cltl.utils][mci-cltl-utils]
* [info.metacommunity.cltl.application][dobelle-app]

## Licensing

AFFTA is licensed under the terms of the [Eclipse Public License 1.0][EPL]

## Availability

The primary source tree for AFFTA is hosted at GitHub:

[git@github.com:MetaCommunity/affta.git][affta]


## Development Plan


### AFFTA-1.3

#### Goals - AFFTA-1.3

* [COMPLETED] Develop test recording framework, seperate from test definition framework

* Develop a 'TEST-SUITE' class; Integate w/ ASDF

* [COMPLETED] Focusing on `test-classes.lisp`, implement `FUNCTIONAL-TEST`

* Focusing on the `info.metacommunity.cltl.utils` system,
  develop `CLASS-PROTOCOL-TEST`

* Develop _use case_ example for `CLASS-PROTOCOL-TEST`, towards definition of a portable `READ-WRITE-LOCK` for _Bordeaux Threads_

* [COMPLETED] Focusing on `test-protocol.lisp`, revise the primary test protocol so as to accept a single TEST-SPECIFIER argument in each of:

>    `DO-TEST`
>    `DO-TEST-SETUP`
>    `DO-TEST-CLEANUP`


* [COMPLETED] Focusing on `test-recording.lisp`, differentiate TEST-GOAL properties from TEST-RECORD properties

* [COMPLETED] Revise DO-TEST, DO-TEST-SETUP, DO-TEST-CLEANUP for test paramterization, cf. TEST-GOAL

* Remove DO-RECORDED-TEST, ensuring that stand-alone comments within the definition body [source code] are moved into seperate documentation files

* [COMPLETED] Ensure that all "top-level" types and interfaces are available externally from within #:TEST


#### Progress - AFFTA-1.3

**TO DO:**

* Move UNION-STREAM comments into normative documentation (OpenOffice)

* Continue with revisions onto other AFFTA-1.3 version-context goals

* Sidebars:

    * **Granite MIDE** - Develop ASDF extension for exporting remote AWS image
        * Referencing EC2 API tools, make direct reference to the Java API as applied in those tools defined for image export
        * Issue: Managing incomplete downloads with EC2 + {? Alexandria ?}

    * MetaCommunity **APPLICATION** class: Develop onto....
        * Lisp image's process - in an extension of a class POSIX-PROCESS, cf. OSICAT
        * McCLIM 'application frames'
        * Eclipse extensions - again extending into Java, as cf. CL+J+CCL
        * ETC.

**Progress**

* Develop initial prototype - see comments in [test-protocol.lisp][test-protocol]
    * TEST-COMPONENT [Class]
        * define
        * subclass of ASDF:COMPONENT
        
    * TEST [Class]

    * LISP-TEST [Class]

    * FUNCTIONAL-TEST [Class]

    * FUNCTIONAL-SETF-TEST [Class] {FIXME: Unimplemented}

    * CLASS-PROTOCOL-TEST [Class] {FIXME: Unimplemented}
 
    * TEST-SUITE [Class]
        * define
        * subclass of ASDF:SYSTEM

    * FUNCTIONAL-TEST [Class]
        * rename from VALUES-TEST
        * remove slot definitions effectively shadowed by TEST-GOAL
        * add slot definitions for LAMBA-BODY, LAMBDA-FUNCTION
        * initialize LAMBDA-FUNCTION from LAMBDA-BODY when SHARED-INITIALIZE for LAMBDA-FUNCTION
        
    * TEST-GOAL [Class]
        * refer to [test-protocol][test-protocol]
        * Summary: Effectivley, a test goal `B` encapsulates a test `A`, also storing a set of structural qualities for appliation of `A` within a _testing session_. The stuctural qualities stored in a _test goal_, `B` may provide features for the environment in which the _test form_ of `A` is to be evaluted (e.g directly in a Lisp session, or externally in a host operating system process)
        * See also: `TEST`, `TEST-RECORD`

    * LISP-TEST-COAL [Class] - refer to [test-protocol][test-protocol]

    * TEST-RECORD [Class] - refer to [test-recording][test-recording]

    * DEFSUITE [Macro]  - define; refer to [test-protocol][test-protocol]
    
    * DEFTEST* [Macro]
        * define
        * refer to [test-protocol][test-protocol]
        * document
            * Purpose: "Batch mode" interface for test definition
            * Syntax: DEFTEST* NAME SUITE {PROPERTY-SPECIFIER}* => INST
                * NAME: A test name, a string or a symbol
                * SUITE: Name of a test suite, a string or a symbol
                * PROPERTY-SPECIFIER: Initialization argument for a test class
                * INST: an object
           * Description
               * The macro DEFTEST* provides an interface for defining a test instance. The syntax of DEFTEST* macro allows for specification of test setup, test cleanup, test lambda, and test description forms
               * See also: DEFTEST

    * DEFTEST [Macro]
        * define
        * refer to [test-protocol][test-protocol]
        * document
            * Purpose: "Source inline" interface for test definition
            * See also: DEFTEST*; IN-TEST-SUITE; CURRENT-TEST-SUITE
    
    * DEFGOALS [Macro]  - define; refer to [test-protocol][test-protocol]

    * RUN-TEST [Macro]  - define; refer to [test-protocol][test-protocol]
    
    * RUN-TEST-SUITE [Macro]  - define; refer to [test-protocol][test-protocol]

    * DO-TEST-SETUP [Generic Function]
        * System-Supplied Primary Methods
            * DO-TEST-SETUP T FUNCTION
                * {Describe: "No-Op"}
            * DO-TEST-SETUP T TEST
                * {see also: TEST-SETUP-FUNCTION}
    
    * DO-TEST-CLEANUP [Generic Function]
        * System-Supplied Primary Methods
            * DO-TEST-CLEANUP T FUNCTION
                * {Describe: "No-Op"}
            * DO-TEST-CLEANUP T TEST
                * {see also: TEST-CLEANUP-FUNCTION}
        * {See also: test-record-cleanup-results

    * TEST-SETUP-FUNCTION [Generic Function]
        * Summary: Functional interface for pre-test environment setup forms
        
    * TEST-CLEANUP-FUNCTION [Generic Function]
        * Summary: Functional interface for post-test environment cleanup forms
        
    * **AFFTA** [Manual]
        * Begin writing reference manual for AFFTA
        * Format: TBD
        * Sections
            * Overview
            * **Reference**
                * Use Cases
                    * "Source-Inline" Tests
                        * {cf. MKTEST, AFFTA-1.3}
                            * {cf. DO-TEST LIST LIST FUNCTION, AFFTA-1.2}
                        * Application: Functional regression testing
                        * Advantage: When a test form is defined "near" the source code of a form being tested in the test form, then it serves to illustrate the application of the form being tested - as a sort of convenient, initial documentation, furthermore assisting with source code maintainability. It also serves to provide effective test forms, though not in stand-alone test definition files.
                    * "Batch" Tests
                        * Use cases may include: Testing for systems external to a Common Lisp program
                        * Application: Class protocol regression testing; Integration testing
                        * A "Batch" test may serve to provide a combined testing interface for an individual Common Lisp system
                        * In AFFTA 2 and later revisions branch revisions, a "batch testing" interface may be defined for each of: Testing a Common Lisp appliation [cf. `APPLICATION` system]; testing the contents of a _Root FS_ for (e.g) an _image_ to be installed onto an embedded device (cf. BeagleBone Black, other single-board computing platforms, and other Embedded computing platforms)
                * **Defining Tests - Concepts**
                    * Test Components
                        * Overview
                            * In concepts developed within [AFFTA], a _test_, once applied, is comprised of a _test object_, a _test goal_, and a _test record_.
                                * A _test object_ represents a _form_ defined for purpose of evaluating the behaviors of a software system, for conformance with behaviors expected of the software system, by the system's developers. In adition to the lisp _form_ of a _test object_, a _test object_  may also be represented of additional _test utility_ _resources_ essentially external to the _Lisp environment_.
                                * A _test goal_ represents a set of parameters to a _test object_, as well as an expected _result condition_, contingent on successful evaluation of the _test object_. (In the instance of a simple, functional Lisp program test, for a _functional test_ `B` designed as to test the behaviors of a function `A`,  the set of _parameters_ to a _functional_ _test goal_ `G` onto `B` is repreented simply of a list of arguments for the function `A`; the _result condition_ for `G`  onto `B` would be represented of a list of _values_ that the function `A` would be expected to return.)
                                * A _test record_ represents an instance of the evaluation of a _test_ with a single _test goal_ object. (In [AFFTA], a _test record_ for a simple, functional Lisp program test will store any condition objects representing a non-local exit of control from within the _test main form_)
                            * [AFFTA] is designed around a concept of _seperation of concerns_ for definition of _test_ objects, _test goal_ objects, and _test record_ objects.
                            * TO DO: [AFFTA] must also define a concept of a _test suite_, such that should be well integrated within the Lisp environment's system defintiion facility, _viz a viz_ ASDF - as well as to be accessed from a functional interface, _viz a viz_ `CURRENT-TEST-SUITE`
                        * Test Metadata
                            * {Sidebar: Test metadata must be defined such as to be displayed in any medium in which a test may "probably" be evaluated, e.g tet stream of a REPL, if not a full IDE}
                            * Object Naming in AFFTA
                                * Test Names: {Symbols and/or strings}
                                * Test Labels: {TBD}
                            * Test labeling
                                * {xref: APPLICATION:LABELED-OBJECT}
                            * Test Descriptions
                                * Description Format
                        * Dependency onto ASDF
                            * AFFTA _tests_ and _test suites_ may be defined with dependencies, in a semantics effectively extending of the semantics of dependency structures within ASDF _components_ and _systems_
                            * Use case: Situation of when a test suite M for a system A may effectively require that an ASDF system B would be successfully loaded, when the system A, as being tested in the test suite M, does not otherwise "immediately" depend on the system B
                            * {FIXME: This documentation assumes that the reader would be familiar with the semantics of ASDF system definitions, and as such, should be made to include a reference to a tutorial introducing of ASDF}.
                            * {Candidly, this is one part a matter of convenience, another part a matter of convenience applied, thirdly a matter of "Re-use" of "Existing code". Note, specficialy, the availability of component dependency structures as defined within ASDF 3}
                        * Test Dependencies
                            * {See also: Extensions onto ASDF}
                        * Test Definitions
                            * Functional Tests
                                * Monadic, Diadic, and Variadic Functional Tests
                            * Test Structure {TO DO: move this to reference section}
                                * Direct superclasses: test-component
                                * Test Lambda
                                * Test Setup
                                * Test Cleanup
                                * Test Suite
                                * Test Metadata
                                    * Test Name
                                    * Test Description
                        * Test Suites
                            * A test suite represents, effectively, an ordered set of a _test_, and a set of _test goals_50
                            * Test Suite Structure  {TO DO: move this to reference section}
                                * Direct superclasses: test-component asdf:system
                                * Test Suite Setup
                                * Test Suite Cleanup
                                * Test Suite Tests
                                * Test Suite Metadata
                                    * Test Suite Name
                                    * Test Suite Description
                * Applying Tests - Concepts
                    * Test Goals
                    * Test Runtime
                        * Test/Suite Setup
                        * Test/Suite Body
                            * Test suites: "Test suite body" is to run individual tests w/ respective "setup", "body", and "cleanup" forms of each
                            * Test instances: {cf. `unwind-protect`}
                        * Test/Suite Cleanup
                        * Streams
                            * {cf. AFTA-1.4}
                   * Test Result Recording
                       * {cf. "Goals" for AFFTA-1.3}
                * Integration with ASDF
                    * {cf. ASDF:TEST-OP}
                    * {to do: defclass DIAGNOSTIC-TEST-OP}
                    * {to do: defclass PRODUCTION-TEST-OP}
                    * {cf. IN-TEST-SUITE, CURRENT-TEST-SUITE)
                * AFFTA Dictionary
                    * {Integrate OpenOffice w/ CLtL - cf. DFSG and OpenOffice projects}


### AFFTA-1.4

* In parallel with development of the MCi `APPLICATION` system,
  develop an application notificiation protocol for test results

* Developing MCi AFFTA in parallel with the MCi `APPLCIATION` system, 
  implementing the `UNION-STREAM` specification (presently denoted in
  test-protocol.lisp)

* Provide CLIM integration for `FORMAT-TEST-LABEL`

### AFTFA-1.5

* Focusing on the MCi `APPLICATION` system, develop support for using
  the Amazon Web Services (AWS) API for notifying a developer when a
  batch test completes See also: [SNS (AWS)]


* Referencing `test-protocol.lisp` (current revision) Develop a suitable
  batch testing model, for providing integration of AFFTA  with Hudson 
  Continuous Integration (CI) and AWS

### AFFTA-1.6

* Referencing [Dandelion], integrate with the Eclipse IDE
    * Presentation of test metadata
    * Test controls and test reporting
    * Associtation of TEST and TEST-SUITE objects with COMPONENT and SYSTEM objects
    * "Interafcing" w/i Eclipse IDE

### AFFTA-2

**Summary:** focusing on `test-classes.lisp`, develop a `APPLICATION-TEST`

**Goal:** Provide an interface for testing the behaviors of an application, in a context of:

* Interactive desktop applications in Common Lisp
* Server applications in Common Lisp
* Libraries of functions, classes, etc. in Common Lisp

### AFFTA-3

**Summary:** focusing on `test-classes.lisp`, develop a `ROOTFS-TEST`

**Goal:** testing the contents of a _Root FS_ for (e.g) an _image_ to be installed onto an embedded device (cf. BeagleBone Black, other single-board computing platforms, and other Embedded computing platforms)

**Notes**

* Linux kernel testing [See also: One's Diigo bookmarks]
* "Full build process"
    * "Diagnostic distribution" testing - definitions and automation
        * Optimizations for compilation of "diagnostic" system components
    * "Live distribution" testing - definitions and automation
        * Optimizations for compilation of "live" system components
* Integration with _Continuous Integration_ systems
    * Hudson (Eclipse Foundation)
* Integration with application-specific testing frameworks
    * Testing in CCL
    * Testing in SBCL
    * {...}

### Other features to be developed

* SMS messaging for batch testing notifications? (Is that possible w/ AWS?)

* Integration with the debugger (cf. MCi `APPLICATION` system) for
  batch mode tests that can be held for developer interaction


[EPL]: https://www.eclipse.org/legal/epl-v10.html
[affta]: https://github.com/MetaCommunity/affta
[mci-cltl-utils]: https://github.com/MetaCommunity/mci-cltl-utils
[test-protocol]: src/main/cltl/test-protocol.lisp
[test-protocol]: src/main/cltl/test-recording.lisp
[dobelle-app]: https://github.com/MetaCommunity/dobelle-app
