AFFTA - Another Framework for Functional Test Application 
=========================================================

Alternately, Another Framework for Functional Test Automation (AFFTA)
in a Common Lisp Platform

## Summary

_(TBD. API is in revision, towards AFFTA 1.3+)_

## Dependencies

AFFTA 1.3 depends on the following systems:

* [info.metacommunity.cltl.utils][mci-cltl-utils]
* [info.metacommunity.cltl.application][dobelle-app] ;; FIXME: _Need not._

## Licensing

AFFTA is licensed under the terms of the [Eclipse Public License 1.0][EPL]

## Availability

The primary source tree for AFFTA is hosted at [GitHub](http://www.github.com/).

Git URL (SSH): [git@github.com:MetaCommunity/affta.git][affta]


## Reference

### Overview

The [AFFTA][affta] test system is defined to address two primary
_usage cases_ for functional systems testing, in a context of systems
development:
* Interactive _instance tests_
    * May be defined _inline_ with source code being tested
    * May be useful for quick debug in a rapid development _sprint_
    * Need not be accompanied with structured test reports
* Noninteractive _batch tests_
    * Primary concept: Component-oriented functional testing
    * May be defined in source files specifically for defining test
      sessions, separate to source code being tested
    * Should be accompanied with structured test reports
    * Related concepts:
        * Regression Testing
        * Integration Testing
        * Development Metrics

Although [AFFTA][affta] was defined initially for testing of Common
Lisp source code, the functional interface for [AFFTA][affta] has been
defined such that it may be applied for procedures of structured
systems testing, of components not defined in Common Lisp.

### Test Protocol Concepts

#### Test Protocol Concepts - Outline

* Test Classes
    * `Test` - the primary protocol class of this system
    * `Lisp-Test` - a protocol class for tests onto Lisp forms
      effectively redundant to `LISP-TEST`
    * _Blue sky: Additional test classes may be defined for testing of
      systems outside of the Common Lisp implementation environment_
* Test Structures
    * Test Body
    * Optional Test Structures
        * Test Setup Function / Method
        * Test Cleanup Function / Method
        * In short: This system provides three primary ways to define
          each of a _test setup_ or _test cleanup_ procedure for any
          single _test_ object:
            * By providing a _lambda form_ or _function_ to either of
              the `:setup-function` or `:cleanup-function` 
              initialization arguments, respectively, when
              initialization a `test` object
            * By specifying a _function_ or _lambda form_ via either
              `(setf test-setup-function)` or `(setf
              test-cleanup-function)` respectively 
            * By defining a method specialized onto either of the
              generic functions `do-test-setup` or `do-test-cleanup`
                * The _system supplied primary method_ for each of
                  those generic functions will call, respectively, the
                  `test-setup-function` or `test-cleanup-function` for
                  the _test_ -- when either of those is defined to the
                  _test_ (cf. `cl:slot-boundp`, `utils:slot-value*`)
                * The behaviors are unspecified if a method
                  specializing either `do-test-setup` or
                  `do-test-cleanup` would not apply a bound value for
                  the `test-setup-function` or
                  `test-cleanup-function`, respectively.
* Test Goals
    * A _test goal_ represents, effectively, a container for _test
      parameters_ and _expected return values_ onto a single _test_
      definition
* Test Application
    * Test Setup Function (Optional)
    * Test Body
    * Test Cleanup Function (Optional)
    * Testing for return values


### Test Protocol Dictionary

* `test` [Standard Class]
* `test-predicate`, `test-predicate-function`, `test-name`, `test-object` [Accessor]
* `test-setup-function`, `(setf test-setup-function)` [Generic Function]
* `test-cleanup-function`, `(setf test-cleanup-function)` [Generic Function]
* `lisp-test` [Standard Class]
* `test-lambda-form`, `test-lambda-function` [Accessor]
* `test-goal` [Standard Class]
    * Summary: Effectively, a test goal `B` encapsulates a test `A`,
      also storing a set of structural qualities for application of `A`
      within a _testing session_. The structural qualities stored in a
      _test goal_, `B` may provide features for the environment in
      which the _test form_ of `A` is to be evaluated (e.g directly in
      a Lisp session, or externally in a host operating system
      process)
    * See also: `test`, `test-record`
* `lisp-test-goal` ;; FIXME: Reevaluate how this extends `test-goal`
* `do-test-setup` [Generic Function]
* `do-test-cleanup` [Generic Function]
* `do-test` [Generic Function]
* TO DO: `deftest` [Macro]
* TO DO: `with-test` [Macro]
* TO DO: `run-test` [Function]
    * per _goals specified when run-test is evaluated_
* TO DO: `defsuite` [Macro]
* TO DO: `defgoals` [Macro]
* TO DO: `run-suite` [Function]
    * per _goals defined with defgoals_

### Test Recording Concepts

* ...

### Test Recording Dictionary

* `test-record` [Standard Class]
* ...

### Test Reporting Concepts

* Interactive Reports
    * Instance Tests
* Structured Reports
    * Instance Tests
    * Batch Tests

### Test Reporting Dictionary

* ...


### ASDF System Testing Concepts

* ...

### ASDF System Testing Dictionary

* `test-component` (To Do) [Standard Class] 


## Development Plan

### AFFTA-1.3

#### Goals - AFFTA-1.3

* [COMPLETED] Develop test recording framework, separate from test definition framework

* Develop a 'TEST-SUITE' class; Integrate w/ ASDF

* Focusing on the `info.metacommunity.cltl.utils` system,
  develop `CLASS-PROTOCOL-TEST`

* Develop _use case_ example for `CLASS-PROTOCOL-TEST`
    * e.g. towards definition of a portable `READ-WRITE-LOCK` for
      _Bordeaux Threads_
    * alternately, referencing
      [igneous-math](https://github.com/MetaCommunity/igneous-math)
    * Issue: What would be the distinguishing characteristics of a
      _class-protocol test_, broadly?

* [COMPLETED] Focusing on `test-protocol.lisp`, revise the primary test protocol so as to accept a single TEST-SPECIFIER argument in each of:

>    `DO-TEST`
>    `DO-TEST-SETUP`
>    `DO-TEST-CLEANUP`


* [COMPLETED] Focusing on `test-recording.lisp`, differentiate TEST-GOAL properties from TEST-RECORD properties

* [COMPLETED] Revise DO-TEST, DO-TEST-SETUP, DO-TEST-CLEANUP for test paramterization, cf. TEST-GOAL

* [COMPLETED] Remove DO-RECORDED-TEST [unused], ensuring that stand-alone comments
  within the definition body [source code] are moved into separate
  documentation files

* [COMPLETED] Ensure that all "top-level" types and interfaces are available externally from within #:TEST


#### Progress - AFFTA-1.3

**TO DO:**

* [COMPLETED] Move UNION-STREAM comments into normative
  documentation - see [`README-streams.md`](README-streams.md)

* [IN PROGRESS] Continue with revisions onto other AFFTA-1.3 version-context goals
    * Implement `DEFTEST`, `WITH-TEST`, `RUN-TEST`, `DEFSUITE`,
      `DEFGOALS`, `RUN-SUITE` (See previous documentation/notes)
    * Develop prototype for structured test reporting (XML?)

* Sidebar: **Granite MIDE** - Develop ASDF extension for exporting
  remote AWS image
    * Concept: Component-oriented systems development
    * Concept: Host virtualization
    * Concept: Software-Defined Networking (SDN)
    * Developing application onto _EC2 API tools_, make direct reference
      to the Java API as applied in those tools -- beginning with
      those tools defined for AWS EB _image_ export 
    * Issue: Managing incomplete downloads with EC2 + {?
      Alexandria ?}
    * Issue: _Granite MIDE_ project is not yet formally defined
        * McCLIM
        * cf. Climacs
            * Application of McCLIM Drei, ESA components in an
              Emacs-like application frame
        * cf. Eclipse IDE
        * cf. VirtualBox
        * cf. Emacs window embedding (?)


**Progress**

* Develop initial prototype - see comments in [test-protocol.lisp][test-protocol]
        
    * **AFFTA** [Manual] - AFFTA 1.2
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
                        * In AFFTA 2 and later revisions branch revisions, a "batch testing" interface may be defined for each of: Testing a Common Lisp application [cf. `APPLICATION` system]; testing the contents of a _Root FS_ for (e.g) an _image_ to be installed onto an embedded device (cf. BeagleBone Black, other single-board computing platforms, and other Embedded computing platforms)
                * **Defining Tests - Concepts**
                    * Test Components
                        * Overview
                            * In concepts developed within [AFFTA], a _test_, once applied, is comprised of a _test object_, a _test goal_, and a _test record_.
                                * A _test object_ represents a _form_ defined for purpose of evaluating the behaviors of a software system, for conformance with behaviors expected of the software system, by the system's developers. In addition to the lisp _form_ of a _test object_, a _test object_  may also be represented of additional _test utility_ _resources_ essentially external to the _Lisp environment_.
                                * A _test goal_ represents a set of parameters to a _test object_, as well as an expected _result condition_, contingent on successful evaluation of the _test object_. (In the instance of a simple, functional Lisp program test, for a _functional test_ `B` designed as to test the behaviors of a function `A`,  the set of _parameters_ to a _functional_ _test goal_ `G` onto `B` is represented simply of a list of arguments for the function `A`; the _result condition_ for `G`  onto `B` would be represented of a list of _values_ that the function `A` would be expected to return.)
                                * A _test record_ represents an instance of the evaluation of a _test_ with a single _test goal_ object. (In [AFFTA], a _test record_ for a simple, functional Lisp program test will store any condition objects representing a non-local exit of control from within the _test main form_)
                            * [AFFTA] is designed around a concept of _separation of concerns_ for definition of _test_ objects, _test goal_ objects, and _test record_ objects.
                            * TO DO: [AFFTA] must also define a concept of a _test suite_, such that should be well integrated within the Lisp environment's system definition facility, _viz a viz_ ASDF - as well as to be accessed from a functional interface, _viz a viz_ `CURRENT-TEST-SUITE`
                        * Test Metadata
                            * {Sidebar: Test metadata must be defined such as to be displayed in any medium in which a test may "probably" be evaluated, e.g test stream for a REPL, if not a full IDE}
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
                            * {Candidly, this is one part a matter of convenience, another part a matter of convenience applied, thirdly a matter of "Re-use" of "Existing code". Note, superficially, the availability of component dependency structures as defined within ASDF 3}
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
                            * {cf. AFFTA-1.4}
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
  develop an application notification protocol for test results

* Developing MCi AFFTA in parallel with the MCi `APPLICATION` system, 
  implement the `UNION-STREAM` specification (presently denoted in
  test-protocol.lisp)

* Provide CLIM integration for `FORMAT-TEST-LABEL`

### AFFTA-1.5

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
    * Association of TEST and TEST-SUITE objects with COMPONENT and SYSTEM objects
    * "Interfacing" w/i Eclipse IDE

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
        * Optimization for compilation of "diagnostic" system components
    * "Live distribution" testing - definitions and automation
        * Optimization's for compilation of "live" system components
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

<!--  LocalWords:  AFFTA TBD API mci cltl utils dobelle FIXME EPL AWS
 -->
<!--  LocalWords:  GitHub affta Noninteractive setf boundp Accessor
 -->
<!--  LocalWords:  Undefine defsuite defgoals ASDF paramterization EB
 -->
<!--  LocalWords:  MIDE McCLIM Climacs Drei ESA IDE VirtualBox MKTEST
 -->
<!--  LocalWords:  FS BeagleBone Metadata metadata REPL Monadic asdf
 -->
<!--  LocalWords:  Diadic Variadic superclasses Runtime  defclass MCi
 -->
<!--  LocalWords:  OpenOffice CLtL DFSG CLIM SNS ROOTFS Diigo CCL SMS
 -->
<!--  LocalWords:  SBCL src
 -->
