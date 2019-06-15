AFFTA - Another Framework for Functional Test Application
=========================================================

Alternately: Another Framework for Functional Test Automation (AFFTA)
in a Common Lisp Platform

## Summary

_(TBD. API is in revision, towards AFFTA 1.3+)_

[FIXME: AFFTA 1.3 development has been discontinued, along with AFFTA
1.2 development. Each may be construed as comprising a "Boxed" revision,
now. As this system has not been at all widely adopted, any concerns
with regards to backwards compatibility for later revision may be
understood as negligible, in character of concern. The API should not
need any broad and sweeping revisions but it is by no means limited to
previous developments, at this time.]

## Dependencies

AFFTA 1.3 depends on the following systems:

[FIXME: Review "HOW". Document, here, at least with some short remarks]

**Ed. Note: These references are outdated**

* [info.metacommunity.cltl.utils][ltp-main-src]
* [info.metacommunity.cltl.utils.mop][ltp-main-src]

## Licensing

AFFTA is licensed under the terms of the [Eclipse Public License 1.0][EPL]

[FIXME: Document this licensing policy. Note how the EPL, in a manner,
may be somewhat similar to a BSD license, yet provided in a much more
verbose manner of form with regards to indemnification. Consider
developing a formal licensing policy for utilizing the EPL in all
"New" and "Contrib-Independent" projects at Thinkum Labs. Note also, the
dual mode licensing schemes developed in some other projects.]

## Availability

The [primary source tree for AFFTA][affta-src] is hosted at [GitHub](http://www.github.com/).

Git URL (SSH): [git@github.com:thinkum/affta.git][affta-src]


## Reference

**Ed. Note: Oudated Design Documentation**

### Overview

The [AFFTA-SRC][affta-src] test system is defined to address two primary
_usage cases_ for functional systems testing, in a context of systems
development.

* A Generic Development Process - Outline

    1. Begin _development sprint_
    2. Develop software
    3. Test software
    4. Review test results
    5. Revise software, if appropriate
    6. Finalize _development sprint_

[FIXME: Note that a concept of Scrum process has been adopted, here -
albeit, in a manner more _per se_ independent than commercially "Team
Oriented"]

* Generic Concepts

    * Concept: Objects of test

        * Representative of specific _source forms_ [FIXME such that would be
          interpreted as to initialize an _object_ within a single
          _software programming environment_ FIXME - review the
          following paragraph and revise this introductory, perhaps
          oversimplified "Bullet Point" topic].

        * Initialized within the _data space_ of a single _software
          programming environment_ [FIXME: This topic, in itself, may
          serve to introduce some fairly complex concepts, such that may
          not be immediately obvious as for relevance in a software
          system system.] [FIXME Furthermore, this may be revised
          towards a concept of _portable test identifiers_ if not also
          for a manner of portable system-specific _object
          identifiers_. See also, ISO Topic Maps. Note that this is not
          to be construed as interoperable with CORBA.]

    * Concept: Tests

        * The meaning of a concept, _test_, may vary by sense of
          context. By in large, the term is applied - in this software
          system - as representative of a limited concept of
          _statistical experiment_, in a principally theoretical manner
          of regard. In this system, a _test_ is comprised, in an
          abstract manner, as a procedure to be conducted within a
          computational _environment_. The principal subject of a test,
          as such, may be understood as being comprised of an abstract
          definition of _testable functionality_, principally with regards
          to the characteristics of a software system.  The object of a
          test, as such, may be understood as being comprised of any one
          or more principally functional _test forms_, such that may be
          evaluated by a software system as to produce any one or more
          discernible _test result_ values. The test result values -- in
          the statistical metaphor, _samples_ -- may be interpreted for
          comparison to an individual set of predetermined _ideal
          outcome_ values, for each test. As such, the set of _ideal
          outcome_ values for a _test form_ must be known, previous to
          the evaluation of a test - at which point, we may depart from
          the metaphor of a statistical experiment.

        * In _definition_ of tests: Representative of specific _test
          definition_ forms.

        * In _application_ of tests: Representative of a functional
          application of specific _test definitions_, per specific
          _test goals_.

        * In _recording_ and _reporting_ about test results: Representative
          of specific _results_ of specific _test goals_ applied to
          specific _test definitions_ [FIXME: This sense of a test
          should be addressed to particular attention, within this
          software system. The other two senses of test, denoted above,
          have now been addressed in the preceding paragraph. See also,
          Maven and the Eclipse Mylyn framework, principally towards a
          regard of _DevOps_ reference platforms.]

        * Concepts
            * Test Definition
            * Test Goals
            * Test Record
            * Test Report
            * [FIXME: This list may be incomplete, if not more actually
              oversimplified. Juxtapose to the preceding paragraph.]

For the following, a general sense of _style of testing methodology_ is introduced.

* **Interactive _instance tests_** [FIXME: May be evaluated
  noninteractively, in batch mode] [FIXME: Review and Revise this synopsis]

    * May be defined _inline_ with source code being tested

    * May be useful for interactive debugging, as during a rapid
      development _sprint_

    * Need not be accompanied with structured test reports

    * May be developed into _batch tests_, such that would serve to
      provide a manner of _systems reflection_, as well as some
      exacting metrics for determination of _system reliability_,
      during later _maintenance cycles_.


* **Noninteractive _batch tests_** [FIXME: Review; Revise]

    * Concept: Component-oriented functional testing

    * _Batch tests_ may be defined in files separate to, or inline
      with _source forms_ of software code being tested

        * If defined _external_ to the _objects of test_, the _source
          forms_ for _test_ objects may be defined -- whether defined
          directly, as with explicit component definitions, or
          defined indirectly, as within a systems reflection framework
          -- defined as individual _software components_. Those
         _software components_ may then be compiled and applied
         separate to the _source forms_ of the _objects of test_,
         namely as within a software component framework such as
         [ASDF][asdf] in Common Lisp, or in applications of the Java
         Development Kit, [Apache Maven][mvn]

        * If defined _inline_ with the _objects of test_, the _source forms_
          for _test_ objects may be _conditionally escaped_ with
          _compiler flags_ -- as would be applied in a syntax specific
          to the programming language in the application -- such as to
          prevent that those _source forms_ would be interpreted,
          unless the_system_ is being _compiled_ for purpose of
          _testing_

    * Should be productive to _test reports_

    * Corresponding Concepts

        * Regression Testing

        * Integration Testing

        * Development Metrics [FIXME: This is not a project micromanagement system]


Although [AFFTA-SRC][affta-src] was defined initially for testing of Common
Lisp source code, the functional interface for [AFFTA-SRC][affta-src] has been
defined such that it may be applied for procedures of structured
systems testing, for evaluation of components not defined in Common
Lisp.


### Test Protocol - Concepts - Overview

[AFFTA-SRC][affta-src] provides an object-oriented framework for functional
testing, such as would be applied within a Common Lisp programming
environment. [Ed. NB: Refer to JUnit, at least in the bibliography.]

In [AFFTA-SRC][affta-src], a _test_ is defined with a _main function_.
Furthermore, a _setup function_ and/or a _cleanup function_ may also
be defined to a _test_ `;;; FIXME:` Should also allow definition
of _cleanup function_ and _setup function_ to a _test suite_, such
that the respective setup/cleanup functions would allocate resources
required for more than one _test_ defined within the _suite_. (NB: **Test
Composition** - Topic)

A _test goal_  may provide parameters to a test's _main function_,
and may provide parameters to a test's _setup_ and _cleanup_
functions. A _test goal_ should also provide values that may be
compared to any values returned by the test's _main function_, such
that would be compared in application of a _test predicate_.

For purposes of _test definition_, _test application_, and  _test
reporting_  a _test_ may be defined as effectively contained within a
_test suite_.

#### Test Protocol - Concepts - Outline

* **Test Classes** (Baseline test protocol - AFFTA)

    * `Test` - the primary protocol class of this system

    * `Lisp-Test` - a protocol class for tests onto Lisp forms

    * _Blue sky: Additional test classes may be defined for testing of
      systems outside of the Common Lisp programming environment_
      [FIXME: Review and Revise this]

* **Test Structures** (Baseline test protocol - AFFTA)

    * Required Property: Test _main_ form

    * Optional Properties

        * Test Setup Function / Method

        * Test Cleanup Function / Method

        * In short summary: This system provides three primary ways to
          define each of a _test setup_ or _test cleanup_ procedure
          for any single _test_ object

            * By providing a _lambda form_ or _function_ to either of
              the `:setup-function` or `:cleanup-function`
              initialization arguments, respectively, when a `test`
              object is _initialized_

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

* **Test Containers** (Baseline test protocol - AFFTA)\

    * A _test suite_ provides an effective _container_ for _test_
      objects

    * _FIXME:_ Describe how _test goals_ are associated with _test
      objects_

* **Test Goals** (Baseline test protocol - AFFTA)

    * A _test goal_ provides a container for _test parameters_ and
      _expected return values_ onto a single _test_ definition.

    * An extension may define additional _properties_ to a _test
      goal_. For example, a _test goal_ for a _shell application test_
      may provide _properties_ such as:

          * Configurable _parameters_ for defining the _shell
            execution environment_ of the _test_

          * A reference to a specific _filesystem directory_ to apply
            as the _root directory_ of the _test_

          * _Network addresses_ to be applied in the test -- as when
            the _shell application test_ would initialize a _networked
            application_

          * Specifiers for _package name_ and _package version_
            values, if the _shell application test_ requires specific
            _packaged resources_ from within the _shell environment_

* **Test Application** (Baseline test protocol - AFFTA) [FIXME: Note
  that this documentation represents something of an "Internal" level of
  granularity, moreso about the AFFTA implementation than in any ways
  representing how it may be used within individual projects]

    * *Overview:* Generic protocol for test application from within a
      Common Lisp programming environment

    * Property: Test _Setup_ Function (Optional)

    * Property: Test _Main_ Function

    * Property: Test _Cleanup_ Function (Optional)

    * Concept: Testing return values

        * _Expected Return Values_ as provided by a _test goal_ object

        * _Predicate_ as provided by a _test goal_ object

        * Application of _predicate_ to _expected return values_

* **Test Recording** (Baseline test protocol - AFFTA)

    * [FIXME: _Reference to subsequent sections of the documentation_ -
      cf. Test Protocol Dictionary. Note also, other systems for
      software testing, namely for batch application of test
      definitions, vis a vis JUnit. Note also, Mylyn orchestration for
      JUnit. Note furthermore, unit testing support within Continuous
      Integration/DevOps _web portal_ systems such as Jenkins. **NB**
      This system may not utilize any manner of _web portal_ for
      any applications in systems automation tasks. Regardless, perhaps
      in any abstract manner, the functional qualities of Jenkins'
      support for _unit testing_ methodologies may bear some specific
      reference -- principally independent to so many characteristics in
      which Jenkins may represent principally a Java Servlets
      application]

* **Test Reporting**

    * _API TBD_[FIXME: Review the preceding documentation, and revise
      this section of the documentation. Note also that the following
      goals statement should be represented in a manner independent to
      the principal system reference documentation -- perhaps, with a
      goals statement within a project design document.]

    * To Do: Locate/Extend/Define an XML syntax for recording of _test
      definitions_, _test goal_ sets, and _test result_ sets, in a
      syntax agnostic to any single software platform. [FIXME: There is a
      goal, intrinsically represented here, for ensuring portability of
      test result forms. Theoretically, if an abstract syntax for
      software tests may be devised in this system, and documented
      formally, it may be of use for testing in other software
      systems. This, of course, should be accompanied with _proof of
      concept_ in manner beyond any singular characteristics of Common
      Lisp software programs.]

### Test Protocol Dictionary

[FIXME: This section needs an introduction - the concepts represented
here may not been sufficiently addressed in the previous documentation]

[FIXME - Note **subsections** and **later sections**, absent of a
normative "Inline ToC" and available cross-sectional reference syntax,
in this Markdown format]

[FIXME - Some of the following subsections may already exist, in this
document. Note that it can be difficult to structure a document with
the Markdown format]

* **Test Definitions Dictionary**

* **Test Goals Dictionary**

* **Test Application Dictionary**

* **Other Sections** follow

* **Test Recording**

* **Test (Test Results) Reporting** [FIXME: This section should be merged into the
  previous, and both described within a common introduction] [FIXME:
  Need to address a concept of the granularity of test reports - it
  may not be necessary, for instance, to include the source code of a
  test form within a test report, in all site environments, but some
  sites might prefer such]

* [FIXME/TO DO: **Test Review and Analysis** - extensional to the Test
  Recording and Results Reporting dictionary, however
  integrating with a generic concept of **formalization for development
  processes** _vis a vis_ no _per se_ proprietary notion of
  _DevOps_. Although this system may not, in itself, be able to address
  the characteristics of individual test analysis environments, on
  desktop or mobile computing platforms, and within arbitrary _site
  environments_, the concept of _test analysis_ must at least be
  provided to some attention, in this system.]

* **Interface Macros and Functions**

* **Test Automation with ASDF**

#### Test Definitions Dictionary

* `test` [Standard Class]

* `test-predicate`, `test-predicate-function`, `test-name`, `test-object` [Accessor]

* `test-setup-function`, `(setf test-setup-function)` [Generic Function]

* `test-cleanup-function`, `(setf test-cleanup-function)` [Generic Function]

* `lisp-test` [Standard Class]
    * See also: `lisp-test-goal`

* `test-lambda-form`, `test-lambda-function` [Accessor]


#### Test Goals Dictionary

* `test-goal` [Standard Class]
    * See also: `test`, `test-record`

* `lisp-test-goal` [Standard Class]  ;; FIXME: Reevaluate how this extends `test-goal`

* `test-parameters` [Accessor]

* `test-expect-state` [Accessor]

* `test-predicate` [Accessor]


#### Test Application Dictionary

* `do-test-setup` [Generic Function]

* `do-test-cleanup` [Generic Function]

* `do-test` [Generic Function] ;; FIXME: Reevaluate for alignment to RUN-TEST


### Test Recording

#### Test Recording Concepts

In [AFFTA-SRC][affta-src] a _test record_  will contain the following features:

* Reference to _goal object_ provided for _test_
* Reference to _condition object_, as would be initialized either in
  an _error_ resulting in non-local exit of control from within a test
  _main_ function, or created on successful _completion_ of _test_
  `;;; FIXME:` If there is an _error_ resulting in non-local exit of
  control from the _setup_ or _cleanup_ function, the condition object
  created from that _error_ should be stored separately in the _test
  record_. `;;; TO DO:` Note that not every _not-local exit of control_
  will result in initialization of an _error_ object -- e.g. `break`
* References to _values_ returned by the _test setup function_, if any
  _test setup function_ is defined
* References to_values_ returned by _test main function_
* Reference to _values_ returned by _test cleanup function_, if any
  _test cleanup function_ is defined.


#### Test Recording Dictionary

[FIXME: Address the following item, within a "Class Reference" page]
* `test-record` [Standard Class]

[FIXME: Address all of the following, within a single, "Accessor Reference" page]
* `test-goal` [Accessor]
* `test-condition` [Accessor]
* `test-main-values` [Accessor]
* `test-setup-values` [Accessor]
* `test-cleanup-values` [Accessor]

[FIXME: Address each of the following, within a "Condition Reference" page]
* `test-condition` [Condition Class]
* `test-result` [Condition Class]
* `test-result-record` [Accessor]
* `test-failed` [Condition Class]
* `test-succeeded` [Condition Class]
*  See also: `utils:format-condition` [Generic Function]


### Test Reporting

[FIXME: Consider merging this section of the documentation into the Test
Recording section]


#### Test Reporting Concepts


* Interactive Reports

    * Instance Tests [FIXME: Review and refine this concept]

* Structured Reports

    * Instance Tests

    * Batch Tests [FIXME: This is pretty much "The thing" as towards any
      structured reports about test outcomes, in a system.]

#### Test Reporting Dictionary

* _TBD_

#### Interface Macros and Functions

* TO DO: `deftest` [Macro]

* TO DO: `with-test` [Macro]

* TO DO: `run-test` [Function]

    * TBD Test execution per _goals specified when run-test is evaluated_


* TO DO: `defsuite` [Macro]

* TO DO: `defgoals` [Macro]

* TO DO: `run-suite` [Function]

    * TBD Test execution per _goals defined with defgoals_

### ASDF System Testing Concepts

[FIXME: Revise title, to read, **Test Automation with ASDF**. Note
existing ASDF TEST-OP "Stuff". Note also, dependency management in
ASDF - per {components, operation} tries. See, perhaps, the NST system]

[FIXME: Introduce the concept, _test orchestration_ -- or rather, _test
automation_. Note also, Maven. Concerning "Review and Analysis," then
note also, the Mylyn framework in the Eclipse platform]

### ASDF System Testing Dictionary

* `test-component` (To Do) [Standard Class]

    * [FIXME: Address how ASDF may be applied as with a manner of
      **interface** onto any batch testing protocols defined in program
      source code, however utilizing this system. Note also, C, C++, and
      Java software systems. Note, testing support in BSD Ports -
      focusing about FreeBSD ports and pkgsrc port systems, namely.]

### Examples in Application (Early Edition)

**Ed. Note:** This system is still in early design.


## Development Plan

[FIXME - This section of the documentation needs to be completely
reworked, if not also presented in at least one more maintainable
format]

[FIXME: Move this section, all, to a separate Project Management
document. There, add any issue tracking statements - goals, bugs,
changelogs, etc.]

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
                            * In concepts developed within [AFFTA-SRC], a _test_, once applied, is comprised of a _test object_, a _test goal_, and a _test record_.
                                * A _test object_ represents a _form_ defined for purpose of evaluating the behaviors of a software system, for conformance with behaviors expected of the software system, by the system's developers. In addition to the lisp _form_ of a _test object_, a _test object_  may also be represented of additional _test utility_ _resources_ essentially external to the _Lisp environment_.
                                * A _test goal_ represents a set of parameters to a _test object_, as well as an expected _result condition_, contingent on successful evaluation of the _test object_. (In the instance of a simple, functional Lisp program test, for a _functional test_ `B` designed as to test the behaviors of a function `A`,  the set of _parameters_ to a _functional_ _test goal_ `G` onto `B` is represented simply of a list of arguments for the function `A`; the _result condition_ for `G`  onto `B` would be represented of a list of _values_ that the function `A` would be expected to return.)
                                * A _test record_ represents an instance of the evaluation of a _test_ with a single _test goal_ object. (In [AFFTA-SRC], a _test record_ for a simple, functional Lisp program test will store any condition objects representing a non-local exit of control from within the _test main form_)
                            * [AFFTA-SRC] is designed around a concept of _separation of concerns_ for definition of _test_ objects, _test goal_ objects, and _test record_ objects.
                            * TO DO: [AFFTA-SRC] must also define a concept of a _test suite_, such that should be well integrated within the Lisp environment's system definition facility, _viz a viz_ ASDF - as well as to be accessed from a functional interface, _viz a viz_ `CURRENT-TEST-SUITE`
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

* SMS messaging for batch testing notifications? (May it be possible to
  manage this with AWS? Something vis a vis Kannel, perhaps?)

* Integration with the debugger (cf. MCi `APPLICATION` system) for
  batch mode tests that can be held for developer interaction (??)
  [FIXME: Reconsider whether this may represent a tractable design goal,
  beyond how it may serve to illustrate a concept of integrating the
  test system within an interactive development environment in Common
  Lisp and UNIX systems.]


[EPL]: https://www.eclipse.org/legal/epl-v10.html
[affta-src]: https://github.com/thinkum/affta
[ltp-main-src]: https://github.com/thinkum/ltp
[test-protocol]: src/main/cltl/test-protocol.lisp
[test-protocol]: src/main/cltl/test-recording.lisp
[dobelle]: https://github.com/thinkum/dobelle
[ASDF]: http://common-lisp.net/project/asdf/
[mvn]: http://maven.apache.org/

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

<!-- FIXME - Merge LocalWords text -->

<!-- Local Variables: -->
<!-- ispell-buffer-session-localwords: ("Kannel" "NIST" "NST" "PCTE" "SDN" "SRI" "Servlets" "ToC" "changelogs" "pkgsrc" "sandboxed" "virtualization" "vis") -->
<!-- End: -->
