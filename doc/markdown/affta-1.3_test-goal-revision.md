# AFFTA-1.3 TEST-GOAL test protocol revision

## Overview

The functional testing interface defined in AFFTA was revised,
substantially, between AFFTA revisions 1.2 and 1.3.

* AFFTA 1.2, in effect, was the last minor version if the initial
  prototype for the AFFTA system
  
* AFFTA 1.3 provides a TEST-GOAL class, more substantially integrated
  with the AFFTA functional testing protocol

Some conepts were defined in AFFTA 1.2 such that -- although
effectively removed in AFFTA 1.3 -- may be worth of a note, for
further reference.

* Functional forms - Monadic, Diadic, and Variadic Functional Forms

    * Proceeding to AFFTA 1.2, AFFTA 1.0 was designed with an exacting
      representation of _functional lambda lists_, for _functional
      testing_ within Common Lisp applications. In the initial
      prototype, namely AFFTA 1.0, it was thought that the Common Lisp
      functional _namespace_ would be well modeled for functional
      testing, if on a basis of the number  of _required arguments_ to
      the _lambda list_ of the respective _function_. Thus, within
      AFFTA 1.0, there were three individual _implementation classes_
      defined for _functional tests_ in Common Lisp applications:

          * **Monadic Functional Test**, for testing funtions
            accepting of exactly one argument, e.g #'CL:NOT

          * **Diadic Functional Test**, for testing functions
            accepting of exactly two arguments, e.g. #'CL:EQL

          * **Variadic Functional Test**, for testing functions
            accepting of a variable number of arguments, e.g #'CL:=
            
    * It was thought that the correct _implementation class_ could be
      selected in a _dispatching form_, such as would calculate the
      number of _required arguments_ for a function.

    * Those specialized types of functional test, perhaps, may
      have served an illustrative role with regards to low-level
      control flow within a Common Lisp implementation, as _viz a
      viz_ a host machine's underlying _assembler
      routines_. However, proceeding to AFFTA 1.3, it was
      determined that those types of _functional test_ were, in a
      sense, redundant within the AFFTA funtional testing protocol,
      revision 1.3

Though the "major version" number has been retained between the two
respective revisions, however AFFTA 1.3 is not backwards compatible
with AFFTA 1.2.

The following section provides an outline of changes between AFFTA 1.2
and AFFTA 1.3, primarily in regards to functional forms in the AFFTA
functional tsting protocol.

## Outline of changes in `test-protocol.lisp`

* TEST-SETUP-FUNCTION
    - revise COND expression for default clause

* TEST-CLEAUP-FUNCTION 
    - revise COND expression for default clause

* #'DO-TEST-SETUP
    - revise lambda list for providing a TEST-GOAL to the generic function

* #'DO-TEST-CLEANUP
    - revise lambda list for providing a TEST-GOAL to the generic function

* #'DO-TEST
    - revise lambda list for providing a TEST-GOAL to the generic function

* DO-TEST (LIST LIST FUNCTION)
    - removed. Refer to DO-TEST (LIST FUNCTION)

* DO-TEST (LIST LIST DIADIC-VALUES-TEST)
    - removed

* DO-TEST (LIST LIST MONADIC-VALUES-TEST)
    - removed

* DO-TEST (LIST LIST VARIADIC-VALUES-TEST)
    - removed

* DO-TEST (LIST FUNCTION)
    - new; effectively replaces previous DO-TEST (LIST LIST FUNCTION) method.

* DO-TEST :AROUND (T T T)
    - revise lambda list for providing a TEST-GOAL to the generic function
    - refer to subsequent form, DO-TEST :AROUND (LISP-TEST-GOAL T)

* DO-TEST :AROUND (LISP-TEST-GOAL T)
    - new
    - revise lambda list for providing a TEST-GOAL to the generic function
    - store return values from setup, main, and cleanup forms within the test record object
    - ensure that the test-main form will be evaluated within a HANDLER-CASE body
    - revise test results singal for updated TEST-RESULT condition class


* DO-RECORDED-TEST
    - make primary method inactive, proceeding to removing the function 

* %TEST-RECORD-CLASS% - removed
