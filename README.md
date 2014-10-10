AFFTA - Another Framework for Functional Test Application 
=========================================================

Alternately, Another Framework for Functional Test Automation (AFFTA)
in a Common Lisp Platform

## Summary

_(TBD. API is in revision, towards AFFTA 1.3+)_

## Dependencies

AFFTA 1.2 depends on the following systems:

* [info.metacommunity.cltl.utils][mci-cltl-utils]

## Licensing

AFFTA is licensed under the terms of the [Eclipse Public License 1.0][EPL]

## Availability

The primary source tree for AFFTA is hosted at GitHub:

[git@github.com:MetaCommunity/affta.git][affta]


## Development Plan

### AFFTA-1.2

* Develop test recording framework, seperate from test definition framework


### AFFTA-1.3

* Focusing on `test-protocol.lisp`, revise the primary test protocol so
  as to accept a single TEST-SPECIFIER argument in each of:

>    `DO-TEST`
>    `DO-TEST-SETUP`
>    `DO-TEST-CLEANUP`


* Focusing on `test-classes.lisp`, implement `FUNCTIONAL-TEST`

### AFFTA-1.4

* Developing MCi AFFTA in parallel with the MCi `APPLCIATION` system, 
  implementing the `UNION-STREAM` specification (presently denoted in
  test-protocol.lisp)

### AFTA-1.5

* Focusing on the MCi `APPLICATION` system, develop support for using
  the Amazon Web Services (AWS) API for notifying a developer when a
  batch test completes


* Referencing `test-protocol.lisp` (current revision) Develop a suitable
  batch testing model, for providing integration of AFFTA  with Hudson 
  Continuous Integration (CI) and AWS


### AFFTA-2

focusing on `test-classes.lisp`, implement `APPLICATION-TEST`

### AFFTA-3

focusing on `test-classes.lisp`, implement `ROOTFS-TEST`

### Other features to be developed

* SMS messaging for batch testing notifications? (Is that possible w/ AWS?)

* Integration with the debugger (cf. MCi `APPLICATION` system) for
  batch mode tests that can be held for developer interaction


[EPL]: https://www.eclipse.org/legal/epl-v10.html
[affta]: https://github.com/MetaCommunity/affta
[mci-cltl-utils]: https://github.com/MetaCommunity/mci-cltl-utils
