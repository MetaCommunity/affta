Streams in AFFTA
================

## TO DO: Define UNION-STREAM, ANNOTATED-UNION-STREAM

**NOTE:** This may entails development specifically in MCi AFFTA 
and in the MCi APPLICATION system

**Goal:** define a single class of _union stream_ with a
related i/o control backend, such that can encapsulate
all of  the "Streams in CLtL2" and filter those in a
time-ordered manner onto standard POSIX file
descriptors or onto one or more files. That stream
classes, then, may be used during `DO-RECORDED-TEST`. 

**Additional caveats:**
1. The "union stream" should publish an interface
    allowing a program to access each of the individual
    "Streams in CLtL2" seperately for input and/or
    output

2. The "union stream" class may be extended with an
    "annotated union stream" class, such that would
    annotate each line printed onto one of the
    "component streams", similar to syslog annotations
    (minus timestamps. See also: Daemontools//Multilog)

3. The "annotated union stream" class should allow for a
   Common Lisp application to direct all annotated
   output to a single file

4. In application within DO-RECORDED-TEST, the
    "union stream"  and its subclass, "annotated union
    stream" should allow for an application to
    effectively "tee" any of the "component streams"
    and/or the combined "union stream" onto an
    individual file

5. The test framework should "prune out" any zero-length
   files.

6. The "union streams" implementation shall extend of
   Allegro CL's Simple Streams framework

7. The "union streams" implementation should be extended
   for thread-local application within graphical Common
   Lisp programs on a Common Lisp desktop

For purpose of supporting interaction with the
developer, regardless of host platform, AFFTA may be
developed together with the MCI Application system

A "headless test session" application may be defined,
such that would:
1. Either
    * **1.A** Prevent that any of the input streams would be 
       interactive, xor....
    * **1.B** Allow for the developer to <conveniently> provide a
       string or file for input to any of the respective
       input streams
2. Record any entries to the debugger and abort tests
3. Save all output in a file
4. Notify the developer once a test session is concluded
5. Provide a convenient batch test configuration syntax
6. Be capable of running as a "stand alone" application
   in Hudson or Taylor (CI) 

## Streams in CLtL2

*  **"POSIX friendly" streams**
    * `*standard-input*`
    * `*standard-output*`
    * `*error-output*`
* **Other CLtL2 output streams**
    * `*trace-output*`
* **СLtL2 I/O Streams (Bidiirectional Streams)** 
    * `*debug-io*`
    * `*query-io*`
    * `*terminal-io*`

Referencing the CLHS, certainly `*QUERY-IO*` would have been
an interactive stream, possibly implemented onto CLIM,
on the LispM desktop.

Orthogonally, `*DEBUG-IO*` may be used with an interactive
systems diagnostics application, as when the containing
Lisp host is "running in debug mode"

### Gray Streams

* [Gray Streams interface in Franz ACL](http://franz.com/support/documentation/current/doc/gray-streams.htm)
* [Trivial-Gray-Streams project](http://common-lisp.net/project/trivial-gray-streams/)
* Applied in McCLIM

### Simple Streams

* [Simple Streams interface in Franz ACL](http://franz.com/support/documentation/current/doc/streams.htm)
* [Simple Streams interface in SBCL](http://www.sbcl.org/manual/#Simple-Streams)

## Streams in POSIX

* File Descriptors
* Terminals, Pseudoterminals, ...
* ... (LibC API)
* See also: [Osicat](http://common-lisp.net/project/osicat/)

## Streams in a SLIME REPL [Existing precedents in CLtL2/IO]

    (type-of *STANDARD-OUTPUT*)
    => SWANK-BACKEND::SLIME-OUTPUT-STREAM

    (type-of *STANDARD-INPUT*)
    => SWANK-BACKEND::SLIME-INPUT-STREAM

    (eq *STANDARD-OUTPUT* *ERROR-OUTPUT*)
    => t ;; "unlike a POSIX PTY", as "by default" i.e. w/o IO redirection

    (every #'(lambda (s) (typep s 'two-way-stream)) (list *debug-io* *query-io*))
    => T

    (eq *debug-io* *query-io*)
    => T

    (eq *debug-io* *terminal-io*)
    => T
