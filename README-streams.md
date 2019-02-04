Streams in AFFTA
================

[FIXME: Note the tai64n tools from the daemontools project. Note also,
timestamps in Common Lisp programs and timestamps in UNIX
environments. This may be addressed in the LTP project.]

## TO DO: Define UNION-STREAM, ANNOTATED-UNION-STREAM

**NOTE:** This may entail development in MCi AFFTA and in the MCi
APPLICATION (cf. dobelle-app) systems

**Goal:** define a single class of _union stream_ with a
related i/o control backend, such that can encapsulate
all of  the "Streams in CLtL2" and filter those in a
time-ordered manner onto standard POSIX file
descriptors or onto one or more files. That stream
system, then, may be used during `DO-RECORDED-TEST`.

[FIXME: In a manner after daemontools multilog, this may represent
something of a concept of "Anntotated Output Streams," albeit somewhat
conflated - here - with a concept of "Stream Unification" for output
streams. Principally, this is to allow for annotation of output streams,
regardless of output stream implementation. This also does not address
how an "Annotated Output Stream" may be processed for "Timestamp" or
"Stream Source" and "Entry" data, as an input stream.]

[FIXME: A Union Stream may not, _per se_, represent a sufficient
alternative to a PTY. Regarldess of the ambiguity about multilog, here,
this may represent something towards an effort for ensuring that the
output echoed from an output stream, at a PTY or in a file or in any
other manner, will be visible as to resolve the question, "Which output
stream did this arrive from?". To be sure, it may be something of a
hack.]


**Additional caveats:**

1. The "union stream" should publish an interface
    allowing a program to access each of the individual
    "Streams in CLtL2" seperately for input and/or
    output [FIXME: This may be similar to some concepts already
    developed in CLtL2/ANSI CL/CLHS streams. The concept is represented
    independently, here, towards extension for the "Annotated Union
    Streams" concept] [FIXME: This is where it begins to resemble an
    analogy to a PTY]

2. The "union stream" class may be extended with an "annotated union
    stream" class, such that would annotate each line printed onto one
    of the "component streams" [NB The annotation would serve to
    identify which output stream the output is produced from] similar to
    syslog annotations (minus timestamps [FIXME: Revise this]. See also:
    Daemontools//Multilog)

3. The "annotated union stream" class should allow for a Common Lisp
   application to direct all annotated output to a single file

4. In application within DO-RECORDED-TEST, the "union stream" and its
    subclass, "annotated union stream" should allow for an application
    to effectively "tee" any of the "component streams" and/or the
    combined "union stream" onto an individual file

5. The test framework should "prune out" any zero-length files.

6. The "union streams" implementation shall extend of Allegro CL's
   Simple Streams framework

7. The "union streams" implementation should be extended for
   thread-local application within graphical Common Lisp programs on a
   Common Lisp desktop

For purpose of supporting interaction with the developer, regardless of
host platform, AFFTA may be developed together with the MCI Application
system

A "headless test session" application may be defined, such that would:

1. Either

    * **1.A** Prevent that any of the input streams would be
       interactive, or....

    * **1.B** Allow for the developer to <conveniently> provide a string
       or file for input to any of the respective input streams

2. Record any entries to the debugger and abort tests

3. Save all output in a file

4. Notify the developer once a test session is concluded

5. Provide a convenient batch test configuration syntax

6. Be capable of running as a "stand alone" application
   in Hudson or Taylor or Jenkins or (....) (CI Web Portals)
   [FIXME: Reconsider this. Though novel, this procedural outline may be
   a bit too general to be practical]

## Streams in CLtL2

*  **"POSIX friendly" streams** [FIXME Stream Symbols, rather, and
   moreso towards libc conventions.]
    * `*standard-input*`
    * `*standard-output*`
    * `*error-output*`

* **Other CLtL2 output streams**
    * `*trace-output*`

* **СLtL2Bidirectional Streams)**
    * `*debug-io*`
    * `*query-io*`
    * `*terminal-io*`

[Note that all of those bidirectional streams' values may be EQ under
some of SLIME]

Referencing the CLHS, perhaps `*QUERY-IO*` may have been available as
an interactive stream, possibly implemented onto CLIM,
and possibly accessible from a Lisp Machine desktop environment

Orthogonally, `*DEBUG-IO*` may have been used with an interactive
systems diagnostics application, as when the containing
Lisp host is "running in debug mode" [FIXME Speculative, N/A]

[FIXME: If one must speculate - may terminal-io have been used within Multics?]

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

[FIXME: See also SUSv4; note STREAMS from SUN and XSI Streams, historically]

