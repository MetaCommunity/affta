;; test-reporting.lisp - Test result reporting [AFFTA]

(in-package #:info.metacommunity.cltl.test)


(define-condition test-condition ()
  ((test
    :initarg :test
    :reader test-condition-test)))

(defgeneric format-test-label (test stream))

(defgeneric format-test-results (condition test stream))

(defmethod format-condtion ((condition test-condition) (stream stream))
  (let ((test (test-condition-test condition)))
    (format-test-label test stream)
    (format-test-results condition test stream)))


(define-condition test-failed (test-condition)
  ()
  (:report #'format-test-condition))


(defmethod format-test-condition ((condition test-failed)
                                  (stream stream))
  ;; FIXME/TO-DO: #I18N for condition/reporter format control strings
  (princ "Test failed" stream))


(define-condition test-succeeded (test-condition)
  ()
  (:report #'format-test-condition))


(defmethod format-test-condition ((condition test-succeeded)
                                  (stream stream))
  ;; FIXME/TO-DO: #I18N for condition/reporter format control strings
  (princ "Test succeeded" stream))



;;;; Trivial application notification framework for condition handling

;; FIXME: 
;;
;; ;; 0) Document how this <framework> is used within DO-RECORDED-TEST
;;
;;       Towards that regard, an initial summary about the APPLICATION system:
;;
;;        1. [AFFTA] represents the first usage case for this
;;           application notification framework. 
;;
;;           In the origins of this framework's first application
;;           within AFFTA, it was observed: That if an _error_, _warning_,
;;           or other unhandled _condition_ occurs during the
;;           application of a _test protocol_, it may be both feasible
;;           and supportive to the application developer, to ensure
;;           that the developer would be notified of the _condition_ --
;;           moreover, that not only may the _notification_ object
;;           include a representation of the _condition_ object - as
;;           would be in a format appropriate to the respective
;;           _notification media_ - but that furthermore, the
;;           _notification_ object may include a representation of a
;;           single _context object_ indicating the origin of the
;;           _condition_ object.
;;
;;          In the initial development of this application
;;          notification protocol, it was observed that the class NULL
;;          may serve as a  convenient designator for a "null
;;          application" -- as in a context in which an application's
;;          development is essentially being conducted, informally, by
;;          way of direct stream I/O onto a locally accessible Common
;;          Lisp implementation.
;;
;;          This notification protocol may serve as a component of a
;;          system for supporting application design within an
;;          integrated development environment, and may be furthermore
;;          extended for appliation within a server environment.
;;           
;;
;; 1) Move this into a new 'application' system [MCi]
;;
;; 2) Document NOTIFY [standard generic function]
;;
;; 3) Describe %APPLICATION% [Variable]
;;
;; 4.A) Extend this siple framework for an appliation system such that
;;      would be running within an Amazon Web Services (AWS) Elastic
;;      Compute Cloud (EC2) _instance_, and therefore would be able to
;;      utilize the AWS notifications API - namely, as for notifying
;;      an application's maintainer and/or maintenance staff of any
;;      conditions observed within the Common Lisp appliation's
;;      runtime environment(w/ a top priority notification) 
;;      any error conditions, if not also (w/ a less priority
;;      notification) warning conditions occurring within the Common
;;      Lisp application's environment on the server - cf. also
;;      Nagios, Daemontools' Multilog, and SystemD, as in a context of
;;      server (if not also desktop) shell intefaces, as well as (in a
;;      Java server domain) Tomcat, Glassfish, and Liferay
;;      
;; 4.B) Extend this simple framework for a build system
;;      utilizing Hudson[1][2][3] within Fusion Forge[4][5][6][7]
;;      [Java] as in which an %application% would be:
;;
;;         1. In a development domain, an object of functional
;;            testing in a context of continuous integration[8]
;;
;;         2. In a resource management and resource distribution
;;            domain, an object for distribution e.g. to individual
;;            Maven, Debian, Cygwin, and other package repositories,
;;            as in a context of continuous delivery[8]
;;
;;         3. Once distributed, then an object for "issue tracking"
;;            such as with regards to individual issue tracking
;;            services utilized within indivudal Linux distributions
;;            (cf. Debian's 'reportbug' interface and broader issue
;;            tracking system), and  desktop environments (e.g. KDE
;;            and GNOME) as well as within individual applications 
;;            (e.g. Firefox, Google Chrome, and applications developed
;;            as components of the respective desktop environment,
;;            e.g. WebKit) in a context of developer support, if not
;;            moreover in a context of service customer support and
;;            overall customer fulfillment
;;
;;         4. In a  _network services domain_, an object running on
;;            one or more _application hosts_, such that would be
;;            _managed_ -- directly and/or indirectly -- by a
;;            developer developing this software, likewise cf. AWS,
;;            furthermore cf. DeLorme inReach
;;
;; 4.C) Illustrate how this simple generic function may be applied
;;      within each of
;;
;;     A) a desktop %application% using Garnet
;;
;;     B) an HTTP server %application% using Alexandria and/or CL-HTTP
;;
;;     C) an Eclpse IDE extension using ABCL and/or {CL+J, FOIL, ...}
;;        in which the %application% may represent simply an interface
;;        onto the Eclipse IDE - as the Eclipse IDE being, from the
;;        perspective of the Commonn Lisp peer, a _Java Application_ [Java]
;;
;;     D) then define CORBA IDL for Nr. C, and extend as at least a
;;        simple (?) CORBA-integrated message passing / application
;;        notification framework for Common Lisp peer applications
;;
;; Resource notes:
;;
;; [1] [Hudson Continuous Integration](http://hudson-ci.org/)
;; [2] [Hudson Continus Integration - Eclipse Foundation](http://www.eclipse.org/hudson/)
;; [3] [Installing Hudson - Eclipsepedia](http://wiki.eclipse.org/Hudson-ci/Installing_Hudson)
;; [4] [Forum: GForge is now FusionForge](https://fusionforge.org/forum/forum.php?forum_id=7)
;; [5] [FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Main_Page)
;; [6] [Installing - FusionForge Wiki](https://fusionforge.org/plugins/mediawiki/wiki/fusionforge/index.php/Installing)
;; [7] [Debian Package Tracking System - fusionforge](https://packages.qa.debian.org/f/fusionforge.html)
;; [8] Prakash, Winston. [Practicing Continuous Delivery Using Hudson](http://www.eclipse.org/hudson/the-hudson-book/book-hudson.pdf)


(defvar %application% nil)

;; TO DO - w/ study onto POSIX
;; (defclass process ()
;;   ())
;;
;; #+POSIX
;; (defclass thread-process (local-process)
;;   (...))
;;
;; (defgeneric get-process-message-bus (process))
;;
;; (defclass clim-application (process) ...)
;;  ;; application-frame, etc.
;;
;; (defclass remote-process (process) ...)
 

(defgeneric format-condition (condition stream))

(define-condition encapsulated-condition ()
  ;; This class might be semantically similar to CELL-ERROR, but does
  ;; not include ERROR in its class precedence list
  ((object
    :initarg :condition
    :accessor encapsulated-condition-object)))

(define-condition application-condition (encapsulated-condition)
  ((application
    :initarg :application
    :initform %application%
    :reader application-condition-application))
  (:report
   (lambda (c s) (format-condition c s))))

(defmethod format-condition ((condition application-condition)
                             (stream stream))
  ;; FIXME: #I18N
  (format stream "~<Encapsulated condition within ~A:~> ~<~A~>"
          (application-condition-application condition)
          (encapsulated-condition-object condition)))

(define-condition application-error (error application-condition)
  ())

#+NIL
(handler-case
    (simple-program-error "Ping ping ping ping ~s" (get-universal-time))
  (error (c)
    (error 'application-error 
           :application "Foo top level foo" 
           :condition c)))


(define-condition application-warning (warning application-condition)
  ())


(defgeneric notify (application context condition)

  (:method ((application null) context (condition error))
    (error condition))

  (:method ((application null) context (condition warning))
    (warn condition))

  (:method ((application null) context (condition condition))
    (signal condition)))

;; NOTE/TODO: This system now dep. on [system Bordeaux Threads]
#+Bordeaux-Thread
(defun notify* (condition)
  ;; FIXME: 
  ;;
  ;; 1. Define a CLASS, THREAD, utilizing of Bordeaux-Threads'
  ;;    implementation-specific DEFTYPE interface for reference
  ;;
  ;;    [system TBD : MCI // process, container for subsequent items
  ;;    denoted here]
  ;;
  ;; 2. Specialize NOTIFY onto (CONTEXT THREAD)
  ;;
  ;; 3. Define a class, PROCESS, utilizing of OSICAT's POSIX interface
  ;; 
  ;; 4. Specialize NOTIFY onto (CONTEXT PROCESS)
  ;;
  ;; 5. Define a class, HOST, utilizing of AWS' EC2 API
  ;; 
  ;; 6. Define an interface for computing the {AWS messaging} identity
  ;;    for [a] {host administrator[s]} (cf. also AWS IAM) via the AWS
  ;;    API [Java]
  ;;
  ;; 7. Specialize NOTIFY onto HOST, such that a host administrator
  ;;    would be notified of the respective CONDITION in the specified
  ;;    APPLICATION at the effective "base" of the notification stack
  ;;
  ;; 8. Specify a clear policy that for a "Development server," the
  ;;    "host administrator" is a developer - as then in a context
  ;;    primarily of executive dissertations, if not
  ;;    entrepreneurialism
  ;;
  (notify %application% (bordeaux-threads:current-thread) condition))
