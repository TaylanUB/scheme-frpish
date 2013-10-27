;;; FRPish --- Functional reactive programming alike framework

;; Copyright (C) 2013  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions frp functional reactive

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 


;;; Code:

(define-module (frpish base)
  #:export
  (
   make-value
   taint!
   get
   trigger!
   ))

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 control))

(define-record-type <value>
  (%make-value expression dependencies dependents cache version)
  value?
  (expression   expression)
  (dependencies dependencies)
  (dependents   dependents)
  (cache        get-cache set-cache!)
  (version      version   set-version!))

(define-syntax make-value
  (syntax-rules ()
    ((_ (dependency ...) expression)
     (let ((expression* (lambda ()
                          (let ((dependency (get-cache dependency)) ...)
                            expression)))
           (dependencies (list dependency ...)))
       (let ((value (%make-value
                     expression* dependencies (make-weak-key-hash-table)
                     (if (null? dependencies)
                         (expression*)
                         #f)
                     (if (null? dependencies)
                         (cons #f #f)
                         (let ((table (make-hash-table)))
                           (for-each (lambda (root)
                                       (hashq-set! table root #f))
                                     (find-roots dependencies))
                           table)))))
         (hashq-set! (dependents dependency) value #f) ...
         value)))))

(define (find-roots values)
  (let loop ((roots '())
             (values values))
    (if (null? values)
        roots
        (loop (lset-union eq? roots (let* ((value (car values))
                                           (dependencies (dependencies value)))
                                      (if (null? dependencies)
                                          (list value)
                                          (find-roots dependencies))))
              (cdr values)))))

(define (taint! value)
  (set-version! value (cons #f #f)))

(define (tainted? value)
  (and (not (pair? (version value)))
       (let/ec return
         (hash-for-each (lambda (root version*)
                          (if (not (eq? (version root) version))
                              (return #t)))
                        (version value))
         #f)))

(define (primitive-refresh! value)
  (let ((result ((expression value))))
    (set-cache! value result)
    (hash-for-each-handle (lambda (entry)
                            (set-cdr! entry (version (car entry))))
                          (version value))))

(define (refresh-if-needed! value)
  (when (tainted? value)
    (for-each refresh-if-needed! (dependencies value))
    (primitive-refresh! value)))

(define (get value)
  (refresh-if-needed! value)
  (get-cache value))

(define (trigger! value)
  (primitive-refresh! value)
  (hash-for-each (lambda (value _)
                   (trigger! value))
                 (dependents value)))

;;; base.scm ends here
