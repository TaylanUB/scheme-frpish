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

(use-modules (srfi srfi-9))

(define-record-type <value>
  (%make-value expression dependencies dependents cache tainted?)
  value?
  (expression   expression)
  (dependencies dependencies)
  (dependents   dependents)
  (cache        get-cache set-cache!)
  (tainted?     tainted?  set-tainted!))

(define-syntax make-value
  (syntax-rules ()
    ((_ (value ...) expression)
     (let ((expression* (lambda ()
                          (let ((value (get-cache value)) ...)
                            expression)))
           (dependencies (list value ...)))
       (let ((value* (%make-value
                      expression* dependencies (make-weak-key-hash-table)
                      #f #t)))
         (for-each (lambda (dependency)
                     (hashq-set! (dependents dependency) value* #f))
                   dependencies)
         value*)))))

(define (taint! value)
  (unless (tainted? value)
    (set-tainted! value #t)
    (hash-for-each (lambda (value _)
                     (taint! value))
                   (dependents value))))

(define (primitive-refresh! value)
  (let ((result ((expression value))))
    (set-cache! value result)
    (set-tainted! value #f)))

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
