;;; oclosure-tests.e; --- Tests for Open Closures  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'oclosure)
(require 'cl-lib)

(oclosure-define (oclosure-test
                  (:copier oclosure-test-copy)
                  (:copier oclosure-test-copy1 (fst)))
  "Simple OClosure."
  fst snd name)

(cl-defmethod oclosure-test-gen ((_x compiled-function)) "#<bytecode>")

(cl-defmethod oclosure-test-gen ((_x cons)) "#<cons>")

(cl-defmethod oclosure-test-gen ((_x oclosure))
  (format "#<oclosure:%s>" (cl-call-next-method)))

(cl-defmethod oclosure-test-gen ((_x oclosure-test))
  (format "#<oclosure-test:%s>" (cl-call-next-method)))

(ert-deftest oclosure-test ()
  (let* ((i 42)
         (ocl1 (oclosure-lambda (oclosure-test (fst 1) (snd 2) (name "hi"))
                   ()
                 (list fst snd i)))
         (ocl2 (oclosure-lambda (oclosure-test (name (cl-incf i)) (fst (cl-incf i)))
                   ()
                 (list fst snd 152 i))))
    (should (equal (list (oclosure-test--fst ocl1)
                         (oclosure-test--snd ocl1)
                         (oclosure-test--name ocl1))
                   '(1 2 "hi")))
    (should (equal (list (oclosure-test--fst ocl2)
                         (oclosure-test--snd ocl2)
                         (oclosure-test--name ocl2))
                   '(44 nil 43)))
    (should (equal (funcall ocl1) '(1 2 44)))
    (should (equal (funcall ocl2) '(44 nil 152 44)))
    (should (equal (funcall (oclosure-test-copy ocl1 :fst 7)) '(7 2 44)))
    (should (equal (funcall (oclosure-test-copy1 ocl1 9)) '(9 2 44)))
    (should (cl-typep ocl1 'oclosure-test))
    (should (cl-typep ocl1 'oclosure))
    (should (member (oclosure-test-gen ocl1)
                    '("#<oclosure-test:#<oclosure:#<cons>>>"
                      "#<oclosure-test:#<oclosure:#<bytecode>>>")))
    ))

(ert-deftest oclosure-test-limits ()
  (should
   (condition-case err
       (let ((lexical-binding t)
             (byte-compile-debug t))
         (byte-compile '(lambda ()
                          (let ((inc-where nil))
                            (oclosure-lambda (advice (where 'foo)) ()
                              (setq inc-where (lambda () (setq where (1+ where))))
                              where))))
         nil)
     (error
      (and (eq 'error (car err))
           (string-match "where.*mutated" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand-all '(oclosure-define oclosure--foo a a))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot name: a$" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand-all
               '(oclosure-define (oclosure--foo (:parent advice)) where))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot name: where$" (cadr err))))))
  (should
   (condition-case err
       (progn (macroexpand '(oclosure-lambda (advice (where 1) (where 2)) () where))
              nil)
     (error
      (and (eq 'error (car err))
           (string-match "Duplicate slot: where$" (cadr err)))))))

(oclosure-define (oclosure-test-mut
                  (:parent oclosure-test)
                  (:copier oclosure-test-mut-copy))
  "Simple OClosure with a mutable field."
  (mut :mutable t))

(ert-deftest oclosure-test-mutate ()
  (let* ((f (oclosure-lambda (oclosure-test-mut (fst 0) (mut 3))
                (x)
              (+ x fst mut)))
         (f2 (oclosure-test-mut-copy f :fst 50)))
    (should (equal (oclosure-test-mut--mut f) 3))
    (should (equal (funcall f 5) 8))
    (should (equal (funcall f2 5) 58))
    (cl-incf (oclosure-test-mut--mut f) 7)
    (should (equal (oclosure-test-mut--mut f) 10))
    (should (equal (funcall f 5) 15))
    (should (equal (funcall f2 15) 68))))

(oclosure-define (oclosure-test-mixin1 (:mixin t) (:parent oclosure-test))
  a b (bm :mutable t))
(oclosure-define (oclosure-test-mixin2 (:mixin t) (:parent oclosure-test))
  a c (cm :mutable t))
(oclosure-define (oclosure-test-mixin3
                  (:parent oclosure-test-mixin1 oclosure-test-mixin2))
  d)

(ert-deftest oclosure-test-mixin ()
  (let ((ocl1 (oclosure-lambda (oclosure-test-mixin1 (a 'a1) (b 'b1) (bm 'bm1))
                  (x) (list a b x)))
        (ocl2 (oclosure-lambda (oclosure-test-mixin2 (a 'a2) (c 'c2) (cm 'cm2))
                  (x) (list a c x)))
        (ocl3 (oclosure-lambda (oclosure-test-mixin3
                                (a 'a3) (b 'b3) (bm 'bm3) (c 'c3) (cm 'cm3)
                                (d 'd3))
                  (x) (list a b c d x))))
    (should (cl-typep ocl3 'oclosure-test-mixin1))
    (should (cl-typep ocl3 'oclosure-test-mixin2))
    (should (equal 'a1 (oclosure-test-mixin1--a ocl1)))
    (should (equal 'a3 (oclosure-test-mixin1--a ocl3)))
    (should (equal 'a2 (oclosure-test-mixin2--a ocl2)))
    (should (equal 'a3 (oclosure-test-mixin2--a ocl3)))
    (should (equal 'b1 (oclosure-test-mixin1--b ocl1)))
    (should (equal 'b3 (oclosure-test-mixin1--b ocl3)))
    (should (equal 'bm1 (oclosure-test-mixin1--bm ocl1)))
    (should (equal 'bm3 (oclosure-test-mixin1--bm ocl3)))
    (should (equal 'c2 (oclosure-test-mixin2--c ocl2)))
    (should (equal 'c3 (oclosure-test-mixin2--c ocl3)))
    (should (equal 'cm2 (oclosure-test-mixin2--cm ocl2)))
    (should (equal 'cm3 (oclosure-test-mixin2--cm ocl3)))
    (should (equal 'd3 (oclosure-test-mixin3--d ocl3)))))

(ert-deftest oclosure-test-slot-value ()
  (require 'eieio)
  (let ((ocl1 (oclosure-lambda (oclosure-test-mixin1 (a 'a1) (b 'b1) (bm 'bm1))
                  (x) (list a b x)))
        (ocl2 (oclosure-lambda (oclosure-test-mixin2 (a 'a2) (c 'c2) (cm 'cm2))
                  (x) (list a c x)))
        (ocl3 (oclosure-lambda (oclosure-test-mixin3
                                (a 'a3) (b 'b3) (bm 'bm3) (c 'c3) (cm 'cm3)
                                (d 'd3))
                  (x) (list a b c d x))))
    (should (equal 'a1  (slot-value ocl1 'a)))
    (should (equal 'a2  (slot-value ocl2 'a)))
    (should (equal 'a3  (slot-value ocl3 'a)))
    (should (equal 'b1  (slot-value ocl1 'b)))
    (should (equal 'b3  (slot-value ocl3 'b)))
    (should (equal 'bm1 (slot-value ocl1 'bm)))
    (should (equal 'bm3 (slot-value ocl3 'bm)))
    (should (equal 'c2  (slot-value ocl2 'c)))
    (should (equal 'c3  (slot-value ocl3 'c)))
    (should (equal 'cm2 (slot-value ocl2 'cm)))
    (should (equal 'cm3 (slot-value ocl3 'cm)))
    (should (equal 'd3  (slot-value ocl3 'd)))
    (setf (slot-value ocl3 'cm) 'new-cm3)
    (should (equal 'new-cm3 (slot-value ocl3 'cm)))
    (should-error (setf (slot-value ocl3 'c) 'new-cm3) :type 'setting-constant)
    (should (equal 'c3 (slot-value ocl3 'c)))
    ))

(ert-deftest oclosure-test-anonymous ()
  (let ((ocl1 (oclosure-lambda (oclosure-test
                                oclosure-test-mixin1
                                (fst 'fst) (snd 'snd)
                                (a 'a))
                  (x)
                (list x fst)))
        (ocl2 (oclosure-lambda (oclosure-test (fst 'fst) (snd 'snd))
                  (x)
                "Doc"
                (interactive "P")
                (list x snd))))

    (should (equal (oclosure-test-mixin1--a ocl1)
                   'a))
    (should (equal (oclosure-test--snd ocl1)
                   'snd))
    (should (equal (funcall ocl1 'x) '(x fst)))
    (should (cl-typep ocl1 'oclosure-test))
    (should (cl-typep ocl1 'oclosure-test-mixin1))
    (should (cl-typep ocl1 '(and oclosure-test oclosure-test-mixin1)))

    (should (cl-typep ocl2 '(and oclosure-command oclosure-documented)))
    (should (equal (interactive-form ocl2) '(interactive "P")))
    (should (commandp ocl2))
    (should (equal (let ((current-prefix-arg 'pfx))
                     (call-interactively ocl2))
                   '(pfx snd)))
    (should (equal "Doc" (documentation ocl2)))))

;;; oclosure-tests.el ends here.
