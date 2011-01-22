;;; align-cljlet.el  -- Align clojure let functions

;; Copyrigth (C) 2011  Glen Stampoultzis

;; Author: Glen Stampoultzis <gstamp(at)gmail.com>
;; Version: $Id:$
;; Keywords; clojure, align, let
;; URL: https://github.com/gstamp/align-cljlet
;;

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Description:
;;
;; This program exists because I was tired of manually aligning let
;; statements in clojure.  This program is designed to quickly and
;; easily allow let forms to be aligned.  This is my first emacs
;; lisp program and as a result if probably less than optimal.  Feel
;; free to suggest improvements or send in patches.
;;
;; This program was inspired by align-let.el although does not share
;; any of it's code.  I had considered altering align-let.el to
;; work correctly with Clojure however it was easiler to simply
;; start from scratch.
;; 
;;
;;; Known limitations:
;;
;; * This program requires clojure mode to be running in order to
;;   function correctly.
;;
;;; Installation:
;;
;;   To use align-cljlet.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'align-cljlet)
;;
;;; Usage:
;;
;; To invoke simply position anywhere inside the let statement and
;; invoke:
;;
;; M-x align-cljlet
;;
;; You may wish to bound this to a specific key.
;;


(defun acl-found-let ()
  (looking-at "\\s(let"))
  

(defun acl-try-go-up ()
  (condition-case nil
      (up-list -1)
    (error
     (error "Not in a \"let\" form")))
  t)

(defun acl-find-let ()
  (while
      (if (acl-found-let)
          nil
        (acl-try-go-up)
        ))
  t)

(defun acl-goto-next-pair ()
  (interactive)
  (condition-case nil
      (progn
        (forward-sexp)
        (forward-sexp)
        (forward-sexp)
        (backward-sexp)
        t)
    (error nil)))

(defun acl-get-width ()
  (save-excursion
    (let ((p (point)))
      (forward-sexp)
      (- (point) p))))

(defun acl-calc-width ()
  (save-excursion
    (let ((width 0))
      (while (progn
               (if (> (acl-get-width) width)
                   (setq width (acl-get-width)))
               (acl-goto-next-pair)))
      width)))

(defun acl-respace-single-let (max-width)
  (save-excursion
    (let (p current-width difference)
      (setq p (point))
      (forward-sexp)
      (forward-sexp)
      (backward-sexp)
      (setq current-width (- (- (point) p) 1)
            difference    (- max-width current-width))
      
      (cond ((> difference 0)
             (insert (make-string difference ? )))
            ((< difference 0)
             (delete-backward-char (abs difference))))
      
      )))

(defun acl-respace-let (width)
  (while (progn
           (acl-respace-single-let width)
           (acl-goto-next-pair))))

(defun acl-align-let ()
  ;; move to start of [
  (down-list 2)
  (let ((w (acl-calc-width)))
    (acl-respace-let w)
    ))

(defun align-cljlet ()
  (interactive)
  (save-excursion
    (if (acl-find-let)
        (acl-align-let))))


(provide 'align-cljlet)

