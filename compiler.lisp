(defun compile-regex (regex &optional
                              (success-cont-label 'success)
                              (failure-cont-label 'failure))
  
  (let* ((label (gensym "label"))
         (code (etypecase regex
                 (character
                  (compile-character regex
                                     label
                                     success-cont-label
                                     failure-cont-label))
                 (list
                  (destructuring-bind (operator &rest operands) regex
                    (ecase operator
                      ((or :or union :union)
                       (compile-or operands success-cont-label failure-cont-label))
                      ((and :and and :and)
                       (compile-and (first operands)
                                    (second operands)
                                    success-cont-label
                                    failure-cont-label))
                      ((kleene :kleene
                        closure :closure
                        * :*)
                       (compile-kleene operands label success-cont-label failure-cont-label))

                      ((plus :plus
                        :+ +)
                       (compile-regex
                        `(:and ,(first operands)
                               (:kleene ,(first operands)))
                        success-cont-label
                        'fail))))))))
    
    (values (cons (format nil "~a:" label) code) label)))

(defun compile-and (a b success-cont-label failure-cont-label)
    (multiple-value-bind (b-code b-label)
        (compile-regex b success-cont-label
                       failure-cont-label)
      (multiple-value-bind (a-code a-label)
          (compile-regex a b-label
                         failure-cont-label)
        (append a-code b-code))))

(defun compile-character (regex cur-label success-cont-label failure-cont-label)
  (append
   (list (format nil "    cmp #'~a', d0" regex))
   (case failure-cont-label
     (success (list (format nil "    bne FAIL_SUCCESS")))
     ;(failure (list (format nil "    bne FAILURE")))
     (t (list (format nil "    bne FAIL"))))
   (case success-cont-label
     (success
      (list (format nil "    bra SUCCESS")))

     (recur
      (list (format nil "    move.w #~a, (a1)+" cur-label)
            (format nil "    move.w #swap, (a1)+")
            (format nil "    cmp d4, a1")
            (format nil "    bge EXPENDED_MEMORY")
            (format nil "    NEXT")))
     (otherwise
      (list (format nil "    move.w #~a, (a1)+" success-cont-label)
            (format nil "    move.w #swap, (a1)+")
            (format nil "    cmp d4, a1")
            (format nil "    bge EXPENDED_MEMORY")
            (format nil "    NEXT"))))))


(defun compile-or (operands success-cont-label failure-cont-label)
    (multiple-value-bind (a-code a-label)
        (compile-regex (first operands)
                       success-cont-label
                       'fail)
      (multiple-value-bind (b-code b-label)
          (compile-regex (second operands)
                         success-cont-label
                         'fail)
        (append
         (list
          (format nil "    move.w #~a, -(a0)" b-label)
          (format nil "    bra ~a" a-label))
         a-code
         b-code))))

(defun compile-kleene (operands current-label
                       success-cont-label
                       failure-cont-label)
    (multiple-value-bind (a-code a-label)
        (compile-regex (first operands)
                       'recur
                       success-cont-label)
      (if (eq 'success success-cont-label)
          (cons
           (format nil "    moveq #1, d7")
           a-code)
          (append
           (list
            (format nil "    subq.l #2, a0")
            (format nil "    move.w #~a, (a0)"
                    (case success-cont-label
                      (recur current-label)
                      (otherwise success-cont-label)))
              
            (format nil "    bra ~a" a-label))
           a-code))))



(defun print-program (prog)
  (loop for ex in prog do
       (princ ex)
       (terpri)))
