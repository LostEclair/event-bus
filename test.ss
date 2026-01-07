(import (chezscheme)
        (bus))

(define (string-contains? str substr)
  (let ([str-length (string-length str)]
        [sub-length (string-length substr)])
    (cond
     [(< str-length sub-length) #f]
     [(string=? (substring str 0 sub-length) substr) #t]
     [else (string-contains? (substring str 1 str-length) substr)])))

(define (count predicate list)
  (let loop ([list list] [acc 0])
    (if (null? list)
        acc
        (loop (cdr list) (if (predicate (car list))
                             (+ acc 1)
                             acc)))))

(define (say formatting . arguments)
  (apply format #t (string-append "; " formatting "~%")
         arguments))

(define (run-tests)
  (let ([results '()])
    (define (test name test-body)
      (let ([result (test-body)])
        (set! results (cons (cons name result) results))
        (say "Test ~90,1,1,'.@<~A ~> ~A" name (if result "OK" "FAIL"))
        result))

    (test "make-event-bus returns a procedure" (lambda () (procedure? (make-event-bus))))

    (test "'attach! and 'propagate! with single receiver"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (output (open-output-string))
                   (receiver (lambda (x)
                               (display (string-append "Got: " x) output))))
              (bus 'attach! 'test-event receiver)
              (bus 'propagate! 'test-event "hello")
              (bus 'propagate! 'test-event "world")
              (let ((result (get-output-string output)))
                (and (string-contains? result "Got: hello")
                     (string-contains? result "Got: world"))))))

    (test "multiple receivers for same event"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (output1 (open-output-string))
                   (output2 (open-output-string))
                   (receiver1 (lambda (x y)
                                (display (string-append "R1:" x y) output1)))
                   (receiver2 (lambda (x y)
                                (display (string-append "R2:" x y) output2))))
              (bus 'attach! 'multi receiver1)
              (bus 'attach! 'multi receiver2)
              (bus 'propagate! 'multi "a" "b")
              (let ((r1 (get-output-string output1))
                    (r2 (get-output-string output2)))
                (and (string=? r1 "R1:ab")
                     (string=? r2 "R2:ab"))))))

    (test "'detach! removes receiver"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (output (open-output-string))
                   (receiver (lambda ()
                               (display "called" output))))
              (bus 'attach! 'detach!-test receiver)
              (bus 'propagate! 'detach!-test)
              (bus 'detach! 'detach!-test receiver)
              (bus 'propagate! 'detach!-test)
              (let ((result (get-output-string output)))
                (string=? result "called")))))

    (test "'receivers returns vector of all receivers"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (r1 (lambda () 1))
                   (r2 (lambda () 2))
                   (r3 (lambda () 3)))
              (bus 'attach! 'event-a r1)
              (bus 'attach! 'event-a r2)
              (bus 'attach! 'event-b r3)
              (let ((vec (bus 'receivers)))
                (and (vector? vec)
                     (= (vector-length vec) 2))))))

    (test "'receivers with event returns list for that event"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (r1 (lambda () 1))
                   (r2 (lambda () 2))
                   (r3 (lambda () 3)))
              (bus 'attach! 'event-a r1)
              (bus 'attach! 'event-a r2)
              (bus 'attach! 'event-b r3)
              (let ((lst (bus 'receivers 'event-a)))
                (and (list? lst)
                     (= (length lst) 2)
                     (memq r1 lst)
                     (memq r2 lst))))))

    (test "'reset! clears all receivers"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (output (open-output-string))
                   (receiver (lambda () (display "x" output))))
              (bus 'attach! 'event1 receiver)
              (bus 'attach! 'event2 receiver)
              (bus 'reset!)
              (bus 'propagate! 'event1)
              (bus 'propagate! 'event2)
              (let ((result (get-output-string output)))
                (string=? result "")))))

    (test "'propagate! with multiple arguments"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (result (box '()))
                   (receiver (lambda args
                               (set-box! result args))))
              (bus 'attach! 'multi-args receiver)
              (bus 'propagate! 'multi-args 'a 'b 'c 'd)
              (equal? (unbox result) '(a b c d)))))

    (test "'propagate! with no receivers does nothing"
          (lambda ()
            (let ((bus (make-event-bus)))
              (bus 'propagate! 'non-existent 'arg1 'arg2)
              #t)))

    (test "'detach! non-existent receiver does nothing"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (r1 (lambda () 1))
                   (r2 (lambda () 2)))
              (bus 'attach! 'event r1)
              (bus 'detach! 'event r2)
              (let ((lst (bus 'receivers 'event)))
                (= (length lst) 1)))))

    (test "'receivers for non-existent event returns empty list"
          (lambda ()
            (let ((bus (make-event-bus)))
              (null? (bus 'receivers 'no-such-event)))))

    (test "receivers maintain attachment order (LIFO)"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (output (open-output-string))
                   (r1 (lambda () (display "1" output)))
                   (r2 (lambda () (display "2" output)))
                   (r3 (lambda () (display "3" output))))
              (bus 'attach! 'order r1)
              (bus 'attach! 'order r2)
              (bus 'attach! 'order r3)
              (bus 'propagate! 'order)
              (let ((result (get-output-string output)))
                (string=? result "321")))))

    (test "receiver attachment can be checked using 'has-attached?"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (r1 (lambda () 1))
                   (r2 (lambda () 2)))
              (bus 'attach! 'event r1)
              (and (bus 'has-attached? 'event r1)
                   (not (bus 'has-attached? 'event r2))))))

    (test "'attach! with non-procedure throws error"
          (lambda ()
            (let ((bus (make-event-bus)))
              (guard (condition
                      [(error? condition)
                       (and (string-contains?
                             (condition-message condition)
                             "Provided event receiver is not a procedure")
                            (equal? (condition-irritants condition) '("not a procedure")))])
                (bus 'attach! 'event "not a procedure")
                #f))))

    (test "'detach! with non-procedure throws error"
          (lambda ()
            (let ((bus (make-event-bus))
                  (receiver (lambda () 1)))
              (bus 'attach! 'event receiver)
              (guard (condition
                      [(error? condition)
                       (and (string-contains?
                             (condition-message condition)
                             "Provided event receiver is not a procedure")
                            (equal? (condition-irritants condition) '("not a procedure")))])
                (bus 'detach! 'event "not a procedure")
                #f))))

    (test "'has-attached? with non-procedure throws error"
          (lambda ()
            (let ((bus (make-event-bus)))
              (guard (condition
                      [(error? condition)
                       (and (string-contains?
                             (condition-message condition)
                             "Provided event receiver is not a procedure")
                            (equal? (condition-irritants condition) '("not a procedure")))])
                (bus 'has-attached? 'event "not a procedure")
                #f))))

    (test "'attach! same receiver twice throws error"
          (lambda ()
            (let ((bus (make-event-bus))
                  (receiver (lambda () 1)))
              (bus 'attach! 'event receiver)
              (guard (condition
                      [(error? condition)
                       (and (string-contains?
                             (condition-message condition)
                             "Provided event receiver has been already attached")
                            (eq? (car (condition-irritants condition)) 'event)
                            (eq? (cadr (condition-irritants condition)) receiver))])
                (bus 'attach! 'event receiver)
                #f))))

    (test "'detach! non-existent event does nothing"
          (lambda ()
            (let ((bus (make-event-bus))
                  (receiver (lambda () 1)))
              (bus 'detach! 'non-existent receiver)
              #t)))

    (test "receiver error does not stop propagation to other receivers"
          (lambda ()
            (let* ((bus (make-event-bus))
                   (output (open-output-string))
                   (bad-receiver (lambda (msg)
                                   ;; Bus will give us a warning, but won't crash.
                                   (error 'bad-receiver "Im really bad!! :<")))
                   (good-receiver (lambda (msg)
                                    (display msg output))))
              (bus 'attach! 'error-test bad-receiver)
              (bus 'attach! 'error-test good-receiver)
              (bus 'propagate! 'error-test "hello")
              (let ((result (get-output-string output)))
                (string=? result "hello")))))

    (test "concurrent 'attach! and 'propagate!"
          (lambda ()
            (let ((bus (make-event-bus))
                  (counter 0)
                  (mutex (make-mutex)))
              (define (increment)
                (with-mutex mutex
                  (set! counter (+ counter 1))))
              (let ((attach-threads
                     (map (lambda (i)
                            (fork-thread
                             (lambda ()
                               (let ((receiver (lambda () (increment))))
                                 (bus 'attach! 'concurrent receiver)))))
                          (iota 10))))
                (for-each thread-join attach-threads)
                (let ((propagate-threads
                       (map (lambda (i)
                              (fork-thread
                               (lambda ()
                                 (bus 'propagate! 'concurrent))))
                            (iota 5))))
                  (for-each thread-join propagate-threads)
                  (let ((expected-counter (* (length (bus 'receivers 'concurrent)) 5)))
                    (= counter expected-counter)))))))

    (test "concurrent 'attach! and 'detach!"
          (lambda ()
            (let ((bus (make-event-bus))
                  (counter 0)
                  (mutex (make-mutex)))
              (define (increment)
                (with-mutex mutex
                  (set! counter (+ counter 1))))
              (let ((receiver1 (lambda () (increment)))
                    (receiver2 (lambda () (increment)))
                    (receiver3 (lambda () (increment))))
                (bus 'attach! 'concurrent-detach receiver1)
                (bus 'attach! 'concurrent-detach receiver2)
                (bus 'attach! 'concurrent-detach receiver3)
                (let ((threads
                       (append
                        (map (lambda (i)
                               (fork-thread
                                (lambda ()
                                  (let ((new-receiver (lambda () (increment))))
                                    (bus 'attach! 'concurrent-detach new-receiver)))))
                             (iota 3))
                        (map (lambda (i)
                               (fork-thread
                                (lambda ()
                                  (bus 'detach! 'concurrent-detach receiver2))))
                             (iota 2)))))
                  (for-each thread-join threads)
                  (bus 'propagate! 'concurrent-detach)
                  (let ((min-expected 2)
                        (max-expected 5))
                    (and (>= counter min-expected)
                         (<= counter max-expected))))))))

    (let ([passed (count cdr results)]
          [total (length results)])
      (say "Passed ~D test~:P out of ~D." passed total)
      (= passed total))))

(exit (if (run-tests) 0 1))
