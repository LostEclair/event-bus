#!chezscheme

(import (chezscheme)
        (prefix (bus) bus:))

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
  (apply format #t (string-append "[" formatting "]~%")
         arguments))

(define (raises? thunk)
  (call/cc (lambda (k)
             (with-exception-handler (lambda (e)
                                       (k #t))
               (lambda ()
                 (thunk)
                 #f)))))

(define (run-tests)
  (say "Running test suite on ~A" (scheme-version #t))
  (let ([results '()])
    (define (test name test-body)
      (let ([result (guard (e [else #f]) (test-body))])
        (set! results (cons (cons name result) results))
        (say "~90,1,1,'.@<~A ~> ~A" name (if result "OK" "FAIL"))
        result))

    (test "bus:make-event-bus returns a procedure"
          (lambda ()
            (procedure? (bus:make-event-bus #f))))

    (test "bus:make-event-bus type-checks it's arguments"
          (lambda ()
            (raises? (lambda () (bus:make-event-bus "not-a-boolean")))))

    (test "'attach! type-checks it's arguments"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (and
               (raises? (lambda () (bus 'attach! 123 (lambda () #f))))
               (raises? (lambda () (bus 'attach! 'test "not-a-proc")))))))

    (test "'detach! type-checks it's arguments"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (and
               (raises? (lambda () (bus 'detach! 123 (lambda () #f))))
               (raises? (lambda () (bus 'detach! 'test "not-a-proc")))))))

    (test "'detach-id! type-checks it's arguments"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (and
               (raises? (lambda () (bus 'detach-id! 123 (gensym))))
               (raises? (lambda () (bus 'detach-id! 'test "not-a-symbol")))))))

    (test "'attach! returns a symbol id"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [id (bus 'attach! 'test (lambda () #f))])
              (symbol? id))))

    (test "'attach! returns unique ids per attachment"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [receiver (lambda () #f)]
                   [id1 (bus 'attach! 'test receiver)]
                   [id2 (bus 'attach! 'test receiver)])
              (not (eq? id1 id2)))))

    (test "'attach! and 'propagate! with single receiver"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [output (open-output-string)]
                   [receiver (lambda (x)
                               (display x output))])
              (bus 'attach! 'test receiver)
              (bus 'propagate! 'test "x")
              (bus 'propagate! 'test "y")
              (bus 'go!)
              (string-contains? (get-output-string output) "xy"))))

    (test "nothing happens until 'go! is sent"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [output (open-output-string)]
                   [receiver (lambda ()
                               (display "x" output))])
              (bus 'attach! 'test receiver)
              (not (string-contains? (get-output-string output) "x")))))

    (test "'propagate! type-checks its event argument"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (raises? (lambda () (bus 'propagate! "not-a-symbol"))))))

    (test "multiple receivers on same event all fire"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [count 0])
              (bus 'attach! 'test (lambda () (set! count (+ count 1))))
              (bus 'attach! 'test (lambda () (set! count (+ count 1))))
              (bus 'attach! 'test (lambda () (set! count (+ count 1))))
              (bus 'propagate! 'test)
              (bus 'go!)
              (= count 3))))

    (test "receivers fire in attachment order"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [output (open-output-string)])
              (bus 'attach! 'test (lambda () (display "a" output)))
              (bus 'attach! 'test (lambda () (display "b" output)))
              (bus 'attach! 'test (lambda () (display "c" output)))
              (bus 'propagate! 'test)
              (bus 'go!)
              (string=? (get-output-string output) "abc"))))

    (test "events in queue fire in propagation order"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [output (open-output-string)])
              (bus 'attach! 'a (lambda () (display "a" output)))
              (bus 'attach! 'b (lambda () (display "b" output)))
              (bus 'attach! 'c (lambda () (display "c" output)))
              (bus 'propagate! 'a)
              (bus 'propagate! 'b)
              (bus 'propagate! 'c)
              (bus 'go!)
              (string=? (get-output-string output) "abc"))))

    (test "receivers receive multiple arguments from 'propagate!"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [received '()])
              (bus 'attach! 'test (lambda (a b c)
                                    (set! received (list a b c))))
              (bus 'propagate! 'test 1 2 3)
              (bus 'go!)
              (equal? received '(1 2 3)))))

    (test "'detach! removes a receiver"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [fired #f]
                   [receiver (lambda () (set! fired #t))])
              (bus 'attach! 'test receiver)
              (bus 'detach! 'test receiver)
              (bus 'propagate! 'test)
              (bus 'go!)
              (not fired))))

    (test "'detach! on non-attached receiver does not raise"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (bus 'detach! 'test (lambda () #f))
              #t)))

    (test "'detach! only removes the matching receiver"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [count 0]
                   [r1 (lambda () (set! count (+ count 1)))]
                   [r2 (lambda () (set! count (+ count 1)))])
              (bus 'attach! 'test r1)
              (bus 'attach! 'test r2)
              (bus 'detach! 'test r1)
              (bus 'propagate! 'test)
              (bus 'go!)
              (= count 1))))

    (test "'detach-id! removes receiver by id"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [fired #f]
                   [id (bus 'attach! 'test (lambda () (set! fired #t)))])
              (bus 'detach-id! 'test id)
              (bus 'propagate! 'test)
              (bus 'go!)
              (not fired))))

    (test "'detach-id! only removes the targeted receiver"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [count 0]
                   [id (bus 'attach! 'test (lambda () (set! count (+ count 1))))])
              (bus 'attach! 'test (lambda () (set! count (+ count 1))))
              (bus 'detach-id! 'test id)
              (bus 'propagate! 'test)
              (bus 'go!)
              (= count 1))))

    (test "'reset! clears all receivers"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [fired #f])
              (bus 'attach! 'test (lambda () (set! fired #t)))
              (bus 'reset!)
              (bus 'propagate! 'test)
              (bus 'go!)
              (not fired))))

    (test "'reset! clears pending events"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [fired #f])
              (bus 'attach! 'test (lambda () (set! fired #t)))
              (bus 'propagate! 'test)
              (bus 'reset!)
              (bus 'go!)
              (not fired))))

    (test "'go! clears the pending queue after execution"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [count 0])
              (bus 'attach! 'test (lambda () (set! count (+ count 1))))
              (bus 'propagate! 'test)
              (bus 'go!)
              (bus 'go!)
              (= count 1))))

    (test "propagating an event with no receivers does nothing"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (bus 'propagate! 'no-receivers-here)
              (bus 'go!)
              #t)))

    (test "separate events do not interfere with each other"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [fired-a #f]
                   [fired-b #f])
              (bus 'attach! 'a (lambda () (set! fired-a #t)))
              (bus 'attach! 'b (lambda () (set! fired-b #t)))
              (bus 'propagate! 'a)
              (bus 'go!)
              (and fired-a (not fired-b)))))

    (test "same procedure can attach to different events in unique mode"
          (lambda ()
            (let* ([bus (bus:make-event-bus #t)]
                   [count 0]
                   [receiver (lambda () (set! count (+ count 1)))])
              (bus 'attach! 'event-a receiver)
              (bus 'attach! 'event-b receiver)
              (bus 'propagate! 'event-a)
              (bus 'propagate! 'event-b)
              (bus 'go!)
              (= count 2))))

    (test "'attach! in unique mode does not allow same procedures per-event"
          (lambda ()
            (let* ([bus (bus:make-event-bus #t)]
                   [receiver (lambda () #f)])
              (bus 'attach! 'test receiver)
              (raises? (lambda ()
                         (bus 'attach! 'test receiver))))))

    (test "unique mode allows re-attaching after detach"
          (lambda ()
            (let* ([bus (bus:make-event-bus #t)]
                   [receiver (lambda () #f)])
              (bus 'attach! 'test receiver)
              (bus 'detach! 'test receiver)
              (bus 'attach! 'test receiver)
              #t)))

    (test "unique mode allows re-attaching after detach-id!"
          (lambda ()
            (let* ([bus (bus:make-event-bus #t)]
                   [receiver (lambda () #f)]
                   [id (bus 'attach! 'test receiver)])
              (bus 'detach-id! 'test id)
              (bus 'attach! 'test receiver)
              #t)))

    (test "non-unique mode allows duplicate procedure attachments"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [count 0]
                   [receiver (lambda () (set! count (+ count 1)))])
              (bus 'attach! 'test receiver)
              (bus 'attach! 'test receiver)
              (bus 'propagate! 'test)
              (bus 'go!)
              (= count 2))))

    (test "unknown message raises an error"
          (lambda ()
            (let ([bus (bus:make-event-bus #f)])
              (raises? (lambda () (bus 'totally-unknown-message))))))

    (test "multiple buses are independent"
          (lambda ()
            (let* ([bus1 (bus:make-event-bus #f)]
                   [bus2 (bus:make-event-bus #f)]
                   [fired1 #f]
                   [fired2 #f])
              (bus1 'attach! 'test (lambda () (set! fired1 #t)))
              (bus2 'attach! 'test (lambda () (set! fired2 #t)))
              (bus1 'propagate! 'test)
              (bus1 'go!)
              (and fired1 (not fired2)))))

    (test "'reset! on one bus does not affect another"
          (lambda ()
            (let* ([bus1 (bus:make-event-bus #f)]
                   [bus2 (bus:make-event-bus #f)]
                   [fired #f])
              (bus2 'attach! 'test (lambda () (set! fired #t)))
              (bus1 'reset!)
              (bus2 'propagate! 'test)
              (bus2 'go!)
              fired)))

    (test "'attach! returns a symbol, which starts with ebus-procedure-token:"
          (lambda ()
            (let* ([bus (bus:make-event-bus #f)]
                   [token (bus 'attach! 'test (lambda () #f))])
              (string-contains? (symbol->string token) "ebus-procedure-token:"))))

    (let ([all-passed (for-all cdr results)])
      (say "Passed ~R out of ~R test~:P"
           (count cdr results)
           (length results))
      all-passed)))

(exit (if (time (run-tests)) 0 1))
