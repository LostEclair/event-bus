;;; For the license information, please check out the License file.

#!chezscheme

(library (bus)
  (export make-event-bus)
  (import (chezscheme))

  (define-syntax type-check!
    ;; Type checks a set of values.
    ;; Either raises a warning, or error
    (syntax-rules (is or error warning in)
      [(_) #t]
      [(_ (what is p or error in who msg) rest ...)
       (begin (unless (p what)
                (error who msg what))
              (type-check! rest ...))]
      [(_ (what is p or warning in who msg) rest ...)
       (begin (unless (p what)
                (warning who msg what))
              (type-check! rest ...))]))

  (define (make-event-bus unique)
    ;; Creates an event bus
    ;; Provide #t to unique argument to make it check, whether some functions are
    ;;   already attached to the bus
    (type-check! [unique is boolean?
                         or error in 'make-event-bus "Unique must be a boolean"])
    (let ([receivers (make-eq-hashtable)]
          [pending '()]
          [lock (make-mutex)])
      (define (attach! e proc)
        ;; Attach a receiver to an event
        (type-check! [e is symbol?
                        or warning in 'attach! "Event must be a symbol"]
                     [proc is procedure?
                           or error in 'attach! "Receiver must be a procedure"])
        (let ([recv-id (gensym)])
          (with-mutex lock
            (hashtable-update! receivers e
                               (lambda (value)
                                 (when (and unique
                                            (memp (lambda (pair)
                                                    (eq? (cdr pair) proc))
                                                  value))
                                   (error 'attach! "Provided event receiver has been already attached!" e proc))
                                 (append value (list (cons recv-id proc))))
                               '())
            recv-id)))

      (define (detach! e proc)
        ;; Detach a receiver from an event
        (type-check! [e is symbol?
                        or warning in 'detach! "Event must be a symbol"]
                     [proc is procedure?
                           or error in 'detach! "Receiver must be a procedure"])
        (with-mutex lock
          (hashtable-update! receivers e
                             (lambda (l)
                               (remp (lambda (p)
                                       (eq? (cdr p) proc))
                                     l))
                             '())))

      (define (detach-id! e id)
        ;; Detach a receiver from an event (by id)
        (type-check! [e is symbol?
                        or warning in 'detach-id! "Event must be a symbol"]
                     [id is symbol?
                         or error in 'detach-id! "Receiver id must be a symbol"])
        (with-mutex lock
          (hashtable-update! receivers e
                             (lambda (lst)
                               (remp (lambda (pair)
                                       (eq? (car pair) id))
                                     lst))
                             '())))

      (define (reset!)
        ;; Clears both receivers and pending tables
        (with-mutex lock
          (hashtable-clear! receivers)
          (set! pending '())))

      (define (propagate! e . args)
        ;; Add the event to the queue
        (type-check! [e is symbol?
                        or warning in 'propagate! "Event must be a symbol"])
        (with-mutex lock
          (set! pending (append pending (list (cons e args))))))

      (define (go!)
        ;; Kickstart all the events
        (with-mutex lock
          (for-each (lambda (entry)
                      (let ([e (car entry)]
                            [args (cdr entry)])
                        (for-each (lambda (pair)
                                    (apply (cdr pair) args))
                                  (hashtable-ref receivers e '()))))
                    pending)
          (set! pending '())))

      (let ([supported-messages `([attach! . ,attach!]
                                  [detach! . ,detach!]
                                  [detach-id! . ,detach-id!]
                                  [reset! . ,reset!]
                                  [propagate! . ,propagate!]
                                  [go! . ,go!]
                                  [brrr! . ,go!])])
        (lambda (m . args)
          (apply (let ([proc (assq m supported-messages)])
                   (if proc
                       (cdr proc)
                       (error 'make-event-bus "Did not understood a message" m)))
                 args))))))
