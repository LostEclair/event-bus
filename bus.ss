;;; For the license information, please check out the License file.

;;; An implementation of a synchronous event bus.
;;; Meaning if something is propagated, receivers are called immediately.
;;;
;;; Also, it's thread unsafe. Have fun.

#!chezscheme

(library (bus)
  (export make-event-bus)
  (import (chezscheme))

  (define (make-event-bus)
    (let ([event-receivers (make-eq-hashtable)])
      (define (propagate event . arguments)
        "Call all of the receivers with given arguments"
        (let ([receivers (hashtable-ref event-receivers event '())])
          (for-each (lambda (receiver)
                      (guard (exception [else (format (current-error-port) "; Error in event receiver for ~A: ~A (~A)~%"
                                                      event exception receiver)])
                        (apply receiver arguments)))
                    receivers)))

      (define (has-attached? event procedure)
        (unless (procedure? procedure)
          (error 'has-attached? "Provided event receiver is not a procedure" procedure))
        (memq procedure (hashtable-ref event-receivers event '())))

      (define (attach event procedure)
        "Attach a receiver to an event"
        (unless (procedure? procedure)
          (error 'attach "Provided event receiver is not a procedure" procedure))
        (hashtable-update! event-receivers event (lambda (value)
                                                   (when (has-attached? event procedure)
                                                     (error 'attach "Provided event receiver has been already attached to the event"
                                                            event procedure))
                                                   (cons procedure value))
                           '()))

      (define (detach event procedure)
        "Detach a receiver from an event"
        (unless (procedure? procedure)
          (error 'detach "Provided event receiver is not a procedure" procedure))
        (hashtable-update! event-receivers event (lambda (value)
                                                   (remq! procedure value))
                           '()))

      (define (reset)
        "Clears the internal event-receivers table"
        (hashtable-clear! event-receivers))

      (define receivers
        (case-lambda
          [() (hashtable-cells event-receivers)]
          [(event) (hashtable-ref event-receivers event '())]))

      (lambda (message . arguments)
        (let ([available-messages `((propagate . ,propagate)
                                    (has-attached? . ,has-attached?)
                                    (attach . ,attach)
                                    (detach . ,detach)
                                    (reset . ,reset)
                                    (receivers . ,receivers))])
          (apply (let ([found-message (assq message available-messages)])
                   (if found-message
                       (cdr found-message)
                       (error 'make-event-bus "Did not understand a message" message)))
                 arguments))))))
