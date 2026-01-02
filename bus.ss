;;; For the license information, please check out the License file.

;;; An implementation of a synchronous event bus.
;;; Meaning if something is propagated, receivers are called immediately.

#!chezscheme

(library (bus)
  (export make-event-bus)
  (import (chezscheme))

  (define (make-event-bus)
    (let ([event-receivers (make-eq-hashtable)]
          [mutex (make-mutex)])
      ;; Call all of the receivers with given arguments
      (define (propagate event . arguments)
        (let ([receivers (with-mutex mutex
                           (hashtable-ref event-receivers event '()))])
          (for-each (lambda (receiver)
                      (guard (exception [else (format (current-error-port) "; Warning from an event-bus: Guard triggered while propagating ~A: ~S from ~A~%"
                                                      event (condition-message exception) receiver)])
                        (apply receiver arguments)))
                    receivers)))

      (define (has-attached? event procedure)
        (unless (procedure? procedure)
          (error 'has-attached? "Provided event receiver is not a procedure" procedure))
        (with-mutex mutex
          (memq procedure (hashtable-ref event-receivers event '()))))

      ;; Attach a receiver to an event
      (define (attach event procedure)
        (unless (procedure? procedure)
          (error 'attach "Provided event receiver is not a procedure" procedure))
        (with-mutex mutex
          (hashtable-update! event-receivers event (lambda (value)
                                                     (when (memq procedure value)
                                                       (error 'attach "Provided event receiver has been already attached to the event"
                                                              event procedure))
                                                     (cons procedure value))
                             '())))

      ;; Detach a receiver from an event
      (define (detach event procedure)
        (unless (procedure? procedure)
          (error 'detach "Provided event receiver is not a procedure" procedure))
        (with-mutex mutex
          (hashtable-update! event-receivers event (lambda (value)
                                                     (remq procedure value))
                             '())))

      ;; Clears the internal event-receivers table
      (define (reset)
        (with-mutex mutex
          (hashtable-clear! event-receivers)))

      (define receivers
        (case-lambda
          [() (with-mutex mutex
                (let ([cells (hashtable-cells event-receivers)])
                  (vector-map (lambda (cell)
                                (cons (car cell)
                                      (list-copy (cdr cell))))
                              cells)))]
          [(event) (with-mutex mutex
                     (list-copy (hashtable-ref event-receivers event '())))]))

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
