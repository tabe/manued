(library (manued)
  (export unmatched-parenthesis?
          terminating-escape?
          missing-separator?
          unknown-command?
          parse
          string->datum
          datum->before&after)
  (import (rnrs))

  (define *left-parenthesis* #\[)

  (define *right-parenthesis* #\])

  (define *substitution-command* #\/)

  (define *swap-command* #\|)

  (define *escape-command* #\~)

  (define (make-substitution left right)
    (list 'sub left right))

  (define (make-swap alpha beta gamma)
    (list 'swap alpha beta gamma))

  (define-condition-type &unmatched-parenthesis &condition
    make-unmatched-parenthesis unmatched-parenthesis?)

  (define-condition-type &terminating-escape &condition
    make-terminating-escape terminating-escape?)

  (define-condition-type &missing-separator &condition
    make-missing-separator missing-separator?)

  (define-condition-type &unknown-command &condition
    make-unknown-command unknown-command?)

  (define (read-manued port)
    (let loop ((stack '())
               (temp '()))
      (let ((c (get-char port)))
        (cond ((eof-object? c)
               (if (null? stack)
                   temp
                   (raise (make-unmatched-parenthesis))))
              ((char=? c *left-parenthesis*)
               (loop (cons temp stack) '()))
              ((char=? c *right-parenthesis*)
               (if (null? stack)
                   (raise (make-unmatched-parenthesis))
                   (loop (cdr stack) (cons temp (car stack)))))
              ((char=? c *escape-command*)
               (let ((d (get-char port)))
                 (if (eof-object? d)
                     (raise (make-terminating-escape))
                     (loop stack (cons (cons c d) temp)))))
              (else (loop stack (cons c temp)))))))

  (define (split/char c ls)
    (let loop ((ls ls)
               (r '()))
      (if (null? ls)
          (raise (make-missing-separator))
          (let ((head (car ls)))
            (if (and (char? head) (char=? c head))
                (values r (cdr ls))
                (loop (cdr ls) (cons head r)))))))

  (define (restructure x)
    (cond ((char? x) x)
          ((list? x)
           (let loop ((x x)
                      (r '()))
             (if (null? x)
                 (raise (make-missing-separator))
                 (let ((head (car x)))
                   (if (char? head)
                       (cond ((char=? head *substitution-command*)
                              (make-substitution
                               (reverse (restructure-map (cdr x)))
                               r))
                             ((char=? head *swap-command*)
                              (call-with-values
                                  (lambda () (split/char *swap-command* (cdr x)))
                                (lambda (beta gamma)
                                  (make-swap
                                   (reverse (restructure-map gamma))
                                   (restructure-map beta)
                                   r))))
                             (else (loop (cdr x) (cons head r))))
                       (loop (cdr x) (cons (restructure head) r)))))))
          (else (cdr x))))

  (define (restructure-map ls)
    (map restructure ls))

  (define (parse port)
    (assert (textual-port? port))
    (reverse (restructure-map (read-manued port))))

  (define (string->datum str)
    (call-with-port (open-string-input-port str) parse))

  (define (datum->before&after datum)
    (define (traverse datum before after cont)
      (if (null? datum)
          (cont before after)
          (let ((x (car datum)))
            (if (char? x)
                (traverse (cdr datum) (cons x before) (cons x after) cont)
                (case (car x)
                  ((sub)
                   (traverse (cdr datum)
                             (append (reverse (cadr x)) before)
                             (append (reverse (caddr x)) after)
                             cont))
                  ((swap)
                   (traverse (cdr datum)
                             (traverse (apply append (cdr x)) before '() (lambda x (car x)))
                             (traverse (apply append (reverse (cdr x))) '() after (lambda x (cadr x)))
                             cont))
                  (else (raise (condition (make-unknown-command)
                                          (make-irritants-condition x)))))))))
    (traverse datum
              '()
              '()
              (lambda (b a) (values (reverse b) (reverse a)))))

)
