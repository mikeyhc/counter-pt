#!/usr/bin/env chibi-scheme

(import
  (chibi show)
  (chibi sqlite3)
  (chibi time)
  (scheme small)
  (srfi 1)
  (srfi 26)
  (srfi 115)
  (srfi 130))

(define ctx #f)

(define meals
  '("breakfast"
    "morning snacks"
    "lunch"
    "afternoon snacks"
    "dinner"))

(define db (sqlite3-open "./counter.db"))

(define (ctx-string)
  (cond
    ((not ctx) "")
    ((eq? ctx 'product) "product")
    ((and (list? ctx) (eq? (car ctx) 'meal))
     (show #f "meal:" (cadr ctx) ":" (caddr ctx)))
    (else "?")))

(define (now-stamp)
  (let ((now (seconds->time (current-seconds))))
   (show
     #f
     (with ((pad-char #\0))
           (padded/left 4 (+ 1900 (time-year now))) "-"
           (padded/left 2 (+ 1 (time-month now))) "-"
           (padded/left 2 (time-day now))))))

(define (process-date str)
  (let ((date (if (string=? str "now") (now-stamp) str))
        (date-regex (rx (= 4 num) "-" (= 2 num) "-" (= 2 num))))
    (if (regexp-match? (regexp-matches date-regex date))
      date
      #f)))

(define (process-meal tokens)
  (if (or (null? tokens) (null? (cdr tokens)))
    (show #t "no meal or date provided for meal mode" nl)
    (let ((meal (string-join (cdr tokens) " ")))
     (if (find (cut string=? meal <>) meals)
       (let ((date (process-date (car tokens))))
         (if date
           (set! ctx (list 'meal date  meal))
           (show #t "invalid date \"" (car tokens) "\"" nl)))
       (show #t "invalid meal " meal nl)))))

(define (process-mode tokens)
  (cond
    ((null? tokens) (set! ctx #f))
    ((string=? (car tokens) "product") (set! ctx 'product))
    ((string=? (car tokens) "meal") (process-meal (cdr tokens)))
    (else
      (show #t "unknown mode: \"" (escaped (car tokens)) "\"" nl))))

(define (prompt str)
  (show #t str " ")
  (read-line))

;; TODO: handle more cases
(define (yn-prompt str)
  (let ((input (prompt str)))
    (cond
      ((string=? input "y") #t)
      ((string=? input "n") #f)
      (else
        (show #t "please enter y or n" nl)
        (yn-prompt str)))))

;; TODO: trim whitespace
(define (null-prompt str)
  (let ((input (prompt str)))
    (if (= (string-length input) 0)
      'NULL
      input)))

(define (add-product name energy fat carbs fiber protein salt liquid)
  (sqlite3-do db
    `(insert
       (into products)
       (values #(,name ,energy ,fat ,carbs ,fiber ,protein ,salt ,liquid)))))

(define (add-product-repl tokens)
  (if (null? tokens)
    (show #t "no product name provided" nl)
    ;; TODO: add number prompt
    (let* ((name (string-join tokens " "))
           (energy (null-prompt "energy (kJ)?"))
           (fat (null-prompt "fat?"))
           (carbs (null-prompt "carbohydrates?"))
           (fiber (null-prompt "fiber?"))
           (protein (null-prompt "protein?"))
           (salt (null-prompt "salt?"))
           (liquid (yn-prompt "liquid?")))
      ;; TODO: show confirmation
      (add-product name energy fat carbs fiber protein salt liquid))))

(define (add-meal-item meal date item quantity)
  (sqlite3-do
   db
   `(insert
      (into meal_items)
      (values #(,meal ,date ,item ,quantity)))))

(define (register-meal)
  (sqlite3-do
    db
    `(insert
       (into meals)
       (valies #(,(caddr ctx) ,(cadr ctx))))))

(define (add-meal-item-repl tokens)
  (if (null? tokens)
    (show #t "no meal item provided")
    (let* ((name (string-join tokens " "))
           (quantity (prompt "quantity (g/ml)?")))
      (add-meal-item (caddr ctx) (cadr ctx) name quantity))))

(define (process-product-command tokens)
  (cond
    ((string=? (car tokens) "add") (add-product-repl (cdr tokens)))
    (else (show #t "unknown product command \"" (car tokens) "\"" nl))))

(define (process-meal-command tokens)
  (cond
    ((string=? (car tokens) "register") (register-meal))
    ((string=? (car tokens) "add") (add-meal-item-repl (cdr tokens)))
    (else (show #t "unknown meal command \"" (car tokens) "\"" nl))))

(define (process-command tokens)
  (cond
    ((not ctx) (show #t "no context set!" nl))
    ((eq? ctx 'product) (process-product-command tokens))
    ((and (list ctx) (eq? (car ctx) 'meal)) (process-meal-command tokens))
    (else (show #t "invalid context \"" ctx "\"" nl))))

(define (process line)
  (let ((tokens (string-split line " ")))
   (cond
     ((null? tokens) (show #t "no command provided" nl))
     ((string=? (car tokens) "mode") (process-mode (cdr tokens)))
     ((string=? (car tokens) "quit") (exit 0))
     (else (process-command tokens)))))

(show #t "> ")
(do
  ((line (read-line) (read-line)))
  ((eof-object? line))
  (process line)
  (show #t (ctx-string) "> "))
