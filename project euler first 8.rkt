;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |project euler first 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Problem 1: Multiples of 3 and 5

(define 3-or-5? (λ (n) (or (= 0 (modulo n 3)) (= 0 (modulo n 5)))))

(define (sum-through-n n)
  (cond
    [(= n 2) 0]
    [(> n 2) (+ (if (3-or-5? n) n 0)
                (sum-through-n (sub1 n)))]))

; Problem 2: Even Fibonacci numbers

(define (sum-fibonacci i j)
  (local [(define n (+ i j))]
    (cond
      [(> n 4000000) 2]
      [(< n 4000000) (+ (if (even? n) n 0)
                        (sum-fibonacci j n))])))

; Problem 6: Sum square difference

(define (square-diff n)
  (- (* (/ (* n (+ n 1)) 2) (/ (* n (+ n 1)) 2))
     (sum-squares n)))

(define (sum-squares n)
  (cond
    [(= n 1) 1]
    [(> n 1) (+ (* n n)
                (sum-squares (sub1 n)))]))

; Problem 7: 10001st prime

(define (find-prime n0)
  (local [(define (find-prime/acc n primes-so-far)
            (cond
              [(= n n0) (first primes-so-far)]
              [(< n n0) (find-prime/acc (add1 n)
                                        (cons (next-prime primes-so-far)
                                              primes-so-far))]))]
    (find-prime/acc 1 '(2))))

(define (next-prime lon)
  (local [(define (next-prime/test t lon)
            (cond
              [(ormap (λ (n) (zero? (modulo t n))) lon) (next-prime/test (add1 t) lon)]
              [else t]))]
    (next-prime/test (add1 (first lon)) lon)))
 