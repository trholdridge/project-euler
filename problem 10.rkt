;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |problem 10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Summation of primes
; find the sum of all primes below 2 million

; sum-primes : Nat -> Nat
; takes maximum number (>= 3) and sums all primes below the number
(check-expect (sum-primes 3) 2)
(check-expect (sum-primes 15) 41)
(define (sum-primes n0)
  (local [; sum-primes/acc : Nat Nat [List-of Nat] -> Nat
          ; ACCUMULATOR: sum-so-far adds up the primes that have been found
          ; ACCUMULATOR: primes-so-far lists the primes found
          (define (sum-primes/acc n sum-so-far primes-so-far)
            (cond
              [(>= n n0) sum-so-far]
              [(< n n0) (if (ormap (λ (test) (zero? (modulo n test))) primes-so-far)
                            (sum-primes/acc (+ 2 n) sum-so-far primes-so-far)
                            (sum-primes/acc (+ 2 n)
                                            (+ n sum-so-far)
                                            (cons n primes-so-far)))]))]
    (sum-primes/acc 3 2 '(2))))