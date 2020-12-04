#lang br
(require racket/file rackunit)

(define passports (for/list ([pstr (string-split (file->string "04.rktd") #rx"\n\n+")])
                    (for/hash ([kvstr (string-split pstr)])
                      (apply values (string-split kvstr ":")))))

(define required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")) ; "cid" is not required

(define (passport-keys-valid? p)
  (for*/and ([passport-keys (in-value (hash-keys p))]
            [field required-fields])
    (member field passport-keys)))

(check-equal? (count passport-keys-valid? passports) 202)

(define (stringval-in-range str low high)
  (<= low (string->number str) high))

(define (passport-values-valid? p)
  (and
   (stringval-in-range (hash-ref p "byr") 1920 2002)
   (stringval-in-range (hash-ref p "iyr") 2010 2020)
   (stringval-in-range (hash-ref p "eyr") 2020 2030)
   (let ([hgt (hash-ref p "hgt")])
     (cond
       [(string-suffix? hgt "cm") (stringval-in-range (string-trim hgt "cm") 150 193)]
       [(string-suffix? hgt "in") (stringval-in-range (string-trim hgt "in") 59 76)]
       [else #false]))
   (let ([hcl (hash-ref p "hcl")])
     (and (regexp-match #rx"#......" hcl) (string->number (string-trim hcl "#") 16)))
   (member (hash-ref p "ecl") '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
   (let ([pid (hash-ref p "pid")]) (and (= 9 (string-length pid)) (string->number pid)))))

(check-equal? (count (λ (p) (and (passport-keys-valid? p) (passport-values-valid? p))) passports) 137)