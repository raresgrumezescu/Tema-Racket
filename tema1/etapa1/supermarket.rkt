#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (define C (make-counter index 0 '()))
  C)


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (match C
    [(counter index tt queue)
    (define Cn (struct-copy counter C [tt (+ tt minutes)] )) Cn]))


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic

(define (min-tt-tail counters acc)
  (if (null? counters)
      acc
      (cond
        ((> (cdr acc) (counter-tt (car counters))) (min-tt-tail (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters)))))
        ((and (= (cdr acc) (counter-tt (car counters))) (> (car acc) (counter-index (car counters)))) (min-tt-tail (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters)))))
        (else (min-tt-tail (cdr counters) acc)))))

(define (min-tt counters)
  (if (null? counters)
      null
      (min-tt-tail (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))))))


(define C1 (make-counter 1 10 '((ion . 15))))
(define C2 (make-counter 2 16 '((andrei . 12))))
(define C3 (make-counter 3 2 '((alina . 10))))
(define C4 (make-counter 4 5 '((marcu . 5))))
(define C5 (make-counter 5 5 '((marian . 26))))
(define C6 (make-counter 6 2 '((ionel . 7))))
(define C7 (make-counter 7 11 '((filip . 12))))

(define case-lista (list C1 C2 C3 C4 C5 C6 C7))

; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (define Cn (struct-copy counter C [queue (append (counter-queue C) (cons (cons name n-items) null))] [tt (+ (counter-tt C) n-items)])) Cn)

;(define (add-to-counter C name n-items)
 ; (define Cn (struct-copy counter C [queue (append (counter-queue C) (cons (cons name n-items) null))])) C)

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește

(define (casa-timp-minim case timp-minim)
  (if (null? case)
      timp-minim
      (if (> timp-minim (counter-tt (car case)))
          (casa-timp-minim (cdr case) (counter-tt (car case)) )
          (casa-timp-minim (cdr case) timp-minim ))))

(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (cond
                                       ((= index (counter-index C1)) (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4))
                                       ((= index (counter-index C2)) (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4))
                                       ((= index (counter-index C3)) (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4))
                                       ((= index (counter-index C4)) (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes))))]
        [(list name n-items)        (if (<= n-items 11)
                                        (cond
                                          ((= (casa-timp-minim (list C1 C2 C3 C4) (counter-tt C1)) (counter-tt C1)) (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4))
                                          ((= (casa-timp-minim (list C1 C2 C3 C4) (counter-tt C1)) (counter-tt C2)) (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))
                                          ((= (casa-timp-minim (list C1 C2 C3 C4) (counter-tt C1)) (counter-tt C3)) (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))
                                          ((= (casa-timp-minim (list C1 C2 C3 C4) (counter-tt C1)) (counter-tt C4)) (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))))
                                        (cond
                                          ((= (casa-timp-minim (list C2 C3 C4) (counter-tt C2)) (counter-tt C2)) (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4))
                                          ((= (casa-timp-minim (list C2 C3 C4) (counter-tt C2)) (counter-tt C3)) (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4))
                                          ((= (casa-timp-minim (list C2 C3 C4) (counter-tt C2)) (counter-tt C4)) (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))))) ])))
