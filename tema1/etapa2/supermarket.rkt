#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (define C (make-counter index 0 0 '()))
  C)


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.


(define (update f counters index)
  (foldl (λ(x acc)(if (= (counter-index x) index)
                  (append acc (cons (f x) null))
                  (append acc (cons x null)))) null counters))


; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.


(define tt+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (make-counter index (+ tt minutes) et queue) ]))))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define et+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (make-counter index tt (+ et minutes) queue) ]))))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.


(define add-to-counter
  (λ (name n-items)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (if (null? queue)
             (struct-copy counter C [queue (append (counter-queue C) (cons (cons name n-items) null))] [tt (+ (counter-tt C) n-items)] [et (+ (counter-et C) n-items)])
             (struct-copy counter C [queue (append (counter-queue C) (cons (cons name n-items) null))] [tt (+ (counter-tt C) n-items)] [et (counter-et C)]))]))))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)

(define Cnull
  (make-counter -1 10000 10000 '()))

(define (make-pair-tt C)
  (cons (counter-index C) (counter-tt C)))

(define (make-pair-et C)
  (cons (counter-index C) (counter-et C)))


(define (general-func f counters)
  (if (null? counters)
      (f Cnull)
      (if (<= (cdr (f (car counters))) (cdr (general-func f (cdr counters))))
          (f (car counters))
          (general-func f (cdr counters)))))

(define min-tt
  (λ (counters)
    (general-func make-pair-tt counters))) ; folosind funcția de mai sus

(define min-et
  (λ (counters)
    (general-func make-pair-et counters))) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (cond
    ((= (length (counter-queue C)) 0) C)
    ((= (length (counter-queue C)) 1) (struct-copy counter C [queue (cdr (counter-queue C))] [tt 0] [et 0]))
    ((> (length (counter-queue C)) 1) (struct-copy counter C [queue (cdr (counter-queue C))] [tt (- (counter-tt C) (counter-et C))] [et (cdr (cadr (counter-queue C)))]))))
    

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)


(define (min-et-mai-special counters)
  (if (null? counters)
      (cons 10000 10000)
      (if (and (<= (counter-et (car counters)) (cdr (min-et-mai-special (cdr counters)))) (> (length (counter-queue (car counters))) 0))
          (cons (counter-index (car counters)) (counter-et (car counters)))
          (min-et-mai-special (cdr counters)))))


(define (average-time counters time number)
  (if (null? counters)
      (/ time number)
      (average-time (cdr counters) (+ time (counter-tt (car counters))) (+ number 1))))

(define (add-counters counters counters2 average)
  (if (>= average (average-time counters2 0 0))
      counters
      (add-counters (append counters (cons (make-counter (+ (counter-index (car (reverse counters))) 1) 0 0 '()) null)) (append counters2 (cons (make-counter (+ (counter-index (car (reverse counters2))) 1) 0 0 '()) null)) average)))


(define (serve requests fast-counters slow-counters)

  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)

        [(list 'ensure average)      (if (>= average (average-time (append fast-counters slow-counters) 0 0))
                                         (serve (cdr requests) fast-counters slow-counters)
                                         (serve (cdr requests) fast-counters (add-counters slow-counters (append fast-counters slow-counters) average)))]
        
        [(list 'delay index minutes) (cond
                                       ((member index (map (λ(c)(counter-index c)) fast-counters)) (serve (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) slow-counters))
                                       ((member index (map (λ(c)(counter-index c)) slow-counters)) (serve (cdr requests) fast-counters (update (tt+ minutes) (update (et+ minutes) slow-counters index) index))))]
         [(list name n-items)        (if (<= n-items ITEMS)
                                         (if (= (car (min-tt fast-counters)) (car (min-tt (append fast-counters slow-counters))))
                                             (serve (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)
                                             (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))
                                        (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))]
        [(list 'remove-first)          (if (= (car (min-et-mai-special fast-counters)) (car (min-et-mai-special (append fast-counters slow-counters))))
                                             (serve (cdr requests) (update remove-first-from-counter fast-counters (car (min-et-mai-special fast-counters))) slow-counters)
                                             (serve (cdr requests) fast-counters (update remove-first-from-counter slow-counters (car (min-et-mai-special (append fast-counters slow-counters))))))]

                                  
        )))
                                     

