#lang racket

#| 
autor: Rafael Portácio 

Trabalharemos com uma ditribuição de números semelhante a uma
matriz. Considerações iniciais para a ideia utilizada nesse código:
note que seria impossível que tivessemos uma coluna de água mais alta
do que a mais alta coluna da matriz.  Assim como seria impossível
um "bloco" de água não ser cercado pelos 4 lados (pois a água
escorreria).
|#

(require "trapped-water.rkt")

; recebe uma lista de listas de números e devolve o maior elemento
(define (maxmtrx m)
  (maxlst (map maxlst m))) 

; definição de ponto da distribuição
(define (pos i j) (cons i j)) 

; retorna o elemento na posição p da nossa distribuição
(define (elt-i-j m p) 
  (if (and (col-limit? m p) (lin-limit? m p))
      (list-ref (list-ref m (- (lin-pos p) 1)) (- (col-pos p) 1))
      0))

; ponto inicial da distribuição
(define init (pos 1 1)) 

; retorna símbolo indicando que a matriz terminou de ser analisada
(define end-point 'over) 

; número da linha que o ponto está
(define (lin-pos p) (car p)) 

; número da coluna que o ponto está
(define (col-pos p) (cdr p)) 

; o ponto p está numa linha válida da matriz m?
(define (lin-limit? m p) 
  (and (>= (length m) (lin-pos p))
       (> (lin-pos p) 0)))

; o ponto p está numa coluna válida da matriz m?
(define (col-limit? m p) 
  (and (>= (length (list-ref m 1)) (col-pos p))
       (> (col-pos p) 0)))

; o ponto p está na última linha da matriz m?
(define (last-lin? m p) 
  (= (length m) (lin-pos p)))

; o ponto p está na última coluna da matriz m?
(define (last-col? m p) 
  (= (length (list-ref m (- (lin-pos p) 1)))
     (col-pos p)))

; o próximo ponto após p
(define (next m p) 
  (let ([i (lin-pos p)] [j (col-pos p)])
    (cond ((not (last-col? m p)) (pos i (+ j 1)))
          ((and (last-col? m p) (not (last-lin? m p))) (pos (+ i 1) 1))
          (#t end-point))))

; muda matriz m (transformando o elemento na posição p no valor v)
(define (change m p v) 
  (list-set m (- (lin-pos p) 1)
            (list-set (list-ref m (- (lin-pos p) 1)) (- (col-pos p) 1) v)))

;recebe uma matriz e devolve ela "pós-chuva" onde teremos colunas de
;água inclusas. Note que m-orig é a matriz original (sem colunas
;d'água). É possível que a água escape dessa posição?
(define (trapped-water m-orig) 
  (define (possible-escape? m p) 
    (let ([i (lin-pos p)] [j (col-pos p)])
      (> (elt-i-j m p)
         (min (elt-i-j m (pos (+ i 1) j))
              (elt-i-j m (pos (- i 1) j))
              (elt-i-j m (pos i (+ j 1)))
              (elt-i-j m (pos i (- j 1)))))))

  ; põe água em excesso em toda a matriz m (o excesso será removido pela waterflow)
  (define (put-water m) 
    (build-list (length m)
                (λ (x) (build-list (length (first m)) (λ (x) (maxmtrx m))))))

  ; fluxo de água (remove a água que tem como escapar, analisando
  ; ponto a ponto)
  (define (waterflow m p) 
    (cond ((equal? p end-point) m)

          ; testa se não há água acima da coluna neste ponto
          ((equal? (elt-i-j m p) (elt-i-j m-orig p))
           (waterflow m (next m p))) 

          ; essa água que há, pode escapar?
          ((possible-escape? m p)
           (waterflow (change m p (- (elt-i-j m p) 1)) (next m p))) 

          ; há água, mas ela não pode escapar então avançamos para a
          ; analíse do próximo ponto
          (#t (waterflow m (next m p))))) 

  ; chama a waterflow quantas vezes for necessário para que todo o
  ; excesso seja removido
  (define (improve-the-flow m) 
    (cond ((equal? (waterflow m init) m) m)
          (#t (improve-the-flow (waterflow m init)))))

  (improve-the-flow (put-water m-orig)))

; imprimi a lista de listas
(define (print-mtrx m) 
  (display (car m))
  (newline)
  (if (empty? (cdr m)) (newline) (print-mtrx (cdr m))))

; retorna a quantidade de água armazenada
(define (amount-water k) 
  (- (foldr + 0 (map (λ (x) (foldr + 0 x)) (trapped-water k)))
     (foldr + 0 (map (λ (x) (foldr + 0 x)) k))))


#|

(print-mtrx (trapped-water (list (list 9 9 9)
                                 (list 9 0 9)
                                 (list 9 3 2))))

(display "amount:")

(amount-water (list (list 9 9 9)
                    (list 9 0 9)
                    (list 9 3 2)))
(newline)

(print-mtrx (trapped-water (list (list 9 9 9 9 9 9 9 9 9)
                                 (list 3 2 0 4 5 3 6 8 2)
                                 (list 9 9 9 9 9 9 9 9 9))))

(display "amount:")

(amount-water (list (list 9 9 9 9 9 9 9 9 9)
                    (list 3 2 0 4 5 3 6 8 2)
                    (list 9 9 9 9 9 9 9 9 9)))
(newline)
(print-mtrx (trapped-water (list (list 9 7 6 6 7 8 4 6 3)
                                 (list 8 0 0 4 0 0 2 0 4)
                                 (list 6 7 4 0 6 3 0 3 0)
                                 (list 5 0 0 4 0 0 5 0 2)
                                 (list 8 4 0 2 5 8 0 6 0)
                                 (list 9 0 3 0 0 9 0 0 5)
                                 (list 5 3 0 7 0 6 0 5 3)
                                 (list 0 0 7 0 0 5 0 8 4)
                                 (list 4 0 5 6 4 0 7 1 8))))

(display "amount:")

(amount-water (list (list 9 7 6 6 7 8 4 6 3)
                    (list 8 0 0 4 0 0 2 0 4)
                    (list 6 7 4 0 6 3 0 3 0)
                    (list 5 0 0 4 0 0 5 0 2)
                    (list 8 4 0 2 5 8 0 6 0)
                    (list 9 0 3 0 0 9 0 0 5)
                    (list 5 3 0 7 0 6 0 5 3)
                    (list 0 0 7 0 0 5 0 8 4)
                    (list 4 0 5 6 4 0 7 1 8)))
|#
