#lang racket
; mySubmission ;)
(define (constructBST) '())

(define (addToBST BST k v)
  (if (null? BST)
      (list k v '() '())
      (match BST
        [ (list ck cv cl cr)
            (if (< k ck)
                (list ck cv (addToBST cl k v) cr)
                (list ck cv cl (addToBST cr k v) )
            )
        ]
      )
   )
)

(define (searchBST BST k)
  (if (null? BST)
      (list k)
      (match BST
        [ (list ck cv cl cr)
            (cond [(= k ck) (list k cv)]
                  [(< k ck) (searchBST cl k)]
                  [else (searchBST cr k)]
            )
        ]
      )
   )
)

(define (mapBST BST someFunct )
  (if (null? BST)
      '()
      (match BST
        [(list ck cv cl cr) (list ck (someFunct cv) (mapBST cl someFunct) (mapBST cr someFunct)) ]
      )
  )
)

(define (foldBST BST someFunct )
  (if (null? BST)
      (someFunct '() '() '() '())
      (match BST
        [(list a b c d) (someFunct a b (foldBST c someFunct) (foldBST d someFunct))]
      )
  )
)

; HERE TESTS START
;myUserFunctionDefinitions
(define (addAllKeys k v lfv rfv)
  (if (null? k)
      0
      (+ k lfv rfv)
  )
)
(define (concatStringValuesPreOrder k v lfv rfv)
  (if (null? k)
      ""
      (string-append v lfv rfv)
  )
)
(define (concatStringValuesInOrder k v lfv rfv)
  (if (null? k)
      ""
      (string-append lfv v rfv)
  )
)
(define (flattenTree k v lfv rfv)
  (if (null? k)
      '()
      (append lfv (list k v)  rfv)
  )
)
(define (minimumKey k v lfv rfv)
  (if (null? k)
      '()
      (if (null? lfv)
          k
          lfv
      )
  )
)
(define (addAllValueLengths k v lfv rfv)
  (if (null? k)
      0
      (+ k (length v) lfv rfv)
  )
)
(define (longestListValue k v lfv rfv)
  (if (null? k)
      0
      (max (length v) lfv rfv)
  )
)
(define (maxValue k v lfv rfv)
  (if (null? k)
      0
      (max v lfv rfv)
  )
)

;mytests
;first build a tree; not output to check at this point
(define bn0 (constructBST))
(define bn1 (addToBST bn0 4 "-4"))
(define bn2 (addToBST bn1 6 "-6"))
(define bn6 (addToBST (addToBST (addToBST (addToBST bn2 2 "-2") 1 "-1") 3 "-3") 5 "-5"))

; now test if all works
(searchBST bn2 5)
(searchBST bn2 4)
(searchBST bn6 4)
(searchBST bn6 5)
(searchBST bn6 1)
(searchBST bn6 7)
(foldBST bn6 addAllKeys )
(foldBST bn6 minimumKey ) 
(foldBST bn6 flattenTree ) 
(foldBST bn6 concatStringValuesPreOrder ) 
(foldBST bn6 concatStringValuesInOrder )
(let ([mt (mapBST bn6 string->number)]) (searchBST mt 4))
(let ([mt (mapBST bn6 string->number)]) (searchBST mt 5))
(let ([mt (mapBST bn6 string->number)]) (searchBST mt 1))
(let ([mt (mapBST bn6 string->number)]) (searchBST mt 7))
(let ([mt (mapBST bn6 string->number)]) (searchBST bn6 4))
(let ([mt (mapBST bn6 string->number)]) (foldBST bn6 flattenTree ))


;mytests 2
;first build a tree; not output to check at this point
(define bl0 (constructBST))
(define bl1 (addToBST bl0 3 '()))
(define bl2 (addToBST bl1 1 '("a" "b" "c")))
(define bl6 (addToBST (addToBST (addToBST (addToBST bl2 2 '("d")) 4 '("f" "g")) 7 '("i" "j" "k" "l")) 5 '("h")))

; now test if all works
(searchBST bl6 3)
(searchBST bl6 2)
(searchBST bl6 7)
(foldBST bl6 flattenTree ) 
(foldBST bl6 longestListValue ) 
(let ([mt (mapBST bl6 length)]) (searchBST mt 3))
(let ([mt (mapBST bl6 length)]) (searchBST mt 7))
(let ([mt (mapBST bl6 length)]) (foldBST mt maxValue ))

