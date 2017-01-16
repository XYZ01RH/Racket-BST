#lang racket

(define bst 
  '( 3 "3"
       ( 1 "1"
           ()
           ( 2 "2" () ())
           )
       ( 5 "5" () () )
       )
  ) 

; Makes a new binary search tree with the given values
(define (makeBST key value left right)
  (list key value left right)
  )

; Makes an empty binary search tree
(define (constructBST)
  '()
  )

; Getter method to get this key
(define (getKey bst)
  (list-ref bst 0)
  )

; Getter method to get this value
(define (getValue bst)
  (list-ref bst 1)
  )

; Getter method to get left subtree
(define (getLeft bst)
  (list-ref bst 2)
  )

; Getter method to get right subtree
(define (getRight bst)
  (list-ref bst 3)
  )

; Creates a new tree with the added key value pairs to the appropriate place in the BST
(define (addToBST bst key value)
  (cond
    [(null? bst) (makeBST key value null null)]
    [(= key (getKey bst))
     (makeBST (getKey bst) value (getLeft bst) (getRight bst))]
    [(< key (getKey bst))
     (makeBST [getKey bst] [getValue bst] (addToBST (getLeft bst) key value) (getRight bst))]
    [else
     (makeBST [getKey bst] [getValue bst] (getLeft bst) (addToBST (getRight bst) key value))]
  ))

; Searches the tree for the given key
; Returns just the key if it is not in the BST
; Returns the key value pair if it is in the BST
(define (searchBST bst key)
  (cond
    [(null? bst) (list key)]
    [else (cond
            [(= key (getKey bst))
             (list (getKey bst) (getValue bst))]
            [(< key (getKey bst))
             (searchBST (getLeft bst) key)]
            [(> key (getKey bst))
             (searchBST (getRight bst) key)])])
  )

; Maps a new BST with the given passed in function
(define (mapBST bst someFunct)
  ; empty tree
  (cond ((null? bst)
         '())
        ; leaf node
        ((and (null? (getRight bst)) (null? (getLeft bst)))
         (makeBST (getKey bst) (someFunct (getValue bst)) '() '()))
        ; empty right subtree
        ((and (null? (getRight bst)) (not (null? (getLeft bst))))
         (makeBST (getKey bst) (someFunct (getValue bst))
                  (mapBST (getLeft bst) someFunct)
                  '()))
        ; empty left subtree
        ((and (null? (getLeft bst)) (not (null? (getRight bst))))
         (makeBST (getKey bst) (someFunct (getValue bst))
                  '()
                  (mapBST (getRight bst) someFunct)))
        ; both subtrees are present
        (else
         (makeBST (getKey bst) (someFunct (getValue bst))
                  (mapBST (getLeft bst) someFunct)
                  (mapBST (getRight bst) someFunct))))
  )

; Folds a new BST with the given passed in function
(define (foldBST bst someFunct)
  (cond
    [(null? bst) (someFunct '() '() '() '())]
    [else (someFunct
           (getKey bst) (getValue bst)
           (foldBST (getLeft bst) someFunct)
           (foldBST (getRight bst) someFunct))])
  )

; Instructor Provided Method
(define (addAllKeys k v lfv rfv)
  (if (null? k) ; no need to check if v,lfv,rfv are also '(); if k is '(), it must have been a call (addAllKeys '() '() '() '())
      0 ; if called with null node, return 0
      (+ k lfv rfv) ; otherwise, add node's key to left-fold-value (sum of keys from left subtree) and right-fold-value (sum of keys from right subtree))
      )
  )

; Instructor Provided Method
(define (minimumKey k v lfv rfv)
  (if (null? k)
      '()
      (if (null? lfv)
          k
          lfv
          )
      )
  )

; Instructor Provided Method
(define (flattenTree k v lfv rfv)
  (if (null? k)
      '()
      (append lfv (list k v)  rfv)
      )
  )

; Instructor Provided Method
(define (concatStringValuesPreOrder k v lfv rfv)
  (if (null? k)
      ""
      (string-append v lfv rfv)
      )
  )

; Instructor Provided Method
(define (concatStringValuesInOrder k v lfv rfv)
  (if (null? k)
      ""
      (string-append lfv v rfv)
      )
  )

; Test cases
(define b0 (constructBST))
(define b1 (addToBST b0 4 "4"))
(define b2 (addToBST b1 6 "6"))
(define b6 (addToBST (addToBST (addToBST (addToBST b2 2 "2") 1 "1") 3 "3") 5 "5"))
(define mappedB6 (mapBST b6 string->number))

b0
b1
b2
b6
mappedB6

; Test Search Method for existing key in BST b6
(println (searchBST b6 1))


; Test Search Method for non existing key in BST b6
(println (searchBST b6 27))

; Instructor Provided Tests
(foldBST bst addAllKeys ) ; should return: 11
(foldBST bst minimumKey ) ; should return: 1
(foldBST bst flattenTree ) ; should return: '(1 "1" 2 "2" 3 "3" 5 "5")
(foldBST bst concatStringValuesPreOrder ) ; should return: "3125"
(foldBST bst concatStringValuesInOrder ) ; should return: "1235"