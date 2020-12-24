#lang racket

(require "read.rkt")

(require pict pict/tree-layout)

(define (show-tree t)
  (show-pict (tree->pict t 'COM)))

(define (tree->pict t root)
  (match-define (and p (struct* pict ([width width] [height height])))
    (naive-layered
     (let loop ([key root])
       (define node (hash-ref t key))
       (define edges (for/list ([child-key (node-children node)])
                       (tree-edge (loop child-key))))
       (apply tree-layout edges #:pict (cc-superimpose
                                        (disk 32 #:draw-border? #f)
                                        (colorize (disk 30 #:draw-border? #f) "white")
                                        (text (symbol->string key)))))))

  (cc-superimpose
   (filled-rectangle width height #:color "white" #:border-color "white")
   p))
