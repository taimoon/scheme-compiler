;;; directed graph
(define (make-hashset)
  (make-hash-table symbol=? symbol-hash))

(define (hashset-add! hs x)
  (hash-table-intern! hs x (lambda () #t)))

(define (hashset->list hs) (hash-table-keys hs))

(define (hashset-contains? hs e)
  (hash-table-ref hs e (lambda () #f)))

(define (make-graph)
  (make-hash-table symbol=? symbol-hash))

(define (%graph-make-entry)
  (vector 0 (make-hashset)))

(define (graph-add-node! g v)
  (hash-table-intern! g v %graph-make-entry))

(define (graph-add-edge! g src dst)
  (hash-table-intern! g dst %graph-make-entry)
  (hashset-add! (vector-ref (hash-table-intern! g src %graph-make-entry) 1) dst))

(define (graph-neighbour->list g n)
  (hashset->list (vector-ref (hash-table-ref g n) 1)))

(define (graph-node-attr g n)
  (vector-ref (hash-table-ref g n) 0))

(define (graph-node-set-attr! g n v)
  (vector-set! (hash-table-ref g n) 0 v))

(define (graph-nodes g)
  (map car (hash-table->alist g)))

(define (graph->edgelist g)
  (hash-table-fold g
    (lambda (src neighbour r)
      (append (map (lambda (dst) (list src dst)) (hashset->list (vector-ref neighbour 1)))
              r))
    '()))

(define (graph->graph/inverted g)
  (let ((g* (make-graph)))
    (for-each (lambda (n) (graph-add-node! g* n)) (graph-nodes g)) 
    (for-each (lambda (e) (graph-add-edge! g* (cadr e) (car e))) (graph->edgelist g))
    g*))

(define (graph-edges-count g)
  (hash-table-fold g (lambda (k v c) (+ (hash-table-size (vector-ref v 1)) c)) 0))

(define (graph-nodes-count g)
  (hash-table-size g))

(define dfs (let ()
  (define (dfs-visit g n neighbour-f discovery finish k)
    (if (not (memq n discovery))
        (let recur ((discovery (cons n discovery))
                    (finish finish)
                    (ns (neighbour-f (graph-neighbour->list g n))))
          (if (pair? ns)
              (dfs-visit g (car ns) neighbour-f discovery finish
                (lambda (discovery finish) (recur discovery finish (cdr ns))))
              (k discovery (cons n finish))))
        (k discovery finish)))
  (case-lambda
    ((g k)
      (let recur ((discovery '())
                  (finish '())
                  (nodes (graph-nodes g)))
       (if (pair? nodes)
           (dfs-visit g (car nodes) (lambda (x) x) discovery finish
             (lambda (discovery finish)
               (recur discovery finish (cdr nodes))))
           (k discovery finish))))
    ((g n k)
     (dfs-visit g n (lambda (x) x) '() '() k))
    ((g n neighbour-f k)
     (dfs-visit g n neighbour-f '() '() k))
    ((g n neighbour-f discovery k)
     (dfs-visit g n neighbour-f discovery '() k)))))

(define (order-by xs ord)
  (fold-right
    (lambda (o s)
      (let ((r (memq o xs)))
        (if r
            (cons o s)
            s)))
    '()
    ord))

(define (kosaraju-components g)
  (define inv-g (graph->graph/inverted g))
  (define (revisit rev-finish)
    (let iter ((SCC '())
               (discovery '())
               (nodes rev-finish))
        (cond
          ((not (pair? nodes)) SCC)
          ((memq (car nodes) discovery)
           (iter SCC discovery (cdr nodes)))
          (else
            (dfs inv-g
                 (car nodes)
                 (lambda (neighbour) (order-by neighbour rev-finish))
                 discovery
                 (lambda (discovery* _)
                   (iter (cons (set-diff discovery* discovery) SCC)
                         discovery*
                         (cdr nodes))))))))
  (dfs g
    (lambda (discovery rev-finish) (revisit rev-finish))))

#;
(let ()
(define (set-diff s1 s2)
  (filter (lambda (x) (not (member x s2))) s1))
(pretty-print
  (dfs
    (edgelist->graph
      '((x1 x2) (x1 x5) (x1 x9)
        (x2 x3) (x3 x4) (x5 x6) (x5 x8)
        (x6 x7) (x9 x10)))
    list))
#;'((h) (f g) (d c) (b e a))
(let* ((G (edgelist->graph
           '((c d) (c g)
             (a b)
             (b e) (b f) (b c)
             (d c) (d h)
             (e a)
             (f g)
             (g f) (g h)
             (h h)))))
  (pretty-print (kosaraju-components G)))
)