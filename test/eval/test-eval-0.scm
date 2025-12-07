(define (println x) (display x) (newline))
(define (install-uwu)
  (eval
    '(begin
      (define (uwu) (println "uwu"))
      (println (and (eq? (%symbol-value 'uwu) uwu) (procedure? uwu)))
      ((%symbol-value 'uwu))
      (uwu)
      uwu)))
((install-uwu))
(uwu)
