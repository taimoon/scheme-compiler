(load "compiler.scm")
(match (command-line-arguments)
  [("recompile-runtime")
   (compile-runtime)]
  [("recompile-top-prog")
   (compile-top-prog)]
  [("-o" ,output-name ,input-path)
   (make-obj! (cons 'begin (read-sexps-from-path input-path)) output-name)]
  [("-link" ,output-path . ,files)
   (apply linking-files-to-exe! (cons output-path files))]
  [(,input-path ,output-path "link-with-top-prog")
   (make-obj! (cons 'begin (read-sexps-from-path input-path)) "a")
   (linking-files-to-exe! output-path "top-prog.linker" "a.linker")]
  [,args (error "unmatch" args)])