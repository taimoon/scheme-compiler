(import (only (compiler-amd64) compile-prog compile-main concat-object))
(include "compiler-rider.scm")
(rider compile-prog compile-main concat-object)