`%in%` <- function(member, hset) {
  if(is.hset(hset)) return(sapply(member, function(x) inclusion.member(x, hset)))
  else return(match(member, hset, nomatch = 0) > 0)
  #else return(sapply(member, function(x) match(x, hset, nomatch = 0) > 0))
}


`<=`    <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset1, hset2), return(.Primitive("<=")(hset1, hset2)))
`>=`    <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset2, hset1), return(.Primitive(">=")(hset1, hset2)))

`<`     <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset1, hset2, strictly = TRUE), return(.Primitive("<")(hset1, hset2)))
`>`     <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset2, hset1, strictly = TRUE), return(.Primitive(">")(hset1, hset2)))

`%=<=%` <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset1, hset2, exactly  = TRUE), stop("the operator '%=<=%' is defined only for arguments of class 'hset'"))
`%=>=%` <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset2, hset1, exactly  = TRUE), stop("the operator '%=>=%' is defined only for arguments of class 'hset'"))

`%=<%`  <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset1, hset2, strictly = TRUE, exactly = TRUE), stop("the operator '%=<%' is defined only for arguments of class 'hset'"))
`%=>%`  <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hset1.included.to.hset2(hset2, hset1, strictly = TRUE, exactly = TRUE), stop("the operator '%=>%' is defined only for arguments of class 'hset'"))

`==`    <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2), hsets.are.equal(hset1, hset2), return(.Primitive("==")(hset1, hset2)))
`!=`    <- function(hset1, hset2) ifelse(is.hset(hset1) & is.hset(hset2),!hsets.are.equal(hset1, hset2), return(.Primitive("!=")(hset1, hset2)))


`%&~%`   <- function(hset1, hset2) intersection(hset1, hset2, semantic = "value")
`%&&~%`  <- function(hset1, hset2) intersection(hset1, hset2, semantic = "value")
`%and~%` <- function(hset1, hset2) intersection(hset1, hset2, semantic = "value")
`%&%`    <- function(hset1, hset2) intersection(hset1, hset2, semantic = "refer")
`%&&%`   <- function(hset1, hset2) intersection(hset1, hset2, semantic = "refer")
`%and%`  <- function(hset1, hset2) intersection(hset1, hset2, semantic = "refer")

`%|~%`  <- function(hset1, hset2) union(hset1, hset2, semantic = "value")
`%||~%` <- function(hset1, hset2) union(hset1, hset2, semantic = "value")
`%or~%` <- function(hset1, hset2) union(hset1, hset2, semantic = "value")
`%|%`   <- function(hset1, hset2) union(hset1, hset2, semantic = "refer")
`%||%`  <- function(hset1, hset2) union(hset1, hset2, semantic = "refer")
`%or%`  <- function(hset1, hset2) union(hset1, hset2, semantic = "refer")

`%-~%`    <- function(hset1, hset2) difference(hset1, hset2, semantic = "value")
`%-%`     <- function(hset1, hset2) difference(hset1, hset2, semantic = "refer")
`%!imp~%` <- function(hset1, hset2) difference(hset1, hset2, semantic = "value")
`%!imp%`  <- function(hset1, hset2) difference(hset1, hset2, semantic = "refer")

`%+~%`   <- function(hset1, hset2) setsum(hset1, hset2, semantic = "value")
`%+%`    <- function(hset1, hset2) setsum(hset1, hset2, semantic = "refer")
`%sum~%` <- function(hset1, hset2) setsum(hset1, hset2, semantic = "value")
`%sum%`  <- function(hset1, hset2) setsum(hset1, hset2, semantic = "refer")

`%--~%`  <- function(hset1, hset2) symmdiff(hset1, hset2, semantic = "value")
`%xor~%` <- function(hset1, hset2) symmdiff(hset1, hset2, semantic = "value")
`%--%`   <- function(hset1, hset2) symmdiff(hset1, hset2, semantic = "refer")
`%xor%`  <- function(hset1, hset2) symmdiff(hset1, hset2, semantic = "refer")

