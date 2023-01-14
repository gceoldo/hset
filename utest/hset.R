library(testthat)

source("/Users/giacomoceoldo/Github/hsets/htest2.R")
source("/Users/giacomoceoldo/Github/hsets/operations.R")

message("test initialization")
test_that("test initialization", {
  
  expect_s4_class(object = hset(), class = "hset")
  expect_s4_class(object = hset(NULL), class = "hset")
  expect_s4_class(object = hset(hset()), class = "hset")
  
  expect_s4_class(object = hset(c(1,2,3)), class = "hset")
  expect_s4_class(object = hset(logical()), class = "hset")
  expect_s4_class(object = hset(complex()), class = "hset")
  
  expect_s4_class(object = hset(list()), class = "hset")
  expect_s4_class(object = hset(list(list())), class = "hset")
  expect_s4_class(object = hset(list(1,hset())), class = "hset")
  expect_s4_class(object = hset(list(list(),hset())), class = "hset")
  
  expect_error(object = hset(data.frame()))
  expect_error(object = hset(list(1,hset(),list(1,data.frame()))))
  
  expect_false(is.generalized(hset()))
  expect_true(is.generalized(hset(gen=T)))
  expect_true(is.generalized(hset(members = 1, multiplicities = 1.5)))
  expect_true(is.generalized(as.generalized(hset())))
  expect_false(is.generalized(as.not.generalized(hset(gen=T))))
  
  expect_length(members(hset()), 0)
  expect_type(members(hset()), "character")
  expect_length(multiplicities(hset()),0)
  expect_length(multiplicities(hset(generalized = TRUE)),0)
  expect_equal(size.support(hset()), 0)
  expect_equal(size.support(hset(list(1,list(-1,2)))), 2)
  expect_error(cardinality(hset(generalized = TRUE)))
  expect_equal(cardinality(hset(c(1,2,3), c(1,2.5,3))), sum(c(1,2.5,3)))

})

message("test semantics")
test_that("test semantics", {
  
  s <- hset(c(1,2),c(1,1))
  cs <- copy.hset(s)
  expect_equal(is.generalized(s), is.generalized(cs))
  expect_equal(members(s), members(cs))
  expect_equal(multiplicities(s), multiplicities(cs))
  set.numeric.multiplicity(cs, "1", get.numeric.multiplicity(cs, "1") + 1.5)
  expect_false(get.numeric.multiplicity(s, "1") == get.numeric.multiplicity(cs, "1"))
  rm(list = c("s", "cs"))
  
  s <- hset(c(1,2),c(1,1))
  rs <- refer.to.hset(s)
  set.numeric.multiplicity(rs, "1", get.numeric.multiplicity(rs, "1") + 1.5)
  expect_true(get.numeric.multiplicity(s, "1") == get.numeric.multiplicity(rs, "1"))
  rm(list = c("s", "rs"))

})

message("test inclusion element")
test_that("test inclusion element", {
  m <- inclusion.member
  l <- label.member
  
  s <- hset(list(1,2,list(1,3)))
  expect_true( m(l(c(1,3)), s))
  expect_true( m(l(1), s))
  expect_false(m(l(c(1,2)), s))
  rm(s)
  
  s <- hset(list(1,2,list(1,3)), c(1,.5,2))
  expect_true( m(l(c(1,3)), s))
  expect_false(m(l(2), s))
  expect_false(m(l(c(1,3)), s, type.relation = `==`))
  expect_true( m(l(c(1,3)), s, multiplicity = 2))
  expect_true( m(l(c(1,3)), s, multiplicity = 2, type.relation = `==`))
  expect_false(m(l(c(1,3)), s, multiplicity = 2, type.relation = `<`))
  
  expect_false(m("fsdfsd", s))
  expect_false( m("fsdfsd", s, multiplicity = 2))
  rm(s)
  
  rm(m, l)
})

message("test inclusion subset")
test_that("test inclusion subset", {
  i <- hset1.included.to.hset2
  h <- hsets.are.equal
  
  s1 <- hset(list(1,2,list(1,3)))
  s2 <- hset(list(1,2,list(1,3),4))
  s3 <- hset(list(1,2,3))
  expect_true( i(s1,s2))
  expect_true( i(s1,copy.hset(s1)))
  expect_true( i(s1,s2,strictly = TRUE))
  expect_false(i(s1,copy.hset(s1),strictly = TRUE))
  expect_false(i(s1,s3))
  expect_true( h(s1,s1))
  expect_true( h(s1,copy.hset(s1)))
  expect_false(h(s1,s2))
  rm(s1,s2,s3)
  
  s1 <- hset(list(1,2,list(1,3)),c(2,3,4))
  s2 <- hset(list(1,2,list(1,3)),c(2,3,5))
  s3 <- hset(list(1,2,list(1,3)),c(2,3,3))
  s4 <- hset(list(1,2,list(1,3),4),c(2,3,4,5))
  expect_true( i(s1,s2))
  expect_true( i(s1,copy.hset(s1)))
  expect_true( i(s1,s2,strictly = TRUE))
  expect_true( i(s1,s4,strictly = FALSE))
  expect_true( i(s1,s4,strictly = TRUE))
  expect_false(i(s1,copy.hset(s1),strictly = TRUE))
  expect_false(i(s1,s3))
  expect_true( h(s1,s1))
  expect_true( h(s1,copy.hset(s1)))
  expect_false(h(s1,s3))
  rm(s1,s2,s3,s4)
  
  rm(i)
})

message("test generalized set operations")
test_that("test generalized set operations", {
  op <- hset.operation.numeric
  
  # trivial operations:
  
  
  # binary operations:
  
  s1 <- hset(list(1,2,c(2,3)), c(1,2,3))
  s2 <- hset(list(c(2,3),c(1,2),1),c(2,3,4))
  inter <- hset(list(1,c(2,3)),c(1,2))
  union <- hset(list(1,2,c(1,2),c(2,3)),c(4,2,3,3))
  dif12 <- hset(list(2, c(2,3)), c(2,1)) 
  dif21 <- hset(list(1, c(1,2)), c(3,3))
  sum12 <- hset(list(1,2,c(1,2),c(2,3)),c(5,2,3,5))
  
  s1.and.s2 <- op(s1, s2, operation = min,  identity.is.universe = TRUE,  semantic = "value")
  s1.or.s2  <- op(s1, s2, operation = max,  identity.is.universe = FALSE, semantic = "value")
  s1.min.s2 <- op(s1, s2, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  s2.min.s1 <- op(s2, s1, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  s1.sum.s2 <- op(s1, s2, operation = sum,  identity.is.universe = FALSE, semantic = "value")
  rm(s1, s2)
  expect_true(hsets.are.equal(inter, s1.and.s2))
  expect_true(hsets.are.equal(union, s1.or.s2 ))
  expect_true(hsets.are.equal(dif12, s1.min.s2))
  expect_true(hsets.are.equal(dif21, s2.min.s1))
  expect_true(hsets.are.equal(sum12, s1.sum.s2))
  rm(inter, union, dif12, dif21, sum12)
  rm(s1.and.s2, s1.or.s2, s1.min.s2, s2.min.s1, s1.sum.s2)
  
  
  # operations with general arity:
  
  s1 <- hset(list(1,2,c(2,3)), c(1,2,3))
  s2 <- hset(list(c(2,3),c(1,2),1),c(2,3,4))
  s3 <- hset(list(2,1), c(1,1))
  s4 <- hset(list(c(1,2),1),c(3,1))
  
  inter <- hset(1,1)
  union <- hset(list(1,2,c(1,2),c(2,3)),c(4,2,3,3))
  dif12 <- hset(list(2,c(2,3)),c(1,1))
  dif21 <- hset(1,1)
  sumal <- hset(list(1,2,c(1,2),c(2,3)),c(7,3,6,5))
  empty <- hset(generalized = TRUE)
  
  and <- op(s1, s2, s3, s4, operation = min,  identity.is.universe = TRUE,  semantic = "value")
  or  <- op(s1, s2, s3, s4, operation = max,  identity.is.universe = FALSE, semantic = "value")
  d12 <- op(s1, s2, s3, s4, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  d21 <- op(s2, s1, s3, s4, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  sm  <- op(s1, s2, s3, s4, operation = sum,  identity.is.universe = FALSE, semantic = "value")
  em  <- op(inter, dif21,   operation = pdif, identity.is.universe = FALSE, semantic = "value")
  rm(s1, s2, s3, s4)
  expect_true(hsets.are.equal(inter, and))
  expect_true(hsets.are.equal(union, or ))
  expect_true(hsets.are.equal(dif12, d12))
  expect_true(hsets.are.equal(dif21, d21))
  expect_true(hsets.are.equal(sumal, sm ))
  expect_true(hsets.are.equal(empty, em ))
  rm(inter, union, dif12, dif21, sumal, empty)
  rm(and, or, d12, d21, sm, em)
  
  
  rm(op)
})

message("test nongeneralized set operations")
test_that("test nongeneralized set operations", {
  op <- hset.operation.logical
  
  # trivial operations:
  
  
  # binary operations:
  
  s1 <- hset(list(1,2,c(2,3)))
  s2 <- hset(list(c(2,3),c(1,2),1))
  inter <- hset(list(1,c(2,3)))
  union <- hset(list(1,2,c(1,2),c(2,3)))
  dif12 <- hset(2) 
  dif21 <- hset(list(c(1,2)))
  
  s1.and.s2 <- op(s1, s2, operation = all,  identity.is.universe = TRUE,  semantic = "value")
  s1.or.s2  <- op(s1, s2, operation = any,  identity.is.universe = FALSE, semantic = "value")
  s1.min.s2 <- op(s1, s2, operation = nimp, identity.is.universe = FALSE, semantic = "value")
  s2.min.s1 <- op(s2, s1, operation = nimp, identity.is.universe = FALSE, semantic = "value")
  rm(s1, s2)
  expect_true(hsets.are.equal(inter, s1.and.s2))
  expect_true(hsets.are.equal(union, s1.or.s2 ))
  expect_true(hsets.are.equal(dif12, s1.min.s2))
  expect_true(hsets.are.equal(dif21, s2.min.s1))
  rm(inter, union, dif12, dif21)
  rm(s1.and.s2, s1.or.s2, s1.min.s2, s2.min.s1)
  
  
  # operations with general arity:
  
  s1 <- hset(list(1,2,4,c(2,3)))
  s2 <- hset(list(c(2,3),c(1,2),1))
  s3 <- hset(list(2,1))
  s4 <- hset(list(c(1,2),1))
  
  inter <- hset(1)
  union <- hset(list(1,2,c(1,2),c(2,3),4))
  dif12 <- hset(4)
  dif21 <- hset()
  
  and <- op(s1, s2, s3, s4, operation = all,  identity.is.universe = TRUE,  semantic = "value")
  or  <- op(s1, s2, s3, s4, operation = any,  identity.is.universe = FALSE, semantic = "value")
  d12 <- op(s1, s2, s3, s4, operation = nimp, identity.is.universe = FALSE, semantic = "value")
  d21 <- op(s2, s1, s3, s4, operation = nimp, identity.is.universe = FALSE, semantic = "value")
  rm(s1, s2, s3, s4)
  expect_true(hsets.are.equal(inter, and))
  expect_true(hsets.are.equal(union, or ))
  expect_true(hsets.are.equal(dif12, d12))
  expect_true(hsets.are.equal(dif21, d21))
  rm(inter, union, dif12, dif21)
  rm(and, or, d12, d21)
  
  
  rm(op)
})

message("test mixed set operations")
test_that("test mixed set operations", {
  op <- hset.operation.numeric
  
  # trivial operations:
  
  
  # binary operations:
  
  s1 <- hset(list(1,2,c(2,3)), c(1,2,3))
  s2 <- hset(list(c(2,3),c(1,2),1))
  inter <- hset(list(1,c(2,3)),c(1,1))
  union <- hset(list(1,2,c(1,2),c(2,3)),c(1,2,1,3))
  dif12 <- hset(list(2, c(2,3)), c(2,2)) 
  dif21 <- hset(list(c(1,2)), generalized = TRUE)
  sum12 <- hset(list(1,2,c(1,2),c(2,3)),c(2,2,1,4))
  
  s1.and.s2 <- op(s1, s2, operation = min,  identity.is.universe = TRUE,  semantic = "value")
  s1.or.s2  <- op(s1, s2, operation = max,  identity.is.universe = FALSE, semantic = "value")
  s1.min.s2 <- op(s1, s2, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  s2.min.s1 <- op(s2, s1, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  s1.sum.s2 <- op(s1, s2, operation = sum,  identity.is.universe = FALSE, semantic = "value")
  rm(s1, s2)
  expect_true(hsets.are.equal(inter, s1.and.s2))
  expect_true(hsets.are.equal(union, s1.or.s2 ))
  expect_true(hsets.are.equal(dif12, s1.min.s2))
  expect_true(hsets.are.equal(dif21, s2.min.s1))
  expect_true(hsets.are.equal(sum12, s1.sum.s2))
  rm(inter, union, dif12, dif21, sum12)
  rm(s1.and.s2, s1.or.s2, s1.min.s2, s2.min.s1, s1.sum.s2)
  
  
  # operations with general arity:
  
  s1 <- hset(list(1,2,c(2,3)))
  s2 <- hset(list(c(2,3),c(1,2),1),c(2,3,4))
  s3 <- hset(list(2,1))
  s4 <- hset(list(c(1,2),1))
  
  inter <- hset(1,1)
  union <- hset(list(1,2,c(1,2),c(2,3)),c(4,1,3,2))
  dif12 <- hset(generalized = TRUE)
  dif21 <- hset(list(1,c(1,2),c(2,3)),c(1,2,1))
  sumal <- hset(list(1,2,c(1,2),c(2,3)),c(7,2,4,3))
  
  and <- op(s1, s2, s3, s4, operation = min,  identity.is.universe = TRUE,  semantic = "value")
  or  <- op(s1, s2, s3, s4, operation = max,  identity.is.universe = FALSE, semantic = "value")
  d12 <- op(s1, s2, s3, s4, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  d21 <- op(s2, s1, s3, s4, operation = pdif, identity.is.universe = FALSE, semantic = "value")
  sm  <- op(s1, s2, s3, s4, operation = sum,  identity.is.universe = FALSE, semantic = "value")
  rm(s1, s2, s3, s4)
  expect_true(hsets.are.equal(inter, and))
  expect_true(hsets.are.equal(union, or ))
  expect_true(hsets.are.equal(dif12, d12))
  expect_true(hsets.are.equal(dif21, d21))
  expect_true(hsets.are.equal(sumal, sm ))
  rm(inter, union, dif12, dif21, sumal)
  rm(and, or, d12, d21, sm)
  
  
  rm(op)
})

message("test generics")
test_that("test generics", {
  expect_equal(1,1)
})  
  
