inclusion.member <- function(member, hset, multiplicity = 1L, type.relation = `<=`) {
  if(length(member) != 1L) stop("member must be of length 1")
  if(length(multiplicity) != 1L) stop("multiplicity must be of length 1")
  if(!is.generalized(hset)) return(get.logical.multiplicity(hset, as.character(member))) 
  else return(type.relation(multiplicity, get.numeric.multiplicity(hset, as.character(member))))
}



# operations with arity 2

hset1.included.to.hset2 <- function(hset1, hset2, strictly = FALSE, exactly = FALSE) {
  if(size.support(hset1) > size.support(hset2)) return(FALSE)
  if(!some.operand.is.generalized(hset1,hset2)) {
    for(i in members(hset1)) if(is.null(hset2@htable[[i]])) return(FALSE) # don't like this line
    if(strictly) return(size.support(hset1) < size.support(hset2))
    return(TRUE)
  }
  else {
    mem <- members(hset1)
    total.absolute.difference <- 0.0
    for(i in mem) {
      difference <- get.numeric.multiplicity(hset2,i) - get.numeric.multiplicity(hset1,i)
      if(difference < 0) return(FALSE)
      total.absolute.difference <- total.absolute.difference + abs(difference)
    }
    if(strictly & exactly) return((size.support(hset1) != size.support(hset2)) & (total.absolute.difference == 0))
    if(strictly) return((size.support(hset1) != size.support(hset2)) | (total.absolute.difference != 0))
    if(exactly)  return(total.absolute.difference == 0)
    else return(TRUE)
  }
}

some.operand.is.generalized <- function(hset1, ...) return(is.generalized(hset1) | any(are.generalized(...)))

hsets.are.equal <- function(hset1, hset2) {
  if(size.support(hset1) != size.support(hset2)) return(FALSE)
  else {
    if(some.operand.is.generalized(hset1, hset2)) return(all(members(hset1) == members(hset2)) & all(multiplicities(hset1) == multiplicities(hset2)))
    else return(all(members(hset1) == members(hset2)))
  }
  
} 


# operations with arity at least 1

hset.operation.numeric <- function(hset1, ..., operation = max, identity.is.universe = FALSE, semantic = "refer") {
  if(operation.is.trivial(...)) return(hset1)
  sets <- list(...)
  new.hset <- create.new.hset(hset1, generalized = TRUE, semantic = semantic)
  if(identity.is.universe) for(m in members(hset1)) for(i in seq_along(sets)) update.numeric.multiplicity(new.hset, sets[[i]], m, operation)
  else for(i in seq_along(sets)) for(m in members(sets[[i]])) update.numeric.multiplicity(new.hset, sets[[i]], m, operation)
  return(new.hset)
}
hset.operation.logical <- function(hset1, ..., operation = any, identity.is.universe = FALSE, semantic = "refer") {
  if(operation.is.trivial(...)) return(hset1)
  sets <- list(...)
  new.hset <- create.new.hset(hset1, generalized = FALSE, semantic = semantic)
  if(identity.is.universe) for(m in members(hset1)) for(i in seq_along(sets)) update.logical.multiplicity(new.hset, sets[[i]], m, operation)
  else for(i in seq_along(sets)) for(m in members(sets[[i]])) update.logical.multiplicity(new.hset, sets[[i]], m, operation)
  return(new.hset)
} 

operation.is.generalized <- function(hset1, ...) {
  if(!all.operands.are.hsets(hset1, ...)) stop("'hset1' must be of class 'hset', and all elements in '...' must be of class 'hset'")
  return(some.operand.is.generalized(hset1, ...))
}



update.numeric.multiplicity <- function(hset1, hset2, member, operation) set.numeric.multiplicity(hset1, member, operation(get.numeric.multiplicity(hset1, member), get.numeric.multiplicity(hset2, member)))
update.logical.multiplicity <- function(hset1, hset2, member, operation) set.logical.multiplicity(hset1, member, operation(get.logical.multiplicity(hset1, member), get.logical.multiplicity(hset2, member)))
operation.is.trivial <- function(...) return(...length() == 0L)
all.operands.are.hsets <- function(hset1, ...) return(is.hset(hset1) & all(sapply(list(...), is.hset)))
create.new.hset <- function(hset1, generalized, semantic) {
  if(semantic.is.valid(semantic)) {
    if(semantic == "refer") new.hset <- refer.to.hset(hset1, generalized = generalized)
    if(semantic == "value") new.hset <- copy.hset(hset1, generalized = generalized)
    return(new.hset)
  } else stop("'semantic' must be 'refer' or 'value'")
}
semantic.is.valid <- function(semantic) return(semantic %in% c("refer", "value"))



# basic operations

intersection <- function(hset1, ..., semantic = "refer") { # all, min
  if(operation.is.generalized(hset1, ...)) return(hset.operation.numeric(hset1, ..., operation = min,  identity.is.universe = TRUE,  semantic = semantic))
  else                                     return(hset.operation.logical(hset1, ..., operation = all,  identity.is.universe = TRUE,  semantic = semantic))
}

union <- function(hset1, ..., semantic = "refer") { # any, max
  if(operation.is.generalized(hset1, ...)) return(hset.operation.numeric(hset1, ..., operation = max,  identity.is.universe = FALSE, semantic = semantic))
  else                                     return(hset.operation.logical(hset1, ..., operation = any,  identity.is.universe = FALSE, semantic = semantic))
}

setsum <- function(hset1, ..., semantic = "refer") { # any, sum (sum between sets equivalent to union)
  if(operation.is.generalized(hset1, ...)) return(hset.operation.numeric(hset1, ..., operation = sum,  identity.is.universe = FALSE, semantic = semantic))
  else                                     return(hset.operation.logical(hset1, ..., operation = any,  identity.is.universe = FALSE, semantic = semantic))
}

difference <- function(hset1, ..., semantic = "refer") { # nimp, diff
  if(operation.is.generalized(hset1, ...)) return(hset.operation.numeric(hset1, ..., operation = pdif, identity.is.universe = FALSE, semantic = semantic))
  else                                     return(hset.operation.logical(hset1, ..., operation = nimp, identity.is.universe = FALSE, semantic = semantic))
}

symmdiff <- function(hset1, ..., semantic = "refer") { #
  if(operation.is.generalized(hset1, ...)) return(hset.operation.numeric(hset1, ..., operation = sdif,  identity.is.universe = FALSE, semantic = semantic))
  else                                     return(hset.operation.logical(hset1, ..., operation = niff,  identity.is.universe = FALSE, semantic = semantic))
}




nimp <- function(...) {
  if(...length() > 0L) {
    operands <- list(...)
    return(operands[[1]] & (!any(unlist(operands[-1]))))
  }
  else {
    warning("In nimp() : no arguments to nimp; returning FALSE")
    return(FALSE)
  }
}
pdif <- function(...) {
  if(...length() > 0L) {
    operands <- list(...)
    total.diff <- operands[[1]]
    if(length(operands) > 1L) for(i in 2:length(operands)) total.diff <- total.diff - operands[[i]]
    return(max(total.diff, 0L))
  } 
  else {
    warning("In pdif() : no arguments to pdif; returning 0")
    return(0L)
  }
}

niff <- function(...) {
  if(...length() > 0L) {
    operands <- list(...)
    return((sum(operands) %% 2L) == 0L)
  }
  else {
    warning("In nimp() : no arguments to nimp; returning FALSE")
    return(FALSE)
  }
}
sdif <- function(...) { 
  if(...length() > 0L) {
    operands <- list(...)
    total.diff <- operands[[1]]
    if(length(operands) > 1L) for(i in 2:length(operands)) total.diff <- total.diff - operands[[i]]
    return(abs(total.diff))
  } 
  else {
    warning("In pdif() : no arguments to pdif; returning 0")
    return(0L)
  }
}



