setClass("hset", slots = c(htable = "hash", info = "environment"))

hset <- function(members = NULL, multiplicities = NULL, generalized = FALSE) {
  if(!is.null(multiplicities)) generalized <- TRUE
  #set <- new("hset", htable = hash::hash(), info = new_environment(list(generalized = generalized)))
  set <- new("hset", htable = hash::hash(), info = new.env(FALSE, parent = emptyenv()))
  set@info[["generalized"]] <- generalized
  add.members.to.hset(set, members, multiplicities) #O(length(members))
  return(set)
}

is.hset <- function(x) is(x, "hset")
as.hset <- function(x) ifelse(is.hset(x), return(x), return(hset(members = x, generalized = FALSE)))
is.generalized <- function(hset) return(hset@info[["generalized"]])
are.generalized <- function(...) {
  if(...length() == 0L) stop("0 arguments passed to 'are.generalized' which requires at least 1")
  else sapply(list(...), is.generalized)
}
as.generalized <- function(hset, suppress.warning = FALSE) {
  if(!is.generalized(hset)) {
    mem <- members(hset)
    for(i in seq_along(mem)) hset@htable[[mem[i]]] <- 1L
    hset@info[["generalized"]] <- TRUE
  }
  else if(!suppress.warning) warning("'hset' is already generalized")
  return(hset)
}
as.not.generalized <- function(hset, suppress.warning = FALSE) {
  if(is.generalized(hset)) {
    mem <- members(hset)
    for(i in seq_along(mem)) hset@htable[[mem[i]]] <- ""
    hset@info[["generalized"]] <- FALSE
  }
  else if(!suppress.warning) warning("'hset' is already not generalized")
  return(hset)
}


setMethod("show","hset", function(object) {
  cat('Object of class "hset"\n')
  cat(paste0("number of elements: ", length(object@htable), "\n"))
  cat(paste0("generalized: ", object@info[["generalized"]], "\n"))
})

setMethod("print","hset", function(x, ...) {
  cat('Object of class "hset"\n')
  cat(paste0("number of elements: ", length(x@htable), "\n"))
  cat(paste0("generalized: ", x@info[["generalized"]], "\n"))
  cat('elements:\n')
  print(hash::keys(x@htable))
  if(x@info[["generalized"]]) {
    cat('multiplicities:\n')
    print(unname(hash::values(x@htable)))
  }
})




members <- function(hset) return(hash::keys(hset@htable))
multiplicities  <- function(hset) ifelse(is.generalized(hset), return(hash::values(hset@htable)), return(rep(1L, length(hset@htable))))
size.support <- function(hset) return(length(hset@htable))
cardinality <- function(hset) ifelse(is.generalized(hset), return(sum(multiplicities(hset))), return(size.support(hset)))




clone.of.hset <- function(current.hset, generalized = NA_integer_) {
  if(is.na(generalized)) new.hset <- hset(generalized = is.generalized(current.hset))
  else new.hset <- hset(generalized = generalized)
  mem <- members(current.hset)
  mul <- multiplicities(current.hset)
  for(i in seq_along(mem)) add.member(new.hset, mem[[i]], mul[[i]])
  return(new.hset)
}
refer.to.hset <- function(current.hset, generalized = NA_integer_) {
  if(is.na(generalized)) return(current.hset)
  else {
    if(generalized) current.hset <- as.generalized(current.hset, suppress.warning = TRUE)
    else current.hset <- as.not.generalized(current.hset, suppress.warning = TRUE)
    return(current.hset)
  }
}


add.member <- function(hset, member, multiplicity = NULL) { # not used in set operations
  if(is.generalized(hset)) {
    if(is.null(multiplicity)) multiplicity <- 1L
    else if(!is.number(multiplicity)) stop("error")
    set.numeric.multiplicity(hset, member, get.numeric.multiplicity(hset, member) + multiplicity)
  }
  else set.logical.multiplicity(hset, member, TRUE)
  return(invisible(NULL))
}
exclude.member <- function(hset, member) { # not used in set operations
  suppressWarnings(hset@htable[[member]] <- NULL, classes = "warning")
  return(invisible(NULL))
}

add.members.to.hset <- function(hset, members, multiplicities = NULL) { # not used in set operations
  if(!is.hset(hset)) stop("argument 'hset' must be of class 'hset'")
  if(!are.valid(members, multiplicities)) stop("error")
  vector.labels <- labels.members(members, labels.are.sorted.and.unique = FALSE)
  for(i in seq_along(vector.labels)) add.member(hset, vector.labels[i], multiplicities[i]) # NULL[i] is NULL, for all i
  return(invisible(NULL))
}


get.numeric.multiplicity <- function(hset, member) return(convert.to.numeric.multiplicity(hset@htable[[member]]))
get.logical.multiplicity <- function(hset, member) return(convert.to.logical.multiplicity(hset@htable[[member]]))
set.numeric.multiplicity <- function(hset, member, multiplicity) {
  if(!is.number(multiplicity)) stop("argument 'multiplicity' must be of type 'numeric' with length 1")
  suppressWarnings( hset@htable[[member]] <- convert.to.generalized.set.domain(multiplicity), classes = "warning")
  return(invisible(NULL))
}
set.logical.multiplicity <- function(hset, member, multiplicity) {
  if(!is.predicate(multiplicity)) stop("argument 'multiplicity' must be of type 'logical' with length 1")
  suppressWarnings( hset@htable[[member]] <- convert.to.set.domain(multiplicity), classes = "warning")
  return(invisible(NULL))
}


convert.to.numeric.multiplicity <- function(x) {
  if(is.null(x)) return(0L)
  if(x == "") return(1L)
  return(x) # x is already a (numeric) valid multiplicity
}
convert.to.logical.multiplicity <- function(x) {
  if(is.null(x)) return(FALSE)
  if(x == "") return(TRUE)
  return(x) # x is already a (numeric) valid multiplicity
}
convert.to.set.domain <- function(x) ifelse(x == TRUE, return(""), return(NULL))
convert.to.generalized.set.domain <- function(x) ifelse(is.positive(x), return(x), return(NULL))


is.positive <- function(x, tollerance = 0L) {
  if(!is.number(x)) return(FALSE)
  return(x > (0L + tollerance))
}
is.number <- function(x) {
  if(!is.numeric(x))  return(FALSE)
  if(length(x) != 1L) return(FALSE)
  if(any(x %in% c(Inf, -Inf, NaN, NA_integer_, NA_real_))) return(FALSE)
  return(TRUE)
}
is.predicate <- function(x) {
  if(!is.logical(x))  return(FALSE)
  if(length(x) != 1L) return(FALSE)
  if(is.na(x)) return(FALSE)
  return(TRUE)
}

is.valid <- function(member) {
  if(is.null(member)) return(TRUE)
  if(is.hset(member)) return(TRUE)
  if(is.vector(member)) {
    if(is.atomic(member)) return(TRUE)
    if(is.list(member)) {
      if(length(member) == 0L) return(TRUE)
      for(i in seq_along(member)) if(!is.valid(member[[i]])) return(FALSE)
      return(TRUE)
    }
  }
  return(FALSE)
}

are.valid <- function(member, multiplicity) {
  if(is.null(member)) return(is.null(multiplicity))
  if(is.hset(member)) return(is.number(multiplicity) | is.null(multiplicity))
  if(is.vector(member)) {
    if(is.atomic(member)) {
      if(is.numeric(multiplicity)) return(length(multiplicity) == length(member))
      return(is.null(multiplicity))
    }
    if(is.list(member)) {
      if(length(member) == 0L) return(is.number(multiplicity) | is.null(multiplicity))
      for(i in seq_along(member)) if(!is.valid(member[[i]])) return(FALSE) # member convertible to hset
      if(is.numeric(multiplicity)) return(length(multiplicity) == length(member))
      return(is.null(multiplicity))
    }
  }
  return(FALSE)
}


labels.members <- function(members, labels.are.sorted.and.unique) {
  if(is.hset(members)) vector.labels <- label.member(members)
  else {
    vector.labels <- vector("list", length(members))
    for(i in seq_along(members)) vector.labels[[i]] <- label.member(members[[i]])
  }
  if(labels.are.sorted.and.unique) return(sort(unique(unlist(vector.labels)))) #O(n^4/3) I think
  else return(unlist(vector.labels)) #O(n)
}

label.member <- function(member) {
  if(is.null(member))  return(character(0L))
  if(is.hset(member))  return(label.hset(member))
  if(is.vector(member)) {
    if(is.atomic(member)) return(label.atomic(member))
    if(is.list(member))   return(label.list(member))
    stop("if argument 'member' is a vector, its mode must be 'atomic', or 'list'")
  }
  stop("argument 'member' must be null, or a vector, or of class 'hset'")
}

label.hset <- function(hset) return(paste0("{",paste0(members(hset), collapse = ","),"}"))
label.atomic <- function(atomic.vector) {
  if(length(atomic.vector) == 0L) return(character(0L))
  if(is.character(atomic.vector)) atomic.vector <- atomic.vector[which(atomic.vector != "")]
  ordered.labels <- sort(as.character(unique(atomic.vector)))
  if(length(ordered.labels) == 1L) return(ordered.labels)
  else return(paste0("{",paste0(ordered.labels, collapse = ","),"}"))
}
label.list <- function(list.vector) {
  ordered.labels <- labels.members(list.vector, labels.are.sorted.and.unique = TRUE)
  return(paste0("{",paste0(ordered.labels, collapse = ","),"}"))
}








