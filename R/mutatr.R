find_applicable_mutations <- function(ast) {
  muts <- list()
  for (key in names(mutations)) {
    muts[[key]] <- c()
  }
  visitor <- list(
    exprlist = function(es, r, v) lapply(es, visit, v, roles$ExprList),
    atomic = function(a, r, v) {
      for (m in all_applicable(a, r)) {
        new_mut <- rlang::hash(a)
        muts[[m]] <<- append(muts[[m]], list(new_mut))
      }
    },
    name = function(n, r, v) {
      for (m in all_applicable(n, r)) {
        new_mut <- rlang::hash(n)
        muts[[m]] <<- append(muts[[m]], list(new_mut))
      }
    },
    call = function(f, as, r, v) {
      call <- as.call(c(f, as))
      for (m in all_applicable(call, r)) {
        new_mut <- rlang::hash(call)
        muts[[m]] <<- append(muts[[m]], list(new_mut))
      }

      visit(f, v, roles$FunName)
      arg_role <- switch(name_as_string(f),
        "while" = roles$Cond,
        "if" = roles$Cond,
        "return" = roles$Ret,
        roles$Arg
      )
      lapply(as, visit, v, arg_role)
    },
    pairlist = function(l, r, v) lapply(l, visit, v, roles$PairList)
  )

  visit(ast, visitor, roles$Root)
  return(muts)
}

apply_list <- function(l, v) {
  new_l <- lapply(l, visit, v)
  new_l <- Filter(function(e) !is.null(e), new_l)
  return(new_l)
}

apply_mutation <- function(ast, mutation, srcref) {
  cat("Applying", mutation, "at", srcref, "\n")
  visitor <- list(
    exprlist = function(es, r, v) {
      new_list <- apply_list(es, v)
      return(as.expression(new_list))
    },
    atomic = function(a, r, v) {
      if (rlang::hash(a) == srcref) {
        return(mutations[[mutation]]$mutate(a))
      }
      return(a)
    },
    name = function(n, r, v) {
      if (rlang::hash(n) == srcref) {
        return(mutations[[mutation]]$mutate(n))
      }
      return(n)
    },
    call = function(f, as, r, v) {
      call <- as.call(c(f, as))
      if (rlang::hash(call) == srcref) {
        return(mutations[[mutation]]$mutate(call))
      }

      new_name <- visit(f, v, r)
      new_args <- apply_list(as, v)
      new_call <- as.call(c(new_name, new_args))
      return(new_call)
    },
    pairlist = function(l, r, v) {
      new_list <- apply_list(l, v)
      return(as.pairlist(new_list))
    }
  )

  return(visit(ast, visitor))
}

name_as_string <- function(name) {
  return(paste(name, collapse = ""))
}

build_probs <- function(applicable) {
  probs_by_category <- list(
    "arithmetic" = 0.5,
    "branch condition" = 0.5,
    "condition boundary" = 0.5,
    "function name" = 0.5,
    "increment" = 0.5,
    "logic" = 0.5,
    "negative condition" = 0.5,
    "swap boolean" = 0.5,
    "swap sign" = 0.5,
    "void call" = 0.5,
    "return value" = 0.5
  )
  probs <- lapply(applicable, function(mutation) probs_by_category[[mutation$mutation]])
  return(probs)
}

#' Generate n mutations for the given abstract syntax tree.
#'
#' @param asts The abstract syntax trees to generate mutations for. Must be a named
#' lists with the name being the file path and the value being the abstract syntax tree.
#' @param n The number of mutations to generate.
#'
#' @return A list of n mutated abstract syntax trees with the applied mutation
#'
#' @export
generate_mutations <- function(asts, n) {
  applicable <- list()
  for (file in names(asts)) {
    applicable_per_file <- find_applicable_mutations(asts[[file]])
    for (mutation in names(applicable_per_file)) {
      for (srcref in applicable_per_file[[mutation]]) {
        applicable <- append(applicable, list(list(mutation = mutation, srcref = srcref, file = file)))
      }
    }
  }

  if (length(applicable) < n) {
    cat("Only", length(applicable), "mutations found. Requested", n, "mutations.\n")
    n <- length(applicable)
  }

  mutants <- list()
  for (mutation in sample(applicable, n, prob = build_probs(applicable))) {
    mutant <- apply_mutation(asts[[mutation$file]], mutation$mutation, mutation$srcref)
    mutants <- append(mutants, list(c(mutation, list(mutant = mutant))))
  }
  return(mutants)
}

test <- function() {
  files <- c(
    "/home/luke/src/master-thesis/package/R/flowr_utils.R", # nolint
    "/home/luke/src/master-thesis/package/R/utils.R" # nolint
  )
  asts <- lapply(files, parse, keep.source = TRUE) |> setNames(files)
  generate_mutations(asts, 1000)
}
