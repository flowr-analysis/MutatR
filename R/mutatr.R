find_applicable_mutations <- function(ast) {
  muts <- list()
  for (key in names(mutations)) {
    muts[[key]] <- c()
  }
  visitor <- list(
    exprlist = function(es, r, v) {
      lapply(es, function(e) visit(e, v, Roles$ExprList))
    },
    atomic = function(a, r, v) {
      for (m in all_applicable(a, r)) muts[[m]] <<- c(muts[[m]], rlang::hash(a))
    },
    name = function(n, r, v) {
      for (m in all_applicable(n, r)) muts[[m]] <<- c(muts[[m]], rlang::hash(n))
    },
    call = function(f, as, r, v) {
      call <- as.call(c(f, as))
      for (m in all_applicable(call, r)) muts[[m]] <<- c(muts[[m]], rlang::hash(call))
      visit(f, v, Roles$FunName)
      arg_role <- switch(name_as_string(f),
        "while" = Roles$Cond,
        "if" = Roles$Cond,
        "return" = Roles$Ret,
        Roles$Arg
      )
      lapply(as, function(a) visit(a, v, arg_role))
    },
    pairlist = function(l, r, v) {
      lapply(l, function(l) visit(l, v, Roles$PairList))
    }
  )

  visit(ast, visitor, Roles$Root)
  return(muts)
}

apply_list <- function(l, v) {
  new_l <- lapply(l, function(e) visit(e, v))
  new_l <- Filter(function(e) !is.null(e), new_l)
  return(new_l)
}

apply_mutation <- function(ast, kind, srcref) {
  visitor <- list(
    exprlist = function(es, r, v) {
      new_list <- apply_list(es, v)
      return(as.expression(new_list))
    },
    atomic = function(a, r, v) {
      if (rlang::hash(a) == srcref) {
        return(mutations[[kind]]$mutate(a))
      }
      return(a)
    },
    name = function(n, r, v) {
      if (rlang::hash(n) == srcref) {
        return(mutations[[kind]]$mutate(n))
      }
      return(n)
    },
    call = function(f, as, r, v) {
      call <- as.call(c(f, as))
      if (rlang::hash(call) == srcref) {
        return(mutations[[kind]]$mutate(call))
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

merge_lists_by_names <- function(ls) {
  names <- unique(unlist(lapply(ls, names)))
  merged <- list()
  for (name in names) {
    merged[[name]] <- unlist(lapply(ls, function(l) l[[name]]))
  }
  return(merged)
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
    "void call" = 0.5
  )
  probs <- lapply(applicable, function(mutation) probs_by_category[[mutation$cat]])
  return(probs)
}

generate_mutations <- function(ast, n) {
  applicable <- list()
  applicable_by_category <- find_applicable_mutations(ast)
  for (category in names(applicable_by_category)) {
    for (mut in applicable_by_category[[category]]) {
      applicable <- append(applicable, list(list(cat = category, mut = mut)))
    }
  }

  if (length(applicable) < n) {
    cat("Only", length(applicable), "mutations found. Requested", n, "mutations.\n")
    n <- length(applicable)
  }

  asts <- list()
  selected <- sample(applicable, n, prob = build_probs(applicable))
  for (mutation in selected) {
    category <- mutation$cat
    mut <- mutation$mut
    cat("Applying", mut, "mutation in", category, "category.\n")
    ast <- apply_mutation(ast, category, mut)
    asts <- append(asts, list(list(ast = ast, category = category, mut = mut)))
  }
  return(asts)
}

test <- function() {
  ast <- parse("/home/luke/src/cran-packages-coverage/mutatR/inst/example.R", keep.source = TRUE)
  new_ast <- generate_mutations(ast, 1000)
}
