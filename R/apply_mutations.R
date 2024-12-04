apply_on_list <- function(l, v) {
  finished <- FALSE
  new_l <- lapply(l, function(e) {
    if (finished) {
      return(e)
    }

    new_e <- visit(e, v)
    finished <<- new_e$finished
    return(new_e$ast)
  })
  new_l <- Filter(function(e) !is.null(e), new_l)
  return(list(finished = finished, ast = new_l))
}

apply_mutation <- function(ast, mutation, srcref) {
  cat("Applying", mutation, "at", srcref, "\n")
  visitor <- list(
    exprlist = function(es, v, ...) {
      new_list <- apply_on_list(es, v)
      return(list(finished = new_list$finished, ast = as.expression(new_list$ast)))
    },
    pairlist = function(l, v, ...) {
      new_list <- apply_on_list(l, v)
      return(list(finished = new_list$finished, ast = as.pairlist(new_list$ast)))
    },
    atomic = function(a, ...) {
      if (rlang::hash(a) == srcref) {
        return(list(finished = TRUE, ast = mutations[[mutation]]$mutate(a)))
      }
      return(list(finished = FALSE, ast = a))
    },
    name = function(n, ...) {
      if (rlang::hash(n) == srcref) {
        return(list(finished = TRUE, ast = mutations[[mutation]]$mutate(n)))
      }
      return(list(finished = FALSE, ast = n))
    },
    call = function(f, as, v, ...) {
      call <- as.call(c(f, as))
      if (rlang::hash(call) == srcref) {
        return(list(finished = TRUE, ast = mutations[[mutation]]$mutate(call)))
      }

      new_name <- visit(f, v)
      if (new_name$finished) {
        return(list(finished = TRUE, ast = as.call(c(new_name$ast, as))))
      }

      new_args <- apply_on_list(as, v)
      new_call <- as.call(c(new_name$ast, new_args$ast))
      return(list(finished = new_args$finished, ast = new_call))
    }
  )

  result <- visit(ast, visitor)
  if (!result$finished) {
    warning("Mutation not found")
  }
  return(result$ast)
}