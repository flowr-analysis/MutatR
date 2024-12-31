apply_on_list <- function(l, v, p) {
  finished <- FALSE
  new_l <- lapply(l, function(e) {
    if (finished) {
      return(e)
    }

    new_e <- visit(e, v, p)
    if (rlang::is_missing(new_e)) {
      return(rlang::missing_arg())
    }
    finished <<- new_e$finished
    return(new_e$ast)
  })

  new_l <- Filter(function(e) !(is.character(e) && e == remove_me), new_l)
  return(list(finished = finished, ast = new_l))
}

compare_identifier <- function(id, mut) {
  is_id <- id
  if (is_id == unsure_id) {
    return(FALSE)
  }
  target_id <- mut$node_id
  return(is_id == target_id)
}

apply_mutation <- function(ast, mutation) {
  visitor <- list(
    exprlist = function(es, v, p, ...) {
      id <- get_id(es, p)
      if (compare_identifier(id, mutation)) {
        return(list(finished = TRUE, ast = mutation$fun()))
      }
      new_list <- apply_on_list(es, v, id)
      return(list(finished = new_list$finished, ast = as.expression(new_list$ast)))
    },
    pairlist = function(l, ...) {
      return(list(finished = FALSE, ast = l))
    },
    atomic = function(a, v, p, ...) {
      id <- get_id(a, p)
      if (compare_identifier(id, mutation)) {
        return(list(finished = TRUE, ast = mutation$fun()))
      }
      return(list(finished = FALSE, ast = a))
    },
    name = function(n, v, p, ...) {
      id <- get_id(n, p)
      if (compare_identifier(id, mutation)) {
        return(list(finished = TRUE, ast = mutation$fun()))
      }
      return(list(finished = FALSE, ast = n))
    },
    call = function(cl, v, p, ...) {
      id <- get_id(cl, p)
      parts <- split_up_call(cl)
      f <- parts$name
      as <- parts$args

      if (compare_identifier(id, mutation)) {
        return(list(finished = TRUE, ast = mutation$fun()))
      }

      new_name <- visit(f, v, id)
      if (new_name$finished) {
        return(list(finished = TRUE, ast = as.call(c(new_name$ast, as))))
      }

      new_args <- apply_on_list(as, v, id)
      if (f == "function") {
        args <- if (length(a <- new_args$ast) == 2) a[[1]] else list()
        body <- new_args$ast[[length(new_args$ast)]]
        new_call <- rlang::new_function(args, body)
      } else {
        new_call <- as.call(c(new_name$ast, new_args$ast)) |> copy_attribs(cl)
      }
      return(list(finished = new_args$finished, ast = new_call))
    }
  )

  result <- visit(ast, visitor, -1)
  if (!result$finished) {
    warning("Mutation not found")
  }
  return(result$ast)
}
