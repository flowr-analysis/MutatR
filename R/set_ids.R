make_new_id_fun <- function(prev = 0) {
  return(function() {
    id <- prev + 1
    prev <<- id
    return(id)
  })
}

make_getter_setter_funs <- function(l = list()) {
  set_id <- function(elem, id = NULL, parent = NULL) {
    if (is.null(elem)) {
      return(elem)
    }

    id <- id %||% new_id()
    if (is.symbol(elem)) {
      l <<- append(l, list(list(elem = as.character(elem), parent = parent, id = id)))
    } else {
      attributes(elem)$node_id <- id
    }
    return(elem)
  }

  get_id <- function(elem, parent) {
    if (is.symbol(elem)) {
      found <- list()
      for (e in l) {
        if (e$parent == parent && e$elem == as.character(elem)) {
          found <- c(found, e$id)
        }
      }
      if (length(found) == 1) {
        return(found[[1]])
      } else {
        return(unsure_id)
      }
    }
    return(attr(elem, "node_id"))
  }

  return(list(getter = get_id, setter = set_id))
}

unsure_id <- "hmm i dont know"

new_id <- make_new_id_fun()
getter_setter <- make_getter_setter_funs()
get_id <- getter_setter$getter
set_id <- getter_setter$setter

set_ids <- function(ast) {
  visitor <- list(
    exprlist = function(es, v, p) {
      id <- new_id()
      return(lapply(es, visit, v, id) |> as.expression() |> copy_attribs(es) |> set_id(id = id, parent = p))
    },
    pairlist = function(l, v, p) set_id(l, parent = p),
    atomic = function(a, v, p) set_id(a, parent = p),
    name = function(n, v, p) set_id(n, parent = p),
    call = function(cl, v, p) {
      id <- new_id()
      parts <- split_up_call(cl)
      f <- visit(parts$name, v, id)
      as <- lapply(parts$args, visit, v, id)
      return(as.call(c(f, as)) |> set_id(id = id, parent = p) |> copy_attribs(cl))
    }
  )
  return(visit(ast, visitor, -1))
}
