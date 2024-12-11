make_new_id_fun <- function(prev = 0) {
  return(function() {
    id <- prev + 1
    prev <<- id
    return(id)
  })
}
new_id <- make_new_id_fun()
set_id <- function(elem) {
  if (is.null(elem)) {
    return(elem)
  }

  if (!is.symbol(elem)) {
    id <- new_id()
    attributes(elem)$node_id <- id
  }
  return(elem)
}

get_id <- function(elem) {
  if (is.symbol(elem)) {
    return(as.character(elem))
  }
  return(attr(elem, "node_id"))
}

copy_attribs <- function(dest, src, filter = c("node_id")) {
  filter <- c(filter, names(attributes(dest))) |> unique()

  src_attrs <- attributes(src)
  src_attrs <- src_attrs[!names(src_attrs) %in% filter]

  dest_attrs <- attributes(dest)
  attributes(dest) <- c(src_attrs, dest_attrs)

  return(dest)
}

add_ids <- function(ast) {
  visitor <- list(
    exprlist = function(es, v) {
      es <- set_id(es)
      return(lapply(es, visit, v) |> as.expression() |> copy_attribs(es))
    },
    pairlist = function(l, v) set_id(l),
    atomic = function(a, v) set_id(a),
    name = function(n, v) set_id(n),
    call = function(cl, v) {
      parts <- split_up_call(cl)
      f <- visit(parts$name, v)
      as <- lapply(parts$args, visit, v)
      return(as.call(c(f, as)) |> set_id() |> copy_attribs(cl))
    }
  )
  return(visit(ast, visitor))
}
