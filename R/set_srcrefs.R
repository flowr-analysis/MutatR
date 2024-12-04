find_pd_id <- function(srcref) {
  pd <- getParseData(srcref)
  pd_expr <- (
    (pd$line1 == srcref[[1]] & pd$line2 == srcref[[3]]) |
      (pd$line1 == srcref[[7]] & pd$line2 == srcref[[8]])
  ) &
    pd$col1 == srcref[[2]] &
    pd$col2 == srcref[[4]] &
    pd$token == "expr"
  parent_idcs <- which(pd_expr)
  if (length(parent_idcs) == 0) {
    cat("No id found for", srcref, "\n")
    return(NULL)
  }
  if (length(parent_idcs) > 1) parent_idcs <- parent_idcs[[1]]

  ids <- pd$id[parent_idcs]
  return(ids)
}

find_children <- function(pd, parent_id) {
  pd_children <- pd[pd$parent == parent_id, ]
  pd_children <- pd_children[order(pd_children$line1, pd_children$col1), ]

  pd_children <- pd_children[pd_children$token != "COMMENT", ]
  return(pd_children)
}

make_srcref <- function(i, srcfile, pd) {
  srcref(srcfile, c(
    pd$line1[i],
    pd$col1[i],
    pd$line2[i],
    pd$col2[i],
    pd$col1[i],
    pd$col2[i],
    pd$line1[i],
    pd$line2[i]
  ))
}

set_srcref <- function(elem, ref) {
  if(is.null(elem)) {
    print("huh")
  }
  if (!is.name(elem)) {
    attr(elem, "srcref") <- ref
  }
  return(elem)
}

cal <- function(f, as, v, down) {
  srcfile <- down$srcfile
  child_pd <- find_children(down$pd, down$id)
  args_srcrefs <- switch(name_as_string(f),
    "if" = {
      cond <- make_srcref(3, srcfile, child_pd)
      then <- make_srcref(5, srcfile, child_pd)
      els <- make_srcref(7, srcfile, child_pd)
      list(cond, then, els)[seq_along(as)]
    },
    "while" = {
      cond <- make_srcref(3, srcfile, child_pd)
      body <- make_srcref(5, srcfile, child_pd)
      list(cond, body)
    },
    "for" = {
      cond <- make_srcref(2, srcfile, child_pd)
      body <- make_srcref(3, srcfile, child_pd)
      list(cond, body)
    },
    "function" = {
      body <- make_srcref(nrow(child_pd), srcfile, child_pd)
      list(NULL, body)
    },
    "+" = ,
    "-" = ,
    "*" = ,
    "/" = ,
    "+" = ,
    "+" = ,
    "^" = ,
    "%in%" = ,
    "<-" = ,
    "->" = {
      lhs <- make_srcref(1, srcfile, child_pd)
      rhs <- make_srcref(3, srcfile, child_pd)
      list(lhs, rhs)
    },
    "{" = lapply(seq(from = 2, length.out = length(as)), make_srcref, srcfile, child_pd),
    "$" = return(as.call(c(f, as))),
    {
      if(length(child_pd$text) < 2) {
        print("huh")
      }
      from <- if (child_pd$text[[2]] == "(") 3 else 2
      lapply(seq(from = from, by = 2, length.out = length(as)), make_srcref, srcfile, child_pd)
    }
  )
  as <- lapply(seq_along(as), function(i) {
    arg <- as[[i]]
    arg_srcref <- args_srcrefs[[i]]
    arg_pd <- getParseData(arg_srcref)
    arg_id <- find_pd_id(arg_srcref)

    arg <- set_srcref(arg, arg_srcref)
    a <- visit(arg, v, modifyList(down, list(id = arg_id, pd = arg_pd, elem = arg)))
    return(a)
  })
  return(as.call(c(f, as)))
}

add_srcrefs <- function(ast) {
  visitor <- list(
    exprlist = function(es, v, ...) {
      srcfile <- attr(es, "srcfile")
      srcrefs <- getSrcref(es)
      es <- lapply(seq_along(es), function(i) {
        expr <- es[[i]]
        expr_srcref <- srcrefs[[i]]
        expr_pd <- getParseData(expr_srcref)
        expr_id <- find_pd_id(expr_srcref)

        expr <- set_srcref(expr, expr_srcref)
        expr <- visit(expr, visitor, list(id = expr_id, pd = expr_pd, elem = expr, srcfile = srcfile))
        return(expr)
      })
      return(as.expression(es))
    },
    pairlist = function(l, v, ...) {
      # TODO: visit children
      # as.pairlist(lapply(l, visit, v))
      return(as.pairlist(l))
    },
    atomic = function(a, ...) a,
    name = function(n, ...) n,
    call = cal
  )
  visit(ast, visitor)
}