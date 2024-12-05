make_srcref <- function(pd, srcfile) {
  if (is.null(pd)) {
    return(NULL)
  }
  srcref(srcfile, c(
    pd$line1,
    pd$col1,
    pd$line2,
    pd$col2,
    pd$col1,
    pd$col2,
    pd$line1,
    pd$line2
  ))
}

set_srcref <- function(elem, pd, srcfile) {
  ref <- make_srcref(pd, srcfile)
  if (!is.null(elem) && !is.name(elem) && !is.null(ref)) {
    attr(elem, "srcref") <- ref
  }
  return(elem)
}

make_pd_hirarchy <- function(pd, root = pd[pd$parent == 0, ]$id) {
  comment_marker <- "COMMENT"
  hirarchy <- lapply(root, function(id) {
    elem <- pd[pd$id == id, ]
    if (elem$token == "COMMENT") {
      return(comment_marker)
    }
    children <- pd[pd$parent == id, ]
    children_h <- Filter(function(elem) {
      return(length(elem) != 0)
    }, lapply(seq_len(nrow(children)), function(i) {
      child <- children[i, ]
      make_pd_hirarchy(pd, child$id)
    }))
    list(elem = elem, children = children_h)
  })
  return(Filter(function(elem) elem != comment_marker, hirarchy))
}

ops <- c(
  "~", "+", "-", "*", "/", "^", "**", "%%", "%/%", "%*%", "%o%", "%x%", "==", "!=", ">", ">=", "<", "<=", "&",
  "&&", "|", "||", "!", "%in%", "<-", ":=", "<<-", "->", "->>", "=", ":", "?"
)

cal <- function(cl, v, pd, srcfile) {
  parts <- split_up_call(cl)
  f <- parts$name
  as <- parts$args

  arg_pds <- switch(name_as_string(f),
    "if" = {
      cond <- pd$children[[3]][[1]]
      then <- pd$children[[5]][[1]]
      els <- if (length(pd$children) >= 7) pd$children[[7]][[1]] else NULL
      list(cond, then, els)[seq_along(as)]
    },
    "while" = {
      cond <- pd$children[[3]][[1]]
      body <- pd$children[[5]][[1]]
      list(cond, body)
    },
    "for" = {
      cond <- pd$children[[2]][[1]]
      var <- cond$children[[2]][[1]]
      sequ <- cond$children[[4]][[1]]
      body <- pd$children[[3]][[1]]
      list(var, sequ, body)
    },
    "function" = {
      body <- pd$children[[length(pd$children)]]
      list(NULL, body)
    },
    "{" = {
      children <- if (is.null(names(pd))) pd[[1]]$children else pd$children
      lapply(seq(from = 2, length.out = length(as)), function(i) children[[i]][[1]])
    },
    {
      if (name_as_string(f) %in% ops && length(as) == 2) {
        list(pd$children[[1]][[1]], pd$children[[3]][[1]])
      } else if (name_as_string(f) %in% ops && length(as) == 1) {
        list(pd$children[[2]][[1]])
      } else {
        cat(sprintf("unknown function %s\n", name_as_string(f)))
        rep(list(NULL), length(as))
      }
    }
  )
  as <- lapply(seq_along(as), function(i) {
    arg <- as[[i]]
    arg_pd <- arg_pds[[i]]
    visit(arg, v, arg_pd, srcfile)
  })
  return(as.call(c(f, as)) |> set_srcref(pd$elem, srcfile))
}

add_srcrefs <- function(ast) {
  pd <- getParseData(ast) |> make_pd_hirarchy()
  visitor <- list(
    exprlist = function(es, v, pd, ...) {
      srcfile <- attr(es, "srcfile")
      lapply(seq_along(es), function(i) {
        e <- es[[i]]
        pd <- pd[[i]]
        visit(e, v, pd, srcfile)
      }) |> as.expression()
    },
    pairlist = function(l, v, pd, srcfile) set_srcref(l, pd$elem, srcfile),
    atomic = function(a, v, pd, srcfile) set_srcref(a, pd$elem, srcfile),
    name = function(n, v, pd, srcfile) set_srcref(n, pd$elem, srcfile),
    call = cal
  )
  visit(ast, visitor, pd, NULL)
}
