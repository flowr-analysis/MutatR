make_srcref <- function(pd, srcfile, parent_srcref) {
  if (is.null(pd)) {
    return(parent_srcref)
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

set_srcref <- function(elem, srcref) {
  if (!is.null(elem) && !is.name(elem) && !is.null(srcref)) {
    attr(elem, "srcref") <- srcref
  }
  return(elem)
}

set_srcref_alt <- function(elem, pd, srcfile, parent_srcref) {
  srcref <- make_srcref(pd, srcfile, parent_srcref)
  return(set_srcref(elem, srcref))
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

cal <- function(cl, v, pd, srcfile, parent_srcref) {
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
    "(" = {
      list(pd$children[[2]][[1]])
    },
    "$" = ,
    "[[" = {
      lhs <- pd$children[[1]][[1]]
      rhs <- pd$children[[3]][[1]]
      list(lhs, rhs)
    },
    {
      if (name_as_string(f) %in% ops && length(as) == 2) {
        list(pd$children[[1]][[1]], pd$children[[3]][[1]])
      } else if (name_as_string(f) %in% ops && length(as) == 1) {
        list(pd$children[[2]][[1]])
      } else {
        is_normal_call <- {
          expected_len <- if (length(as) == 0) 3 else length(as) + length(as) - 1 + 3
          expected_len == length(pd$children)
        }
        if (is_normal_call) {
          lapply(seq(from = 3, by = 2, length.out = length(as)), function(i) {
            pd$children[[i]][[1]]
          })
        } else {
          rep(list(NULL), length(as))
        }
      }
    }
  )
  cl_srcref <- make_srcref(pd$elem, srcfile, parent_srcref)
  as <- lapply(seq_along(as), function(i) {
    arg <- as[[i]]
    arg_pd <- arg_pds[[i]]
    visit(arg, v, arg_pd, srcfile, cl_srcref)
  }) |> setNames(names(as))
  f <- (visit(f, v, pd$children[[1]][[1]], srcfile, cl_srcref))
  return(as.call(c(f, as)) |> copy_attribs(cl) |> set_srcref(cl_srcref))
}

add_srcrefs <- function(ast) {
  pd <- getParseData(ast) |> make_pd_hirarchy()
  visitor <- list(
    exprlist = function(es, v, pd, srcfile, parent_srcref) {
      srcfile <- attr(es, "srcfile")
      srcrefs <- getSrcref(es)
      lapply(seq_along(es), function(i) {
        e <- es[[i]]
        pd <- pd[[i]]
        srcref <- srcrefs[[i]]
        visit(e, v, pd, srcfile, srcref) # not really the parent srcref but whatever
      }) |>
        as.expression() |>
        copy_attribs(es)
    },
    pairlist = function(l, v, pd, srcfile, parent_srcref) set_srcref_alt(l, pd$elem, srcfile, parent_srcref),
    atomic = function(a, v, pd, srcfile, parent_srcref) set_srcref_alt(a, pd$elem, srcfile, parent_srcref),
    name = function(n, v, pd, srcfile, parent_srcref) set_srcref_alt(n, pd$elem, srcfile, parent_srcref),
    call = cal
  )
  visit(ast, visitor, pd, NULL, NULL)
}
