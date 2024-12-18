make_srcref <- function(pd, srcfile, parent_srcref) {
  if (is.null(pd)) {
    ref <- parent_srcref
    attr(ref, "parent") <- TRUE
  } else {
    ref <- srcref(srcfile, c(
      pd$line1,
      pd$col1,
      pd$line2,
      pd$col2,
      pd$col1,
      pd$col2,
      pd$line1,
      pd$line2
    ))
    attr(ref, "parent") <- FALSE
  }
  return(ref)
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

make_pd_hirarchy <- function(pd, root_ids = pd[pd$parent == 0, ]$id) {
  hirarchy <- list()
  for (id in root_ids) {
    root_elem <- pd[pd$id == id, ]
    if (root_elem$token == "COMMENT") next
    children <- pd[pd$parent == id, ]
    children <- seq_len(nrow(children)) |> lapply(function(i) children[i, ])
    children_hirarchy <- list()
    for (child in children) {
      child_hirarchy <- Filter(function(child) length(child) != 0, make_pd_hirarchy(pd, child$id))
      if (length(child_hirarchy) == 0) next
      children_hirarchy <- c(children_hirarchy, child_hirarchy)
    }
    hirarchy <- append(hirarchy, list(list(elem = root_elem, children = children_hirarchy)))
  }
  return(hirarchy)
}

parse_srcrefs_from_args <- function(arg_pds, target_length, srcfile, parent) {
  if (arg_pds[[2]]$elem$text != "(" || arg_pds[[length(arg_pds)]]$elem$text != ")") {
    return(rep(list(NULL), target_length))
  }
  if (length(arg_pds) <= 3) { # no arguments
    return(list())
  }
  inner <- arg_pds[3:(length(arg_pds) - 1)]
  groups <- {
    groups <- list()
    group <- list()
    for (x in inner) {
      if (x$elem$text == ",") {
        groups <- append(groups, list(group))
        group <- list()
      } else {
        group <- append(group, list(x))
      }
    }
    append(groups, list(group))
  }
  arg_pds <- lapply(groups, function(group) {
    if (length(group) == 1) return(group[[1]]) # single argument
    if (length(group) == 3) return(group[[3]]) # named argument
    return(NULL)
  })
  if (length(arg_pds) != target_length) {
    return(rep(list(NULL), target_length))
  }
  return(arg_pds)
}

ops <- c(
  "~", "+", "-", "*", "/", "^", "**", "%%", "%/%", "%*%", "%o%", "%x%", "==", "!=", ">", ">=", "<", "<=", "&",
  "&&", "|", "||", "!", "%in%", "<-", ":=", "<<-", "->", "->>", "=", ":", "?", "::", ":::", "%||%", "$"
)

cal <- function(cl, v, pd, srcfile, parent_srcref) {
  parts <- split_up_call(cl)
  f <- parts$name
  as <- parts$args

  arg_pds <- switch(name_as_string(f),
    "if" = {
      cond <- pd$children[[3]]
      then <- pd$children[[5]]
      els <- if (length(pd$children) >= 7) pd$children[[7]] else NULL
      list(cond, then, els)[seq_along(as)]
    },
    "while" = {
      cond <- pd$children[[3]]
      body <- pd$children[[5]]
      list(cond, body)
    },
    "for" = {
      cond <- pd$children[[2]]
      var <- cond$children[[2]]
      sequ <- cond$children[[4]]
      body <- pd$children[[3]]
      list(var, sequ, body)
    },
    "function" = {
      body <- pd$children[[length(pd$children)]]
      list(NULL, body)
    },
    "{" = {
      children <- if (is.null(names(pd))) pd$children else pd$children
      lapply(seq(from = 2, length.out = length(as)), function(i) children[[i]])
    },
    "(" = {
      list(pd$children[[2]])
    },
    "[[" = ,
    "[" = {
      pd_i <- 1
      pd_args <- list()
      for (i in seq_along(as)) {
        if (rlang::is_missing(as[[i]])) {
          pd_args <- append(pd_args, list(NULL))
          pd_i <- pd_i + 1
        } else {
          pd_args <- c(pd_args, list(pd$children[[pd_i]]))
          pd_i <- pd_i + 2
        }
      }
      pd_args
    },
    {
      if (name_as_string(f) %in% ops && length(as) == 2) {
        list(pd$children[[1]], pd$children[[3]])
      } else if (name_as_string(f) %in% ops && length(as) == 1) {
        list(pd$children[[2]])
      } else {
        parse_srcrefs_from_args(pd$children, length(as))
      }
    }
  )
  cl_srcref <- make_srcref(pd$elem, srcfile, parent_srcref)
  as <- lapply(seq_along(as), function(i) {
    arg <- as[[i]]
    arg_pd <- arg_pds[[i]]
    visit(arg, v, arg_pd, srcfile, cl_srcref)
  }) |> stats::setNames(names(as))
  f <- visit(f, v, pd$children[[1]], srcfile, cl_srcref)
  return(as.call(c(f, as)) |> copy_attribs(cl) |> set_srcref(cl_srcref))
}

has_semicolon <- function(pd) {
  if (pd$elem$text == ";") {
    return(TRUE)
  }
  lapply(pd$children, function(child) {
    child <- child
    has_semicolon(child)
  }) |>
    unlist() |>
    any()
}

merge_srcrefs <- function(srcfile, srcrefs) {
  if (length(srcrefs) == 0) return(NULL)
  first <- srcrefs[[1]]
  last <- srcrefs[[length(srcrefs)]]
  ref <- c(first[[1]], first[[2]], last[[3]], last[[4]], first[[5]], last[[6]], first[[7]], last[[8]])
  res <- srcref(srcfile, ref)
  attr(res, "parent") <- FALSE
  return(res)
}

add_srcrefs <- function(ast) {
  unconsidered_exprs <- 0
  considered_exprs <- 0
  pd <- utils::getParseData(ast) |> make_pd_hirarchy()
  visitor <- list(
    exprlist = function(es, v, pd, srcfile, parent_srcref) {
      srcfile <- attr(es, "srcfile")
      child_srcrefs <- utils::getSrcref(es)
      srcref <- merge_srcrefs(srcfile, child_srcrefs)
      lapply(seq_along(es), function(i) {
        e <- es[[i]]
        pd <- pd[[i]]
        if (has_semicolon(pd)) { # When a semicolon is present, we have a hard time interpreting the parse data
          print("Ignored expression because of a semicolon")
          unconsidered_exprs <<- unconsidered_exprs + 1
          return(e)
        } else {
          considered_exprs <<- considered_exprs + 1
        }
        srcref <- child_srcrefs[[i]]
        visit(e, v, pd, srcfile, srcref) # not really the parent srcref but whatever
      }) |>
        as.expression() |>
        copy_attribs(es) |>
        set_srcref(srcref)
    },
    pairlist = function(l, v, pd, srcfile, parent_srcref) set_srcref_alt(l, pd$elem, srcfile, parent_srcref),
    atomic = function(a, v, pd, srcfile, parent_srcref) set_srcref_alt(a, pd$elem, srcfile, parent_srcref),
    name = function(n, v, pd, srcfile, parent_srcref) set_srcref_alt(n, pd$elem, srcfile, parent_srcref),
    call = cal
  )
  ast <- visit(ast, visitor, pd, NULL, NULL)
  return(list(ast = ast, unconsidered_exprs = unconsidered_exprs, considered_exprs = considered_exprs))
}
