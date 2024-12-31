find_applicable_mutations <- function(ast, srcref_filter) { # nolint: cyclocomp_linter.
  muts <- list()
  visitor <- list(
    exprlist = function(es, v, r, ps, pid) {
      srcref <- get_srcref(es, ps)
      id <- get_id(es, pid)
      if (id != unsure_id && srcref_filter(srcref, id)) {
        for (m in all_applicable(es, r)) {
          muts <<- append(muts, list(m |> append(list(srcref = srcref, node_id = id))))
        }
      }
      lapply(es, visit, v, roles$ExprListElem, srcref, id)
    },
    pairlist = function(ls, v, r, ps, pid) NULL,
    atomic = function(a, v, r, ps, pid) {
      srcref <- get_srcref(a, ps)
      id <- get_id(a, pid)
      if (id == unsure_id || !srcref_filter(srcref, id)) {
        return()
      }
      for (m in all_applicable(a, r)) {
        muts <<- append(muts, list(m |> append(list(srcref = srcref, node_id = id))))
      }
    },
    name = function(n, v, r, ps, pid) {
      srcref <- get_srcref(n, ps)
      id <- get_id(n, pid)
      if (id == unsure_id || !srcref_filter(srcref, id)) {
        return()
      }
      for (m in all_applicable(n, r)) {
        muts <<- append(muts, list(m |> append(list(srcref = srcref, node_id = id))))
      }
    },
    call = function(cl, v, r, ps, pid) {
      srcref <- get_srcref(cl, ps)
      id <- get_id(cl, pid)

      if (id != unsure_id && srcref_filter(srcref, id)) {
        for (m in all_applicable(cl, r)) {
          muts <<- append(muts, list(m |> append(list(srcref = srcref, node_id = id))))
        }
      }

      parts <- split_up_call(cl)
      f <- parts$name
      as <- parts$args

      fn <- name_as_string(f)

      visit(f, v, roles$FunName, srcref, id)

      default_role <- {
        role <- roles$Arg
        attr(role, "fname") <- fn
        role
      }
      lapply(seq_along(as), function(i) {
        a <- as[[i]]
        role <- switch(fn,
          "while" = if (i == 1) roles$Cond else roles$ExprListElem,
          "if" = if (i == 1) roles$Cond else roles$ExprListElem,
          "return" = roles$Ret,
          "{" = roles$ExprListElem,
          default_role
        )
        visit(a, v, role, srcref, id)
      })
    }
  )

  visit(ast, visitor, roles$Root, NULL, -1)
  return(muts)
}
