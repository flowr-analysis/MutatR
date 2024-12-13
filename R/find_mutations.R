find_applicable_mutations <- function(ast) {
  muts <- list()
  visitor <- list(
    exprlist = function(es, v, r, p) {
      for (m in all_applicable(es, r)) {
        muts <<- append(muts, list(m |> append(list(srcref = srcref, node_id = get_id(es)))))
      }
      lapply(es, function(e) visit(e, v, roles$ExprListElem, get_srcref(e, p)))
    },
    pairlist = function(ls, v, r, p) NULL,
    atomic = function(a, v, r, p) {
      for (m in all_applicable(a, r)) {
        muts <<- append(muts, list(m |> append(list(srcref = get_srcref(a, p), node_id = get_id(a)))))
      }
    },
    name = function(n, v, r, p) {
      for (m in all_applicable(n, r)) {
        muts <<- append(muts, list(m |> append(list(srcref = get_srcref(n, p), node_id = get_id(n)))))
      }
    },
    call = function(cl, v, r, p) {
      srcref <- get_srcref(cl, p)
      id <- get_id(cl)

      for (m in all_applicable(cl, r)) {
        muts <<- append(muts, list(m |> append(list(srcref = srcref, node_id = id))))
      }

      parts <- split_up_call(cl)
      f <- parts$name
      as <- parts$args

      fn <- name_as_string(f)

      visit(f, v, roles$FunName, srcref)

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
        visit(a, v, role, srcref)
      })
    }
  )

  visit(ast, visitor, roles$Root, NULL)
  return(muts)
}
