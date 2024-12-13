find_applicable_mutations <- function(ast) {
  muts <- list()
  visitor <- list(
    exprlist = function(es, v, r, p) lapply(es, function(e) visit(e, v, roles$ExprList, get_srcref(e, p))),
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

      visit(f, v, roles$FunName, srcref)
      arg_role <- switch(name_as_string(f),
        "while" = roles$Cond,
        "if" = roles$Cond,
        "return" = roles$Ret,
        "{" = roles$ExprList,
        roles$Arg
      )
      lapply(as, visit, v, arg_role, srcref)
    }
  )

  visit(ast, visitor, roles$Root, NULL)
  return(muts)
}
