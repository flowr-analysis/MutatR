Roles <- list(
  Arg = "Argument",
  FunName = "Function Name",
  Cond = "Condition",
  Ret = "Return",
  ExprList = "Expression List Element",
  PairList = "Pair List Element",
  Root = "Root"
)

visit <- function(ast, visitor, role) {
  if (is.expression(ast)) {
    return(visitor$exprlist(ast, role, visitor))
  } else if (is.atomic(ast)) {
    return(visitor$atomic(ast, role, visitor))
  } else if (is.name(ast)) {
    return(visitor$name(ast, role, visitor))
  } else if (is.call(ast)) {
    name <- ast[[1]]
    if (name == "function") { # strip the srcref at the last place
      return(visitor$call(name, as.list(ast[2:(length(ast) - 1)]), role, visitor))
    }
    return(visitor$call(name, as.list(ast[-1]), role, visitor))
  } else if (is.pairlist(ast)) {
    return(visitor$pairlist(as.list(ast), role, visitor))
  } else {
    stop("Don't know how to handle type ", typeof(ast), call. = FALSE)
  }
}
