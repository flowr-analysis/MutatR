roles <- list(
  Arg = "Argument",
  FunName = "Function Name",
  Cond = "Condition",
  Ret = "Return",
  ExprList = "Expression List Element",
  PairList = "Pair List Element",
  Root = "Root"
)

visit <- function(ast, visitor, down) {
  if ("pre" %in% names(visitor)) {
    visitor$pre(ast, visitor, down)
  }
  if (is.expression(ast)) {
    return(visitor$exprlist(ast, visitor, down))
  } else if (is.atomic(ast)) {
    return(visitor$atomic(ast, visitor, down))
  } else if (is.name(ast)) {
    return(visitor$name(ast, visitor, down))
  } else if (is.call(ast)) {
    name <- ast[[1]]
    if (name == "function") { # strip the srcref at the last place
      return(visitor$call(name, as.list(ast[2:(length(ast) - 1)]), visitor, down))
    }
    return(visitor$call(name, as.list(ast[-1]), visitor, down))
  } else if (is.pairlist(ast)) {
    return(visitor$pairlist(as.list(ast), visitor, down))
  } else {
    stop("Don't know how to handle type ", typeof(ast), call. = FALSE)
  }
}
