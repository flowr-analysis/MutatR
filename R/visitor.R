roles <- list(
  Arg = "Argument",
  FunName = "Function Name",
  Cond = "Condition",
  Ret = "Return",
  ExprList = "Expression List Element",
  PairList = "Pair List Element",
  Root = "Root"
)

visit <- function(ast, visitor, ...) {
  if ("pre" %in% names(visitor)) {
    visitor$pre(ast, visitor, ...)
  }
  if (rlang::is_missing(ast)) {
    return(rlang::missing_arg())
  } else if (is.expression(ast)) {
    return(visitor$exprlist(ast, visitor, ...))
  } else if (is.atomic(ast)) {
    return(visitor$atomic(ast, visitor, ...))
  } else if (is.name(ast)) {
    return(visitor$name(ast, visitor, ...))
  } else if (is.call(ast)) {
    name <- ast[[1]]
    if (name == "function") {
      args <- ast[[2]]
      body <- ast[[3]]
      return(visitor$call(name, list(args, body), visitor, ...))
    }
    return(visitor$call(name, as.list(ast[-1]), visitor, ...))
  } else if (is.pairlist(ast)) {
    return(visitor$pairlist(ast, visitor, ...))
  } else {
    stop("Don't know how to handle type ", typeof(ast), call. = FALSE)
  }
}
