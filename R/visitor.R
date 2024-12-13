roles <- list(
  Arg = "Argument",
  FunName = "Function Name",
  Cond = "Condition",
  Ret = "Return",
  ExprListElem = "Expression List Element",
  Root = "Root"
)

split_up_call <- function(ast) {
  name <- ast[[1]]
  if (name == "function") {
    args <- ast[[2]]
    body <- ast[[3]]
    return(list(name = name, args = list(args, body)))
  }
  return(list(name = name, args = as.list(ast[-1])))
}

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
    return(visitor$call(ast, visitor, ...))
  } else if (is.pairlist(ast)) {
    return(visitor$pairlist(ast, visitor, ...))
  } else {
    stop("Don't know how to handle type ", typeof(ast), call. = FALSE)
  }
}
