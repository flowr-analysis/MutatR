standardize_calls <- function(ast) {
  visitor <- list(
    exprlist = function(es, v) lapply(es, visit, v) |> as.expression() |> copy_attribs(es),
    pairlist = function(l, v) l,
    atomic = function(a, v) a,
    name = function(n, v) n,
    call = function(cl, v) {
      as <- lapply(cl[-1], visit, v)
      s_call <- pryr::standardise_call(as.call(c(cl[[1]], as)))
      return(s_call |> copy_attribs(cl))
    }
  )
  return(visit(ast, visitor))
}
