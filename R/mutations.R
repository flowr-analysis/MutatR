rename_op <- function(ast, to) {
  eval(to)
  if (is.name(ast)) {
    return(function() as.name(to))
  }
  return(function() {
    ast[[1]] <- as.name(to)
    return(ast)
  })
}

appl_fun <- function(f, ...) {
  args <- list(...)
  return(function() as.call(c(as.name(f), args)) |> eval())
}

literal <- list( # nolint: cyclocomp_linter.
  is_applicable = function(ast, role) {
    return(is.numeric(ast) || is.character(ast) || is.logical(ast) || identical(ast, quote(NULL)))
  },
  get_mutations = function(ast) {
    muts <- list()
    if (is.numeric(ast)) {
      for (inc in c(-1, 1, NA)) {
        id <- sprintf("%d:%d", as.numeric(ast), inc)
        muts <- append(muts, list(list(mut_id = id, fun = appl_fun("+", ast, inc))))
      }
      return(muts)
    }
    if (identical(ast, quote(NULL))) {
      return(list(list(mut_id = "null:42", fun = function() quote(42))))
    }
    if (isFALSE(ast)) {
      return(list(list(mut_id = "false:true", fun = function() quote(TRUE))))
    }
    if (isTRUE(ast)) {
      return(list(list(mut_id = "true:false", fun = function() quote(FALSE))))
    }
    if (is.character(ast)) {
      return(list(list(mut_id = "str", fun = NULL))) # TODO
    }
  }
)

logic_mutations <- list(
  "&" = c("|"),
  "|" = c("&"),
  "&&" = c("||"),
  "||" = c("&&")
)
logic <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && length(ast) - 1 == 2 && name_as_string(ast[[1]]) %in% names(logic_mutations))
  },
  get_mutations = function(ast) {
    muts <- list()
    name <- ast[[1]] |> as.character()
    for (to in logic_mutations[[name]]) {
      id <- sprintf("%s:%s", name, to)
      muts <- append(muts, list(list(mut_id = id, fun = rename_op(ast, to))))
    }
    return(muts)
  }
)

negativ_cond_mutations <- list(
  "==" = c("!="),
  "!=" = c("=="),
  ">" = c("<="),
  "<" = c(">="),
  ">=" = c("<"),
  "<=" = c(">")
)
negative_condition <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && length(ast) - 1 == 2 && name_as_string(ast[[1]]) %in% names(negativ_cond_mutations))
  },
  get_mutations = function(ast) {
    muts <- list()
    name <- ast[[1]] |> as.character()
    for (to in negativ_cond_mutations[[name]]) {
      id <- sprintf("%s:%s", name, to)
      muts <- append(muts, list(list(mut_id = id, fun = rename_op(ast, to))))
    }
    return(muts)
  }
)

cond_boundary_mutations <- list(
  ">" = c(">="),
  "<" = c("<="),
  ">=" = c(">"),
  "<=" = c("<")
)
condition_boundary <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && length(ast) - 1 == 2 && name_as_string(ast[[1]]) %in% names(cond_boundary_mutations))
  },
  get_mutations = function(ast) {
    muts <- list()
    name <- ast[[1]] |> as.character()
    for (to in cond_boundary_mutations[[name]]) {
      id <- sprintf("%s:%s", name, to)
      muts <- append(muts, list(list(mut_id = id, fun = rename_op(ast, to))))
    }
    return(muts)
  }
)

arithmetic_mutations <- list(
  "+" = c("-", "*", "/"),
  "-" = c("+", "*", "/"),
  "*" = c("/", "+", "-"),
  "/" = c("*", "+", "-")
)
arithmetic <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && length(ast) - 1 == 2 && name_as_string(ast[[1]]) %in% names(arithmetic_mutations))
  },
  get_mutations = function(ast) {
    muts <- list()
    name <- ast[[1]] |> as.character()
    for (to in arithmetic_mutations[[name]]) {
      id <- sprintf("%s:%s", name, to)
      muts <- append(muts, list(list(mut_id = id, fun = rename_op(ast, to))))
    }
    return(muts)
  }
)

name_mutations <- list(
  "lapply" = c("sapply", "vapply"),
  "sapply" = c("vapply"),
  "vapply" = c("sapply"),
  "isFALSE" = c("isTRUE"),
  "isTRUE" = c("isFALSE"),
  "[[" = c("[")
)
function_name <- list(
  is_applicable = function(ast, role) {
    return(is.name(ast) && role == roles$FunName && name_as_string(ast) %in% names(name_mutations))
  },
  get_mutations = function(ast) {
    muts <- list()
    name <- ast |> as.character()
    for (to in name_mutations[[name]]) {
      id <- sprintf("%s:%s", name, to)
      muts <- append(muts, list(list(mut_id = id, fun = rename_op(ast, to))))
    }
    return(muts)
  }
)

branch_condition <- list(
  is_applicable = function(ast, role) {
    return(role == roles$Cond && !identical(ast, quote(TRUE) && !identical(ast, quote(FALSE))))
  },
  get_mutations = function(ast) {
    return(list(
      list(mut_id = "true", fun = function() quote(TRUE)),
      list(mut_id = "false", fun = function() quote(FALSE))
    ))
  }
)

unary_oper_mutations <- list(
  "+" = c("-"),
  "-" = c("+")
)
sign_swap <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && length(ast) - 1 == 1 && name_as_string(ast[[1]]) %in% names(unary_oper_mutations))
  },
  get_mutations = function(ast) {
    muts <- list()
    name <- ast[[1]] |> as.character()
    for (to in unary_oper_mutations[[name]]) {
      id <- sprintf("%s:%s", name, to)
      muts <- append(muts, list(list(mut_id = id, fun = rename_op(ast, to))))
    }
    return(muts)
  }
)

is_assignment <- function(name) {
  return(name %in% c("<-", "->", "="))
}

void_call <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && role == roles$ExprList && !is_assignment(name_as_string(ast[[1]])))
  },
  get_mutations = function(ast) {
    return(list(list(mut_id = "remove", fun = function() NULL)))
  }
)

return_value <- list(
  is_applicable = function(ast, role) {
    return(role == roles$Ret)
  },
  get_mutations = function(ast) {
    if (identical(ast, quote(NULL))) {
      return(list(list(mut_id = "nonnull", fun = function() quote(42))))
    }
    return(list(list(mut_id = "null", fun = function() quote(NULL))))
  }
)

mutate_c <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && name_as_string(ast[[1]]) == "c")
  },
  get_mutations = function(ast) {
    if (length(ast) - 1 == 0) { # no arguments provided
      return(list(list(mut_id = "non-empty", fun = function() quote(42))))
    }
    return(list(
      list(mut_id = "remove", fun = function() {
        as <- as.list(ast[-1])
        as <- as[-sample(seq_along(as), 1)]
        return(as.call(c(ast[[1]], as))) # remove one component
      }),
      list(mut_id = "add", fun = function() {
        as <- as.list(ast[-1])
        as <- c(as, quote(41))
        return(as.call(c(ast[[1]], as))) # add one component
      })
    ))
  }
)

mutate_identical <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && name_as_string(ast[[1]]) %in% c("==", "identical") && length(ast) - 1 >= 2)
  },
  get_mutations = function(ast) {
    name <- ast[[1]]
    if (name == "identical") {
      return(list(list(mut_id = "identical:==", fun = function() {
        as <- head(as.list(ast[-1]), 2)
        return(as.call(c(as.name("=="), as)))
      })))
    }
    if (name == "==") {
      return(list(list(mut_id = "==:identical", fun = function() {
        as <- as.list(ast[-1])
        return(as.call(c(as.name("identical"), as)))
      })))
    }
  }
)

create_call <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && ast[[1]] == "{")
  },
  get_mutations = function(ast) {
    return(list(list(
      id = "add warning",
      fun = function() {
        as <- c(as.list(ast[-1]), quote(warning("warning created by mutatr")))
        return(as.call(c(ast[[1]], as)))
      }
    ), list(
      id = "add error",
      fun = function() {
        as <- c(as.list(ast[-1]), quote(stop("error created by mutatr")))
        return(as.call(c(ast[[1]], as)))
      }
    )))
  }
)

mutations <- list(
  "arithmetic" = list(prob = 0.5, mutation = arithmetic),
  "branch condition" = list(prob = 0.5, mutation = branch_condition),
  "condition boundary" = list(prob = 0.5, mutation = condition_boundary),
  "function name" = list(prob = 0.5, mutation = function_name),
  "literal" = list(prob = 0.5, mutation = literal),
  "logic" = list(prob = 0.5, mutation = logic),
  "negative condition" = list(prob = 0.5, mutation = negative_condition),
  "swap sign" = list(prob = 0.5, mutation = sign_swap),
  "void call" = list(prob = 0.5, mutation = void_call),
  "return value" = list(prob = 0.5, mutation = return_value),
  "mutate c" = list(prob = 0.5, mutation = mutate_c),
  "mutate identical" = list(prob = 0.5, mutation = mutate_identical),
  "create call" = list(prob = 0.5, mutation = create_call)
)

any_applicable <- function(ast, role) {
  return(length(all_applicable()) > 0)
}

all_applicable <- function(ast, role) {
  res <- list()
  for (key in names(mutations)) {
    mut <- mutations[[key]]$mutation
    if (mut$is_applicable(ast, role)) {
      muts <- mut$get_mutations(ast) |> lapply(append, list(cat = key))
      res <- c(res, muts)
    }
  }
  return(res)
}
