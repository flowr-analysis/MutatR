remove_me <- "(: HOPEFULLY THIS TEXT WILL NEVER SHOW UP IN CODE :)"

rename_op <- function(ast, to) {
  force(to)
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

function_blacklist <- c("print", "cat", "message", "warning", "stop",
  "log_trace", "log_debug", "log_info", "log_warn", "log_error", "log_fatal",
  "logdebug", "loginfo", "logwarn", "logerror", "logfine", "logfiner", "logfinest"
)

mutate_string <- function(ast, role) {
  if (!is.character(ast)) {
    return(FALSE)
  }
  parent_function <- attr(role, "fname")
  return(!isTRUE(parent_function %in% function_blacklist))
}

literal <- list( # nolint: cyclocomp_linter.
  is_applicable = function(ast, role) {
    if (!is.atomic(ast) || is.na(ast)) {
      return(FALSE)
    }
    return(is.numeric(ast) || mutate_string(ast, role) || is.logical(ast))
  },
  get_mutations = function(ast) {
    muts <- list()
    if (is.numeric(ast)) {
      for (inc in c(-1, 1, NA)) {
        id <- sprintf("%f:%d", as.numeric(ast), inc)
        muts <- append(muts, list(list(mut_id = id, fun = appl_fun("+", ast, inc))))
      }
      return(muts)
    }
    if (isFALSE(ast)) {
      return(list(list(mut_id = "false:true", fun = function() quote(TRUE))))
    }
    if (isTRUE(ast)) {
      return(list(list(mut_id = "true:false", fun = function() quote(FALSE))))
    }
    if (is.character(ast)) {
      if (nchar(ast) == 0) {
        return(list(list(mut_id = "add", fun = function() quote("mutatr string"))))
      }

      muts <- (list(list(
        mut_id = "remove",
        fun = function() substring(ast, 2)
      ), list(
        mut_id = "add",
        fun = function() paste(ast, "mutated")
      )))

      if (nchar(ast) > 1) {
        muts <- append(muts, list(list(
          mut_id = "empty",
          fun = function() quote("")
        )))
      }

      return(muts)
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
  "any" = c("all"),
  "all" = c("any"),
  "[[" = c("["),
  "union" = c("intersect", "setdiff"),
  "intersect" = c("union", "setdiff"),
  "setdiff" = c("union", "intersect")
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
    return(role == roles$Cond && !isTRUE(ast) && !isFALSE(ast))
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

is_check <- function(name) {
  is_functions <- c(
    "is.array", "is.atomic", "is.call", "is.character", "is.complex", "is.data.frame", "is.double", "is.element",
    "is.empty.model", "is.environment", "is.expression", "is.factor", "is.finite", "is.function", "is.hashtab",
    "is.infinite", "is.integer", "is.language", "is.leaf", "is.list", "is.loaded", "is.logical", "is.matrix", "is.mts",
    "is.na", "is.name", "is.nan", "is.null", "is.numeric", "is.object", "is.ordered", "is.package_version",
    "is.pairlist", "is.primitive", "is.qr", "is.R", "is.raster", "is.raw", "is.recursive", "is.relistable", "is.single",
    "is.stepfun", "is.symbol", "is.table", "is.ts", "is.tskernel", "is.unsorted", "is.vector",
    "setequal", "identical",
    "==", "!=", ">", "<", ">=", "<="
  )
  return(name %in% is_functions)
}

function_replacement <- list(
  is_applicable = function(ast, role) {
    if (!is.call(ast)) {
      return(FALSE)
    }

    name <- name_as_string(ast[[1]])
    remove_void_call <- role == roles$ExprListElem && !is_assignment(name)
    is_length <- name == "length"
    is_check <- is_check(name)
    return(remove_void_call || is_length || is_check)
  },
  get_mutations = function(ast) {
    name <- name_as_string(ast[[1]])
    remove_void_call <- !is_assignment(name)
    is_length <- name == "length"
    is_check <- is_check(name)
    muts <- list()
    if (remove_void_call) {
      muts <- append(muts, list(list(mut_id = "remove", fun = function() remove_me)))
    }
    if (is_length) {
      muts <- append(muts, list(
        list(mut_id = "length:0", fun = function() quote(0)),
        list(mut_id = "length:1", fun = function() quote(1)),
        list(mut_id = "length:5", fun = function() quote(5))
      ))
    }
    if (is_check) {
      muts <- append(muts, list(
        list(mut_id = sprintf("%s:true", name), fun = function() quote(TRUE)),
        list(mut_id = sprintf("%s:false", name), fun = function() quote(FALSE))
      ))
    }
    return(muts)
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
      return(list(list(mut_id = "add", fun = function() quote(42))))
    }
    muts <- list(
      list(mut_id = "remove", fun = function() {
        as <- as.list(ast[-1])
        as <- as[-sample(seq_along(as), 1)]
        return(as.call(c(ast[[1]], as)))
      }),
      list(mut_id = "add", fun = function() {
        as <- as.list(ast[-1])
        as <- c(as, quote(41))
        return(as.call(c(ast[[1]], as)))
      })
    )

    if (length(ast[-1]) > 1) {
      muts <- append(muts, list(
        list(mut_id = "empty", fun = function() {
          as <- list()
          return(as.call(c(ast[[1]], as)))
        })
      ))
    }

    return(muts)
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
    return(is.expression(ast) || is.call(ast) && ast[[1]] == "{")
  },
  get_mutations = function(ast) {
    if (is.expression(ast)) {
      as <- as.list(ast)
      return(list(list(mut_id = "add warning", fun = function() {
        as <- c(as, quote(warning("warning created by mutatr")))
        return(as.expression(as))
      }), list(mut_id = "add error", fun = function() {
        as <- c(as, quote(stop("error created by mutatr")))
        return(as.expression(as))
      })))
    } else if (is.call(ast)) {
      as <- as.list(ast[-1])
      return(list(list(mut_id = "add warning", fun = function() {
        as <- c(as, quote(warning("warning created by mutatr")))
        return(as.call(c(ast[[1]], as)))
      }), list(mut_id = "add error", fun = function() {
        as <- c(as, quote(stop("error created by mutatr")))
        return(as.call(c(ast[[1]], as)))
      })))
    }
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
  "function replacement" = list(prob = 0.5, mutation = function_replacement),
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
