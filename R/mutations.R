bool_swap <- list(
  is_applicable = function(ast, role) {
    return(is.logical(ast))
  },
  mutate = function(ast) {
    if (identical(ast, quote(TRUE))) {
      return(quote(FALSE))
    }
    if (identical(ast, quote(FALSE))) {
      return(quote(TRUE))
    }
    return(ast)
  }
)

increment <- list(
  is_applicable = function(ast, role) {
    return(is.numeric(ast))
  },
  mutate = function(ast) {
    return(ast + sample(c(-1, 1), 1))
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
  mutate = function(ast) {
    ast[[1]] <- as.name(sample(logic_mutations[[ast[[1]]]], 1))
    return(ast)
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
  mutate = function(ast) {
    ast[[1]] <- as.name(sample(negativ_cond_mutations[[ast[[1]]]], 1))
    return(ast)
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
  mutate = function(ast) {
    ast[[1]] <- as.name(sample(cond_boundary_mutations[[ast[[1]]]], 1))
    return(ast)
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
  mutate = function(ast) {
    ast[[1]] <- as.name(sample(arithmetic_mutations[[ast[[1]]]], 1))
    return(ast)
  }
)

function_name <- list(
  is_applicable = function(ast, role) {
    return(is.name(ast) && role == roles$FunName && name_as_string(ast) %in% c("lapply", "sapply", "vapply"))
  },
  mutate = function(ast) {
    return(as.name(sample(c("lapply", "sapply", "vapply"), 1)))
  }
)

branch_condition <- list(
  is_applicable = function(ast, role) {
    return(role == roles$Cond)
  },
  mutate = function(ast) {
    return(sample(c(quote(TRUE), quote(FALSE)), 1))
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
  mutate = function(ast) {
    ast[[1]] <- as.name(sample(unary_oper_mutations[[ast[[1]]]], 1))
    return(ast)
  }
)

is_assignment <- function(name) {
  return(name %in% c("<-", "->", "="))
}

void_call <- list(
  is_applicable = function(ast, role) {
    return(is.call(ast) && role == roles$ExprList && !is_assignment(name_as_string(ast[[1]])))
  },
  mutate = function(ast) {
    return(NULL)
  }
)

return_value <- list(
  is_applicable = function(ast, role) {
    return(role == roles$Ret)
  },
  mutate = function(ast) {
    if (identical(ast, quote(NULL))) {
      return(quote(numeric(length = 0)))
    }
    return(quote(NULL))
  }
)

# https://pitest.org/quickstart/mutators/
mutations <- list(
  "arithmetic" = list(prob = 0.5, mutation = arithmetic),
  "branch condition" = list(prob = 0.5, mutation = branch_condition),
  "condition boundary" = list(prob = 0.5, mutation = condition_boundary),
  "function name" = list(prob = 0.5, mutation = function_name),
  "increment" = list(prob = 0.5, mutation = increment),
  "logic" = list(prob = 0.5, mutation = logic),
  "negative condition" = list(prob = 0.5, mutation = negative_condition),
  "swap boolean" = list(prob = 0.5, mutation = bool_swap),
  "swap sign" = list(prob = 0.5, mutation = sign_swap),
  "void call" = list(prob = 0.5, mutation = void_call),
  "return value" = list(prob = 0.5, mutation = return_value)
)

any_applicable <- function(ast, role) {
  return(length(all_applicable()) > 0)
}

all_applicable <- function(ast, role) {
  muts <- c()
  for (key in names(mutations)) {
    mut <- mutations[[key]]$mutation
    if (mut$is_applicable(ast, role)) {
      muts <- c(muts, key)
    }
  }
  return(muts)
}
