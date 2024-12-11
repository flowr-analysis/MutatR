name_as_string <- function(name) {
  return(paste(name, collapse = ""))
}

build_probs <- function(applicable, overwrite) {
  probs <- lapply(applicable, function(mutation) {
    overwrite[[mutation$cat]] %||% mutations[[mutation$cat]]$prob
  })
  return(probs)
}

get_srcref <- function(ast, parent = NULL) {
  srcref <- getSrcref(ast)
  if (is.null(srcref)) {
    return(parent)
  }
  return(srcref)
}

compare_identifier <- function(elem, mut) {
  is_id <- get_id(elem)
  target_id <- mut$node_id
  return(is_id == target_id)
}

#' Generate n mutations for the given abstract syntax tree.
#'
#' @param asts The abstract syntax trees to generate mutations for. Must be a named
#' lists with the name being the file path and the value being the abstract syntax tree.
#' @param n The number of mutations to generate.
#' @param filter A function that takes the mutation name, the source reference and the
#' file path and returns a boolean indicating whether the mutation can be applied.
#' @param probabilities A named list of probabilities for each mutation. If a mutation
#' is not in the list, the default probability is used.
#' @param seed The seed that determines what mutations are selected. If NULL, a random
#' seed is used.
#'
#' @return A list of n mutated abstract syntax trees with the applied mutation
#'
#' @export
generate_mutants <- function(asts, n, filter = function(...) TRUE, probabilities = list(), seed = NULL) {
  set.seed(seed) # TODO: return seed
  applicable <- list()
  for (file in names(asts)) {
    applicable <- find_applicable_mutations(asts[[file]])
    for (mutation in names(applicable)) {
      p <- filter(mutation$cat, mutation$srcref, file)
      if (isFALSE(p)) next
      applicable <- append(applicable, list(list(mutation = mutation, srcref = srcref, file = file)))
    }
  }

  if (length(applicable) < n) {
    cat("Only", length(applicable), "mutations found. Requested", n, "mutations.\n")
    n <- length(applicable)
  } else {
    cat("Found", length(applicable), "mutations.\n")
  }

  mutants <- list()
  for (mutation in sample(applicable, n, prob = build_probs(applicable, probabilities))) {
    file <- getSrcFilename(mutation$srcref, full.names = TRUE)
    mutant <- apply_mutation(asts[[file]], mutation)
    mutants <- append(mutants, list(append(mutation, list(mutant = mutant))))
  }
  return(mutants)
}

# FIXME: print(x = 2, y = 2) entfernt die namen
test <- function() {
  # files <- c(
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/askpass.R",
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/interactive.R",
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/onload.R",
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/ssh.R"
  # )
  files <- c(
    "/home/luke/src/master-thesis/mutatR/example.R" # nolint
  )
  asts <- files |>
    lapply(parse, keep.source = TRUE) |>
    setNames(files) |>
    lapply(add_srcrefs) |>
    lapply(add_ids)
  mutants <- generate_mutants(asts, 1000)
  for (mutant in mutants) {
    if (is.expression(mutant$mutant)) {
      code <- lapply(mutant$mutant, deparse, control = NULL) |> paste(collapse = "\n")
    } else {
      code <- deparse(mutant, control = NULL)
    }
    cat(code, "\n\n")
  }
}

test2 <- function(pkg) {
  src_path <- file.path(pkg, "R")
  files <- list.files(src_path, recursive = TRUE, full.names = TRUE, pattern = "\\.R$")
  asts <- lapply(files, parse, keep.source = TRUE) |> setNames(files)
  asts <- lapply(asts, add_srcrefs)
  invisible(generate_mutants(asts, 1000))
}
