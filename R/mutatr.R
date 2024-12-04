name_as_string <- function(name) {
  return(paste(name, collapse = ""))
}

build_probs <- function(applicable) {
  probs <- lapply(applicable, function(mutation) mutations[[mutation$mutation]]$prob)
  return(probs)
}

#' Generate n mutations for the given abstract syntax tree.
#'
#' @param asts The abstract syntax trees to generate mutations for. Must be a named
#' lists with the name being the file path and the value being the abstract syntax tree.
#' @param n The number of mutations to generate.
#' @param filter A function that takes the mutation name, the source reference and the
#' file path and returns a boolean indicating whether the mutation can be applied.
#'
#' @return A list of n mutated abstract syntax trees with the applied mutation
#'
#' @export
generate_mutations <- function(asts, n, filter = function(...) TRUE) {
  applicable <- list()
  for (file in names(asts)) {
    applicable_per_file <- find_applicable_mutations(asts[[file]])
    for (mutation in names(applicable_per_file)) {
      for (srcref in applicable_per_file[[mutation]]) {
        p <- filter(mutation, srcref, file)
        if (isFALSE(p)) next
        applicable <- append(applicable, list(list(mutation = mutation, srcref = srcref, file = file)))
      }
    }
  }

  if (length(applicable) < n) {
    cat("Only", length(applicable), "mutations found. Requested", n, "mutations.\n")
    n <- length(applicable)
  }

  mutants <- list()
  for (mutation in sample(applicable, n, prob = build_probs(applicable))) {
    mutant <- apply_mutation(asts[[mutation$file]], mutation$mutation, mutation$srcref)
    mutants <- append(mutants, list(c(mutation, list(mutant = mutant))))
  }
  return(mutants)
}

test <- function() {
  files <- c(
    "/home/luke/src/master-thesis/package/R/flowr_utils.R", # nolint
    "/home/luke/src/master-thesis/package/R/utils.R" # nolint
  )
  asts <- lapply(files, parse, keep.source = TRUE) |> setNames(files)
  asts <- lapply(asts, add_srcrefs)
  invisible(generate_mutations(asts, 1000))
}
