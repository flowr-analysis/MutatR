name_as_string <- function(name) {
  return(paste(name, collapse = ""))
}

build_probs <- function(mutants, overwrite) {
  probs <- lapply(mutants, function(mutant) {
    overwrite[[mutant$cat]] %||% mutations[[mutant$cat]]$prob
  })
  return(probs)
}

get_srcref <- function(ast, parent = NULL) {
  srcref <- utils::getSrcref(ast) %||% parent
  return(srcref)
}

compare_identifier <- function(elem, mut) {
  is_id <- get_id(elem)
  target_id <- mut$node_id
  return(is_id == target_id)
}

copy_attribs <- function(dest, src, filter = c("node_id")) {
  filter <- c(filter, names(attributes(dest))) |> unique()

  src_attrs <- attributes(src)
  src_attrs <- src_attrs[!names(src_attrs) %in% filter]

  dest_attrs <- attributes(dest)
  attributes(dest) <- c(src_attrs, dest_attrs)

  return(dest)
}

#' Generate n mutations for the given abstract syntax tree.
#'
#' @param asts The abstract syntax trees to generate mutations for. Must be a named
#' lists with the name being the file path and the value being the abstract syntax tree.
#' @param n The number of mutations to generate.
#' @param probabilities A named list of probabilities for each mutation. If a mutation
#' is not in the list, the default probability is used.
#' @param seed The seed that determines what mutations are selected. If NULL, a random
#' seed is used.
#'
#' @return A list of n mutants and the used seed
#'
#' @export
generate_mutants <- function(
    asts, n,
    probabilities = list(),
    seed = NULL,
    require_parsable = TRUE) {
  set.seed(seed)
  mutants <- list()
  for (file in names(asts)) {
    print(file)

    mutant_hashes <- list(rlang::hash_file(file))
    mutants_per_file <- list()

    applicable_per_file <- find_applicable_mutations(asts[[file]])
    print(length(applicable_per_file))
    for (mutation in applicable_per_file) {
      can_apply <- TRUE
      tryCatch(mutant <- apply_mutation(asts[[file]], mutation), error = function(e) {
        cat("Could not apply\n")
        print(e)
        can_apply <<- FALSE
      })
      if (!can_apply) next

      if (require_parsable) {
        does_parse <- TRUE
        if (is.expression(mutant)) {
          code <- lapply(mutant, deparse, control = NULL) |> paste(collapse = "\n")
        } else {
          code <- deparse(mutant, control = NULL)
        }

        hash <- rlang::hash(code)
        if (hash %in% mutant_hashes) next
        mutant_hashes <- c(mutant_hashes, hash)

        tryCatch(parse(text = code), error = function(e) {
          print(e)
          does_parse <<- FALSE
        })
        if (!does_parse) next
      }

      mutants_per_file <- append(mutants_per_file, list(append(mutation, list(mutant = mutant))))
    }
    print(length(mutants_per_file))
    mutants <- c(mutants, mutants_per_file)
  }

  if (length(mutants) < n) {
    cat("Only", length(mutants), "mutations found. Requested", n, "mutations.\n")
    n <- length(mutants)
  } else {
    cat("Found", length(mutants), "mutations.\n")
  }

  mutants <- sample(mutants, n, prob = build_probs(mutants, probabilities))

  return(mutants)
}

test <- function() {
  # nolint start
  # files <- c(
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/askpass.R",
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/interactive.R",
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/onload.R",
  #   "/home/luke/src/cran-packages-coverage/pkgs/askpass/R/ssh.R"
  # )
  # nolint end
  files <- c(
    "/home/luke/src/master-thesis/mutatR/example.R" # nolint
  )
  asts <- files |>
    lapply(parse, keep.source = TRUE) |>
    stats::setNames(files) |>
    # lapply(standardize_calls) |> # does not work if we don't source the file into an environment
    lapply(add_srcrefs) |>
    lapply(set_ids)
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
  asts <- lapply(files, parse, keep.source = TRUE) |> stats::setNames(files)
  asts <- lapply(asts, add_srcrefs)
  invisible(generate_mutants(asts, 1000))
}
