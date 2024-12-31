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

copy_attribs <- function(dest, src, filter = c("node_id")) {
  filter <- c(filter, names(attributes(dest))) |> unique()

  src_attrs <- attributes(src)
  src_attrs <- src_attrs[!names(src_attrs) %in% filter]

  dest_attrs <- attributes(dest)
  attributes(dest) <- c(src_attrs, dest_attrs)

  return(dest)
}

setup_ast <- function(files) {
  considered_exprs <- 0
  unconsidered_exprs <- 0

  asts <- files |>
    lapply(parse, keep.source = TRUE) |>
    stats::setNames(files) |>
    # lapply(standardize_calls) |> # does not work if we don't source the file into an environment
    lapply(function(ast) {
      ast <- add_srcrefs(ast)
      considered_exprs <<- considered_exprs + ast$considered_exprs
      unconsidered_exprs <<- unconsidered_exprs + ast$unconsidered_exprs
      return(ast$ast)
    }) |>
    lapply(set_ids)
  return(list(asts = asts, considered_exprs = considered_exprs, unconsidered_exprs = unconsidered_exprs))
}

#' Generate n mutations for the given abstract syntax tree.
#'
#' @param files Set of files to generate mutations for.
#' @param n The number of mutations to generate.
#' @param probabilities A named list of probabilities for each mutation. If a mutation
#' is not in the list, the default probability is used.
#' @param seed The seed that determines what mutations are selected. If NULL, a random
#' seed is used.
#' @param filters A list of two filters. `filters$file` takes a file name and returns whether
#' the package should consider this file. `filters$srcref` takes a file and a srcref and returns
#' whether a mutations should be applied at this location.
#'
#' @return A list of n mutants and the used seed
#'
#' @export
generate_mutants <- function( # nolint: cyclocomp_linter.
    files, n,
    probabilities = list(),
    seed = NULL,
    filters = list(file = function(f) TRUE, srcref = function(f, s, id) TRUE)) {
  asts <- setup_ast(files)

  considered_exprs <- asts$considered_exprs
  unconsidered_exprs <- asts$unconsidered_exprs
  asts <- asts$asts

  could_not_apply_counter <- 0
  could_not_parse_counter <- 0
  identical_counter <- 0

  set.seed(seed)

  mutants <- list()
  for (file in Filter(filters$file, names(asts))) {
    mutant_hashes <- list(rlang::hash_file(file))
    mutants_per_file <- list()

    applicable_per_file <- find_applicable_mutations(asts[[file]], function(s, id) filters$srcref(file, s, id))
    for (mutation in applicable_per_file) {
      can_apply <- TRUE
      tryCatch(mutant <- apply_mutation(asts[[file]], mutation), error = function(e) {
        could_not_apply_counter <<- could_not_apply_counter + 1
        can_apply <<- FALSE
      })
      if (!can_apply) next

      does_parse <- TRUE
      if (is.expression(mutant)) {
        code <- lapply(mutant, deparse, control = NULL)
      } else {
        code <- deparse(mutant, control = NULL) |> list()
      }

      hash <- rlang::hash(code)
      if (hash %in% mutant_hashes) {
        identical_counter <- identical_counter + 1
        next
      }
      mutant_hashes <- c(mutant_hashes, hash)

      tmp_file <- tempfile(fileext = ".R")
      for (line in code) write(line, file = tmp_file, append = TRUE, sep = " ")
      tryCatch(parse(file = tmp_file), error = function(e) {
        does_parse <<- FALSE
        could_not_parse_counter <<- could_not_parse_counter + 1
      })
      if (!does_parse) next

      mutants_per_file <- append(mutants_per_file, list(append(mutation, list(mutant = mutant, file = file))))
    }
    mutants <- c(mutants, mutants_per_file)
  }

  if (length(mutants) < n) n <- length(mutants)

  if (n == 0) {
    return(list(
      mutants = list(),
      considered_exprs = considered_exprs,
      unconsidered_exprs = unconsidered_exprs,
      could_not_apply = could_not_apply_counter,
      could_not_parse = could_not_parse_counter
    ))
  }

  mutants <- sample(mutants, n, prob = build_probs(mutants, probabilities))

  return(list(
    mutants = mutants,
    considered_exprs = considered_exprs,
    unconsidered_exprs = unconsidered_exprs,
    could_not_apply = could_not_apply_counter,
    could_not_parse = could_not_parse_counter
  ))
}

# nolint start.
test <- function() {
  files <- list()
  for (pkg in c("doBy")) {
    src <- file.path("/home/luke/src/cran-packages-coverage/pkgs", pkg, "R")
    fs <- list.files(src, recursive = TRUE, full.names = TRUE, pattern = "\\.(r|R)$")
    files <- c(files, fs)
  }
  # files <- list("/home/luke/src/master-thesis/mutatR/example.R")

  mutants <- generate_mutants(files, 1000)
  print(mutants)
  # for (mutant in mutants) {
  #   if (is.expression(mutant$mutant)) {
  #     code <- lapply(mutant$mutant, deparse, control = NULL) |> paste(collapse = "\n")
  #   } else {
  #     code <- deparse(mutant, control = NULL)
  #   }
  #   cat(code, "\n\n")
  # }
}
# nolint end.