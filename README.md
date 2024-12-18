# MutatR

Mutate R code.

## Available Mutators
We currently support the following mutations (in no particular order):

- Arithmetic operator replacement:
    | Original | Mutation 1 | Mutation 2 | Mutation 3 |
    |----------|------------|------------|------------|
    | `+`      | `-`        | `*`        | `/`        |
    | `-`      | `+`        | `*`        | `/`        |
    | `*`      | `+`        | `-`        | `/`        |
    | `/`      | `+`        | `-`        | `*`        |
- Branch condition replacement:
    - Conditions of `if` statements and `while` loops are replaced by `TRUE` or `FALSE`
- Condition boundary mutation:
    | Original | Mutation |
    |----------|----------|
    | `>`      | `>=`     |
    | `>=`     | `>`      |
    | `<`      | `<=`     |
    | `<=`     | `<`      |
- Function name replacement:
    | Original    | Mutation 1  | Mutation 2  |
    |-------------|-------------|-------------|
    | `lapply`    | `sapply`    | `vapply`    |
    | `sapply`    | `lapply`    |             |
    | `vapply`    | `sapply`    |             |
    | `isTRUE`    | `isFALSE`   |             |
    | `isFALSE`   | `isTRUE`    |             |
    | `[[`        | `[`         |             |
    | `any`       | `all`       |             |
    | `all`       | `any`       |             |
    | `==`        | `identical` |             |
    | `identical` | `==`        |             |
    | `union`     | `intersect` | `setdiff`   |
    | `intersect` | `union`     | `setdiff`   |
    | `setdiff`   | `union`     | `intersect` |
- Literal mutation:
    - Numeric literals are replaced by `NA` or in- or decremented by 1
    - Logical literals are replaced by their negation
    - Character literals are modified by appending to the end, removing the first character, or by replacing it with an empty string
        - This is only done when the string does not appear inside typical logging functions, like `print`, `cat`, `message`, or `log_trace`
- Logic operator replacement:
    | Original | Mutation |
    |----------|----------|
    | `&`      | `\|`      |
    | `\|`      | `&`      |
    | `&&`     | `\|\|`     |
    | `\|\|`     | `&&`     |
- Relational operator replacement:
    | Original | Mutation |
    |----------|----------|
    | `==`     | `!=`     |
    | `!=`     | `==`     |
    | `>`      | `<=`     |
    | `>=`     | `<`      |
    | `<`      | `>=`     |
    | `<=`     | `>`      |
- Sign replacement:
    | Original | Mutation |
    |----------|----------|
    | `+`      | `-`      |
    | `-`      | `+`      |
- Function replacement
    - Calls whose result is not assigned by `<-`, `<<-`, `->`, `->>`, or `=` are removed
    - Calls to `length` are replaced by `0`, `1`, and `5`
    - "is"-checkes (`is.character`, `is.logical`, ... , and relation operators) are replaced by `TRUE` and `FALSE`
- Return value replacement:
    | Original        | Mutation |
    |-----------------|----------|
    | `NULL`          | `42`     |
    | everything else | `NULL`   |
- Vector modification:
    - Wenn the function `c` is called, we either remove and element, add an additional, or remove all elements
- Call insertion
    - A call to `stop` or `warning` is inserted into blocks (`{ ... }`)
