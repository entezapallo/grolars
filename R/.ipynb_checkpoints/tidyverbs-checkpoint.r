# tidyverbs.R
# Tidy-first verbs for grolars
# Requires: translator_r2polars.R, rlang

# ---- helpers ----

.schema <- function(.data) attr(.data, "schema")

.propagate <- function(result, schema) {
  attr(result, "schema") <- schema
  result
}

# ---- select ----

select.PlRLazyFrame <- function(.data, ...) {
  cols <- as.character(rlang::ensyms(...))
  schema <- .schema(.data)
  unknown <- setdiff(cols, schema)
  if (length(unknown)) stop("Columns not found: ", paste(unknown, collapse = ", "))
  exprs <- lapply(cols, polars:::col)
  .propagate(.data$select(exprs), cols)
}

select.PlRDataFrame <- function(.data, ...) {
  cols <- as.character(rlang::ensyms(...))
  schema <- .schema(.data)
  unknown <- setdiff(cols, schema)
  if (length(unknown)) stop("Columns not found: ", paste(unknown, collapse = ", "))
  exprs <- lapply(cols, polars:::col)
  .propagate(.data$lazy()$select(exprs)$collect(engine = "streaming"), cols)
}

# ---- filter ----

filter.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  exprs <- lapply(quos, .quo2expr, schema = schema)
  result <- Reduce(function(lf, e) lf$filter(e), exprs, init = .data)
  .propagate(result, schema)
}

filter.PlRDataFrame <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  exprs <- lapply(quos, .quo2expr, schema = schema)
  lf <- .data$lazy()
  result <- Reduce(function(l, e) l$filter(e), exprs, init = lf)
  .propagate(result$collect(engine = "streaming"), schema)
}

# ---- mutate ----

mutate.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  exprs <- .quos2named_exprs(quos, schema)
  new_cols <- names(quos)[nchar(names(quos)) > 0]
  result <- .data$with_columns(exprs)
  .propagate(result, union(schema, new_cols))
}

mutate.PlRDataFrame <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  exprs <- .quos2named_exprs(quos, schema)
  new_cols <- names(quos)[nchar(names(quos)) > 0]
  result <- .data$lazy()$with_columns(exprs)$collect(engine = "streaming")
  .propagate(result, union(schema, new_cols))
}

# ---- arrange ----

arrange.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  # detect desc() wrapper
  exprs <- lapply(quos, function(q) {
    expr <- rlang::get_expr(q)
    if (is.call(expr) && as.character(expr[[1]]) == "desc") {
      list(col = as.character(expr[[2]]), desc = TRUE)
    } else {
      list(col = as.character(expr), desc = FALSE)
    }
  })
  cols <- sapply(exprs, `[[`, "col")
  desc <- sapply(exprs, `[[`, "desc")
  .propagate(.data$sort(cols, descending = desc, maintain_order = TRUE), schema)
}

arrange.PlRDataFrame <- function(.data, ...) {
  lf <- .data$lazy()
  attr(lf, "schema") <- .schema(.data)
  result <- arrange.PlRLazyFrame(lf, ...)
  .propagate(result$collect(engine = "streaming"), .schema(.data))
}

# ---- rename ----

rename.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  new_names <- names(quos)
  old_names <- as.character(sapply(quos, rlang::get_expr))
  result <- .data$rename(old_names, new_names)
  new_schema <- schema
  for (i in seq_along(old_names)) {
    new_schema[new_schema == old_names[i]] <- new_names[i]
  }
  .propagate(result, new_schema)
}

rename.PlRDataFrame <- function(.data, ...) {
  lf <- .data$lazy()
  attr(lf, "schema") <- .schema(.data)
  result <- rename.PlRLazyFrame(lf, ...)
  .propagate(result$collect(engine = "streaming"), attr(result, "schema"))
}

# ---- distinct ----

distinct.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  dots <- rlang::ensyms(...)
  if (length(dots) == 0) {
    .propagate(.data$unique(subset = NULL, keep = "first", maintain_order = TRUE), schema)
  } else {
    cols <- as.character(dots)
    .propagate(.data$unique(subset = cols, keep = "first", maintain_order = TRUE), schema)
  }
}

distinct.PlRDataFrame <- function(.data, ...) {
  lf <- .data$lazy()
  attr(lf, "schema") <- .schema(.data)
  result <- distinct.PlRLazyFrame(lf, ...)
  .propagate(result$collect(engine = "streaming"), .schema(.data))
}

# ---- drop_na ----

drop_na.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  dots <- rlang::ensyms(...)
  cols <- if (length(dots) == 0) NULL else as.character(dots)
  .propagate(.data$drop_nulls(subset = cols), schema)
}

drop_na.PlRDataFrame <- function(.data, ...) {
  lf <- .data$lazy()
  attr(lf, "schema") <- .schema(.data)
  result <- drop_na.PlRLazyFrame(lf, ...)
  .propagate(result$collect(engine = "streaming"), .schema(.data))
}

# ---- group_by + summarise ----

group_by.PlRLazyFrame <- function(.data, ...) {
  schema <- .schema(.data)
  cols <- as.character(rlang::ensyms(...))
  grp <- .data$group_by(lapply(cols, polars:::col), maintain_order = TRUE)
  attr(grp, "schema") <- schema
  attr(grp, "group_keys") <- cols
  grp
}

summarise.PlRLazyGroupBy <- function(.data, ...) {
  schema <- .schema(.data)
  quos <- rlang::enquos(...)
  exprs <- .quos2named_exprs(quos, schema)
  keys <- attr(.data, "group_keys")
  result <- .data$agg(exprs)
  .propagate(result, union(keys, names(quos)))
}

# alias
summarize.PlRLazyGroupBy <- summarise.PlRLazyGroupBy

# ---- slice ----

slice.PlRLazyFrame <- function(.data, rows) {
  schema <- .schema(.data)
  offset <- as.integer(rows[1]) - 1L
  length <- as.integer(length(rows))
  .propagate(.data$slice(offset, length), schema)
}

slice.PlRDataFrame <- function(.data, rows) {
  lf <- .data$lazy()
  attr(lf, "schema") <- .schema(.data)
  result <- slice.PlRLazyFrame(lf, rows)
  .propagate(result$collect(engine = "streaming"), .schema(.data))
}

# ---- register S3 methods ----

# dplyr generics
registerS3method("select",    "PlRLazyFrame",    select.PlRLazyFrame,    envir = asNamespace("dplyr"))
registerS3method("select",    "PlRDataFrame",    select.PlRDataFrame,    envir = asNamespace("dplyr"))
registerS3method("filter",    "PlRLazyFrame",    filter.PlRLazyFrame,    envir = asNamespace("dplyr"))
registerS3method("filter",    "PlRDataFrame",    filter.PlRDataFrame,    envir = asNamespace("dplyr"))
registerS3method("mutate",    "PlRLazyFrame",    mutate.PlRLazyFrame,    envir = asNamespace("dplyr"))
registerS3method("mutate",    "PlRDataFrame",    mutate.PlRDataFrame,    envir = asNamespace("dplyr"))
registerS3method("arrange",   "PlRLazyFrame",    arrange.PlRLazyFrame,   envir = asNamespace("dplyr"))
registerS3method("arrange",   "PlRDataFrame",    arrange.PlRDataFrame,   envir = asNamespace("dplyr"))
registerS3method("rename",    "PlRLazyFrame",    rename.PlRLazyFrame,    envir = asNamespace("dplyr"))
registerS3method("rename",    "PlRDataFrame",    rename.PlRDataFrame,    envir = asNamespace("dplyr"))
registerS3method("distinct",  "PlRLazyFrame",    distinct.PlRLazyFrame,  envir = asNamespace("dplyr"))
registerS3method("distinct",  "PlRDataFrame",    distinct.PlRDataFrame,  envir = asNamespace("dplyr"))
registerS3method("group_by",  "PlRLazyFrame",    group_by.PlRLazyFrame,  envir = asNamespace("dplyr"))
registerS3method("summarise", "PlRLazyGroupBy",  summarise.PlRLazyGroupBy, envir = asNamespace("dplyr"))
registerS3method("summarize", "PlRLazyGroupBy",  summarize.PlRLazyGroupBy, envir = asNamespace("dplyr"))
registerS3method("slice",     "PlRLazyFrame",    slice.PlRLazyFrame,     envir = asNamespace("dplyr"))
registerS3method("slice",     "PlRDataFrame",    slice.PlRDataFrame,     envir = asNamespace("dplyr"))
registerS3method("drop_na",   "PlRLazyFrame",    drop_na.PlRLazyFrame,   envir = asNamespace("tidyr"))
registerS3method("drop_na",   "PlRDataFrame",    drop_na.PlRDataFrame,   envir = asNamespace("tidyr"))