.get_schema <- function(lf) names(lf$collect_schema())  # guarda "\"Sepal.Length\""

.match_col <- function(name, schema) {
  match <- schema[grepl(paste0('"', name, '"'), schema, fixed = TRUE)]
  if (length(match) == 0) stop("Column '", name, "' not found")
  match[1]
}
.lazy_readers <- list(
  csv     = function(p) polars:::PlRLazyFrame$new_from_csv(
    source                       = p$source,
    separator                    = p$separator,
    has_header                   = p$has_header,
    ignore_errors                = FALSE,
    skip_rows                    = 0L,
    cache                        = TRUE,
    missing_utf8_is_empty_string = FALSE,
    low_memory                   = FALSE,
    rechunk                      = p$rechunk,
    skip_rows_after_header       = 0L,
    encoding                     = "utf8",
    try_parse_dates              = FALSE,
    eol_char                     = "\n",
    raise_if_empty               = TRUE,
    truncate_ragged_lines        = FALSE,
    decimal_comma                = FALSE,
    glob                         = TRUE,
    row_index_offset             = 0L,
    comment_prefix               = NULL,
    quote_char                   = NULL,
    null_values                  = NULL,
    infer_schema_length          = NULL,
    row_index_name               = NULL,
    n_rows                       = NULL,
    overwrite_dtype              = NULL,
    schema                       = NULL,
    storage_options              = NULL,
    include_file_paths           = NULL
  ),
  parquet = function(p) polars:::PlRLazyFrame$new_from_parquet(
    source               = p$source,
    cache                = TRUE,
    parallel             = "auto",
    rechunk              = p$rechunk,
    low_memory           = FALSE,
    use_statistics       = TRUE,
    try_parse_hive_dates = TRUE,
    glob                 = TRUE,
    missing_columns      = "raise",
    row_index_offset     = 0L,
    storage_options      = NULL,
    n_rows               = NULL,
    row_index_name       = NULL,
    hive_partitioning    = NULL,
    schema               = NULL,
    hive_schema          = NULL,
    include_file_paths   = NULL
  ),
  arrow = function(p) polars:::PlRLazyFrame$new_from_ipc(
    source               = p$source,
    cache                = TRUE,
    rechunk              = p$rechunk,
    try_parse_hive_dates = TRUE,
    row_index_offset     = 0L,
    n_rows               = NULL,
    row_index_name       = NULL,
    storage_options      = NULL,
    hive_partitioning    = NULL,
    hive_schema          = NULL,
    include_file_paths   = NULL
  ),
  ndjson = function(p) polars:::PlRLazyFrame$new_from_ndjson(
    source              = p$source,
    infer_schema_length = 100L,
    batch_size          = NULL,
    low_memory          = FALSE,
    rechunk             = p$rechunk,
    row_index_offset    = 0L,
    ignore_errors       = FALSE,
    schema              = NULL,
    row_index_name      = NULL,
    n_rows              = NULL,
    include_file_paths  = NULL
  )
)

.eager_readers <- list(
  csv     = function(p) .lazy_readers$csv(p)$collect(engine = "streaming"),
  parquet = function(p) .lazy_readers$parquet(p)$collect(engine = "streaming"),
  ndjson  = function(p) .lazy_readers$ndjson(p)$collect(engine = "streaming"),
  arrow   = function(p) polars:::PlRDataFrame$read_ipc_stream(
    source           = p$source,
    row_index_offset = 0L,
    rechunk          = p$rechunk,
    columns          = NULL,
    projection       = NULL,
    n_rows           = NULL,
    row_index_name   = NULL
  )
)

# ---- public readers ----

grl_read_csv <- function(input, sep = ",", mode = c("lazy", "chunk", "full"), header = TRUE) {
  mode <- match.arg(mode)
  p <- list(source = input, separator = sep, has_header = header, rechunk = TRUE)
  switch(mode,
    lazy  = { lf <- .lazy_readers$csv(p);  attr(lf, "schema") <- .get_schema(lf); lf },
    full  = { lf <- .lazy_readers$csv(p);  df <- .eager_readers$csv(p); attr(df, "schema") <- .get_schema(lf); df },
    chunk = { p$rechunk <- FALSE; lf <- .lazy_readers$csv(p); df <- .eager_readers$csv(p); attr(df, "schema") <- .get_schema(lf); df }
  )
}

grl_read_parquet <- function(input, mode = c("lazy", "chunk", "full")) {
  mode <- match.arg(mode)
  p <- list(source = input, rechunk = TRUE)
  switch(mode,
    lazy  = { lf <- .lazy_readers$parquet(p);  attr(lf, "schema") <- .get_schema(lf); lf },
    full  = { lf <- .lazy_readers$parquet(p);  df <- .eager_readers$parquet(p); attr(df, "schema") <- .get_schema(lf); df },
    chunk = { p$rechunk <- FALSE; lf <- .lazy_readers$parquet(p); df <- .eager_readers$parquet(p); attr(df, "schema") <- .get_schema(lf); df }
  )
}

grl_read_arrow <- function(input, mode = c("lazy", "chunk", "full")) {
  mode <- match.arg(mode)
  p <- list(source = input, rechunk = TRUE)
  switch(mode,
    lazy  = { lf <- .lazy_readers$arrow(p);  attr(lf, "schema") <- .get_schema(lf); lf },
    full  = { lf <- .lazy_readers$arrow(p);  df <- .eager_readers$arrow(p); attr(df, "schema") <- .get_schema(lf); df },
    chunk = { p$rechunk <- FALSE; lf <- .lazy_readers$arrow(p); df <- .eager_readers$arrow(p); attr(df, "schema") <- .get_schema(lf); df }
  )
}

grl_read_ndjson <- function(input, mode = c("lazy", "chunk", "full")) {
  mode <- match.arg(mode)
  p <- list(source = input, rechunk = TRUE)
  switch(mode,
    lazy  = { lf <- .lazy_readers$ndjson(p);  attr(lf, "schema") <- .get_schema(lf); lf },
    full  = { lf <- .lazy_readers$ndjson(p);  df <- .eager_readers$ndjson(p); attr(df, "schema") <- .get_schema(lf); df },
    chunk = { p$rechunk <- FALSE; lf <- .lazy_readers$ndjson(p); df <- .eager_readers$ndjson(p); attr(df, "schema") <- .get_schema(lf); df }
  )
}

grl_collect <- function(.data, engine = "streaming") {
  schema <- attr(.data, "schema")
  df <- .data$collect(engine = engine)
  attr(df, "schema") <- schema
  df
}