# bridge_grolars.R
# In-memory bridge: Polars <-> Arrow <-> R
# Requires: nanoarrow, arrow

# ================================================================
# internal: PlRDataFrame -> arrow Table (no disk)
# ================================================================

.plr_to_arrow <- function(df) {
  stream_ptr <- nanoarrow::nanoarrow_allocate_array_stream()
  polars:::PlRDataFrame_export_stream(df$.ptr)(stream_ptr)
  arrow::as_arrow_table(nanoarrow::as_nanoarrow_array_stream(stream_ptr))
}

# fallback si nanoarrow falla (usa tempfile con arrow)
.plr_to_arrow_fallback <- function(df) {
  tmp <- tempfile(fileext = ".arrow")
  on.exit(unlink(tmp))
  lf <- df$lazy()
  polars:::PlRLazyFrame_sink_ipc(lf$.ptr)(
    target          = tmp,
    compression     = "uncompressed",
    compat_level    = "oldest",
    sync_on_close   = "none",
    maintain_order  = TRUE,
    mkdir           = TRUE,
    storage_options = NULL
  )$collect(engine = "streaming")
  arrow::read_ipc_file(tmp)
}

.to_arrow_table <- function(df) {
  tryCatch(
    .plr_to_arrow(df),
    error = function(e) .plr_to_arrow_fallback(df)
  )
}

# ================================================================
# internal: R / tibble / matrix / arrow -> PlRDataFrame
# ================================================================

.r_to_plr <- function(x) {
  if (inherits(x, "ArrowTabular")) {
    stream_ptr <- nanoarrow::as_nanoarrow_array_stream(x)
    return(polars:::PlRDataFrame_import_stream(stream_ptr))
  }
  if (is.matrix(x)) x <- as.data.frame(x)
  # data.frame o tibble: via arrow en memoria
  tbl <- arrow::as_arrow_table(x)
  stream_ptr <- nanoarrow::as_nanoarrow_array_stream(tbl)
  polars:::PlRDataFrame_import_stream(stream_ptr)
}

# ================================================================
# public: as.data.frame / as_tibble / as_arrow_table
# ================================================================

as.data.frame.PlRDataFrame <- function(x, ...) {
  tbl <- .to_arrow_table(x)
  as.data.frame(tbl)
}

as.data.frame.PlRLazyFrame <- function(x, ...) {
  df <- x$collect(engine = "streaming")
  as.data.frame.PlRDataFrame(df)
}

as_tibble.PlRDataFrame <- function(x, ...) {
  tbl <- .to_arrow_table(x)
  tibble::as_tibble(tbl)
}

as_tibble.PlRLazyFrame <- function(x, ...) {
  df <- x$collect(engine = "streaming")
  as_tibble.PlRDataFrame(df)
}

as_arrow_table.PlRDataFrame <- function(x, ...) {
  .to_arrow_table(x)
}

as_arrow_table.PlRLazyFrame <- function(x, ...) {
  df <- x$collect(engine = "streaming")
  .to_arrow_table(df)
}

# ================================================================
# public: as.grolars_frame  (R / tibble / matrix / arrow -> PlR)
# ================================================================

as.grolars_frame <- function(x, mode = c("full", "lazy", "chunk")) {
  mode <- match.arg(mode)

  df <- tryCatch(
    .r_to_plr(x),
    error = function(e) stop("as.grolars_frame: cannot convert object of class '",
                              paste(class(x), collapse = "/"), "': ", conditionMessage(e))
  )

  schema <- .get_schema(df$lazy())
  attr(df, "schema") <- schema

  switch(mode,
    full  = df,
    chunk = { lf <- df$lazy(); attr(lf, "schema") <- schema; lf },
    lazy  = { lf <- df$lazy(); attr(lf, "schema") <- schema; lf }
  )
}

# ================================================================
# register S3
# ================================================================

registerS3method("as.data.frame", "PlRDataFrame", as.data.frame.PlRDataFrame)
registerS3method("as.data.frame", "PlRLazyFrame", as.data.frame.PlRLazyFrame)
registerS3method("as_tibble",     "PlRDataFrame", as_tibble.PlRDataFrame,     envir = asNamespace("tibble"))
registerS3method("as_tibble",     "PlRLazyFrame", as_tibble.PlRLazyFrame,     envir = asNamespace("tibble"))
registerS3method("as_arrow_table","PlRDataFrame", as_arrow_table.PlRDataFrame,envir = asNamespace("arrow"))
registerS3method("as_arrow_table","PlRLazyFrame", as_arrow_table.PlRLazyFrame,envir = asNamespace("arrow"))