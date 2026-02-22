# writers_grolars.R
# Write PlRDataFrame / PlRLazyFrame to disk

# ================================================================
# internal: normalise to LazyFrame
# ================================================================

.to_lazy <- function(.data) {
  if (inherits(.data, "PlRLazyFrame")) return(.data)
  if (inherits(.data, "PlRDataFrame")) return(.data$lazy())
  stop("Expected PlRDataFrame or PlRLazyFrame")
}

# ================================================================
# grl_write_csv
# ================================================================

grl_write_csv <- function(.data,
                           path,
                           sep            = ",",
                           has_header     = TRUE,
                           batch_size     = 1024L,
                           datetime_format = NULL,
                           date_format     = NULL,
                           time_format     = NULL,
                           float_precision = NULL,
                           null_value      = "") {

  df <- if (inherits(.data, "PlRLazyFrame")) {
    .data$collect(engine = "streaming")
  } else {
    .data
  }

  polars:::PlRDataFrame_write_csv(df$.ptr)(
    path            = path,
    include_bom     = FALSE,
    include_header  = has_header,
    separator       = chartr(",", sep, ","),
    line_terminator = "\n",
    quote_char      = "\"",
    batch_size      = as.integer(batch_size),
    datetime_format = datetime_format,
    date_format     = date_format,
    time_format     = time_format,
    float_precision = float_precision,
    null_value      = null_value,
    quote_style     = "necessary"
  )
  invisible(.data)
}

# ================================================================
# grl_write_parquet
# ================================================================

grl_write_parquet <- function(.data,
                               path,
                               compression      = c("zstd", "snappy", "lz4", "gzip", "brotli", "uncompressed"),
                               compression_level = NULL,
                               row_group_size    = NULL,
                               stats             = TRUE) {

  compression <- match.arg(compression)
  lf <- .to_lazy(.data)

  polars:::PlRLazyFrame_sink_parquet(lf$.ptr)(
    path              = path,
    compression       = compression,
    compression_level = compression_level,
    statistics        = stats,
    row_group_size    = row_group_size,
    data_pagesize_limit = NULL,
    maintain_order    = TRUE,
    cloud_options     = NULL,
    credential_provider = NULL,
    retries           = 3L,
    sync_on_close     = "none",
    mkdir             = TRUE
  )$collect(engine = "streaming")

  invisible(.data)
}

# ================================================================
# grl_write_arrow  (IPC / feather v2)
# ================================================================

grl_write_arrow <- function(.data,
                             path,
                             compression = c("uncompressed", "lz4", "zstd")) {

  compression <- match.arg(compression)
  lf <- .to_lazy(.data)

  polars:::PlRLazyFrame_sink_ipc(lf$.ptr)(
    target          = path,
    compression     = compression,
    compat_level    = "newest",
    sync_on_close   = "none",
    maintain_order  = TRUE,
    mkdir           = TRUE,
    storage_options = NULL
  )$collect(engine = "streaming")

  invisible(.data)
}

# ================================================================
# grl_write_ndjson
# ================================================================

grl_write_ndjson <- function(.data, path) {

  df <- if (inherits(.data, "PlRLazyFrame")) {
    .data$collect(engine = "streaming")
  } else {
    .data
  }

  polars:::PlRDataFrame_write_ndjson(df$.ptr)(path = path)
  invisible(.data)
}