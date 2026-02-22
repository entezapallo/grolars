# print_utils.R
# Print methods and grl_resume for grolars objects

# ================================================================
# grl_resume
# ================================================================

grl_resume <- function(.data,
                        na_info    = c("count", "ratio", "percent"),
                        tibleshape = c("wide", "long")) {

  na_info    <- match.arg(na_info)
  tibleshape <- match.arg(tibleshape)

  df <- if (inherits(.data, "PlRLazyFrame")) {
    .data$collect(engine = "streaming")
  } else {
    .data
  }

  schema <- attr(.data, "schema")
  cols   <- gsub('"', '', schema)
  cols   <- cols[nchar(cols) > 0]
  nrows  <- df$height()
  ncols  <- length(cols)

  raw_schema_obj <- df$lazy()$collect_schema()

  dtypes <- sapply(cols, function(col) {
    ptr <- raw_schema_obj[[col]]
    tryCatch(
      .Call(polars:::savvy_PlRDataType_as_str__impl, ptr, FALSE),
      error = function(e) "unknown"
    )
  })

  na_lf  <- df$lazy()$null_count()$collect(engine = "streaming")
  na_vec <- sapply(cols, function(col) {
    tryCatch({
      s <- na_lf$get_column(col)          # ← fix: sin comillas
      polars:::PlRSeries_to_r_vector(s$.ptr)[[1]]
    }, error = function(e) NA_integer_)
  })

  all_clean <- all(na_vec == 0, na.rm = TRUE)
  has_na <- ifelse(is.na(na_vec), FALSE, na_vec > 0)

  na_display <- switch(na_info,
    count   = na_vec,
    ratio   = round(na_vec / nrows, 4),
    percent = paste0(round(na_vec / nrows * 100, 2), "%")
  )

 info <- if (tibleshape == "wide") {
    df_info <- data.frame(column = cols, dtype = dtypes, has_na = has_na, stringsAsFactors = FALSE)
    if (any(has_na)) df_info$na_info <- na_display
    df_info
     } else {
    wide <- data.frame(column = cols, dtype = dtypes, has_na = has_na, stringsAsFactors = FALSE)
    if (any(has_na)) wide$na_info <- na_display
    tdf <- as.data.frame(t(wide[-1]))
    colnames(tdf) <- cols
    rownames(tdf) <- names(wide)[-1]
    tdf
  }


  result <- list(shape = c(rows = nrows, cols = ncols), info_tibble = info)
  class(result) <- "grl_resume"
  result
}

print.grl_resume <- function(x, ...) {
  cat("\n  shape:", x$shape["rows"], "rows x", x$shape["cols"], "cols\n\n")
  print(x$info_tibble, row.names = TRUE)
  cat("\n")
  invisible(x)
}

# ================================================================
# print helpers
# ================================================================

.grl_head_tail <- function(df, n = 30) {
  nrows <- df$height()
  if (nrows <= n * 2) return(df)
  head_df <- df$slice(0L, as.integer(n))
  tail_df <- df$slice(as.integer(nrows - n), as.integer(n))
  polars:::concat_df(list(head_df, tail_df))
}

.grl_to_tibble <- function(df) {
  tmp <- tempfile(fileext = ".arrow")
  on.exit(unlink(tmp))
  
  lf <- df$lazy()$cast_all(polars:::PlRDataType$new_from_name("String"), strict = FALSE)
  
  polars:::PlRLazyFrame_sink_ipc(lf$.ptr)(
  target          = tmp,
  compression     = "uncompressed",
  compat_level    = "oldest",
  sync_on_close   = "none",
  maintain_order  = TRUE,
  mkdir           = TRUE,
  storage_options = NULL
)$collect(engine = "streaming")
  
  arrow::read_ipc_file(tmp) |> tibble::as_tibble()
}

# ================================================================
# print.PlRDataFrame
# ================================================================

print.PlRDataFrame <- function(x, type = c("tibble", "rust", "full_rust"), n = 30L, ...) {
  type <- match.arg(type)

  switch(type,
    tibble = {
      df_sub <- .grl_head_tail(x, n)
      nrows  <- x$height()
      cat("# PlRDataFrame:", nrows, "rows x", x$width(), "cols\n")
      tbl <- .grl_to_tibble(df_sub)
      if (!is.null(tbl)) print(tbl) else cat(x$as_str(), "\n")
      if (nrows > n * 2) cat("# ... showing head(", n, ") + tail(", n, ")\n", sep = "")
    },
    rust = {
      df_sub <- .grl_head_tail(x, n)
      cat(df_sub$as_str(), "\n")
      if (x$height() > n * 2) cat("# ... showing head(", n, ") + tail(", n, ")\n", sep = "")
    },
    full_rust = {
      cat(x$as_str(), "\n")
    }
  )
  invisible(x)
}

# ================================================================
# print.PlRLazyFrame
# ================================================================

print.PlRLazyFrame <- function(x, type = c("tibble", "rust", "full_rust"), n = 30L, ...) {
  type <- match.arg(type)
  schema <- attr(x, "schema")
  cols   <- gsub('"', '', schema[nchar(schema) > 2])

  cat("# PlRLazyFrame [lazy]\n")
  cat("# cols:", paste(cols, collapse = ", "), "\n")
  cat("# plan:\n")
  cat(x$describe_plan(), "\n")
  invisible(x)
}

# ================================================================
# print.PlRSeries
# ================================================================

print.PlRSeries <- function(x, type = c("tibble", "rust", "full_rust"), n = 30L, ...) {
  type <- match.arg(type)

  switch(type,
    tibble = {
      cat("# PlRSeries:", x$name(), "[", x$dtype()$as_str(), "]\n")
      vec <- polars:::PlRSeries_to_r_vector(x$.ptr)
      len <- length(vec)
      sub <- if (len > n * 2) c(head(vec, n), tail(vec, n)) else vec
      print(sub)
      if (len > n * 2) cat("# ... showing head(", n, ") + tail(", n, ")\n", sep = "")
    },
    rust = ,
    full_rust = {
      cat(x$as_str(), "\n")
    }
  )
  invisible(x)
}

# ================================================================
# register
# ================================================================

registerS3method("print", "PlRDataFrame",  print.PlRDataFrame)
registerS3method("print", "PlRLazyFrame",  print.PlRLazyFrame)
registerS3method("print", "PlRSeries",     print.PlRSeries)
registerS3method("print", "grl_resume",    print.grl_resume)