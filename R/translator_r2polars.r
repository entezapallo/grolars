# translator_r2polars.R

.grl_env <- new.env(parent = emptyenv())

.grl_env$.col <- function(name) polars:::col(name)  # name = '"Sepal.Length"' ya con comillass

.grl_env$.lit <- function(val) polars:::lit_from_series(
  polars:::PlRSeries$new_f64("", val),
  keep_series = FALSE,
  keep_name   = FALSE
)

.grl_env$.ops <- list(
  "+"  = function(l, r) polars:::PlRExpr_add(l$.ptr)(r),
  "-"  = function(l, r) polars:::PlRExpr_sub(l$.ptr)(r),
  "*"  = function(l, r) polars:::PlRExpr_mul(l$.ptr)(r),
  "/"  = function(l, r) polars:::PlRExpr_div(l$.ptr)(r),
  "^"  = function(l, r) polars:::PlRExpr_pow(l$.ptr)(r),
  ">"  = function(l, r) polars:::PlRExpr_gt(l$.ptr)(r),
  ">=" = function(l, r) polars:::PlRExpr_gt_eq(l$.ptr)(r),
  "<"  = function(l, r) polars:::PlRExpr_lt(l$.ptr)(r),
  "<=" = function(l, r) polars:::PlRExpr_lt_eq(l$.ptr)(r),
  "==" = function(l, r) polars:::PlRExpr_eq(l$.ptr)(r),
  "!=" = function(l, r) polars:::PlRExpr_neq(l$.ptr)(r),
  "&"  = function(l, r) polars:::PlRExpr_and(l$.ptr)(r),
  "|"  = function(l, r) polars:::PlRExpr_or(l$.ptr)(r)
)

.grl_env$.fns <- list(
  sqrt  = function(x) polars:::PlRExpr_pow(x$.ptr)(.grl_env$.lit(0.5)),
  abs   = function(x) polars:::PlRExpr_abs(x$.ptr)(),
  log   = function(x) polars:::PlRExpr_log(x$.ptr)(.grl_env$.lit(exp(1))),
  log2  = function(x) polars:::PlRExpr_log(x$.ptr)(.grl_env$.lit(2)),
  log10 = function(x) polars:::PlRExpr_log(x$.ptr)(.grl_env$.lit(10)),
  exp   = function(x) polars:::PlRExpr_exp(x$.ptr)(),
  mean  = function(x) polars:::PlRExpr_mean(x$.ptr)(),
  sum   = function(x) polars:::PlRExpr_sum(x$.ptr)(),
  min   = function(x) polars:::PlRExpr_min(x$.ptr)(),
  max   = function(x) polars:::PlRExpr_max(x$.ptr)(),
  n     = function(x) polars:::PlRExpr_count(x$.ptr)(),
  sd    = function(x) polars:::PlRExpr_std(x$.ptr)(ddof = 1L),
  var   = function(x) polars:::PlRExpr_var(x$.ptr)(ddof = 1L),
  ceil  = function(x) polars:::PlRExpr_ceil(x$.ptr)(),
  floor = function(x) polars:::PlRExpr_floor(x$.ptr)(),
  round = function(x, digits = 0L) polars:::PlRExpr_round(x$.ptr)(decimals = as.integer(digits)),
  is.na = function(x) polars:::PlRExpr_is_null(x$.ptr)()
)


.grl_env$.r2polars <- function(expr, schema) {
  ops <- .grl_env$.ops
  fns <- .grl_env$.fns

  if (is.symbol(expr)) {
      nm <- as.character(expr)
      real_nm <- .match_col(nm, schema)
      return(.grl_env$.col(real_nm))}
  if (is.numeric(expr) || is.logical(expr) || is.character(expr)) {
    return(.grl_env$.lit(expr))
  }
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    if (fn %in% names(ops) && length(expr) == 3) {
      l <- .grl_env$.r2polars(expr[[2]], schema)
      r <- .grl_env$.r2polars(expr[[3]], schema)
      return(ops[[fn]](l, r))
    }
    if (fn == "!" && length(expr) == 2) {
      x <- .grl_env$.r2polars(expr[[2]], schema)
      return(polars:::PlRExpr_not(x$.ptr)())
    }
    if (fn %in% names(fns)) {
      args <- lapply(as.list(expr)[-1], .grl_env$.r2polars, schema = schema)
      return(do.call(fns[[fn]], args))
    }
    stop("Function '", fn, "' not supported in grolars expressions")
  }
  stop("Unsupported expression type: ", class(expr))
}

.quo2expr <- function(quo, schema) {
  .grl_env$.r2polars(rlang::get_expr(quo), schema)
}

.quos2named_exprs <- function(quos, schema) {
  lapply(seq_along(quos), function(i) {
    expr <- .quo2expr(quos[[i]], schema)
    nm   <- names(quos)[i]
    if (!is.null(nm) && nchar(nm) > 0) polars:::PlRExpr_alias(expr$.ptr)(nm) else expr
  })
}