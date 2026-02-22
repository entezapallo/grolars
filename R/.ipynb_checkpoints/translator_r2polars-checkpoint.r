# translator_r2polars.R
# Translates R expressions to PlRExpr (polars expressions)
# Used internally by grolars verbs (select, filter, mutate, etc.)

# ---- atom translators ----

.col <- function(name) polars:::col(name)

.lit <- function(val) polars:::lit_from_series(
  polars:::PlRSeries$new_f64("", val)
)

# ---- binary operator map ----

.ops <- list(
  "+"  = function(l, r) l$add(r),
  "-"  = function(l, r) l$sub(r),
  "*"  = function(l, r) l$mul(r),
  "/"  = function(l, r) l$div(r),
  "^"  = function(l, r) l$pow(r),
  ">"  = function(l, r) l$gt(r),
  ">=" = function(l, r) l$gt_eq(r),
  "<"  = function(l, r) l$lt(r),
  "<=" = function(l, r) l$lt_eq(r),
  "==" = function(l, r) l$eq(r),
  "!=" = function(l, r) l$neq(r),
  "&"  = function(l, r) l$and(r),
  "|"  = function(l, r) l$or(r)
)

# ---- function map ----

.fns <- list(
  sqrt  = function(x) x$pow(.lit(0.5)),
  abs   = function(x) x$abs(),
  log   = function(x) x$log(.lit(exp(1))),
  log2  = function(x) x$log(.lit(2)),
  log10 = function(x) x$log(.lit(10)),
  exp   = function(x) x$exp(),
  mean  = function(x) x$mean(),
  sum   = function(x) x$sum(),
  min   = function(x) x$min(),
  max   = function(x) x$max(),
  n     = function(x) x$count(),
  sd    = function(x) x$std(ddof = 1L),
  var   = function(x) x$var(ddof = 1L),
  ceil  = function(x) x$ceil(),
  floor = function(x) x$floor(),
  round = function(x, digits = 0L) x$round(decimals = as.integer(digits)),
  is.na = function(x) x$is_null()
)
# ---- main translator ----

# Recursively translates an R expression (as AST) to a PlRExpr
.r2polars <- function(expr, schema) {
  
  # symbol → col
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% schema) return(.col(nm))
    stop("Column '", nm, "' not found in schema: ", paste(schema, collapse=", "))
  }
  
  # literal
  if (is.numeric(expr) || is.logical(expr) || is.character(expr)) {
    return(.lit(expr))
  }
  
  # call
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    
    # binary operators
    if (fn %in% names(.ops) && length(expr) == 3) {
      l <- .r2polars(expr[[2]], schema)
      r <- .r2polars(expr[[3]], schema)
      return(.ops[[fn]](l, r))
    }
    
    # unary !
    if (fn == "!" && length(expr) == 2) {
      x <- .r2polars(expr[[2]], schema)
      return(x$not())
    }
    
    # known functions
    if (fn %in% names(.fns)) {
      args <- lapply(as.list(expr)[-1], .r2polars, schema = schema)
      return(do.call(.fns[[fn]], args))
    }
    
    stop("Function '", fn, "' not supported in grolars expressions")
  }
  
  stop("Unsupported expression type: ", class(expr))
}

# ---- public interface ----

# Translates a quosure to PlRExpr
.quo2expr <- function(quo, schema) {
  .r2polars(rlang::get_expr(quo), schema)
}

# Translates named quosures to named list of PlRExpr (for mutate)
# e.g. mutate(col3 = col2 * sqrt(col1))
.quos2named_exprs <- function(quos, schema) {
  lapply(seq_along(quos), function(i) {
    expr <- .quo2expr(quos[[i]], schema)
    nm   <- names(quos)[i]
    if (!is.null(nm) && nchar(nm) > 0) expr$alias(nm) else expr
  })
}