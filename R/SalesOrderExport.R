# =====================================================================
# SalesOrder - Export tab: data access
#   Source view: rds_erp_vw_rpt_PNSerialNoTrace
#   The filter conditions built on the Export tab are appended to the query,
#   so the filtering happens in the database.
# =====================================================================

#' Field list used by the Export tab
#'   label      : result column name
#'   expr       : expression in the SELECT list
#'   where_expr : expression used by the WHERE clause (defaults to expr)
#'   type       : text / number / date
#' @export
SalesOrderExport_fields <- function() {
  data.frame(
    label = c("Sales OrderID", "PN", "Product Name", "Serial Number",
              "Delivery Date", "Delivery Location", "Production Date",
              "Sales OrderQty", "Total Delivery Qty"),
    expr = c("[FSalesOrderID]", "[FPN]", "[FProductName]", "[FSerialNumber]",
             "cast([FDeliveryDate] as date)", "[FDeliveryLocation]",
             "cast([FProductionDate] as date)", "[FSalesOrderQty]",
             "[FTotalDeliveryQty]"),
    where_expr = c("[FSalesOrderID]", "[FPN]", "[FProductName]", "[FSerialNumber]",
                   "cast([FDeliveryDate] as date)", "[FDeliveryLocation]",
                   "cast([FProductionDate] as date)", "[FSalesOrderQty]",
                   "[FTotalDeliveryQty]"),
    type = c("text", "text", "text", "text",
             "date", "text", "date",
             "number", "number"),
    stringsAsFactors = FALSE
  )
}

#' Quote a SQL Server string literal
#' @export
SalesOrderExport_sql_string <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

#' WHERE built from the filter rows
#' @export
SalesOrderExport_where <- function(FWhere = "") {
  if (is.null(FWhere) || !nzchar(FWhere)) return("")
  paste0("(", FWhere, ")")
}

#' Query used by the Export tab
#' @export
SalesOrderExport_select_sql <- function(FWhere = "", cols = NULL) {
  meta <- SalesOrderExport_fields()
  if (!is.null(cols) && length(cols)) {
    meta <- meta[meta$label %in% cols, , drop = FALSE]
    if (!nrow(meta)) meta <- SalesOrderExport_fields()
  }
  sel <- paste(paste0(meta$expr, " as [", meta$label, "]"), collapse = ",
       ")
  sql <- paste0("select ", sel, "
  from rds_erp_vw_rpt_PNSerialNoTrace")
  w <- SalesOrderExport_where(FWhere = FWhere)
  if (nzchar(w)) sql <- paste0(sql, "
 where ", w)
  sql
}

#' Run the export query
#' @export
SalesOrderExport_select <- function(erp_token, FWhere = "", cols = NULL) {
  sql <- SalesOrderExport_select_sql(FWhere = FWhere, cols = cols)
  res <- tsda::sql_select2(token = erp_token, sql = sql)
  res
}
