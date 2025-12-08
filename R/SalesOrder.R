
#' 查询SalesOrder_select
#'
#' @param erp_token

#' @return 无返回值
#' @export
#'
#' @examples
#' SalesOrder_select()
SalesOrder_select<- function(erp_token,FSalesOrderID) {

  sql=paste0("
  select * from rds_erp_vw_rpt_PNSerialNoTrace where FSalesOrderID='",FSalesOrderID,"'
             ")

  res=tsda::sql_select2(token = erp_token,sql = sql)
  return(res)
}
