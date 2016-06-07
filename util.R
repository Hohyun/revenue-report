library(RODBC)
library(ROracle)
library(nzr)  #nettenza R package
library(lubridate)
library(xts)
library(dplyr)
library(tidyr)

# this shoud be setup correctly befor execution
bi.dsn <- "ERPDB"
erp.id <- "rapps"
erp.pswd <- "zectxk4932"

# DB Connection String----------------------------------------------------------------------
## ERP connection string
drv <- dbDriver("Oracle")
erp.connect.string <- 
  "(DESCRIPTION = 
    (ADDRESS_LIST = 
      (LOAD_BALANCE = YES)
      (FAILOVER = YES)
      (ADDRESS = 
        (PROTOCOL = tcp)
        (HOST = erpprddb1_vip.koreanair.com)
        (PORT = 1601))
      (ADDRESS = 
        (PROTOCOL = tcp)
        (HOST = erpprddb2_vip.koreanair.com)
        (PORT = 1601)))
    (CONNECT_DATA = 
      (SERVICE_NAME = ERPPRD)))"

# Function definitions --------------------------------------------------------------

## DB: Revenue -----------------------------------------------------------------------
getRevByDay <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT TO_CHAR(BI_REPORT_DATE, 'YYYY-MM-DD') AS REPORT_DATE, CATEGORY, REV_KRW_100M
    FROM (
      SELECT BI_REPORT_DATE,
      CASE	
      WHEN AGENT_SALES_CATEGORY_CODE IN ('IP', 'DP') AND REVENUE_MANAGE_BI_REV_CATE_CD = '매표대금' THEN '1_PAX_S'	
      WHEN AGENT_SALES_CATEGORY_CODE IN ('IP', 'DP') AND REVENUE_MANAGE_BI_REV_CATE_CD = '운송수익' THEN '2_PAX_T'	
      WHEN AGENT_SALES_CATEGORY_CODE IN ('IC', 'DC') AND REVENUE_MANAGE_BI_REV_CATE_CD = '운송수익' THEN '3_CGO_T'	
      ELSE 'ERROR'	
      END CATEGORY,	
      ROUND(SUM(REVENUE_MANAGE_BI_REV_KRW_AMT) / 100000000, 8) REV_KRW_100M     
      FROM XXAR.XXAR_REV_STATUS_AGG2
      WHERE BI_REPORT_DATE BETWEEN TO_DATE('%s', 'YYYY-MM-DD') AND TO_DATE('%s', 'YYYY-MM-DD')	
      AND (REVENUE_MANAGE_BI_REV_CATE_CD = '매표대금' AND AGENT_SALES_CATEGORY_CODE IN ('IP', 'DP') OR	
           REVENUE_MANAGE_BI_REV_CATE_CD = '운송수익' AND AGENT_SALES_CATEGORY_CODE IN ('IP', 'IC', 'DP', 'DC'))	
      GROUP BY BI_REPORT_DATE, 
      CASE	
      WHEN AGENT_SALES_CATEGORY_CODE IN ('IP', 'DP') AND REVENUE_MANAGE_BI_REV_CATE_CD = '매표대금' THEN	'1_PAX_S'	
      WHEN AGENT_SALES_CATEGORY_CODE IN ('IP', 'DP') AND REVENUE_MANAGE_BI_REV_CATE_CD = '운송수익' THEN	'2_PAX_T'	
      WHEN AGENT_SALES_CATEGORY_CODE IN ('IC', 'DC') AND REVENUE_MANAGE_BI_REV_CATE_CD = '운송수익' THEN	'3_CGO_T'	
      ELSE 'ERROR'	
      END
    )	
    ORDER BY REPORT_DATE, CATEGORY",
    start.date, end.date
  )
  conn.erp <- dbConnect(drv, erp.id, erp.pswd, dbname=erp.connect.string)
  result <- dbGetQuery(conn.erp, sql_string)
  dbDisconnect(conn.erp)
  return(result)
}

getPaxSalesDom <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT ISO_CURRENCY_CODE CY, 
    SUM(NETT_AMOUNT + FUEL_SURCHARGE_LOCAL_AMOUNT) REVENUE
    FROM BIRA.BIRA_DOM_TKT_ISS_AGG1
    WHERE TICKET_TICKETING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD')
    GROUP BY ISO_CURRENCY_CODE
    ORDER BY ISO_CURRENCY_CODE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

getPaxTrafficDom <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT ISO_CURRENCY_CODE CY,
    SUM(PER_COUPON_NETT_AMOUNT) REVENUE
    FROM BIRA.BIRA_DOM_FLT_LV1_FCT
    WHERE ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD') 
    GROUP BY ISO_CURRENCY_CODE
    ORDER BY ISO_CURRENCY_CODE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

getPaxSalesInt <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT PAY_ISO_CURRENCY_CODE CY,
            ROUND(SUM(NETT_AMOUNT) + SUM(DECODE(FUEL_SURCHARGE_COLLECT_MTHD_CD, 
                                                'Y', FUEL_SURCHARGE_LOCAL_AMOUNT, 0)),2) as REVENUE
    FROM bira.bira_int_tkt_iss_lv1_fct
    WHERE TICKET_VOID_CATEGORY_CODE IS NULL
          AND INT_TICKET_NUMBER LIKE '180%%'
          AND TICKET_TICKETING_DATE BETWEEN  TO_DATE ('%s','YYYY-MM-DD') AND TO_DATE ('%s','YYYY-MM-DD')
    GROUP BY PAY_ISO_CURRENCY_CODE
    ORDER BY PAY_ISO_CURRENCY_CODE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

getPaxTrafficInt <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT ISO_CURRENCY_CODE CY,
    SUM(PER_COUPON_NETT_AMOUNT) AS REVENUE      
    FROM  BIRA.BIRA_INT_FLT_LV1_FCT 
    WHERE ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD')
    AND   DOMESTIC_INTERNATIONAL_CATE_CD ='I'  
    GROUP BY ISO_CURRENCY_CODE
    ORDER BY ISO_CURRENCY_CODE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

getCgoTrafficDom <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT ISO_CURRENCY_CODE CY, SUM(NETT_AMOUNT) REVENUE
    FROM BIRA.BIRA_INT_CGO_DOM_DAILY_REV_FCT 
    WHERE TRANSPORTATION_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD')
    AND (ACCOUNT_CODE LIKE '4%%' OR ACCOUNT_CODE LIKE'5%%') 
    GROUP BY ISO_CURRENCY_CODE
    ORDER BY ISO_CURRENCY_CODE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

getCgoTrafficInt <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT AWB_ISO_CURRENCY_CODE CY, 
    SUM(NET_AMOUNT + NORMAL_COMM_AMOUNT + ISC_AMOUNT + OR_COMM_AMOUNT) AS REVENUE                     
    FROM BIRA.BIRA_INT_CGO_MAIL_TRAN_AR_FCT
    WHERE ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD') 
    AND DOMESTIC_INTERNATIONAL_CATE_CD ='I'  
    GROUP BY AWB_ISO_CURRENCY_CODE
    ORDER BY AWB_ISO_CURRENCY_CODE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

## DB: Exchange Rate -----------------------------------------------------------------
getAvgExRate <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT FROM_ISO_CURRENCY_CODE AS FROM_CY, 
            AVG(ACCOUNTINGS_EXCHANGE_RATE) AS ACCT_RATE
     FROM BIRA.BIRA_GL_DAILY_RATES_DIM
     WHERE EXCHANGE_RATE_APPLICATE_DATE BETWEEN TO_DATE('%s', 'YYYY-MM-DD') AND TO_DATE('%s', 'YYYY-MM-DD')
     AND TO_ISO_CURRENCY_CODE = 'KRW'
     GROUP BY FROM_CY
     ORDER BY FROM_CY", 
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  temp <- nzQuery(sql_string)
  temp <- bind_rows(temp, data.frame(FROM_CY = "KRW", ACCT_RATE ="1.00"))
  arrange(temp, FROM_CY)
} 

## DB: FSC ---------------------------------------------------------------------------
getPaxFscDom <- function(start.date, end.date){  # PAX FSC DOM (BI)
  sql_string <- sprintf(
    "SELECT  SUM(VAT_EXCLUDE_FUEL_SURCHARGE_AMT)/100000000 AS PAX_DOM_FSC
     FROM    BIRA.BIRA_DOM_FLT_LV1_FCT
     WHERE   ACCOUNTING_DATE BETWEEN TO_DATE('%s', 'YYYY-MM-DD') AND TO_DATE('%s', 'YYYY-MM-DD')" ,
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  as.numeric(nzQuery(sql_string)[1][1])
}

getPaxFscInt <- function(start.date, end.date){  # PAX FSC INTL (BI)
  sql_string <- sprintf(
    "SELECT SUM(PRORATION_FSC_KRW_AMT)/100000000 AS PAX_INTL_FSC
     FROM   BIRA.BIRA_INT_FLT_LV1_FCT
     WHERE  ACCOUNTING_DATE BETWEEN TO_DATE('%s', 'YYYY-MM-DD') AND TO_DATE('%s', 'YYYY-MM-DD')" ,
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  result <- as.numeric(nzQuery(sql_string)[1][1])
}

getCgoFscDom <- function(start.date, end.date) {  # CGO FSC DOM (ERP)
  sql_string <- sprintf(
    "SELECT    
    (FLOOR (SUM
    (CASE WHEN AGENT_TYPE_CODE = 'D' 
    THEN TRUNC (((NVL(OIL_PREMIUM_AMOUNT, 0) )/ 100) * 100) 
    * NVL((SELECT NVL(gdr.CONVERSION_RATE, 1)
    FROM GL_DAILY_RATES gdr
    WHERE gdr.FROM_CURRENCY(+) = CURRENCY_CODE AND gdr.TO_CURRENCY(+) = 'KRW'
    AND gdr.CONVERSION_TYPE(+) = '1000'   
    AND gdr.CONVERSION_DATE(+) = TO_DATE(ACCT_D, 'YYYYMMDD')), 1)
    ELSE ((NVL(OIL_PREMIUM_AMOUNT, 0)
    * NVL((SELECT NVL(gdr.CONVERSION_RATE, 1)
    FROM GL_DAILY_RATES  gdr
    WHERE gdr.FROM_CURRENCY(+) = CURRENCY_CODE AND gdr.TO_CURRENCY(+) = 'KRW'
    AND gdr.CONVERSION_TYPE(+) = '1000'   
    AND gdr.CONVERSION_DATE(+) = TO_DATE(ACCT_D, 'YYYYMMDD')), 1)))                            
    END)))/100000000 AS KRW_FSC              
    FROM XXAR_INT_CGO_DOM_FLIGHT_II      
    WHERE ACCT_D BETWEEN '%s' AND '%s' 
    AND PROCESS_FLAG ='PROCESSED'",
    gsub("-", "", start.date), gsub("-", "", end.date)
  )
  conn.erp <- dbConnect(drv, erp.id, erp.pswd, dbname=erp.connect.string)
  result <- as.numeric(dbGetQuery(conn.erp, sql_string)[1][1])
  dbDisconnect(conn.erp)
  return(result)
}

getCgoFscInt <- function(start.date, end.date){  # CGO FSC INTL (BI)
  sql_string <- sprintf(
    "SELECT SUM(FUEL_SURCHARGE_KRW_AMOUNT)/100000000 AS CGO_INTL_FSC        
     FROM   BIRA.BIRA_INT_CGO_MAIL_TRAN_AR_FCT
     WHERE  ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD') 
     AND DOMESTIC_INTERNATIONAL_CATE_CD ='I'" ,
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  as.numeric(nzQuery(sql_string)[1][1])
}

## DB: PAX Count ------------------------------------------------------------------------
getPaxCntDom <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT TO_CHAR(ACCOUNTING_DATE, 'YYYY-MM-DD') AS DATE, SUM(PASSENGERS_COUNT)/10000 PAX_Q_DOM 
    FROM BIRA.BIRA_DOM_FLT_LV1_FCT A
    WHERE ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD')
    GROUP BY ACCOUNTING_DATE
    ORDER BY ACCOUNTING_DATE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

getPaxCntInt <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT TO_CHAR(ACCOUNTING_DATE, 'YYYY-MM-DD') AS DATE, SUM(PASSENGERS_COUNT)/10000 PAX_Q_INT 
    FROM BIRA.BIRA_INT_FLT_LV1_FCT A
    WHERE ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD')
    GROUP BY ACCOUNTING_DATE
    ORDER BY ACCOUNTING_DATE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

## DB: CGO WT ---------------------------------------------------------------------------
getCgoWtDom <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT ACCT_D, SUM(CHARGE_WEIGHT)/10000000 AS CGO_WT_DOM
    FROM XXAR_INT_CGO_DOM_FLIGHT_II
    WHERE ACCT_D BETWEEN TO_DATE('%s', 'YYYY-MM-DD') AND TO_DATE('%s', 'YYYY-MM-DD')
    AND PROCESS_FLAG ='PROCESSED'
    GROUP BY ACCT_D
    ORDER BY ACCT_D",
    start.date, end.date
  )
  conn.erp <- dbConnect(drv, erp.id, erp.pswd, dbname=erp.connect.string)
  result <- dbGetQuery(conn.erp, sql_string)
  dbDisconnect(conn.erp)
  return(result)
}

getCgoWtInt <- function(start.date, end.date){
  sql_string <- sprintf(
    "SELECT TO_CHAR(ACCOUNTING_DATE, 'YYYY-MM-DD') AS DATE,
    SUM(TRANSPORTATION_BIZ_VOL_VLU)/10000000 AS CGO_WT_INT                                            
    FROM BIRA.BIRA_INT_CGO_MAIL_TRAN_AR_FCT
    WHERE ACCOUNTING_DATE BETWEEN TO_DATE('%s','YYYY-MM-DD') AND TO_DATE('%s','YYYY-MM-DD')
    AND DOMESTIC_INTERNATIONAL_CATE_CD ='I'
    GROUP BY ACCOUNTING_DATE
    ORDER BY ACCOUNTING_DATE",
    start.date, end.date
  )
  if (!nzIsConnected()) {
    nzConnectDSN(bi.dsn, verbose = FALSE)  
  }
  nzQuery(sql_string)
}

## Data constructing functions -------------------------------------------------
getAccumulatedRevenue <- function(s_date, e_date){
  temp <- rev_daily[paste(s_date, e_date, sep = "/")]
  apply(temp, 2, sum)
}

getAccumulatedPaxCount <- function(s_date, e_date){
  temp <- pax_count_dom[paste(s_date, e_date, sep = "/")]
  dom <- apply(temp, 2, sum)
  temp <- pax_count_int[paste(s_date, e_date, sep = "/")]
  int <- apply(temp, 2, sum)
  dom + int
}

getAccumulatedCgoWt <- function(s_date, e_date){
  temp <- cgo_wt_dom[paste(s_date, e_date, sep = "/")]
  dom <- apply(temp, 2, sum)
  temp <- cgo_wt_int[paste(s_date, e_date, sep = "/")]
  int <- apply(temp, 2, sum)
  dom + int
}

make_revenue_df_all <- function(){
  mtd_all     = c(rev_mtd_pax, rev_mtd_cgo, rev_mtd_pax + rev_mtd_cgo)
  mtd_all_prv = c(rev_mtd_pax_prv, rev_mtd_cgo_prv, rev_mtd_pax_prv + rev_mtd_cgo_prv)
  ytd_all     = c(rev_ytd_pax, rev_ytd_cgo, rev_ytd_pax + rev_ytd_cgo)
  ytd_all_prv = c(rev_ytd_pax_prv, rev_ytd_cgo_prv, rev_ytd_pax_prv + rev_ytd_cgo_prv)
  mtd_all     = round(mtd_all,0)
  mtd_all_prv = round(mtd_all_prv,0)
  ytd_all     = round(ytd_all,0)
  ytd_all_prv = round(ytd_all_prv,0)
  
  data.frame(pc = c("PAX", "CGO", "TTL"),
             mtd = mtd_all, mtd_prv = mtd_all_prv, mtd_diff = mtd_all - mtd_all_prv, 
             mtd_pct = mtd_all / mtd_all_prv,
             ytd = ytd_all, ytd_prv = ytd_all_prv, ytd_diff = ytd_all - ytd_all_prv,
             ytd_pct = ytd_all / ytd_all_prv,
             row.names = NULL)
}

make_revenue_df_net <- function(){
  mtd_net     = c(rev_mtd_pax_net, rev_mtd_cgo_net, rev_mtd_pax_net + rev_mtd_cgo_net)
  mtd_net_prv = c(rev_mtd_pax_net_prv, rev_mtd_cgo_net_prv, rev_mtd_pax_net_prv + rev_mtd_cgo_net_prv)
  ytd_net     = c(rev_ytd_pax_net, rev_ytd_cgo_net, rev_ytd_pax_net + rev_ytd_cgo_net)
  ytd_net_prv = c(rev_ytd_pax_net_prv, rev_ytd_cgo_net_prv, rev_ytd_pax_net_prv + rev_ytd_cgo_net_prv)
  mtd_net     = round(mtd_net,0)
  mtd_net_prv = round(mtd_net_prv, 0)
  ytd_net     = round(ytd_net, 0)
  ytd_net_prv = round(ytd_net_prv, 0)
  
  data.frame(pc = c("PAX", "CGO", "TTL"),
             mtd = mtd_net, mtd_prv = mtd_net_prv, mtd_diff = mtd_net - mtd_net_prv,
             mtd_pct = mtd_net / mtd_net_prv,
             ytd = ytd_net, ytd_prv = ytd_net_prv, ytd_diff = ytd_net - ytd_net_prv,
             ytd_pct = ytd_net / ytd_net_prv,
             row.names = NULL) 
}

make_revenue_df_month_all <- function(){
  mtd_all     = c(rev_mtd_pax, rev_mtd_cgo, rev_mtd_pax + rev_mtd_cgo)
  mtd_all1    = c(rev_mtd_pax_t1, rev_mtd_cgo_t1, rev_mtd_pax_t1 + rev_mtd_cgo_t1)
  mtd_all_prv = c(rev_mtd_pax_prv, rev_mtd_cgo_prv, rev_mtd_pax_prv + rev_mtd_cgo_prv)
  
  data.frame(pc = c("PAX", "CGO", "TTL"),
             this = round(mtd_all, 0), last = round(mtd_all_prv, 0), 
             diff = round(mtd_all - mtd_all_prv, 0), 
             pct =  round(mtd_all / mtd_all_prv, 3), 
             index = round(mtd_all1 / mtd_all_prv, 3),
             row.names = NULL)
}

make_revenue_df_month_net <- function(){
  mtd_net     = c(rev_mtd_pax_net, rev_mtd_cgo_net, rev_mtd_pax_net + rev_mtd_cgo_net)
  mtd_net_prv = c(rev_mtd_pax_net_prv, rev_mtd_cgo_net_prv, rev_mtd_pax_net_prv + rev_mtd_cgo_net_prv)

  data.frame(pc = c("PAX", "CGO", "TTL"),
             this = round(mtd_net, 0), last = round(mtd_net_prv, 0), 
             diff = round(mtd_net - mtd_net_prv, 0),
             pct  = round(mtd_net / mtd_net_prv, 3),
             row.names = NULL) 
}

make_revenue_df_year_all <- function(){
  ytd_all     = c(rev_ytd_pax, rev_ytd_cgo, rev_ytd_pax + rev_ytd_cgo)
  ytd_all1    = c(rev_ytd_pax_t1, rev_ytd_cgo_t1, rev_ytd_pax_t1 + rev_ytd_cgo_t1)
  ytd_all_prv = c(rev_ytd_pax_prv, rev_ytd_cgo_prv, rev_ytd_pax_prv + rev_ytd_cgo_prv)
  
  data.frame(pc = c("PAX", "CGO", "TTL"),
             this = round(ytd_all, 0), last = round(ytd_all_prv, 0), 
             diff = round(ytd_all - ytd_all_prv, 0),
             pct  = round(ytd_all / ytd_all_prv, 3),
             index = round(ytd_all1 / ytd_all_prv, 3),
             row.names = NULL)
}

make_revenue_df_year_net <- function(){
  ytd_net     = c(rev_ytd_pax_net, rev_ytd_cgo_net, rev_ytd_pax_net + rev_ytd_cgo_net)
  ytd_net_prv = c(rev_ytd_pax_net_prv, rev_ytd_cgo_net_prv, rev_ytd_pax_net_prv + rev_ytd_cgo_net_prv)
  
  data.frame(pc = c("PAX", "CGO", "TTL"),
             this = round(ytd_net, 0), last = round(ytd_net_prv, 0), 
             diff = round(ytd_net - ytd_net_prv, 0),
             pct  = round(ytd_net / ytd_net_prv, 3),
             row.names = NULL) 
}

make_revenue_df_week <- function(){
  wtd     = c(rev_wtd_pax_s, rev_wtd_pax_t, rev_wtd_cgo_t)
  wtd1    = c(rev_wtd_pax_s1, rev_wtd_pax_t1, rev_wtd_cgo_t1)
  wtd_prv = c(rev_wtd_pax_s_prv, rev_wtd_pax_t_prv, rev_wtd_cgo_t_prv)
  
  data.frame(gubun = c("PAX_S", "PAX_T", "CGO_T"),
             this = round(wtd, 0), last = round(wtd_prv, 0), diff = round(wtd - wtd_prv, 0), 
             pct = round(wtd / wtd_prv, 3), index = round(wtd1 / wtd_prv, 3), row.names = NULL)
}

make_cnt_wt_df_week <- function(){
  wtd     = c(pax_count, cgo_wt)
  wtd1    = c(pax_count, cgo_wt)
  wtd_prv = c(pax_count_prv, cgo_wt_prv)
  
  data.frame(gubun = c("PAX_Q", "CGO_WT"),
             this = round(wtd, 2), last = round(wtd_prv, 2), diff = round(wtd - wtd_prv, 2), 
             pct = round(wtd / wtd_prv, 3), row.names = NULL)
}

make_source_df_week <- function(){
  xts_obj <- cbind(round(rev_weekly_pax_s), round(rev_weekly_pax_t), 
                   round(rev_weekly_cgo_t), round(rev_weekly_count, 1), 
                   round(rev_weekly_wt, 2))
  names(xts_obj) <- c("PAX_S_REV", "PAX_T_REV","CGO_T_REV", "PAX_CNT", "CGO_WT")
  data.frame(DATE = index(xts_obj), xts_obj, row.names = NULL)
}

make_source_df_month <- function(){
  xts_obj <- cbind(round(rev_monthly_pax_s), round(rev_monthly_pax_t), 
                   round(rev_monthly_cgo_t), round(rev_monthly_count, 1), 
                   round(rev_monthly_wt, 2))
  names(xts_obj) <- c("PAX_S_REV", "PAX_T_REV","CGO_T_REV", "PAX_CNT", "CGO_WT")
  data.frame(DATE = index(xts_obj), xts_obj, row.names = NULL)
}

## Utility functions ---------------------------------------------------
endpointsOnSaturday <- function(x){
  l = dim(x)[1]
  start_yoil = wday(x[1])
  if (start_yoil == 1) { #sunday
    first_sat = start_yoil + 6  
  } else {
    first_sat = 7 - start_yoil + 1
  }
  saturdays = seq(first_sat, l-1, 7)
  c(0, saturdays, l)
}

progressMessage <- function(n, work_steps, msg){
  cat(sprintf("%2d/%2d: %-45s ", n, work_steps, msg))
}

start.time <- proc.time()
lap.time <- proc.time()

display.lap.time <- function(){
  check.time <- proc.time()
  step.time <- (check.time - lap.time)[3]
  cat(sprintf("(%.2f seconds)\n", step.time))
}

myDiff <-  function(a, b, round_digit) {
  diff <- round(a - b, round_digit)
  if (diff >= 0) {
    paste0("+", format(diff, digit = 5, nsmall = 0, big.mark = ","))
  } else {
    format(diff, digit = 5, nsmall = 0, big.mark = ",")
  }
}

myIcon <- function(a,b) {
  rate <- a / b
  if (rate >= 1) {
    icon("sun-o")
  } else if (rate >= 0.9) {
    icon("cloud")
  } else {
    icon("umbrella")
  }
}

myColor <- function(a, b) {
  rate <- a / b
  if (rate >= 1) {
    "green"
  } else if (rate >= 0.9) {
    "yellow"
  } else {
    "red"
  }
}

myFormat <- function(amt) {
  format(amt, digit = 5, nsmall = 0, big.mark = ",")
}

shade_regions_weekly <- function(xts_ojb, n) {
  l <- length(xts_ojb[,1])
  week <- seq(l, n+1, -52)
  from <- week - n
  to <- week + n
  df <- data.frame(week, from, to)
  df[1,3] <- df[1,1]
  df
}

shade_regions_monthly <- function(xts_ojb, n) {
  l <- length(xts_ojb[,1])
  week <- seq(l, n+1, -12)
  from <- week - n
  to <- week + n
  df <- data.frame(week, from, to)
  df[1,3] <- df[1,1]
  df
}

myDateFormat <- function(d1, d2) {
  from_d = paste0(month(d1), "/", day(d1))
  to_d   = paste0(month(d2), "/", day(d2))
  paste0(from_d, "~", to_d)
}

