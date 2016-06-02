
# Load functions
source("util.R", encoding = "UTF-8")

# Setup dates

as_of_date <- today() - wday(today())     ## If you want, chage this date

wtd_e_date <- as_of_date
wtd_s_date <- wtd_e_date - 6               
wtd_prv_e_date <- wtd_e_date - weeks(52)   
wtd_prv_s_date <- wtd_prv_e_date - 6       

mtd_s_date <- as_of_date - mday(as_of_date) + 1  # dates for monthly view
mtd_e_date <- as_of_date
mtd_prv_s_date <- mtd_s_date - years(1)    
mtd_prv_e_date <- mtd_e_date - years(1)

ytd_s_date <- as_of_date - yday(as_of_date) + 1  # date for yearly view
ytd_e_date <- as_of_date
ytd_prv_s_date <- ytd_s_date - years(1)    
ytd_prv_e_date <- ytd_e_date - years(1)

data_s_date <- ytd_s_date - years(3)        # for daily revenue table
data_e_date <- as_of_date

steps = 10
jindo = c(0.05, 0.05, 0.2, 0.3, 0.02, 0.02, 0.13, 0.13, 0.05, 0.05)

# Fetch data from DB ----------------------------------------------------------------
## BI PDA open
nzConnectDSN(bi.dsn, verbose = FALSE, force = TRUE)
#nzConnect('kalbira', 'KALBIRA', '10.86.19.65', 'ERPDWPRD', verbose = FALSE)

## Fetch data: Daily Revenue Table -------------------------------------------------
cat("\n\nStarting data preparation ...\n")
progressMessage(1, steps, "Fetching revenue & exchange rate ...")
incProgress(jindo[1], detail = sprintf("%d. revenue & exchange rate", 1))

rev_daily <- getRevByDay(data_s_date, data_e_date)
rev_daily <- spread(rev_daily, CATEGORY, REV_KRW_100M)
rev_daily  <- xts(rev_daily[,-1], as.Date(rev_daily[,1]))
## Fetch data: Exchange Rate Table ------------------------------------------------------------------------
## last year same week average rate
ex_rate_week  <- getAvgExRate(wtd_prv_s_date, wtd_prv_e_date)
ex_rate_month <- getAvgExRate(mtd_prv_s_date, mtd_prv_e_date)
ex_rate_year  <- getAvgExRate(ytd_prv_s_date, ytd_prv_e_date)
display.lap.time(); lap.time <- proc.time()


## Fetch data: PAX FSC -------------------------------------------------------------------------------------
progressMessage(2, steps, "Fetching PAX fuel surcharge: domestic ...")
incProgress(jindo[2], detail = sprintf("%d. PAX FSC domestic", 2))

## Domestic
pax_fsc_dom_month     <- getPaxFscDom(mtd_s_date, mtd_e_date)
pax_fsc_dom_year      <- getPaxFscDom(ytd_s_date, ytd_e_date)
pax_fsc_dom_month_prv <- getPaxFscDom(mtd_prv_s_date, mtd_prv_e_date)
pax_fsc_dom_year_prv  <- getPaxFscDom(ytd_prv_s_date, ytd_prv_e_date)
display.lap.time(); lap.time <- proc.time()

## Int'l
progressMessage(3, steps, "Fetching PAX fuel surcharge: int'l ...")
incProgress(jindo[3], detail = sprintf("%d. PAX FSC int'l", 3))
pax_fsc_int_month     <- getPaxFscInt(mtd_s_date, mtd_e_date)
pax_fsc_int_year      <- getPaxFscInt(ytd_s_date, ytd_e_date)
pax_fsc_int_month_prv <- getPaxFscInt(mtd_prv_s_date, mtd_prv_e_date)
pax_fsc_int_year_prv  <- getPaxFscInt(ytd_prv_s_date, ytd_prv_e_date)

## sum
pax_fsc_month         <- pax_fsc_dom_month + pax_fsc_int_month
pax_fsc_year          <- pax_fsc_dom_year  + pax_fsc_int_year
pax_fsc_month_prv     <- pax_fsc_dom_month_prv + pax_fsc_int_month_prv
pax_fsc_year_prv      <- pax_fsc_dom_year_prv + pax_fsc_int_year_prv
display.lap.time(); lap.time <- proc.time()

## Fetch data: CGO FSC --------------------------------------------------------------
progressMessage(4, steps, "Fetching CGO fuel surcharge: domestic ...")
incProgress(jindo[4], detail = sprintf("%d. CGO FSC domestic", 4))
## Domestic
cgo_fsc_dom_month <- getCgoFscDom(mtd_s_date, mtd_e_date)
cgo_fsc_dom_year <- getCgoFscDom(ytd_s_date, ytd_e_date)
cgo_fsc_dom_month_prv <- getCgoFscDom(mtd_prv_s_date, mtd_prv_e_date)
cgo_fsc_dom_year_prv  <- getCgoFscDom(ytd_prv_s_date, ytd_prv_e_date)
display.lap.time(); lap.time <- proc.time()

## Int'l
progressMessage(5, steps, "Fetching CGO fuel surcharge: domestic ...")
incProgress(jindo[5], detail = sprintf("%d. CGO FSC Int'l", 5))
cgo_fsc_int_month <- getCgoFscInt(mtd_s_date, mtd_e_date)
cgo_fsc_int_year  <- getCgoFscInt(ytd_s_date, ytd_e_date)
cgo_fsc_int_month_prv <- getCgoFscInt(mtd_prv_s_date, mtd_prv_e_date)
cgo_fsc_int_year_prv  <- getCgoFscInt(ytd_prv_s_date, ytd_prv_e_date)

## Sum
cgo_fsc_month  <- cgo_fsc_dom_month + cgo_fsc_int_month
cgo_fsc_year   <- cgo_fsc_dom_year + cgo_fsc_int_year
cgo_fsc_month_prv <- cgo_fsc_dom_month_prv + cgo_fsc_int_month_prv
cgo_fsc_year_prv  <- cgo_fsc_dom_year_prv + cgo_fsc_int_year_prv
display.lap.time(); lap.time <- proc.time()

## Fetch Data: PAX Count ------------------------------------------------------------
progressMessage(6, steps, "Fetching PAX count: domestic ...")
incProgress(jindo[6], detail = sprintf("%d. PAX count domestic", 6))

pax_count_dom <- getPaxCntDom(data_s_date, data_e_date)
display.lap.time(); lap.time <- proc.time()

progressMessage(7, steps, "Fetching PAX count: int'l ...")
incProgress(jindo[7], detail = sprintf("%d. PAX count int'l", 7))

pax_count_int <- getPaxCntInt(data_s_date, data_e_date)
### convert to xts  
pax_count_dom <- xts(as.numeric(pax_count_dom[,2]), order.by = as.Date(pax_count_dom[,1], "%Y-%m-%d"))
pax_count_int <- xts(as.numeric(pax_count_int[,2]), order.by = as.Date(pax_count_int[,1], "%Y-%m-%d"))
pax_count_ttl <- pax_count_dom + pax_count_int
display.lap.time(); lap.time <- proc.time()

## Fetch Data: CGO WT ---------------------------------------------------------------
progressMessage(8, steps, "Fetching CGO Weight: domestic ...")
incProgress(jindo[8], detail = sprintf("%d. CGO weight domestic", 8))

cgo_wt_dom    <- getCgoWtDom(data_s_date, data_e_date)
display.lap.time(); lap.time <- proc.time()

progressMessage(9, steps, "Fetching CGO Weight: int'l ...")
incProgress(jindo[9], detail = sprintf("%d. CGO weight domestic", 9))
cgo_wt_int    <- getCgoWtInt(data_s_date, data_e_date)

### convert to xts 
cgo_wt_dom <- xts(as.numeric(cgo_wt_dom[,2]), order.by = as.Date(cgo_wt_dom[,1], "%Y%m%d"))
cgo_wt_int <- xts(as.numeric(cgo_wt_int[,2]), order.by = as.Date(cgo_wt_int[,1], "%Y-%m-%d"))
cgo_wt_ttl <- cgo_wt_dom + cgo_wt_int
display.lap.time(); lap.time <- proc.time()

# Make data ------------------------------------------------------------------------
## Make data: Monthly Revenue Table ------------------------------------------------
progressMessage(10, steps, "Preparing tables needed for app ...")
incProgress(jindo[10], detail = sprintf("%d. constructing tables", 10))
rev_monthly_pax_s <- apply.monthly(rev_daily[,"1_PAX_S"], sum)
rev_monthly_pax_t <- apply.monthly(rev_daily[,"2_PAX_T"], sum)
rev_monthly_cgo_t <- apply.monthly(rev_daily[,"3_CGO_T"], sum)
rev_monthly_count <- apply.monthly(pax_count_ttl, sum)
rev_monthly_wt    <- apply.monthly(cgo_wt_ttl, sum)

## Make data: Weekly Revenue Table (Sun ~ Sat) --------------------------------------
rev_weekly_pax_s  <- period.sum(rev_daily[,"1_PAX_S"], endpointsOnSaturday(rev_daily))
rev_weekly_pax_t  <- period.sum(rev_daily[,"2_PAX_T"], endpointsOnSaturday(rev_daily))
rev_weekly_cgo_t  <- period.sum(rev_daily[,"3_CGO_T"], endpointsOnSaturday(rev_daily))
rev_weekly_count  <- period.sum(pax_count_ttl, endpointsOnSaturday(pax_count_ttl))
rev_weekly_wt     <- period.sum(cgo_wt_ttl, endpointsOnSaturday(cgo_wt_ttl))

## Make data: for Month, Year Report (mtd, ytd) ------------------------------------
### total revenue
rev_mtd_pax <- getAccumulatedRevenue(mtd_s_date, mtd_e_date)[2]
rev_mtd_cgo <- getAccumulatedRevenue(mtd_s_date, mtd_e_date)[3]
rev_ytd_pax <- getAccumulatedRevenue(ytd_s_date, ytd_e_date)[2]
rev_ytd_cgo <- getAccumulatedRevenue(ytd_s_date, ytd_e_date)[3]

### net revenue (fsc excluded)
rev_mtd_pax_net <- rev_mtd_pax - pax_fsc_month
rev_mtd_cgo_net <- rev_mtd_cgo - cgo_fsc_month
rev_ytd_pax_net <- rev_ytd_pax - pax_fsc_year
rev_ytd_cgo_net <- rev_ytd_cgo - cgo_fsc_year

### total revenue - last year
rev_mtd_pax_prv <- getAccumulatedRevenue(mtd_prv_s_date, mtd_prv_e_date)[2]
rev_mtd_cgo_prv <- getAccumulatedRevenue(mtd_prv_s_date, mtd_prv_e_date)[3]
rev_ytd_pax_prv <- getAccumulatedRevenue(ytd_prv_s_date, ytd_prv_e_date)[2]
rev_ytd_cgo_prv <- getAccumulatedRevenue(ytd_prv_s_date, ytd_prv_e_date)[3]

### net revenue - last year
rev_mtd_pax_net_prv <- rev_mtd_pax_prv - pax_fsc_month_prv 
rev_mtd_cgo_net_prv <- rev_mtd_cgo_prv - cgo_fsc_month_prv
rev_ytd_pax_net_prv <- rev_ytd_pax_prv - pax_fsc_year_prv
rev_ytd_cgo_net_prv <- rev_ytd_cgo_prv - cgo_fsc_year_prv

## Make data: for Week Report (wtd) ---------------------------------------------
rev_wtd_pax_s <- getAccumulatedRevenue(wtd_s_date, wtd_e_date)[1]
rev_wtd_pax_t <- getAccumulatedRevenue(wtd_s_date, wtd_e_date)[2]
rev_wtd_cgo_t <- getAccumulatedRevenue(wtd_s_date, wtd_e_date)[3]
pax_count     <- getAccumulatedPaxCount(wtd_s_date, wtd_e_date)
cgo_wt        <- getAccumulatedCgoWt(wtd_s_date, wtd_e_date)

rev_wtd_pax_s_prv <- getAccumulatedRevenue(wtd_prv_s_date, wtd_prv_e_date)[1]
rev_wtd_pax_t_prv <- getAccumulatedRevenue(wtd_prv_s_date, wtd_prv_e_date)[2]
rev_wtd_cgo_t_prv <- getAccumulatedRevenue(wtd_prv_s_date, wtd_prv_e_date)[3]
pax_count_prv     <- getAccumulatedPaxCount(wtd_prv_s_date, wtd_prv_e_date)
cgo_wt_prv        <- getAccumulatedCgoWt(wtd_prv_s_date, wtd_prv_e_date)

## Make data: for detail view, source view  ------------------------------------
revenue_df_week <- make_revenue_df_week()
cnt_wt_df_week  <- make_cnt_wt_df_week()
revenue_df_month_all <- make_revenue_df_month_all()
revenue_df_month_net <- make_revenue_df_month_net()
revenue_df_year_all <- make_revenue_df_year_all()
revenue_df_year_net <- make_revenue_df_year_net()
source_df_week  <- make_source_df_week()
source_df_month <- make_source_df_month()

## Calculate revenue using last year's period average exchange rate ---------------------
### Weekly: rev_wtd_pax_s1, rev_wtd_pax_t1, rev_wtd_cgo_t1 ---------------------------------------------
#### PAX sales
temp <- getPaxSalesDom(wtd_s_date, wtd_e_date)
rev_wtd_pax_s1_dom_krw <- as.numeric(temp %>% filter(CY == "KRW") %>% select(REVENUE))/100000000
rev_wtd_pax_s1_int_lcl <- getPaxSalesInt(wtd_s_date, wtd_e_date)
rev_wtd_pax_s1_int_lcl <- left_join(rev_wtd_pax_s1_int_lcl, ex_rate_week, c("CY" = "FROM_CY"))
rev_wtd_pax_s1_int_lcl <- mutate(rev_wtd_pax_s1_int_lcl, 
                                 KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_wtd_pax_s1 <- rev_wtd_pax_s1_dom_krw + sum(rev_wtd_pax_s1_int_lcl[, "KRW_REV"]) # use this one

#### Pax traffic
temp <- getPaxTrafficDom(wtd_s_date, wtd_e_date)
rev_wtd_pax_t1_dom_krw <- as.numeric(temp %>% filter(CY == "KRW") %>% select(REVENUE))/100000000
rev_wtd_pax_t1_int_lcl <- getPaxTrafficInt(wtd_s_date, wtd_e_date)
rev_wtd_pax_t1_int_lcl <- left_join(rev_wtd_pax_t1_int_lcl, ex_rate_week, c("CY" = "FROM_CY"))
rev_wtd_pax_t1_int_lcl <- mutate(rev_wtd_pax_t1_int_lcl, 
                                 KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_wtd_pax_t1 <- rev_wtd_pax_t1_dom_krw + sum(rev_wtd_pax_t1_int_lcl[, "KRW_REV"]) # use this one

#### CGO traffic
rev_wtd_cgo_t1_dom_lcl <- getCgoTrafficDom(wtd_s_date, wtd_e_date)
rev_wtd_cgo_t1_int_lcl <- getCgoTrafficInt(wtd_s_date, wtd_e_date)
rev_wtd_cgo_t1 <- bind_rows(rev_wtd_cgo_t1_dom_lcl, rev_wtd_cgo_t1_int_lcl)
rev_wtd_cgo_t1 <- left_join(rev_wtd_cgo_t1, ex_rate_week, c("CY" = "FROM_CY"))
rev_wtd_cgo_t1 <- mutate(rev_wtd_cgo_t1, 
                         KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_wtd_cgo_t1 <- sum(rev_wtd_cgo_t1[, "KRW_REV"]) # use this one

### Monthly: rev_mtd_pax_t1, rev_mtd_cgo_t1 ---------------------------------------------
#### PAX
temp <- getPaxTrafficDom(mtd_s_date, mtd_e_date)
rev_mtd_pax_t1_dom_krw <- as.numeric(temp %>% filter(CY == "KRW") %>% select(REVENUE))/100000000
rev_mtd_pax_t1_int_lcl <- getPaxTrafficInt(mtd_s_date, mtd_e_date)
rev_mtd_pax_t1_int_lcl <- left_join(rev_mtd_pax_t1_int_lcl, ex_rate_month, c("CY" = "FROM_CY"))
rev_mtd_pax_t1_int_lcl <- mutate(rev_mtd_pax_t1_int_lcl, 
                                 KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_mtd_pax_t1 <- rev_mtd_pax_t1_dom_krw + sum(rev_mtd_pax_t1_int_lcl[, "KRW_REV"]) # use this one
#### CGO
rev_mtd_cgo_t1_dom_lcl <- getCgoTrafficDom(mtd_s_date, mtd_e_date)
rev_mtd_cgo_t1_int_lcl <- getCgoTrafficInt(mtd_s_date, mtd_e_date)
rev_mtd_cgo_t1 <- bind_rows(rev_mtd_cgo_t1_dom_lcl, rev_mtd_cgo_t1_int_lcl)
rev_mtd_cgo_t1 <- left_join(rev_mtd_cgo_t1, ex_rate_month, c("CY" = "FROM_CY"))
rev_mtd_cgo_t1 <- mutate(rev_mtd_cgo_t1, 
                         KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_mtd_cgo_t1 <- sum(rev_mtd_cgo_t1[, "KRW_REV"]) # use this one

### Yearly: rev_ytd_pax_t1, rev_ytd_cgo_t1 ---------------------------------------------
#### PAX
temp <- getPaxTrafficDom(ytd_s_date, ytd_e_date)
rev_ytd_pax_t1_dom_krw <- as.numeric(temp %>% filter(CY == "KRW") %>% select(REVENUE))/100000000
rev_ytd_pax_t1_int_lcl <- getPaxTrafficInt(ytd_s_date, ytd_e_date)
rev_ytd_pax_t1_int_lcl <- left_join(rev_ytd_pax_t1_int_lcl, ex_rate_year, c("CY" = "FROM_CY"))
rev_ytd_pax_t1_int_lcl <- mutate(rev_ytd_pax_t1_int_lcl, 
                                 KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_ytd_pax_t1 <- rev_ytd_pax_t1_dom_krw + sum(rev_ytd_pax_t1_int_lcl[, "KRW_REV"]) # use this one

#### CGO
rev_ytd_cgo_t1_dom_lcl <- getCgoTrafficDom(ytd_s_date, ytd_e_date)
rev_ytd_cgo_t1_int_lcl <- getCgoTrafficInt(ytd_s_date, ytd_e_date)
rev_ytd_cgo_t1 <- bind_rows(rev_ytd_cgo_t1_dom_lcl, rev_ytd_cgo_t1_int_lcl)
rev_ytd_cgo_t1 <- left_join(rev_ytd_cgo_t1, ex_rate_year, c("CY" = "FROM_CY"))
rev_ytd_cgo_t1 <- mutate(rev_ytd_cgo_t1, 
                         KRW_REV = (as.numeric(REVENUE) * as.numeric(ACCT_RATE))/100000000)
rev_ytd_cgo_t1 <- sum(rev_ytd_cgo_t1[, "KRW_REV"]) # use this one

display.lap.time(); lap.time <- proc.time()
# DB disconnection ------------------------------------------------------------------
nzDisconnect()
end.time <- proc.time()
cat("Data preparation finished.\n")
cat("Save all data into revenue.RData ...\n")
last_updated_time = now()
save.image("revenue.RData")
cat(sprintf("Total running time: %.2f seconds\n", end.time - start.time)[3])

