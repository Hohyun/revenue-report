library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(DT)
library(dygraphs)

# Setup ------------------------------------------------------------
#setwd("D:/Works/ADF/Project/RevenueReport")
## load previous data
load("revenue.RData")
## load functions
source("util.R", encoding = "UTF-8")

# 0.1 Read Me page ---------------------------------------------------------------------------
read_me <- fluidPage(
  title = "도움말",
  
  box(title = "기간", width = 6, status = "primary", solidHeader = TRUE,
      wellPanel(
        strong("주간 실적 : "), wtd_s_date, " ~ ", wtd_e_date, br(),
        strong("월간 실적 : "), mtd_s_date, " ~ ", mtd_e_date, br(),
        strong("년간 실적 : "), ytd_s_date, " ~ ", ytd_e_date
      )
  ),
  box(title = "단위", width = 6, status = "primary", solidHeader = TRUE,
      wellPanel(
        strong("수익 실적 : "), "억원", br(),
        strong("수송 승객 : "), "만명", br(),
        strong("화물 중량 : "), "만톤"
      )
  ),
  box(title = "용어", width = 12, status = "primary", solidHeader = TRUE,
      wellPanel(
        strong("YoY "), strong(em("(Year on Year) :")), "금년 실적 / 전년 실적 (%)", br(),
        strong("+/- "), "전년 대비 증감 수치", br(),
        strong("P.I "), "ⓟ ", strong(em("(Performance Index) : ")), "금년 실적을 전년 동기간 평균환율로 환산한 금액 / 전년 실적 (%)"
      )
  ),
  box(title = "범례", width = 12, status = "primary", solidHeader = TRUE,
      column(4,
             infoBox("PAX", 
                     myFormat(round(rev_wtd_pax_t, 0)),
                     sprintf("%.1f%%, %s, %.1f%%", 
                             rev_wtd_pax_t / rev_wtd_pax_t_prv * 100, 
                             myDiff(rev_wtd_pax_t, rev_wtd_pax_t_prv, 0),
                             rev_wtd_pax_t1 / rev_wtd_pax_t_prv * 100),  
                     icon = myIcon(rev_wtd_pax_t, rev_wtd_pax_t_prv), width = 12,
                     color = myColor(rev_wtd_pax_t, rev_wtd_pax_t_prv)
             ),
             infoBox("사업 구분", 
                     "노선 실적",
                     sprintf("%s, %s, %s", 
                             "YoY", "+/-", "P.I"),
                     icon = myIcon(rev_wtd_pax_t, rev_wtd_pax_t_prv), width = 12,
                     color = myColor(rev_wtd_pax_t, rev_wtd_pax_t_prv)
             )
      ),
      column(4,
             infoBox("금주 실적", 
                     myFormat(round(rev_wtd_pax_s, 0)),
                     sprintf("%.1f%% ⓟ", rev_wtd_pax_s1 / rev_wtd_pax_s_prv * 100),
                     icon = icon("credit-card"), width = 12,
                     color = "blue"),
             infoBox("실적 구분", 
                     sprintf("%s", "노선실적"),
                     sprintf("%s", "P.I"),
                     icon = icon("credit-card"), width = 12,
                     color = "blue")
      ),
      column(4,
             infoBox("전년 대비", 
                     sprintf("%.1f%%", rev_wtd_pax_s / rev_wtd_pax_s_prv * 100),
                     myDiff(rev_wtd_pax_s, rev_wtd_pax_s_prv, 0), 
                     icon = myIcon(rev_wtd_pax_s, rev_wtd_pax_s_prv), width = 12,
                     color = myColor(rev_wtd_pax_s, rev_wtd_pax_s_prv)),
             infoBox("대비 기준", 
                     sprintf("%s", "YoY"),
                     sprintf("%s", "+/-"),
                     icon = myIcon(rev_wtd_pax_s, rev_wtd_pax_s_prv), width = 12,
                     color = myColor(rev_wtd_pax_s, rev_wtd_pax_s_prv))
      )
  )
)

# 0.2 Refresh Data ----------------------------------------------------------------
refresh_data <- fluidPage(
  
  titlePanel("Refresh data!"),
  
  sidebarLayout(
    
    sidebarPanel(
      dateInput("AsOfDate", label = span("As of Date"), value = today() - wday(today())), 
      textInput("bi_dsn", "BI DSN", value = bi.dsn), 
      textInput("erp_id", "ERP ID", value = erp.id), 
      textInput("erp_pswd", "ERP PSWD", value = erp.pswd), hr(),
      actionButton("update", strong("Update Data"), width = "100%")
    ),
    
    mainPanel(
      wellPanel(
        strong("- 요구 사항 : "), "BI 접속용 ODBC가 PC에 설정되어 있어야 함.", br(),
        strong("- 소요 시간 : "), "약 25~35초 소요 예상.", br(),
        strong("- 유의 사항 : "), "작업이 끝난 후 ", span("App을 재 실행", style="color:red"), "요망.", br(),
        strong("- 기준일    : "), "최근 토요일로 자동 수행됨.", hr(),
        "- ERP ID, PSWD가 변경된 경우 util.R을 열어 해당 부분을 수정해야 함.", br(),
        "- 좌측은 정보 안내용임. (프로그램 수행 시 반영되지 않음)"
      )
    )
  )
)

# 1. summary veiw -------------------------------------------------
summary_view <- fluidRow(
  box(width = 12,
      column(4, 
             box(
               title = "주간 실적", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("PAX", 
                       myFormat(round(rev_wtd_pax_t, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               rev_wtd_pax_t / rev_wtd_pax_t_prv * 100, 
                               myDiff(rev_wtd_pax_t, rev_wtd_pax_t_prv, 0),
                               rev_wtd_pax_t1 / rev_wtd_pax_t_prv * 100),  
                       icon = myIcon(rev_wtd_pax_t, rev_wtd_pax_t_prv), width = 12,
                       color = myColor(rev_wtd_pax_t, rev_wtd_pax_t_prv)),
               infoBox("CGO", 
                       myFormat(round(rev_wtd_cgo_t, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               rev_wtd_cgo_t / rev_wtd_cgo_t_prv * 100,
                               myDiff(rev_wtd_cgo_t, rev_wtd_cgo_t_prv, 0),
                               rev_wtd_cgo_t1 / rev_wtd_cgo_t_prv * 100),
                       icon = myIcon(rev_wtd_cgo_t, rev_wtd_cgo_t_prv), width = 12,
                       color = myColor(rev_wtd_cgo_t, rev_wtd_cgo_t_prv)),
               infoBox("TTL", 
                       myFormat(round(rev_wtd_pax_t + rev_wtd_cgo_t, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               (rev_wtd_pax_t + rev_wtd_cgo_t) / 
                                 (rev_wtd_pax_t_prv + rev_wtd_cgo_t_prv) * 100,
                               myDiff(rev_wtd_pax_t + rev_wtd_cgo_t, 
                                      rev_wtd_pax_t_prv + rev_wtd_cgo_t_prv, 0),
                               (rev_wtd_pax_t1 + rev_wtd_cgo_t1) / 
                                 (rev_wtd_pax_t_prv + rev_wtd_cgo_t_prv) * 100),
                       icon = myIcon((rev_wtd_pax_t + rev_wtd_cgo_t), 
                                     (rev_wtd_pax_t_prv + rev_wtd_cgo_t_prv)), 
                       width = 12, fill = TRUE,
                       color = myColor((rev_wtd_pax_t + rev_wtd_cgo_t), 
                                       (rev_wtd_pax_t_prv + rev_wtd_cgo_t_prv)))
             )),
      column(4,
             box(
               title = "월간 실적", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("PAX", 
                       myFormat(round(rev_wtd_pax_t, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               rev_mtd_pax / rev_mtd_pax_prv * 100, 
                               myDiff(rev_mtd_pax, rev_mtd_pax_prv, 0),
                               rev_mtd_pax_t1 / rev_mtd_pax_prv * 100),
                       icon = myIcon(rev_mtd_pax, rev_mtd_pax_prv), width = 12,
                       color = myColor(rev_mtd_pax, rev_mtd_pax_prv)),
               infoBox("CGO", 
                       myFormat(round(rev_mtd_cgo, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               rev_mtd_cgo / rev_mtd_cgo_prv * 100,
                               myDiff(rev_mtd_cgo, rev_mtd_cgo_prv, 0),
                               rev_mtd_cgo_t1 / rev_mtd_cgo_prv * 100),
                       icon = myIcon(rev_mtd_cgo, rev_mtd_cgo_prv), width = 12,
                       color = myColor(rev_mtd_cgo, rev_mtd_cgo_prv)),
               infoBox("TTL", 
                       myFormat(round(rev_mtd_pax + rev_mtd_cgo, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               (rev_mtd_pax + rev_mtd_cgo) / 
                                 (rev_mtd_pax_prv + rev_mtd_cgo_prv) * 100,
                               myDiff(rev_mtd_pax + rev_mtd_cgo, 
                                      rev_mtd_pax_prv + rev_mtd_cgo_prv, 0),
                               (rev_mtd_pax_t1 + rev_mtd_cgo_t1) / 
                                 (rev_mtd_pax_prv + rev_mtd_cgo_prv) * 100),
                       icon = myIcon((rev_mtd_pax + rev_mtd_cgo), 
                                     (rev_mtd_pax_prv + rev_mtd_cgo_prv)), 
                       width = 12, fill = TRUE,
                       color = myColor((rev_mtd_pax + rev_mtd_cgo), 
                                       (rev_mtd_pax_prv + rev_mtd_cgo_prv)))
             )),
      column(4, 
             box(
               title = "년간 실적", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("PAX", 
                       myFormat(round(rev_ytd_pax, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               rev_ytd_pax / rev_ytd_pax_prv * 100, 
                               myDiff(rev_ytd_pax, rev_ytd_pax_prv, 0),
                               rev_ytd_pax_t1 / rev_ytd_pax_prv * 100),
                       icon = myIcon(rev_ytd_pax, rev_ytd_pax_prv), width = 12,
                       color = myColor(rev_ytd_pax, rev_ytd_pax_prv)),
               infoBox("CGO", 
                       myFormat(round(rev_ytd_cgo, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               rev_ytd_cgo / rev_ytd_cgo_prv * 100,
                               myDiff(rev_ytd_cgo, rev_ytd_cgo_prv, 0),
                               rev_ytd_cgo_t1 / rev_ytd_cgo_prv * 100),
                       icon = myIcon(rev_ytd_cgo, rev_ytd_cgo_prv), width = 12,
                       color = myColor(rev_ytd_cgo, rev_ytd_cgo_prv)),
               infoBox("TTL", 
                       myFormat(round(rev_ytd_pax + rev_ytd_cgo, 0)),
                       sprintf("%.1f%%, %s, %.1f%%", 
                               (rev_ytd_pax + rev_ytd_cgo) / 
                                 (rev_ytd_pax_prv + rev_ytd_cgo_prv) * 100,
                               myDiff(rev_ytd_pax + rev_ytd_cgo, 
                                      rev_ytd_pax_prv + rev_ytd_cgo_prv, 0),
                               (rev_ytd_pax_t1 + rev_ytd_cgo_t1) / 
                                 (rev_ytd_pax_prv + rev_ytd_cgo_prv) * 100),
                       icon = myIcon((rev_ytd_pax + rev_ytd_cgo), 
                                     (rev_ytd_pax_prv + rev_ytd_cgo_prv)), 
                       width = 12, fill = TRUE,
                       color = myColor((rev_ytd_pax + rev_ytd_cgo), 
                                       (rev_ytd_pax_prv + rev_ytd_cgo_prv)))
             )
      )
  ),
  box(width = 12,
      box(width = 4, title = "Legend", solidHeader = FALSE, status = "info",
          infoBox("사업 구분", 
                  "노선 실적",
                  sprintf("%s, %s, %s", 
                          "YoY", "+/-", "P.I"),
                  icon = myIcon(rev_wtd_pax_t, rev_wtd_pax_t_prv), width = 12,
                  color = myColor(rev_wtd_pax_t, rev_wtd_pax_t_prv)
          )
      ),
      box(title = "Terminology", width = 8, status = "info", solidHeader = FALSE,
          wellPanel(
            span(strong("YoY "), style="color:red"), strong(em("(Year on Year)")), ": 금년 실적 / 전년 실적 (%), ",
            span(strong("+/- "), style="color:red"), ": 전년 대비 증감 수치", br(),
            span(strong("P.I "), "ⓟ ",  style="color:red"), strong(em("(Performance Index) ")), ": 금년 실적을 전년 동기간 평균환율로 환산한 금액 / 전년 실적 (%)"
          )
      )
  )
)

# 2-1 revenue (week) ---------------------------------------------------------------------
dashboard_week_rev <- fluidRow(
  box(width = 12,
      column(4,
             box(
               title = "여객 발매", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("금주 실적", 
                       myFormat(round(rev_wtd_pax_s, 0)),
                       sprintf("%.1f%% ⓟ", rev_wtd_pax_s1 / rev_wtd_pax_s_prv * 100),
                       icon = icon("credit-card"), width = 12,
                       color = "blue"),
               infoBox("전년 대비", 
                       sprintf("%.1f%%", rev_wtd_pax_s / rev_wtd_pax_s_prv * 100),
                       myDiff(rev_wtd_pax_s, rev_wtd_pax_s_prv, 0), 
                       icon = myIcon(rev_wtd_pax_s, rev_wtd_pax_s_prv), width = 12,
                       color = myColor(rev_wtd_pax_s, rev_wtd_pax_s_prv))
             )
      ),
      column(4,
             box(
               title = "여객 운송", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("금주 실적", 
                       myFormat(round(rev_wtd_pax_t, 0)),
                       sprintf("%.1f%% ⓟ", rev_wtd_pax_t1 / rev_wtd_pax_t_prv * 100),
                       icon = icon("credit-card"), 
                       color = "blue", width = 12),
               infoBox("전년 대비", 
                       sprintf("%.1f%%", rev_wtd_pax_t / rev_wtd_pax_t_prv * 100),
                       myDiff(rev_wtd_pax_t, rev_wtd_pax_t_prv, 0), 
                       icon = myIcon(rev_wtd_pax_t, rev_wtd_pax_t_prv), width = 12,
                       color = myColor(rev_wtd_pax_t, rev_wtd_pax_t_prv))
             )
      ),
      column(4,
             box(
               title = "화물 운송", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("금주 실적", 
                       myFormat(round(rev_wtd_cgo_t, 0)),
                       sprintf("%.1f%% ⓟ", rev_wtd_cgo_t1 / rev_wtd_cgo_t_prv * 100),
                       icon = icon("credit-card"), 
                       color = "blue", width = 12),
               infoBox("전년 대비", 
                       sprintf("%.1f%%", rev_wtd_cgo_t / rev_wtd_cgo_t_prv * 100),
                       myDiff(rev_wtd_cgo_t, rev_wtd_cgo_t_prv, 0), 
                       icon = myIcon(rev_wtd_cgo_t, rev_wtd_cgo_t_prv), width = 12,
                       color = myColor(rev_wtd_cgo_t, rev_wtd_cgo_t_prv))
             )
      ),
      column(12, 
             datatable(revenue_df_week, options = list(dom = 't'),
                       colnames = c('Gubun', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY', 'P.I'),
                       class = 'cell-border stripe', rownames = FALSE) %>%
               formatCurrency(c('this', 'last', 'diff'), '') %>%
               formatPercentage(c('pct', 'index'), 1)
      )
  )
)

# 2-2 count & wt (week) ---------------------------------------------------------------------
dashboard_week_count_wt <- fluidRow(
  box(width = 12,
      column(6,
             box(
               title = "수송 승객", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("금주 실적", 
                       myFormat(round(pax_count, 1)),
                       icon = icon("users"), width = 12,
                       color = "blue"),
               infoBox("전년 대비", 
                       sprintf("%.1f%%", pax_count / pax_count_prv * 100),
                       myDiff(pax_count, pax_count_prv, 1), 
                       icon = myIcon(pax_count, pax_count_prv), width = 12,
                       color = myColor(pax_count, pax_count_prv))
             )
      ),
      column(6,
             box(
               title = "수송 톤수", width = NULL, solidHeader = TRUE, status = "primary",
               infoBox("금주 실적", 
                       myFormat(round(cgo_wt, 2)),
                       icon = icon("truck"), width = 12,
                       color = "blue"),
               infoBox("전년 대비", 
                       sprintf("%.1f%%", cgo_wt / cgo_wt_prv * 100),
                       myDiff(cgo_wt, cgo_wt_prv, 2), 
                       icon = myIcon(cgo_wt, cgo_wt_prv), width = 12,
                       color = myColor(cgo_wt, cgo_wt_prv))
             )
      ),
      column(12,
             datatable(cnt_wt_df_week, options = list(dom = 't'),
                       colnames = c('Gubun', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY'),
                       class = 'cell-border stripe', rownames = FALSE) %>%
               formatCurrency(c('this', 'last', 'diff'), '') %>%
               formatPercentage(c('pct'), 1)
      )
  )
)

# 2-3 weekly trend graph ----------------------------------------------------------
dashboard_week_trend <- fluidRow(
  box(width = 12,
      box(title = "주별 실적 추이 (총수익)", solidHeader = TRUE, status = "primary", width = 12,
          dygraphOutput("dygraph_weekly_rev")
      ),
      box(title = "주별 실적 추이 (수송)", solidHeader = TRUE, status = "primary", width = 12,
          dygraphOutput("dygraph_weekly_cnt_wt")
      )
  )
)

# 3-1 all revenue (month) -------------------------------------------------------
dashboard_month_all_rev <- fluidRow(
  box(width = 12,
      box(
        title = "Passenger", width = 4, solidHeader = TRUE, status = "primary", 
        infoBox("금월 실적", 
                myFormat(round(rev_mtd_pax, 0)), 
                sprintf("%.1f%% ⓟ", rev_mtd_pax_t1 / rev_mtd_pax_prv * 100),
                icon = icon("credit-card"), 
                color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_mtd_pax / rev_mtd_pax_prv * 100), 
                myDiff(rev_mtd_pax, rev_mtd_pax_prv, 0), 
                icon = myIcon(rev_mtd_pax, rev_mtd_pax_prv),  width = 12,  
                color = myColor(rev_mtd_pax, rev_mtd_pax_prv))
      ),
      box(
        title = "Cargo", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금월 실적", 
                myFormat(round(rev_mtd_cgo, 0)), 
                sprintf("%.1f%% ⓟ", rev_mtd_cgo_t1 / rev_mtd_cgo_prv * 100),
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_mtd_cgo / rev_mtd_cgo_prv * 100), 
                myDiff(rev_mtd_cgo, rev_mtd_cgo_prv, 0), 
                icon = myIcon(rev_mtd_cgo, rev_mtd_cgo_prv),  width = 12,  
                color = myColor(rev_mtd_cgo, rev_mtd_cgo_prv))
      ),
      box(
        title = "Total", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금월 실적", 
                myFormat(round(rev_mtd_pax + rev_mtd_cgo, 0)),
                sprintf("%.1f%% ⓟ", (rev_mtd_pax_t1 + rev_mtd_cgo_t1) / (rev_mtd_pax_prv + rev_mtd_cgo_prv) * 100),
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", (rev_mtd_pax + rev_mtd_cgo) / (rev_mtd_pax_prv + rev_mtd_cgo_prv) * 100), 
                myDiff(rev_mtd_pax + rev_mtd_cgo, rev_mtd_pax_prv + rev_mtd_cgo_prv, 0), 
                icon = myIcon(rev_mtd_pax + rev_mtd_cgo, rev_mtd_pax_prv + rev_mtd_cgo_prv),  width = 12,  
                color = myColor(rev_mtd_pax + rev_mtd_cgo, rev_mtd_pax_prv + rev_mtd_cgo_prv))
      ),
      column(12,
             datatable(revenue_df_month_all, options = list(dom = 't'), 
                       colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY', "P.I"),
                       class = 'cell-border stripe', rownames = FALSE)  %>%
               formatCurrency(c('this', 'last', 'diff'), '') %>%
               formatPercentage(c('pct', 'index'), 1)
      )
  )
)

# 3-2 net revenue (month) --------------------------------------------------------------------------------
dashboard_month_net_rev <- fluidRow(
  box(width = 12,
      box(
        title = "Passenger", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금월 실적", 
                myFormat(round(rev_mtd_pax_net, 0)), 
                icon = icon("credit-card"), 
                color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_mtd_pax_net / rev_mtd_pax_net_prv * 100), 
                myDiff(rev_mtd_pax_net, rev_mtd_pax_net_prv, 0), 
                icon = myIcon(rev_mtd_pax_net, rev_mtd_pax_net_prv),  width = 12,  
                color = myColor(rev_mtd_pax_net, rev_mtd_pax_net_prv))
      ),
      
      box(
        title = "Cargo", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금월 실적", 
                format(round(rev_mtd_cgo_net, 0), digit = 5, big.mark = ","), 
                format(round(rev_mtd_cgo_net_prv, 0), digit = 5, big.mark = ","),
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_mtd_cgo_net / rev_mtd_cgo_net_prv * 100), 
                myDiff(rev_mtd_cgo_net, rev_mtd_cgo_net_prv, 0), 
                icon = myIcon(rev_mtd_cgo_net, rev_mtd_cgo_net_prv),  width = 12,  
                color = myColor(rev_mtd_cgo_net, rev_mtd_cgo_net_prv))
      ),
      
      box(
        title = "Total", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금월 실적", 
                myFormat(round(rev_mtd_pax_net + rev_mtd_cgo_net, 0)), 
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", (rev_mtd_pax + rev_mtd_cgo) / (rev_mtd_pax_prv + rev_mtd_cgo_prv) * 100), 
                myDiff(rev_mtd_pax_net + rev_mtd_cgo_net, rev_mtd_pax_net_prv + rev_mtd_cgo_net_prv, 0), 
                icon = myIcon(rev_mtd_pax_net + rev_mtd_cgo_net, rev_mtd_pax_net_prv + rev_mtd_cgo_net_prv),  width = 12,  
                color = myColor(rev_mtd_pax_net + rev_mtd_cgo_net, rev_mtd_pax_net_prv + rev_mtd_cgo_net_prv)
        )
      ),
      column(12,
             datatable(revenue_df_month_net, options = list(dom = 't'), 
                       colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY'),
                       class = 'cell-border stripe', rownames = FALSE)  %>%
               formatCurrency(c('this', 'last', 'diff'), '') %>%
               formatPercentage(c('pct'), 1)
      )
  )
)  

# 3-3 monthly trend graph ----------------------------------------------------------
dashboard_month_trend <- fluidRow(
  box(width = 12,
      box(title = "월별 실적 추이 (총수익)", solidHeader = TRUE, status = "primary", width = 12,
          dygraphOutput("dygraph_monthly_trend")
      )
  )
)

# 4-1 all revenue (year) -------------------------------------------------------
dashboard_year_all_rev <- fluidRow(
  box(width = 12,
      box(
        title = "Passenger", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금년 실적", 
                myFormat(round(rev_ytd_pax, 0)), 
                sprintf("%.1f%% ⓟ", rev_ytd_pax_t1 / rev_ytd_pax_prv * 100),
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_ytd_pax / rev_ytd_pax_prv * 100), 
                myDiff(rev_ytd_pax, rev_ytd_pax_prv, 0), 
                icon = myIcon(rev_ytd_pax, rev_ytd_pax_prv),  width = 12,  
                myColor(rev_ytd_pax, rev_ytd_pax_prv))
      ),
      
      box(
        title = "Cargo", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금년 실적", 
                myFormat(round(rev_ytd_cgo, 0)), 
                sprintf("%.1f%% ⓟ", rev_ytd_cgo_t1 / rev_ytd_cgo_prv * 100),
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_ytd_cgo / rev_ytd_cgo_prv * 100), 
                myDiff(rev_ytd_cgo, rev_ytd_cgo_prv, 0), 
                icon = myIcon(rev_ytd_cgo, rev_ytd_cgo_prv),  width = 12,  
                myColor(rev_ytd_cgo, rev_ytd_cgo_prv))
      ),
      
      box(
        title = "Total", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금년 실적", 
                myFormat(round(rev_ytd_pax + rev_ytd_cgo, 0)), 
                sprintf("%.1f%% ⓟ", (rev_ytd_pax + rev_ytd_cgo) / (rev_ytd_pax_prv + rev_ytd_cgo_prv) * 100), 
                #myFormat(round(rev_ytd_pax_prv + rev_ytd_cgo_prv, 0)),
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", (rev_ytd_pax + rev_ytd_cgo) / (rev_ytd_pax_prv + rev_ytd_cgo_prv) * 100), 
                myDiff(rev_ytd_pax + rev_ytd_cgo, rev_ytd_pax_prv + rev_ytd_cgo_prv, 0), 
                icon = myIcon(rev_ytd_pax + rev_ytd_cgo, rev_ytd_pax_prv + rev_ytd_cgo_prv),  width = 12,  
                color = myColor(rev_ytd_pax + rev_ytd_cgo, rev_ytd_pax_prv + rev_ytd_cgo_prv))
      ),
      column(12,
             datatable(revenue_df_year_all, options = list(dom = 't'), 
                       colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY', "P.I"),
                       class = 'cell-border stripe', rownames = FALSE)  %>%
               formatCurrency(c('this', 'last', 'diff'), '') %>%
               formatPercentage(c('pct', 'index'), 1)
      )
  )
)

# 4-2 net revenue (year) --------------------------------------------------------------------------------
dashboard_year_net_rev <- fluidRow(
  box(width = 12,
      box(
        title = "Passenger", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금년 실적", 
                myFormat(round(rev_ytd_pax_net, 0)), 
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_ytd_pax_net / rev_ytd_pax_net_prv * 100), 
                myDiff(rev_ytd_pax_net, rev_ytd_pax_net_prv, 0), 
                icon = myIcon(rev_ytd_pax_net, rev_ytd_pax_net_prv),  width = 12,  
                myColor(rev_ytd_pax_net, rev_ytd_pax_net_prv))
      ),
      
      box(
        title = "Cargo", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금년 실적", 
                myFormat(round(rev_ytd_cgo_net, 0)), 
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", rev_ytd_cgo_net / rev_ytd_cgo_net_prv * 100), 
                myDiff(rev_ytd_cgo_net, rev_ytd_cgo_net_prv, 0), 
                icon = myIcon(rev_ytd_cgo_net, rev_ytd_cgo_net_prv),  width = 12,  
                myColor(rev_ytd_cgo_net, rev_ytd_cgo_net_prv))
      ),
      
      box(
        title = "Total", width = 4, solidHeader = TRUE, status = "primary",
        infoBox("금년 실적", 
                myFormat(round(rev_ytd_pax_net + rev_ytd_cgo_net, 0)), 
                icon = icon("credit-card"), color = "blue", width = 12),
        infoBox("전년 대비", 
                sprintf("%.1f%%", (rev_ytd_pax_net + rev_ytd_cgo_net) / (rev_ytd_pax_net_prv + rev_ytd_cgo_net_prv) * 100), 
                myDiff(rev_ytd_pax_net + rev_ytd_cgo_net, rev_ytd_pax_net_prv + rev_ytd_cgo_net_prv, 0), 
                icon = myIcon(rev_ytd_pax_net + rev_ytd_cgo_net, rev_ytd_pax_net_prv + rev_ytd_cgo_net_prv),  width = 12,  
                color = myColor(rev_ytd_pax_net + rev_ytd_cgo_net, rev_ytd_pax_net_prv + rev_ytd_cgo_net_prv))
      ),
      column(12,
             datatable(revenue_df_year_net, options = list(dom = 't'), 
                       colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY'),
                       class = 'cell-border stripe', rownames = FALSE)  %>%
               formatCurrency(c('this', 'last', 'diff'), '') %>%
               formatPercentage(c('pct'), 1)
      )
  )
)

# 5-1 Data view (legacy)  -----------------------------------------------------------
tables_view <- fluidRow(
  box(width = 12,
      
      box(title = "주간: 총 노선실적 (억원)", 
          width = 7, solidHeader = TRUE, status = "primary",
          datatable(revenue_df_week, options = list(dom = 't'),
                    colnames = c('Gubun', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY', 'P.I'),
                    class = 'cell-border stripe', rownames = FALSE) %>%
            formatCurrency(c('this', 'last', 'diff'), '') %>%
            formatPercentage(c('pct', 'index'), 1)
      ),
      box(title = "주간: 수송실적 (만명/만톤)", 
          width = 5, solidHeader = TRUE, status = "primary",
          datatable(cnt_wt_df_week, options = list(dom = 't'),
                    colnames = c('Gubun', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY'),
                    class = 'cell-border stripe', rownames = FALSE) %>%
            formatCurrency(c('this', 'last', 'diff'), '') %>%
            formatPercentage(c('pct'), 1)
      ),
      box(title = "월간: 총 노선실적 (억원)", 
          width = 7, solidHeader = TRUE, status = "primary",
          datatable(revenue_df_month_all, options = list(dom = 't'), 
                    colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY', "P.I"),
                    class = 'cell-border stripe', rownames = FALSE)  %>%
            formatCurrency(c('this', 'last', 'diff'), '') %>%
            formatPercentage(c('pct', 'index'), 1)
      ),
      box(title = "월간: 순 노선실적", 
          width = 5, solidHeader = TRUE, status = "primary",
          datatable(revenue_df_month_net, options = list(dom = 't'), 
                    colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY'),
                    class = 'cell-border stripe', rownames = FALSE)  %>%
            formatCurrency(c('this', 'last', 'diff'), '') %>%
            formatPercentage(c('pct'), 1)
      ),  
      box(title = "년간: 총 노선실적 (억원)", 
          width = 7, solidHeader = TRUE, status = "primary",
          datatable(revenue_df_year_all, options = list(dom = 't'), 
                    colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY', "P.I"),
                    class = 'cell-border stripe', rownames = FALSE)  %>%
            formatCurrency(c('this', 'last', 'diff'), '') %>%
            formatPercentage(c('pct', 'index'), 1)
      ),
      box(title = "년간: 순 노선실적", 
          width = 5, solidHeader = TRUE, status = "primary",
          datatable(revenue_df_year_net, options = list(dom = 't'), 
                    colnames = c('PC', year(wtd_e_date), year(wtd_e_date)-1, '+/-', 'YoY'),
                    class = 'cell-border stripe', rownames = FALSE)  %>%
            formatCurrency(c('this', 'last', 'diff'), '') %>%
            formatPercentage(c('pct'), 1)
      )
  )
)

# 5-1 Data view (week) ----------------------------------------------------
data_view_week <- fluidRow(
  box(width = 12,
      box(
        title = "주간 실적", width = 12, solidHeader = TRUE, status = "primary",
        datatable(arrange(source_df_week, desc(DATE)), 
                  class = 'cell-border stripe', rownames = FALSE)  %>%
          formatCurrency(c('PAX_S_REV', 'PAX_T_REV', 'CGO_T_REV'), '')
      )
  )
)

# 5-2 Data view (month) ------------------------------------------------------
data_view_month <- fluidRow(
  box(width = 12,
      box(
        title = "월간 실적", width = 12, solidHeader = TRUE, status = "primary",
        datatable(arrange(source_df_month, desc(DATE)), 
                  class = 'cell-border stripe', rownames = FALSE) %>%
          formatCurrency(c('PAX_S_REV', 'PAX_T_REV', 'CGO_T_REV'), '')
      )
  )
)

# UI: Dashboard constructor ---------------------------------------------------------------
header <- dashboardHeader(
  titleWidth = 180,
  title = sprintf("실적 현황 : %s", paste(month(wtd_e_date), day(wtd_e_date), sep = "/")),
  dropdownMenuOutput("messageMenu")
)

sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    menuItem("Read Me", tabName = "read_me", icon = icon("comment")),
    menuItem("Refresh Data", tabName = "refresh_data", icon = icon("refresh")),
    menuItem("종합 현황", tabName = "summary_view", icon = icon("dashboard"), selected = TRUE),
    menuItem("주간 실적", icon = icon("calendar"),
             menuSubItem("주간 수익", tabName = "dashboard_week_rev", icon = icon("credit-card-alt")),
             menuSubItem("주간 수송", tabName = "dashboard_week_count_wt", icon = icon("users")),
             menuSubItem("실적 추이", tabName = "dashboard_week_trend", icon = icon("line-chart"))
    ),
    menuItem("월간 실적", icon = icon("calendar-o"),
             menuSubItem("총 수익", tabName = "dashboard_month_all_rev", icon = icon("credit-card-alt")),
             menuSubItem("순 수익", tabName = "dashboard_month_net_rev", icon = icon("credit-card")),
             menuSubItem("실적 추이", tabName = "dashboard_month_trend", icon = icon("line-chart"))
             
    ), 
    menuItem("년간 실적", icon = icon("calendar-o"),
             menuSubItem("총 수익", tabName = "dashboard_year_all_rev", icon = icon("credit-card-alt")),
             menuSubItem("순 수익", tabName = "dashboard_year_net_rev", icon = icon("credit-card"))
    ), 
    menuItem("참고 자료", icon = icon("calendar"),
             menuSubItem("도표 종합", tabName = "tables_view", icon = icon("list-ul")),
             menuSubItem("주간 자료", tabName = "data_view_week", icon = icon("list-ul")),
             menuSubItem("월간 자료", tabName = "data_view_month", icon = icon("list-ul"))
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "read_me", read_me),
    tabItem(tabName = "refresh_data", refresh_data),
    tabItem(tabName = "summary_view", summary_view),
    tabItem(tabName = "dashboard_week_rev", dashboard_week_rev),
    tabItem(tabName = "dashboard_week_count_wt", dashboard_week_count_wt),
    tabItem(tabName = "dashboard_week_trend", dashboard_week_trend),
    tabItem(tabName = "dashboard_month_all_rev", dashboard_month_all_rev),
    tabItem(tabName = "dashboard_month_net_rev", dashboard_month_net_rev),
    tabItem(tabName = "dashboard_month_trend", dashboard_month_trend),
    tabItem(tabName = "dashboard_year_all_rev", dashboard_year_all_rev),
    tabItem(tabName = "dashboard_year_net_rev", dashboard_year_net_rev),    
    tabItem(tabName = "tables_view",   tables_view),
    tabItem(tabName = "data_view_week",  data_view_week),
    tabItem(tabName = "data_view_month", data_view_month)
  )
)

ui <- dashboardPage(header, sidebar, body)

# Server --------------------------------------------------------------------------------
server <- function(input, output) { 
  
  output$dygraph_weekly_rev <- renderDygraph({
    weekly_trend <- cbind(rev_weekly_pax_t, rev_weekly_cgo_t)
    names(weekly_trend) <- c("PAX", "CGO")
    
    shades <- shade_regions_weekly(weekly_trend, 3)
    limits <- weekly_trend[shades[1,1],]
    dygraph(weekly_trend,
            main = "Weekly Revenue Trend") %>% 
      dyShading(from = as.character(index(weekly_trend)[shades[1,2]]),  
                to   = as.character(index(weekly_trend)[shades[1,3]])) %>%
      dyShading(from = as.character(index(weekly_trend)[shades[2,2]]),  
                to   = as.character(index(weekly_trend)[shades[2,3]]),
                color = "#FFE6E6") %>%
      dyShading(from = as.character(index(weekly_trend)[shades[3,2]]),  
                to   = as.character(index(weekly_trend)[shades[3,3]])) %>%
      dyShading(from = as.character(index(weekly_trend)[shades[4,2]]),  
                to   = as.character(index(weekly_trend)[shades[4,3]])) %>%
      dyEvent("2015-07-04", "Mers", labelLoc = "bottom") %>%
      dyLimit(as.numeric(limits[1,1])) %>%
      dyLimit(as.numeric(limits[1,2])) %>%
      dyLegend(showZeroValues = "always", hideOnMouseOut = TRUE) %>%
      dyRangeSelector(height = 20, dateWindow = c(ytd_s_date - years(2), wtd_e_date)) %>%
      dyRoller(rollPeriod = 0)
  })
  
  output$dygraph_weekly_cnt_wt <- renderDygraph({
    weekly_trend <- cbind(rev_weekly_count, rev_weekly_wt)
    names(weekly_trend) <- c("PAX_Q", "CGO_WT")
    
    shades <- shade_regions_weekly(weekly_trend, 3)
    limits <- weekly_trend[shades[1,1],]
    
    dygraph(weekly_trend,
            main = "Weekly Volume Trend") %>% 
      dyAxis("y",  label = "PAX COUNT", valueRange = c(0, 70)) %>%
      dyAxis("y2", label = "CGO WT", valueRange = c(0, 14)) %>%
      dySeries("CGO_WT", axis = 'y2') %>%
      dyShading(from = as.character(index(weekly_trend)[shades[1,2]]),  
                to   = as.character(index(weekly_trend)[shades[1,3]])) %>%
      dyShading(from = as.character(index(weekly_trend)[shades[2,2]]),  
                to   = as.character(index(weekly_trend)[shades[2,3]]),
                color = "#FFE6E6") %>%
      dyShading(from = as.character(index(weekly_trend)[shades[3,2]]),  
                to   = as.character(index(weekly_trend)[shades[3,3]])) %>%
      dyShading(from = as.character(index(weekly_trend)[shades[4,2]]),  
                to   = as.character(index(weekly_trend)[shades[4,3]])) %>%
      dyEvent("2015-07-04", "Mers", labelLoc = "bottom") %>%
      dyLimit(as.numeric(limits[1,1])) %>%
      dyLimit(as.numeric(limits[1,2]) * 5) %>%
      dyLegend(showZeroValues = "always", hideOnMouseOut = TRUE) %>%
      dyRangeSelector(height = 20, dateWindow = c(ytd_s_date - years(2), wtd_e_date)) %>%
      dyRoller(rollPeriod = 0)
  })
  
  output$dygraph_monthly_trend <- renderDygraph({
    # predict pax
    pax <- ts(rev_monthly_pax_t[-length(rev_monthly_pax_t)], 
              start = c(2013,1,1), frequency = 12)
    hw <- HoltWinters(pax)
    p1 <- predict(hw, n.ahead = 6, prediction.interval = TRUE)
    cgo <- ts(rev_monthly_cgo_t[-length(rev_monthly_pax_t)],
              start = c(2013,1,1), frequency = 12)
    hw <- HoltWinters(cgo)
    p2 <- predict(hw, n.ahead = 6, prediction.interval = TRUE)
    
    trend <- cbind(pax, cgo, p1, p2)
    shades <- shade_regions_monthly(rev_monthly_pax_t, 1)
    limits <- trend[shades[1,1],]
    
    dygraph(trend, main = "Monthly Revenue Trend") %>%
      #dyAxis("x", drawGrid = FALSE) %>%
      dySeries("pax", label = "PAX") %>%
      dySeries("cgo", label = "CGO") %>%
      dySeries(c("p1.lwr", "p1.fit", "p1.upr"), label = "PAX-p") %>%
      dySeries(c("p2.lwr", "p2.fit", "p2.upr"), label = "CGO-p") %>%
      dyShading(from = as.character(mtd_s_date - months(1)),  
                to   = as.character(mtd_s_date + months(1))) %>%
      dyShading(from = as.character(mtd_s_date - months(13)),  
                to   = as.character(mtd_s_date - months(11)),
                color = "#FFE6E6") %>%
      dyShading(from = as.character(mtd_s_date - months(25)),  
                to   = as.character(mtd_s_date - months(23))) %>%
      dyShading(from = as.character(mtd_s_date - months(37)),  
                to   = as.character(mtd_s_date - months(35))) %>%
      dyEvent("2015-06-01", "Mers", labelLoc = "bottom") %>%
      #dyEvent(mtd_s_date, "", labelLoc = "bottom") %>%
      dyLimit(as.numeric(limits[3])) %>%
      dyLimit(as.numeric(limits[6])) %>%
      dyLegend(showZeroValues = "always", hideOnMouseOut = TRUE) %>%
      dyRangeSelector(height = 20) 
  })
  
  observeEvent(input$update, {
    withProgress(message = 'Preparing', value = 0, {
      source("prepareData.R", encoding = "UTF-8")
    })
    
    # report creation for e-mail
    rmarkdown::render("weekly_revenue_report.Rmd", encoding = "utf-8")
    
    output$messageMenu <- renderMenu({
      dropdownMenu(type = "notifications",
                   notificationItem(
                     text = (sprintf("Data updated time: %s", last_updated_time)),
                     icon = icon("info-circle"),
                     status = "success"),
                   notificationItem(
                     text = "E-mail report was created successfully.",
                     icon = icon("file-text"),
                     status = "success"),
                   notificationItem(
                     text = "You should restart app.",
                     icon = icon("exclamation-triangle"),
                     status = "warning")
      )
    })
  })
}

app <- shinyApp(ui, server)

