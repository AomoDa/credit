
dateList = unique(kpi.dat$date)

# UI
ana_overview_UI <- function(id="credit_ov", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "ana_overview",height=1200,
          #-------------------------------
          # 条形图
          #-------------------------------
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_1_var_select"),label="请选择分析变量",choices=list(
                "信用分"="score",
                "基础素质"="base",
                "行为规范"="behavior",
                "品质服务"="service",
                "参与贡献"="contribute",
                "业务能力-官网"="gw",
                "业务能力-买卖"="mm",
                "业务能力-租赁"="zl"
                ),selected="信用分"),
              selectInput(inputId=ns("plot_1_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("plot_1_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm","租赁"="zl"),
                selected= c("mm","zl"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_1_bin_select"),label="请选择条形宽度",
                choices=c(1,2,5,10,30,50),selected= 10) ,                
              hr(),
              downloadButton(outputId = ns("mt_download"),label = "点击下载数据"),

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_1"))
          ),
          #-------------------------------
          # 统计表格
          #-------------------------------
          box(
              title = "经纪人信用分统计分布", solidHeader = TRUE,width=2,height=600,
              selectInput(inputId=ns("tb_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("tb_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm","租赁"="zl"),
                selected= c("mm","zl"),multiple=TRUE) 

          ),
          box(
              title = "经纪人信用分统计分布", solidHeader = TRUE,width=10,height=600,
              DTOutput(ns("tb_ow_1"))

          ),
          #-------------------------------
          # 百分比图
          #-------------------------------
          box(
              title = "任务完成情况", solidHeader = TRUE,width=6,height=500,       
              plotOutput(ns("plot_2"))
          ),
          box(
              title = "任务完成情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_3"))
          )          

  )
}


# Server
ana_overview_Server <- function(id="credit_ov") {
  moduleServer(
    id,
    function(input, output, session) {

    output$plot_1 <- renderPlotly(
      hist_credit(var=input$plot_1_var_select,
        now_date=input$plot_1_date_select,
        btype=input$plot_1_business_select,
        binwidth= as.numeric(input$plot_1_bin_select)
        )
      )

    output$plot_2 <- renderPlot(
      box_credit(now_date=input$plot_1_date_select)
        
      )

    output$plot_3 <- renderPlotly(
      prob_credit(now_date=input$plot_1_date_select)
        
      )

     output$tb_ow_1 <- renderDataTable(
      table_credit(input$tb_date_select,input$tb_business_select)
        
      )  

    output$mt_download <- downloadHandler(
        filename = function() {
            paste("Credit-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(list("信用分"=finalScore),
                       path = file)
        }
    )       

    }
  )
}



