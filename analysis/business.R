
dateList = unique(kpi.dat$date)

# UI
ana_business_UI <- function(id="credit_mm_zl", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "ana_business",height=1200,
          h2("业务能力 - 买卖指标"),hr(),
          # 买卖指标
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_mm_var_select"),label="请选择分析变量",choices=list(
                "收-速销房"="mm_s_sx",
                "出-速销房"="mm_c_sx",
                "转介客源"="mm_zjky",
                "折扣率"="mm_zk",
                "客户新增"="mm_cxz",
                "客户带看组数"="mm_cdk",
                "房源新增"="mm_fxz",
                "认证委托"="mm_wt",
                "钥匙"="mm_ys"
                ),selected="mm_s_sx"),
              selectInput(inputId=ns("plot_mm_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("plot_mm_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm"),
                selected= c("mm"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_mm_bin_select"),label="请选择条形宽度",
                choices=c(0.1,0.2,0.5,1,2,5,10,20,30,50),selected= 5)

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_mm"))
          ),

          #-------------------------------
          # 统计表格 - 买卖
          #-------------------------------
          box(
              title = "买卖分", solidHeader = TRUE,width=2,height=600,
              selectInput(inputId=ns("tb_mm_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1])              

          ),          
          box(
              title = "买卖指标分统计分布", solidHeader = TRUE,width=10,height=600,
              DTOutput(ns("tb_mm_1"))

          ),

          h2("业务能力 - 租赁指标"),hr(),
           # 租赁指标
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_zl_var_select"),label="请选择分析变量",choices=list(
                "房管收房"="zl_sfg",
                # "房管出房"="zl_cfg",
                "合作出房"="zl_fhz",
                "出着火房"="zl_czh",
                "在管量"="zl_zg",
                "委托续签量"="zl_wt",
                "新增客户"="zl_cxz",
                "房源新增"="zl_fxz",
                "普租实勘"="zl_pzsk"
                ),selected="zl_fhz"),
              selectInput(inputId=ns("plot_zl_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("plot_zl_business_select"),label="请选择业务类型",
                choices=list("租赁"="zl"),
                selected= c("zl"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_zl_bin_select"),label="请选择条形宽度",
                choices=c(0.1,0.2,0.5,1,2,5,10,20,30,50),selected= 5)
          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_zl"))
          ),
          #-------------------------------
          # 统计表格 - 租赁
          #-------------------------------
          box(
              title = "租赁分", solidHeader = TRUE,width=2,height=600,
              selectInput(inputId=ns("tb_zl_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1])              

          ),          
          box(
              title = "租赁指标分统计分布", solidHeader = TRUE,width=10,height=600,
              DTOutput(ns("tb_zl_1"))

          )


  )
}


# Server
ana_business_Server <- function(id="credit_mm_zl") {
  moduleServer(
    id,
    function(input, output, session) {

    output$plot_mm <- renderPlotly(
      hist_credit(var=input$plot_mm_var_select,
        now_date=input$plot_mm_date_select,
        btype=input$plot_mm_business_select,
        binwidth= as.numeric(input$plot_mm_bin_select)
        )
      )

    output$plot_zl <- renderPlotly(
      hist_credit(var=input$plot_zl_var_select,
        now_date=input$plot_zl_date_select,
        btype=input$plot_zl_business_select,
        binwidth= as.numeric(input$plot_zl_bin_select)
        )
      )

     output$tb_mm_1 <- renderDataTable(
      table_mm(input$tb_zl_date_select)
        
      )  
     output$tb_zl_1 <- renderDataTable(
      table_zl(input$tb_mm_date_select)
        
      )  

    }
  )
}



