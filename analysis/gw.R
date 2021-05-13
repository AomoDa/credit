
dateList = unique(kpi.dat$date)

# UI
ana_business_gw_UI <- function(id="credit_gw", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "ana_business_gw",height=1200,
         h2("业务能力 - 官网指标"),hr(),
         # 官网指标
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_gw_var_select"),label="请选择分析变量",choices=list(
                "渗透"="app",
                "400接听率"="call_answer",
                "爱聊一分钟响应率"="al_1min",
                "爱聊三日复聊率"="al_3day",
                "爱聊录入率"="al_luru",
                "爱聊转带看"="al_daikan"),selected="al_1min"),

              selectInput(inputId=ns("plot_gw_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("plot_gw_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm","租赁"="zl"),
                selected= c("mm","zl"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_gw_bin_select"),label="请选择条形宽度",
                choices=c(0.1,0.2,0.5,1,2,5,10,20,30,50),selected= 5)

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_gw"))
          ),
          #-------------------------------
          # 统计表格
          #-------------------------------
          box(
              title = "官网分", solidHeader = TRUE,width=2,height=600,
              selectInput(inputId=ns("tb_gw_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]),
              selectInput(inputId=ns("tb_gw_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm","租赁"="zl"),
                selected= c("mm","zl"),multiple=TRUE)               

          ),          
          box(
              title = "官网指标分统计分布", solidHeader = TRUE,width=10,height=600,
              DTOutput(ns("tb_gw_1"))

          )

  )
}


# Server
ana_business_gw_Server <- function(id="credit_gw") {
  moduleServer(
    id,
    function(input, output, session) {

    output$plot_gw <- renderPlotly(
      hist_credit(var=input$plot_gw_var_select,
        now_date=input$plot_gw_date_select,
        btype=input$plot_gw_business_select,
        binwidth= as.numeric(input$plot_gw_bin_select)
        )
      )


     output$tb_gw_1 <- renderDataTable(
      table_gw(input$tb_gw_date_select,input$tb_gw_business_select)
        
      )   



    }
  )
}



