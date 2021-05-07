
dateList = unique(kpi.dat$date)

# UI
ana_business_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "ana_business",height=1200,
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
                choices=c(1,2,5,10,30,50),selected= 5)

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_gw"))
          ),
          h2("业务能力 - 买卖指标"),hr(),
          # 买卖指标
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_mm_var_select"),label="请选择分析变量",choices=list(
                "买卖成交"="mm_deal",
                "速销"="mm_sx",
                "折扣率"="mm_zk",
                "客户首看"="mm_csk",
                "客户带看组数"="mm_cdk",
                "房源新增"="mm_fxz",
                "认证委托"="mm_wt",
                "钥匙"="mm_ys"
                ),selected="mm_deal"),
              selectInput(inputId=ns("plot_mm_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("plot_mm_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm"),
                selected= c("mm"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_mm_bin_select"),label="请选择条形宽度",
                choices=c(1,2,5,10,30,50),selected= 5)

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_mm"))
          ),
          h2("业务能力 - 租赁指标"),hr(),
           # 租赁指标
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_zl_var_select"),label="请选择分析变量",choices=list(
                "教育背景"="education",
                "特长"="hobby",
                "政治背景"="political",
                "司龄"="entry",
                "执业认证"="qualification",
                "每日一考"="exam"),selected="entry"),
              selectInput(inputId=ns("plot_zl_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]) ,         
              selectInput(inputId=ns("plot_zl_business_select"),label="请选择业务类型",
                choices=list("租赁"="zl"),
                selected= c("zl"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_zl_bin_select"),label="请选择条形宽度",
                choices=c(1,2,5,10,30,50),selected= 5)
          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_zl"))
          )



  )
}


# Server
ana_business_Server <- function(id="credit") {
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

    }
  )
}



