XdateList = unique(kpi.dat$date)
# UI
ana_base_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "ana_base",height=1200,
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("plot_base2_var_select"),label="请选择分析变量",choices=list(
                "教育背景"="education",
                "特长"="hobby",
                "政治背景"="political",
                "司龄"="entry",
                "执业认证"="qualification",
                "每日一考"="exam"),selected="entry"),
              selectInput(inputId=ns("plot_base2_date_select"),label="请选择计算日期",
                choices=XdateList,selected= rev(XdateList)[1]) ,         
              selectInput(inputId=ns("plot_base2_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm","租赁"="zl"),
                selected= c("mm","zl"),multiple=TRUE) ,  
              selectInput(inputId=ns("plot_base2_bin_select"),label="请选择条形宽度",
                choices=c(1,2,5,10,30,50),selected= 10)

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_base2"))
          )



  )
}


# Server
ana_base_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    output$plot_base2 <- renderPlotly(
      hist_credit(var=input$plot_base2_var_select,
        now_date=input$plot_base2_date_select,
        btype=input$plot_base2_business_select,
        binwidth= as.numeric(input$plot_base2_bin_select)
        )
      )


    }
  )
}



