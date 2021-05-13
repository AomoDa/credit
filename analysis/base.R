XdateList = unique(kpi.dat$date)
# UI
ana_base_UI <- function(id="credit_base", label = "请选择文件") {
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
                choices=c(0.1,0.2,0.5,1,2,5,10,20,30,50),selected= 10)

          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_base2"))
          ),

          #-------------------------------
          # 统计表格
          #-------------------------------
          box(
              title = "官网分", solidHeader = TRUE,width=2,height=600,
              selectInput(inputId=ns("tb_base_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1]),
              selectInput(inputId=ns("tb_base_business_select"),label="请选择业务类型",
                choices=list("买卖"="mm","租赁"="zl"),
                selected= c("mm","zl"),multiple=TRUE)               

          ),          
          box(
              title = "官网指标分统计分布", solidHeader = TRUE,width=10,height=600,
              DTOutput(ns("tb_base_1"))

          )

  )
}


# Server
ana_base_Server <- function(id="credit_base") {
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

     output$tb_base_1 <- renderDataTable(
      table_base(input$tb_base_date_select,input$tb_base_business_select)
        
      )  

    }
  )
}



