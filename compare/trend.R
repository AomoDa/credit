
dateList = unique(kpi.dat$date)
mm_jjrList = unique(finalScore$name[finalScore$business=="mm"])
zl_jjrList = unique(finalScore$name[finalScore$business=="zl"])
# UI
ana_comp_trend_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "comp_trend",height=1200,
          h2("经纪人TOP榜单"),hr(),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,
              selectInput(inputId=ns("plot_comp_top_var_select"),label="请选择分析变量",choices=list(
                "信用分"="score",
                "基础素质"="base",
                "行为规范"="behavior",
                "品质服务"="service",
                "参与贡献"="contribute",
                "业务能力-官网"="gw",
                "业务能力-买卖"="mm",
                "业务能力-租赁"="zl"
                ),selected="信用分"),
              # selectInput(inputId=ns("plot_comp_top_date_select"),label="请选择经纪人",
              #   choices=jjrList,selected= jjrList[1]) ,                 
              selectInput(inputId=ns("plot_comp_top_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1])   
          ),
          box(title = "买卖经纪人TOP", solidHeader = TRUE,width=5,height=500,
            plotlyOutput(ns("plot_top_mm_jjr"))
              ),

          box(title = "租赁经纪人TOP", solidHeader = TRUE,width=5,height=500,
            plotlyOutput(ns("plot_top_zl_jjr"))
              ),
          h2("经纪人成长曲线 - 买卖"),hr(),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,   
              selectInput(inputId=ns("plot_comp_grow_var_select_mm"),label="请选择分析变量",choices=list(
                "信用分"="score",
                "基础素质"="base",
                "行为规范"="behavior",
                "品质服务"="service",
                "参与贡献"="contribute",
                "业务能力-官网"="gw",
                "业务能力-买卖"="mm"
                ),selected="信用分"),
              selectInput(inputId=ns("plot_comp_grow_date_select_mm"),label="请选择经纪人",
                choices=mm_jjrList,selected= mm_jjrList[1:3],multiple=TRUE,selectize=3) 
          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=10,height=500,       
              plotlyOutput(ns("plot_grow_mm_jjr"))
          ),
          h2("经纪人成长曲线 - 租赁"),hr(),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,   
              selectInput(inputId=ns("plot_comp_grow_var_select_zl"),label="请选择分析变量",choices=list(
                "信用分"="score",
                "基础素质"="base",
                "行为规范"="behavior",
                "品质服务"="service",
                "参与贡献"="contribute",
                "业务能力-官网"="gw",
                "业务能力-租赁"="zl"
                ),selected="信用分"),
              selectInput(inputId=ns("plot_comp_grow_date_select_zl"),label="请选择经纪人",
                choices=zl_jjrList,selected= zl_jjrList[1:3],multiple=TRUE,selectize=3) 
          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=10,height=500,       
              plotlyOutput(ns("plot_grow_zl_jjr"))
          )          
  )
}


# Server
ana_comp_trend_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    output$plot_top_mm_jjr <- renderPlotly(
      top_credit(var=input$plot_comp_top_var_select,
        now_date=input$plot_comp_top_date_select,
        btype="mm")
      )
    output$plot_top_zl_jjr <- renderPlotly(
      top_credit(var=input$plot_comp_top_var_select,
        now_date=input$plot_comp_top_date_select,
        btype="zl")
      )

    output$plot_grow_zl_jjr <- renderPlotly(
      grow_credit(var=input$plot_comp_grow_var_select_zl,
        jjr_list=input$plot_comp_grow_date_select_zl)
    
    )
    output$plot_grow_mm_jjr <- renderPlotly(
      grow_credit(var=input$plot_comp_grow_var_select_mm,
        jjr_list=input$plot_comp_grow_date_select_mm)
    
    )

    }
  )
}



