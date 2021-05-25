
# UI
ana_comp_new_UI <- function(id="credit_qu", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "comp_qu",height=1200,

          h2("买卖比较"),hr(),
          # box(
          #     title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,   

          #     selectInput(inputId=ns("plot_comp_qu_mm_select"),label="请选择计算日期",
          #       choices=dateList,selected= rev(dateList)[1])            
          # ),
          box(
              title = "买卖大区", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("mm_qu"))
          ),
          box(
              title = "买卖职级", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("mm_level"))
          ),  

          h2("租赁比较"),hr(),        
          box(
              title = "租赁大区", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("zl_qu"))
          ),
          box(
              title = "租赁职级", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("zl_level"))
          )



  )
}


# Server
ana_comp_new_Server <- function(id="credit_qu") {
  moduleServer(
    id,
    function(input, output, session) {


    output$mm_qu<- renderPlotly(
      plot_score_daqu_mm(now_date=as.Date("2021-03-01"))
    
    )

    output$zl_qu<- renderPlotly(
      plot_score_daqu_zl(now_date=as.Date("2021-03-01"))
  
    )

    output$mm_level<- renderPlotly(
      plot_score_level_mm(now_date=as.Date("2021-03-01"))
    
    )

    output$zl_level<- renderPlotly(
      plot_score_level_zl(now_date=as.Date("2021-03-01"))
  
    )




    }
  )
}


