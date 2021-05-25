# UI
ana_comp_qu_UI <- function(id="credit_new", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "comp_new",height=1200,

          h2("买卖比较"),hr(),
          # box(
          #     title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,   

          #     selectInput(inputId=ns("plot_comp_qu_mm_select"),label="请选择计算日期",
          #       choices=dateList,selected= rev(dateList)[1])            
          # ),
          box(
              title = "买卖新人", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("mm_new"))
          ),
          box(
              title = "买卖新人", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("mm_new_line"))
          ),
          h2("租赁比较"),hr(),
          box(
              title = "租赁新人", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("zl_new"))
          ),
          box(
              title = "租赁新人", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("zl_new_line"))
          )


  )
}


# Server
ana_comp_qu_Server <- function(id="credit_new") {
  moduleServer(
    id,
    function(input, output, session) {



    output$mm_new <- renderPlotly(

      plot_score_new_user_box("mm")
    
    )
    output$mm_new_line <- renderPlotly(

      plot_score_new_user_line("mm")
    
    )
    output$zl_new_line <- renderPlotly(

      plot_score_new_user_line("zl")
    
    )

    output$zl_new <- renderPlotly(
      plot_score_new_user_box("zl")
    
    )




    }
  )
}


