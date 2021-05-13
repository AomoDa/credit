
ns.beike = names(score.beike.dat) 
# UI
ana_bk_UI <- function(id="credit_beike", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "comp_beike",height=1200,
        h2("济南贝壳分分析"),

          box(
              title = "贝壳分", solidHeader = TRUE,width=6,height=500,
              selectInput(inputId=ns("ana_plot_beike_var"),label="请选择计算日期",
                selected=ns.beike[1],choices= ns.beike,multiple=FALSE),           
              selectInput(inputId=ns("plot_beike_bin_select"),label="请选择条形宽度",
                choices=c(0.1,0.2,0.5,1,2,5,10,20,30,50),selected= 10)

          ),



          box(
              title = "贝壳分分布情况", solidHeader = TRUE,width=6,height=500,       
              plotlyOutput(ns("plot_beike_1"))
          ),



          #-------------------------------
          # 统计表格 -1 
          #-------------------------------
          box(
              title = "贝壳分", solidHeader = TRUE,width=4,height=600,
              selectInput(inputId=ns("ana_tb_beike_vars"),label="请选择计算日期",
                selected=ns.beike[1:6],choices= ns.beike[1:6],multiple=TRUE)           

          ),          
          box(
              title = "贝壳分统计分布", solidHeader = TRUE,width=8,height=600,
              DTOutput(ns("tb_beike_1"))

          ),

          #-------------------------------
          # 统计表格 -2 
          #-------------------------------
          box(
              title = "贝壳分", solidHeader = TRUE,width=4,height=600,
              selectInput(inputId=ns("ana_tb_beike_vars2"),label="请选择计算日期",
                selected=ns.beike[7:12],choices= ns.beike[7:length(ns.beike)],multiple=TRUE)           

          ),          
          box(
              title = "贝壳分统计分布", solidHeader = TRUE,width=8,height=600,
              DTOutput(ns("tb_beike_2"))

          )



  )
}


# Server
ana_bk_Server <- function(id="credit_beike") {
  moduleServer(
    id,
    function(input, output, session) {

     output$tb_beike_1 <- renderDataTable(
      table_beike(input$ana_tb_beike_vars)
        
      )

    output$plot_beike_1 <- renderPlotly(
      hist_beike(input$ana_plot_beike_var,input$plot_beike_bin_select)
      )            

     output$tb_beike_2 <- renderDataTable(
      table_beike(input$ana_tb_beike_vars2)
        
      )

    }
  )
}



