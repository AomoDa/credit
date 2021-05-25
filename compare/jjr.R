
XdateList = unique(kpi.dat$date)
Xmm_jjrList = unique(finalScore$name[finalScore$business=="mm"])
Xzl_jjrList = unique(finalScore$name[finalScore$business=="zl"])

# UI
ana_comp_jjr_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "comp_jjr",height=1200,

          h2("经纪人成长曲线 - 买卖"),hr(),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,   

              # selectInput(inputId=ns("plot_comp_jjr_mm"),label="请选择经纪人",
              #   choices=Xmm_jjrList,selected= Xmm_jjrList[1:3],multiple=TRUE,selectize=3),
              selectizeInput(inputId=ns("plot_comp_jjr_mm"),label="请选择经纪人",
                choices=NULL,multiple=TRUE,options = list(maxOptions = 20)),

              selectInput(inputId=ns("plot_comp_jjr_mm_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1])            
          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=5,height=500,       
              plotlyOutput(ns("p_mm"))
          ),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=5,height=500,       
              plotlyOutput(ns("p_mm2"))
          ),
          h2("经纪人成长曲线 - 租赁"),hr(),
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=2,height=500,   

              # selectInput(inputId=ns("plot_comp_jjr_zl"),label="请选择经纪人",
              #   choices=Xzl_jjrList,selected= Xzl_jjrList[1:3],multiple=TRUE,selectize=3),
              selectizeInput(inputId=ns("plot_comp_jjr_zl"),label="请选择经纪人",
                choices=NULL,multiple=TRUE,options = list(maxOptions = 20)),

              selectInput(inputId=ns("plot_comp_jjr_zl_date_select"),label="请选择计算日期",
                choices=dateList,selected= rev(dateList)[1])                    
          ) ,
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=5,height=500,       
              plotlyOutput(ns("p_zl"))
          ),   
          box(
              title = "经纪人信用分分布情况", solidHeader = TRUE,width=5,height=500,       
              plotlyOutput(ns("p_zl2"))
          )

  )
}


# Server
ana_comp_jjr_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

  updateSelectizeInput(session, 'plot_comp_jjr_mm', 
    choices = Xmm_jjrList, server = TRUE,
    selected=Xmm_jjrList[1:3])  

  updateSelectizeInput(session, 'plot_comp_jjr_zl', 
    choices = Xzl_jjrList, server = TRUE,
    selected=Xzl_jjrList[1:3]) 


    output$p_zl <- renderPlotly(
      grow_credit(var="score",
        jjr_list=input$plot_comp_jjr_zl)
    
    )
    output$p_zl2 <- renderPlotly(
      bar_jjr_credit(now_date=input$plot_comp_jjr_zl_date_select,
        jjr_list=input$plot_comp_jjr_zl)
    
    )    
    output$p_mm <- renderPlotly(
      grow_credit(var="score",
        jjr_list=input$plot_comp_jjr_mm)
    
    )

    output$p_mm2 <- renderPlotly(
      bar_jjr_credit(now_date=input$plot_comp_jjr_mm_date_select,
        jjr_list=input$plot_comp_jjr_mm)
    
    )



    }
  )
}


