
# UI
arg_behavior_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_behavior",height=1200,
    box(title="司龄 - 信用积分变化趋势",width=6, actionButton(ns("reload"),"R")),
    box(title="司龄 - 信用积分变化趋势",width=6, verbatimTextOutput(ns("txt")))

  )
}



# Server
arg_behavior_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    output$txt <- renderPrint({
    paste("111", "&", "222 : ",env$base$enter$score$transform)
   })


    }
  )
}



