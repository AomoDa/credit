
# UI
oo_oo_UI <- function(id="oo_oo", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "oo_oo",height=1200,
         h2("项目介绍") ,       
          box(title="",
            verbatimTextOutput(ns("para_env"))
              ) 

  )
}


# Server
oo_oo_Server <- function(id="oo_oo") {
  moduleServer(
    id,
    function(input, output, session) {

      output$para_env <- renderText(print("写点什么"))


    }
  )
}



