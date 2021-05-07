
# UI
arg_overview_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_overview",height=1200,
         fluidRow(
          valueBoxOutput(ns("arg_overview_base"))
          ),

  )
}


# Server
arg_overview_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

      output$arg_overview_base<- renderValueBox({
        valueBox(value="基础素质",
          subtitle=350,
          icon = icon("address-card")
        )        
        })


    }
  )
}



