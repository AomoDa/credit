
# UI
arg_jili_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_jili",height=1200,
         h2("激励曲线")  , 

          box(title="A阶段计算公式",width=4,height=400,
                uiOutput(ns("arg_jili_a_step")),
                p("其中："),
                uiOutput(ns("arg_jili_a_step_x")),
                p("常数项："),
                uiOutput(ns("arg_jili_a_step_cs")),
                p("e为自然数")
              ),
          box(title="B阶段计算公式",width=4,height=400,
                uiOutput(ns("arg_jili_b_step")),
                p("其中："),
                uiOutput(ns("arg_jili_b_step_x")),
                p("常数项："),
                uiOutput(ns("arg_jili_b_step_cs")),
                p("Log为自然对数")
              ),
          box(title="C阶段计算公式",width=4,height=400,
                uiOutput(ns("arg_jili_c_step")),
                p("其中："),
                uiOutput(ns("arg_jili_c_step_x")),
                p("常数项："),
                uiOutput(ns("arg_jili_c_step_cs")),
                p("e为自然数")
              ),
          box(title="业务指标 - 激励曲线示意图",width=12, plotOutput(ns("p_jili_example"))),
       
         h2("指标测试"),        
         box(title="激励曲线参数设置",width=2,height=500,
              numericInput(inputId = ns("jili_alpha"),label = HTML("&alpha;"),
              value = 3,min = 1,max = 10,step=1),
              numericInput(inputId = ns("jili_y_alpha"),label = HTML("Y&alpha;"),
              value = 0.6,min = 0.1,max = 1,step=0.1),
              numericInput(inputId = ns("jili_beta"),label = HTML("&beta;"),
              value = 10,min = 5,max = 20,step=1),
              numericInput(inputId = ns("jili_y_beta"),label = HTML("Y&beta;"),
              value = 0.9,min = 0.8,max = 1,step=0.05)
              ),
          box(title="业务指标 - 激励曲线示意图",width=5,height=500,plotOutput(ns("p_jili_test"))),
          box(title="业务指标 - 曲线增长示意图",width=5,height=500,plotOutput(ns("p_jili_test_trend"))),
          box(title="业务指标 - 曲线增长示意图",width=12,height=500,DTOutput(ns("arg_jili_tb")))
          )
}


# Server
arg_jili_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {


    output$p_jili_example <- renderPlot(plot_jili_sim_example())  
    output$p_jili_test <- renderPlot(
        plot_jili_sim_test(alpha = input$jili_alpha,
                           beta = input$jili_beta,
                           c_alpha=input$jili_y_alpha,
                           c_beta=input$jili_y_beta)
      )  
    output$p_jili_test_trend <- renderPlot(
        plot_jili_sim_test_trend(alpha = input$jili_alpha,
                           beta = input$jili_beta,
                           c_alpha=input$jili_y_alpha,
                           c_beta=input$jili_y_beta)
      ) 
    
    output$arg_jili_tb <- DT::renderDT(
         tb_jili_in(alpha = input$jili_alpha,
                           beta = input$jili_beta,
                           c_alpha=input$jili_y_alpha,
                           c_beta=input$jili_y_beta),
         rownames=FALSE
         )

    output$arg_jili_a_step <-  renderUI({
      math = paste0("$$S= \\frac{Y_{\\alpha}}{e^{\\alpha}-1}e^x + Y_{\\alpha} - \\frac{e^{\\alpha}Y_{\\alpha}}{e^{\\alpha}-1}$$")
      txt = paste0("",math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_b_step <-  renderUI({
      math = paste0("$$S= \\frac{Y_{\\beta}-Y_{\\alpha}}{Log(\\beta)-Log(\\alpha)}Log(x) + Y_{\\beta} - \\frac{Y_{\\beta}(Y_{\\beta}-Y_{\\alpha})}{Log(\\beta)-Log(\\alpha)}$$")
      txt = paste0("",math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_c_step <-  renderUI({
      math = paste0("$$S=\\frac{2(1-Y_{\\beta})}{ (1-e^{(\\beta-x)/(\\alpha+\\beta)})} + 2Y_{\\beta} - 1$$")
      txt = paste0("",math,"\n")
      withMathJax(helpText(txt))
      })


    output$arg_jili_a_step_x <-  renderUI({
      math = paste0("$$0 < x\\leq \\alpha$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_b_step_x <-  renderUI({
      math = paste0("$$ \\alpha < x\\leq \\beta$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_c_step_x <-  renderUI({
      math = paste0("$$x> \\beta$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_a_step_cs <-  renderUI({
      math = paste0("$$Y_{\\alpha},Y_{\\beta}$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_b_step_cs <-  renderUI({
      math = paste0("$$Y_{\\alpha},Y_{\\beta}$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_jili_c_step_cs <-  renderUI({
      math = paste0("$$Y_{\\alpha},Y_{\\beta}$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })


    }
  )
}



