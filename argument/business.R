
# UI
arg_business_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_zhuanhua",height=1200,

         h2("转化公式参数设置"),  
         box(title="激励曲线参数设置",width=2,height=400,
              numericInput(inputId = ns("zh_gw_alpha"),label = HTML("&alpha;"),
              value = 2,min = 0,max = 5,step=1),
              numericInput(inputId = ns("zh_gw_beta"),label = HTML("&beta;"),
              value = 3,min = 1,max = 20,step=1),
              numericInput(inputId = ns("zh_gw_succ"),label = "Max S",
              value = 10,min = 10,max = 20,step=1),
              numericInput(inputId = ns("zh_gw_n"),label = "Max N",
              value = 10,min = 10,max = 30,step=1)

              ),         
          box(title="简单公式",width=5,height=400,
                uiOutput(ns("arg_zh_gw_simple")),
                p("其中："),
                p("P为目标率,0 <= P <= 1"),
                p("S为成功量，例如录入量、接听量"),
                p("N为总量，例如线索量")
              ),
          box(title="转化公式",width=5,height=400,
                p("原公式："),
                uiOutput(ns("arg_zh_gw_transform_0")),
                # p("代入参数后："),
                # uiOutput(ns("arg_zh_gw_transform")),
                p("其中："),
                p("P为目标率,0 <= P <= 1"),
                p("S为成功量，例如录入量、接听量"),
                p("N为总量，例如线索量")
              ),
          box(title="简单结果",width=12, plotOutput(ns("zh_gw_simple_result"))),
          box(title="转化结果",width=12, plotOutput(ns("zh_gw_transform_result"))),
          box(title="结果对比",width=12,height=500,DTOutput(ns("zh_gw_tb"))) 


          # hr(),h2("400接听率参数对比"),        
          # box(title="400接听率",width=4,height=400,
          #     selectInput(inputId="arg_business_400_lidu",label="计算粒度",
          #       choices=c("月"),selected="月"),            
          #     selectInput(inputId="arg_business_400_method",label="计算方法司龄",
          #       choices=c("简单计算","区间计算"),selected="简单计算")

          #     ),

          # box(title="400接听率 - 简单计算设置",width=4,height=400,
          #       uiOutput(ns("arg_400_simple")),
          #       p("其中："),
          #       p("P为400接听率,0 <= P <= 1"),
          #       p("S为接听量"),
          #       p("N为呼叫量")
          #     ),

          # box(title="400接听率  - 公示转化设置",width=4,height=400,
          #       uiOutput(ns("arg_400_transform")),
          #       p("其中："),
          #       p("P为400接听率,0 <= P <= 1"),
          #       p("S为接听量"),
          #       p("N为呼叫量"),
          #       p("z为常数，固定值为1.96")
          #     ),
          # box(title="400接听率 - 简单计算",width=6, plotOutput(ns("agr_400_plot_simple"))),
          # box(title="400接听率 - 区间计算",width=6, plotOutput(ns("agr_400_plot_transform"))),          



  )
}


# Server
arg_business_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    output$arg_zh_gw_transform_0 <-  renderUI({
      math = paste0("$$P=\\frac{Log(S+\\alpha)/Log(N+\\beta) + z^2/(2 (N+\\beta) )}{1+z^2/(N+\\beta)}\\times 100\\%$$")
      txt = paste0("",math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_zh_gw_transform <-  renderUI({
      ss = paste0("S+",input$zh_gw_alpha)
      nn = paste0("N+",input$zh_gw_beta)
      math = paste0("$$P=\\frac{Log(",ss,")/Log(",nn,") + z^2/(2*( ",nn,") )}{1+z^2/( ",nn," )}\\times 100\\%$$")
      txt = paste0("",math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_zh_gw_simple <-  renderUI({
      math = paste0("$$P=\\frac{S}{N}","\\times 100\\%$$")
      txt = paste0("",math,"\n")
      withMathJax(helpText(txt))
      })

    output$zh_gw_simple_result <-  renderPlot({
        plot_zh_gw_simple(input$zh_gw_alpha,
                          input$zh_gw_beta,
                          input$zh_gw_succ,
                          input$zh_gw_n)
      })
    output$zh_gw_transform_result <-  renderPlot({
        plot_zh_gw_transform(input$zh_gw_alpha,
                          input$zh_gw_beta,
                          input$zh_gw_succ,
                          input$zh_gw_n)
      })   

  output$zh_gw_tb <- DT::renderDT(
        tb_zh_gw_in(input$zh_gw_alpha,
                          input$zh_gw_beta,
                          input$zh_gw_succ,
                          input$zh_gw_n),
         rownames=FALSE
         )


    # output$arg_400_simple <-  renderUI({
    #   math = paste0("$$P_{400}=\\frac{S}{N}","\\times 100\\%$$")
    #   txt = paste0("400接听率计算公式为:",math,"\n")
    #   withMathJax(helpText(txt))
    #   })

    # output$arg_400_transform <-  renderUI({
    #   math = paste0("$$P_{400}=\\frac{Log(S+2)/Log(N+3) + z^2/(2N+6)}{1+z^2/(N+3)}\\times 100\\%$$")
    #   txt = paste0("400接听率算公式为:",math,"\n")
    #   withMathJax(helpText(txt))
    #   })


    # output$agr_400_plot_simple <-  renderPlot({
    #     plot_400_simple()
    #   })
    # output$agr_400_plot_transform <-  renderPlot({
    #     plot_400_transform()
    #   })    


    }
  )
}



