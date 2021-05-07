
# UI
arg_business_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_business",height=1200,
         h2("400接听率参数对比"),        
          box(title="400接听率",width=4,height=400,
              selectInput(inputId="arg_business_400_lidu",label="计算粒度",
                choices=c("月"),selected="月"),            
              selectInput(inputId="arg_business_400_method",label="计算方法司龄",
                choices=c("简单计算","区间计算"),selected="简单计算")

              ),

          box(title="400接听率 - 简单计算设置",width=4,height=400,
                uiOutput(ns("arg_400_simple")),
                p("其中："),
                p("P为400接听率,0 <= S <= 1"),
                p("S为接听量"),
                p("N为呼叫量")
              ),

          box(title="400接听率  - 公示转化设置",width=4,height=400,
                uiOutput(ns("arg_400_transform")),
                p("其中："),
                p("P为400接听率,0 <= S <= 1"),
                p("S为接听量"),
                p("N为呼叫量"),
                p("z为常数，固定值为1.96")
              ),
          box(title="400接听率 - 简单计算",width=6, plotOutput(ns("agr_400_plot_simple"))),
          box(title="400接听率 - 区间计算",width=6, plotOutput(ns("agr_400_plot_transform"))),          



  )
}


# Server
arg_business_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    output$arg_400_simple <-  renderUI({
      math = paste0("$$P_{400}=\\frac{S}{N}","\\times 100\\%$$")
      txt = paste0("400接听率计算公式为:",math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_400_transform <-  renderUI({
      math = paste0("$$P_{400}=\\frac{Log(S+2)/Log(N+2) + z^2/(2N+4)}{1+z^2/(N+2)}\\times 100\\%$$")
      txt = paste0("400接听率算公式为:",math,"\n")
      withMathJax(helpText(txt))
      })

    output$agr_400_plot_simple <-  renderPlot({
        plot_400_simple()
      })
    # 新增积分
    output$agr_400_plot_transform <-  renderPlot({
        plot_400_transform()
      })    


    }
  )
}



