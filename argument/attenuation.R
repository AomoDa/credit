
# UI
arg_attenuation_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_attenuation",height=1200,
         h2("衰减算法 - 参数对比") ,       
          box(title="衰减计算方法",width=4,height=800,
                uiOutput(ns("arg_atten_math1")),
                p("其中："),
                p("S为最终得分"),
                p("X为每周期的粉"),
                p("w为权重，通常情况下为0～1之间"),
                p("n为衰减周期；n=1代表当前周期，n=2为上个周期，n=3为上上个周期")
              ),
          box(title="例子 - 1",width=4,height=800,
                p("假设每月的得分表为："),
                tableOutput(ns("d1")),
                p("假设衰减周期为6个月，且衰减系数如图"),
                tableOutput(ns("w1")),
                p("最终得分为"),
                uiOutput(ns("arg_atten_math2")),
              ),          
          box(title="例子 - 2",width=4,height=800,
                p("假设每月的得分表为："),
                tableOutput(ns("d2")),
                p("假设衰减周期为6个月，且衰减系数如图"),
                tableOutput(ns("w2")),
                p("最终得分为"),
                uiOutput(ns("arg_atten_math3")),
              ),   

  )
}



d1 = data.frame(`日期`=c("2021-05","2021-04","2021-03","2021-02",
                      "2021-01","2020-12","2020-11","2020-10"),
                 `得分` = c(10,8,10,8,5,10,10,9))
w1 = data.frame(`权重`=c("w1","w2","w3","w4","w5","w6"),
                `权重值`=c(1,0.8,0.5,0.3,0.2,0.1))
d2 = data.frame(`日期`=c("2021-05","2021-04","2021-03"),
                 `得分` = c(10,8,10))

# Server
arg_attenuation_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    output$arg_atten_math1 <-  renderUI({
      math = paste0("$$\\begin{aligned}S&=\\sum_i^n X_i w_i \\\\ &= X_1w_1 + X_2w_2 + ...+X_nw_n\\end{aligned}$$")
      txt = paste0("400接听率计算公式为:",math,"\n")
      withMathJax(helpText(txt))
      })
  
    output$arg_atten_math2 <-  renderUI({
      math = paste0("$$S=10\\times 1.0 +  8\\times 0.8 + \\\\ 
                         10 \\times 0.5 + 8 \\times 0.3 + \\\\ 
                         5 \\times 0.2 + 10 \\times 0.1 \\\\ = 25.8$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$arg_atten_math3 <-  renderUI({
           math = paste0("$$S=10\\times 1.0 +  8\\times 0.8 + \\\\ 
                         10 \\times 0.5 \\\\ = 21.4$$")
      txt = paste0(math,"\n")
      withMathJax(helpText(txt))
      })

    output$d1 <- renderTable(d1)
    output$d2 <- renderTable(d2)
    output$w1 <- renderTable(w1)
    output$w2 <- renderTable(w1)

    }
  )
}



