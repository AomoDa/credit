
# UI
arg_base_UI <- function(id="credit", label = "请选择文件") {
  ns <- NS(id)
  tabItem(tabName = "arg_base",height=1200,
          # h2("个人背景"),
          # box(
          #     title = "教育背景", solidHeader = TRUE,width=4,height=400,
          #     sliderInput(inputId = ns("arg_base_edu_1"),label = "大专以下",
          #       value = env$base$jjr_backgroup$education$score[[1]],min = 0,max = 30,step=1),
          #     sliderInput(inputId = ns("arg_base_edu_2"),label = "大专非统招",
          #       value = env$base$jjr_backgroup$education$score[[2]],min = 0,max = 30,step=1),
          #     sliderInput(inputId = ns("arg_base_edu_3"),label = "大专统招",
          #       value = env$base$jjr_backgroup$education$score[[3]],min = 0,max = 30,step=1),
          # ),
          # box(
          #     title = "教育背景", solidHeader = TRUE,width=4,height=400,
          #     sliderInput(inputId = ns("arg_base_edu_4"),label = "本科及以上非统招",
          #       value = env$base$jjr_backgroup$education$score[[4]],min = 0,max = 30,step=1),
          #     sliderInput(inputId = ns("arg_base_edu_5"),label = "本科及以上统招",
          #       value = env$base$jjr_backgroup$education$score[[5]],min = 0,max = 30,step=1), 
          # ),          
          # box(
          #     title = "特长 & 政治背景", solidHeader = TRUE,width=4,height=400,
          #     sliderInput(inputId = ns("arg_base_hobby_1"),label = "足球、篮球、羽毛球等特长",
          #       value = env$base$jjr_backgroup$hobby$score,min = 0,max = 10,step=1),
          #     sliderInput(inputId = ns("arg_base_political_party"),label = "党员",
          #       value = env$base$jjr_backgroup$political$score$party,min = 0,max = 10,step=1),
          #     sliderInput(inputId = ns("arg_base_political_veteran"),label = "退伍军人",
          #       value = env$base$jjr_backgroup$political$score$veteran,min = 0,max = 10,step=1)
          # ),  
          h2("司龄"),        
          box(title="司龄",width=4,height=400,
              selectInput(inputId="agr_base_entry_lidu",label="计算粒度",
                choices=c("月"),selected="月"),            
              selectInput(inputId="agr_base_entry",label="计算方法司龄",
                choices=c("线性增加","平滑曲线"),selected="线性增加"),
              p("线性增加：按照司龄每增加1个月,信用积分增加固定分"),
              p("平滑曲线：按照数学公式进行信用积分增"),

              ),
          box(title="司龄 - 简单计算设置",width=4,height=400,
                numericInput(inputId = ns("arg_base_entrt_simple"),label = "司龄每增加1个月,信用积分将增加",
                value = env$base$enter$score$simple,min = 0,max = 5,step=1),
                hr(),uiOutput(ns("entry_simple_help")),
                p("其中："),
                p("\tS为信用积分,0 <= S <= 50"),
                p("\tx为司龄，单位：月")
              ),

          box(title="司龄 - 公示转化设置",width=4,height=400,
                numericInput(inputId = ns("arg_base_entrt_transform"),label = "公式参数设置",
                value = env$base$enter$score$transform,min = 12,max =120 ,step=6),
                hr(),
                uiOutput(ns("entry_transform_help")),
                p("其中："),
                p("\tS为信用积分,0 <= S < 50"),
                p("\tx为司龄，单位：月"),
                p("\te为自然数")
              ),

          box(title="司龄 - 信用积分变化趋势",width=6, plotOutput(ns("entry_plot1"))),
          box(title="司龄 - 新增信用积分变化趋势",width=6, plotOutput(ns("entry_plot2"))),


          # h2("执业认证 & 每日一考"),     
          # box(title="经纪人执业认证",width=4,height=400,
          #    sliderInput(inputId = ns("arg_base_qualification_1"),label = "杭州经纪人证",
          #       value = env$base$qualification$score[[1]],min = 0,max = 10,step=1),
          #     sliderInput(inputId = ns("arg_base_qualification_2"),label = "全国协理证",
          #       value = env$base$qualification$score[[2]],min = 0,max = 10,step=1),
          #     sliderInput(inputId = ns("arg_base_qualification_3"),label = "全国经纪人资格证",
          #       value = env$base$qualification$score[[3]],min = 0,max = 10,step=1),
          #     ),
          # box(title="每日一考",width=4,height=400,
          #    sliderInput(inputId = ns("arg_base_exam_1"),label = "96~100分",
          #       value =  env$base$exam$score[[1]],min = -10,max = 20,step=1),
          #    sliderInput(inputId = ns("arg_base_exam_2"),label = "91~95分",
          #       value = env$base$exam$score[[2]],min = -10,max = 20,step=1),         
          #     ),
          # box(title="每日一考",width=4,height=400,
          #   sliderInput(inputId = ns("arg_base_exam_3"),label = "80-90分",
          #       value = env$base$exam$score[[3]],min = -10,max = 20,step=1),
          #   sliderInput(inputId = ns("arg_base_exam_4"),label = "80分以下（不含）",
          #       value = env$base$exam$score[[4]],min = -10,max = 20,step=1),            
          #     ),



        
  )
}


# Server
arg_base_Server <- function(id="credit") {
  moduleServer(
    id,
    function(input, output, session) {

    # observe({
    #   print("base - observe")
    #   env$base$enter$score$transform = input$arg_base_entrt_transform
    #   })

    output$entry_simple_help <-  renderUI({
      math = paste0("$$S=",input$arg_base_entrt_simple,"\\times x$$")
      txt = paste0("司龄信用积分计算公式为:",math,"\n")
      withMathJax(helpText(txt))
      })

    output$entry_transform_help <-  renderUI({
      math = paste0("$$S=\\frac{2}{1+e^{-4x/",input$arg_base_entrt_transform,"}}-1$$")
      txt = paste0("司龄信用积分计算公式为:",math,"\n")
      withMathJax(helpText(txt))
      })
    # 积分
    output$entry_plot1 <-  renderPlot({
        entry_para_plot_1(a=input$arg_base_entrt_simple,t=input$arg_base_entrt_transform)
      })
    # 新增积分
    output$entry_plot2 <-  renderPlot({
        entry_para_plot_2(a=input$arg_base_entrt_simple,t=input$arg_base_entrt_transform)
      })

    }
  )
}










