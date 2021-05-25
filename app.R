# name
projectName = "爱家信用分"
# library
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(DT)
library(logger)
library(psych)
# source
source(file = "env.R")
source(file = "func.R")
source(file = "./data/score.R")
source(file = "./data/clean.R")
source(file = "./data/calculate.R")
source(file = "./analysis/ana_plot.R")
source(file = "./analysis/overview.R")
source(file = "./analysis/base.R")
source(file = "./analysis/gw.R")
source(file = "./analysis/plot_bk.R")
source(file = "./analysis/beike.R")
source(file = "./analysis/business.R")
source(file = "./argument/overview.R")
source(file = "./argument/arg_plot.R")
source(file = "./argument/base.R")
source(file = "./argument/attenuation.R")
source(file = "./argument/behavior.R")
source(file = "./argument/gw.R")
source(file = "./argument/jili.R")
source(file = "./compare/comp_plot.R")
source(file = "./compare/trend.R")
source(file = "./compare/jjr.R")
source(file = "./overview/project.R")
source(file = "./compare/qu.R")
source(file = "./compare/new.R")

# body
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("背景说明", tabName = "oo_oo", icon = icon("bookmark")),        
        menuItem("参数比较", tabName = "argument", icon = icon("th"),
                menuSubItem("衰减算法",tabName="arg_attenuation"),
                menuSubItem("Logit算法",tabName="arg_base"),
                menuSubItem("转化秩",tabName="arg_zhuanhua"),
                menuSubItem("增长曲线",tabName="arg_jili")
            ),
        menuItem("数据分析", tabName = "analysis", icon = icon("calculator"),startExpanded=TRUE,
                menuSubItem("信用分",tabName="ana_overview",selected=TRUE),
                menuSubItem("基础素质",tabName="ana_base"),
                # menuSubItem("行为规范",tabName="ana_behavior"),
                # menuSubItem("品质服务",tabName="ana_service"),
                # menuSubItem("参与贡献",tabName="ana_contribute"),
                menuSubItem("官网指标",tabName="ana_business_gw"),
                menuSubItem("业务能力",tabName="ana_business")
            ),
        menuItem("对比分析", tabName = "compare", icon = icon("balance-scale"),
                menuSubItem("趋势对比",tabName="comp_trend"),
                menuSubItem("经纪人对比",tabName="comp_jjr"),
                menuSubItem("分组对比",tabName="comp_qu"),  
                menuSubItem("新人成长",tabName="comp_new"),                  
                menuSubItem("竞对分析",tabName="comp_beike")
            )                  
        )
)

body <- dashboardBody(
    tags$head(tags$style("section.content { overflow-y: hidden; }")),
    tabItems(
        # #----------------------
        # # 参数设置
        arg_overview_UI(),
        arg_base_UI(),
        arg_behavior_UI(),
        arg_business_UI(),
        arg_attenuation_UI(),
        arg_jili_UI(),
        # # #----------------------
        # # # 数据分析
        ana_overview_UI(),
        ana_business_UI(),
        ana_base_UI(),
        ana_business_gw_UI(),
        #----------------------
        # # 对比分析
        ana_comp_trend_UI(),
        ana_comp_jjr_UI(),
        ana_bk_UI(),
        ana_comp_qu_UI(),
        ana_comp_new_UI(),        
        #----------------------
        # # 其他 
        oo_oo_UI()       
        )
)

# Define server 
server <- function(input, output,session) {
    #----------------------
    # # 参数设置
    arg_overview_Server()
    arg_base_Server()
    arg_behavior_Server()
    arg_business_Server()
    arg_attenuation_Server()
    arg_jili_Server()
    # #----------------------
    # # # 数据分析
    ana_overview_Server()
    ana_business_Server()
    ana_base_Server()
    ana_business_gw_Server()
    #----------------------
    # # 对比分析
    ana_comp_trend_Server()
    ana_comp_jjr_Server()
    ana_bk_Server()
    ana_comp_new_Server()
    ana_comp_qu_Server()
    #----------------------
    # # 其他
    oo_oo_Server()

}

# Define UI 
ui <- dashboardPage(dashboardHeader(title = projectName),sidebar,body)
# Run the application 
shinyApp(ui = ui, server = server)


