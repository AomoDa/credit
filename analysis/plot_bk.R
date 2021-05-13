beike.dat = read_xlsx(path = "data/jinan_beike.xlsx")

score.beike.dat = dplyr::select(beike.dat ,ends_with("分"))


# 经纪人信用分分布
hist_beike <- function(var,binwidth=10){
    binwidth = as.numeric(binwidth)
    finalScore = as.data.frame(score.beike.dat)
    finalScore$tmp = finalScore[,var]
    p1 = finalScore %>% 
        ggplot(aes(x=tmp)) + geom_histogram(binwidth=binwidth,col=I("white"))+
        theme_bw() +  theme(text= element_text(family="STXihei")) +  
        labs(x="信用分",y="经纪人数",title=paste0("贝壳分分布情况: "))
    ggplotly(p1)
}

# 经纪人信用分统计
table_beike <- function(vars){
    finalScore = as.data.frame(score.beike.dat[,vars])
    a=finalScore %>% 
        describe(quant=c(0.25,0.5,0.75),skew=FALSE,IQR=TRUE) %>% 
        t() %>% as.data.frame() %>% round(2)
    a = a[-c(1,8),] 
    row.names(a) <- c("样本量","均值","标准差","最小值","最大值","全距","四分位距","Q25","中位数","Q75")
    return(a)   

}