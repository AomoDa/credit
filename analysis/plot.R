library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)

# 经纪人信用分分布
hist_credit <- function(var,now_date,btype="mm",binwidth=10){
	now_date = as.Date(now_date)
	finalScore$tmp <- finalScore[,var]
	p1 = finalScore %>% 
		filter(credit_date==now_date) %>% 
		filter(business_type %in% btype) %>% 
		ggplot(aes(x=tmp)) + geom_histogram(binwidth=binwidth,col=I("white"))+
		theme_bw() +  theme(text= element_text(family="STXihei")) +  
		labs(x="信用分",y="经纪人数",title=paste0("经纪人信用分分布情况: ",now_date))
	ggplotly(p1)
}

# 经纪人任务完成情况
box_credit <- function(now_date){
	dat = finalScore %>% 
		select(credit_date,id,business_type,
			`基础素质`=base,
			# `行为规范`=behavior,
			`品质服务`=service,
			# `参与贡献`=contribute,
			`官网指标`=gw,
			`买卖指标`=mm,
			`租赁指标`=zl,
			`信用分`=score) %>%
		mutate(`业务类型`=if_else(business_type=="mm","买卖","租赁"))%>%
		select(-business_type)%>%
		gather(var, values, -c(credit_date,id,`业务类型`)) %>%
		filter(credit_date==now_date)
	rvar = with(dat, reorder(var, -values, median))		
	p1 = ggplot(data=dat,aes(x=rvar,y=values,fill=`业务类型`)) + 
		geom_boxplot()+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("各项指标分布: ",now_date))
	return(p1)
}	


# 经纪人任务完成情况
prob_credit <- function(now_date){
	dat = finalScore %>% 
			select(credit_date,id,business_type,
				base,behavior,service,contribute,
				gw,mm,zl,score) %>%
		mutate(credit_date,id,business_type,
			`基础素质`=base/125,
			# `行为规范`=behavior/100,
			`品质服务`=service/180,
			# `参与贡献`=contribute/80,
			`官网指标`=gw/165,
			`买卖指标`=mm/210,
			`租赁指标`=zl/210,
			`信用分`=score/860) %>%
		mutate(`业务类型`=if_else(business_type=="mm","买卖","租赁"))%>%
		select(credit_date:id,`基础素质`:`业务类型`)%>%
		gather(var, values, -c(credit_date,id,`业务类型`)) %>%
		filter(credit_date==now_date)%>%
		group_by(var,`业务类型`) %>%
		summarise(values=mean(values,na.rm=TRUE))
	p1 = ggplot(data=dat,aes(x=var,weight=values,fill=`业务类型`)) + 
		geom_bar(position="dodge")+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("各项指标分布: ",now_date))
	ggplotly(p1)
}	












