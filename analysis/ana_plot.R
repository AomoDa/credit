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



table_credit <- function(now_date,bty){
	now_date = as.Date(now_date)
	a=finalScore %>% 
		filter(business_type %in% bty) %>%
		filter(credit_date==now_date) %>%
		select(`信用分`=score,
				`基础素质`=base,
				`行为规范`=behavior,
				`品质服务`=service,
				`参与贡献`=contribute,
				`官网指标`=gw,
				`买卖指标`=mm,
				`租赁指标`=zl) %>%
		describe(quant=c(0.25,0.75),skew=FALSE,IQR=TRUE) %>% 
		t() %>% as.data.frame() %>% round(2)
	a = a[-c(1,8),]	
	row.names(a) <- c("样本量","均值","标准差","最小值","最大值","全距","四分位距","Q25","Q75")
	return(a)	

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
		summarise(values=mean(values,na.rm=TRUE),groups="drop")
	p1 = ggplot(data=dat,aes(x=var,weight=values,fill=`业务类型`)) + 
		geom_bar(position="dodge")+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("各项指标分布: ",now_date))
	ggplotly(p1)
}	




# 基础素质

table_base <- function(now_date,bty){
	now_date = as.Date(now_date)
	a=finalScore %>% 
		filter(business_type %in% bty) %>%
		filter(credit_date==now_date) %>%
		select(
				`基础素质`=base,
                "教育背景"="education",
                "特长"="hobby",
                "政治背景"="political",
                "司龄"="entry",
                "执业认证"="qualification",
                "每日一考"="exam"
				) %>%
		describe(quant=c(0.25,0.5,0.75),skew=FALSE,IQR=TRUE) %>% 
		t() %>% as.data.frame() %>% round(2)
	a = a[-c(1,8),]	
	row.names(a) <- c("样本量","均值","标准差","最小值","最大值","全距","四分位距","Q25","中位数","Q75")
	return(a)	

}


# 官网分数

table_gw <- function(now_date,bty){
	now_date = as.Date(now_date)
	a=finalScore %>% 
		filter(business_type %in% bty) %>%
		filter(credit_date==now_date) %>%
		select(
				`官网指标`=gw,
                "渗透"="app",
                "400接听率"="call_answer",
                "爱聊一分钟响应率"="al_1min",
                "爱聊三日复聊率"="al_3day",
                "爱聊录入率"="al_luru",
                "爱聊转带看"="al_daikan"
				) %>%
		describe(quant=c(0.25,0.5,0.75),skew=FALSE,IQR=TRUE) %>% 
		t() %>% as.data.frame() %>% round(2)
	a = a[-c(1,8),]	
	row.names(a) <- c("样本量","均值","标准差","最小值","最大值","全距","四分位距","Q25","中位数","Q75")
	return(a)	

}

# 买卖指标
table_mm <- function(now_date,bty="mm"){
	now_date = as.Date(now_date)
	a=finalScore %>% 
		filter(business_type %in% bty) %>%
		filter(credit_date==now_date) %>%
		select(
				`买卖总分`=mm,
                "买卖成交"="mm_deal",
                "速销"="mm_sx",
                "折扣率"="mm_zk",
                "客户首看"="mm_csk",
                "客户带看组数"="mm_cdk",
                "房源新增"="mm_fxz",
                "认证委托"="mm_wt",
                "钥匙"="mm_ys"
				) %>%
		describe(quant=c(0.25,0.5,0.75),skew=FALSE,IQR=TRUE) %>% 
		t() %>% as.data.frame() %>% round(2)
	a = a[-c(1,8),]	
	row.names(a) <- c("样本量","均值","标准差","最小值","最大值","全距","四分位距","Q25","中位数","Q75")
	return(a)	

}


# 租赁指标
table_zl <- function(now_date,bty="zl"){
	now_date = as.Date(now_date)
	a=finalScore %>% 
		filter(business_type %in% bty) %>%
		filter(credit_date==now_date) %>%
		select(
				`租赁总分`=zl,
                "租赁成交"="zl_deal",
                "房管收房"="zl_sfg",
                "房管出房"="zl_cfg",
                "在管量"="zl_zg",
                "委托续签量"="zl_wt",
                "新增客户"="zl_cxz",
                "房源新增"="zl_fxz",
                "普租实勘"="zl_pzsk"
				) %>%
		describe(quant=c(0.25,0.5,0.75),skew=FALSE,IQR=TRUE) %>% 
		t() %>% as.data.frame() %>% round(2)
	a = a[-c(1,8),]	
	row.names(a) <- c("样本量","均值","标准差","最小值","最大值","全距","四分位距","Q25","中位数","Q75")
	return(a)	

}















