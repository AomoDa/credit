#经纪人TOP榜单

top_credit <- function(var,now_date,btype="mm"){
	now_date = as.Date(now_date)
	finalScore$tmp <- round(finalScore[,var])
	dat = finalScore %>% 
		filter(credit_date==now_date) %>% 
		filter(business_type == btype) %>% 
		select(name,tmp) %>% 
		arrange(desc(tmp)) %>% head(20) 
	ns <- with(dat, reorder(name, tmp, sum))	
	p1 = ggplot(data=dat,aes(y=ns,weight=tmp,fill=ns)) + 
	     geom_bar( show.legend =FALSE)+
		theme_bw() +  theme(text= element_text(family="STXihei")) + 
		theme(axis.text.y = element_text(angle=45))+ 
		labs(x="",y="",title=paste0("经纪人信用分TOP20: ",now_date)) +
		coord_cartesian(xlim = c(min(dat$tmp)-10, max(dat$tmp)-10))
	ggplotly(p1)
}

# 经纪人成长趋势
grow_credit <- function(var,jjr_list){
	finalScore$tmp <- round(finalScore[,var])
	p1 = finalScore %>% 
		filter(name %in% jjr_list) %>% 
 		ggplot(aes(x=credit_date,y=tmp,col=name,shape=name)) + 
	    geom_line( show.legend =FALSE) +
	    geom_point(show.legend =FALSE) + 
		theme_bw() +  
		theme(text= element_text(family="STXihei")) + 
		labs(x="",y="",title=paste0("经纪人信用分成长曲线 ")) +
		guides(col = guide_legend(title.position = "top"))
	ggplotly(p1)
}

# 经纪人分数对比


# 经纪人任务完成情况
bar_jjr_credit <- function(now_date,jjr_list){
	now_date = as.Date(now_date)
	dat = finalScore %>% 
		  filter(credit_date==now_date)%>%	
		  filter(name %in% jjr_list)%>%	
			select(credit_date,id,name,business_type,
				base,behavior,service,contribute,
				gw,mm,zl,score) %>%
		mutate(`基础素质`=base,
				`行为规范`=behavior,
				`品质服务`=service,
				`参与贡献`=contribute,
				`官网指标`=gw,
				`买卖指标`=mm,
				`租赁指标`=zl,
				# `信用分`=score
				) %>%
		mutate(`业务类型`=if_else(business_type=="mm","买卖","租赁"))%>%
		select(credit_date:name,`基础素质`:`业务类型`)%>%
		gather(var, values, -c(credit_date,id,`业务类型`,name)) %>%
		group_by(var,name) %>%
		summarise(values=mean(values,na.rm=TRUE))
	rns = with(dat,reorder(name,-values,sum))	
	p1 = ggplot(data=dat,aes(x=rns,weight=values,fill=`var`)) + 
		geom_bar(col=I("white"))+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("各项指标分布: ",now_date)) +
		guides(fill = guide_legend(title.position = "top"))+
		theme(axis.text.x = element_text(angle=45))
	ggplotly(p1)
}	

