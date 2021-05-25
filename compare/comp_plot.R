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


# 区域分析

plot_score_daqu_mm <- function(now_date){
	tmp= finalScore %>% 
			filter(credit_date == now_date) %>% 
			filter(business_type=="mm") %>% 
			select(daqu,score)
	rdaqu = with(tmp,reorder(daqu,-score,median))
	p1 = ggplot(data=tmp,aes(x=rdaqu,y=score,fill=daqu)) + geom_boxplot()+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("信用分大区分布: ",now_date))+
		theme(axis.text.x = element_text(angle=45))
	ggplotly(p1)
}

plot_score_daqu_zl <- function(now_date){
	tmp= finalScore %>% 
			filter(credit_date == now_date) %>% 
			filter(business_type=="zl") %>% 
			select(daqu,score)
	rdaqu = with(tmp,reorder(daqu,-score,median))
	p1 = ggplot(data=tmp,aes(x=rdaqu,y=score,fill=daqu)) + geom_boxplot()+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("信用分大区分布: ",now_date))+
		theme(axis.text.x = element_text(angle=45))
	ggplotly(p1)
}

# 职阶
plot_score_level_mm <- function(now_date){
	tmp= finalScore %>% 
			filter(credit_date == now_date) %>% 
			filter(business_type=="mm") %>% 
			select(level,score)
	rlevel = with(tmp,reorder(level,score,median))
	p1 = ggplot(data=tmp,aes(x=rlevel,y=score,fill=level)) + geom_boxplot()+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("信用分职级分布: ",now_date))+
		theme(axis.text.x = element_text(angle=45))
	ggplotly(p1)
}

plot_score_level_zl <- function(now_date){
	tmp= finalScore %>% 
			filter(credit_date == now_date) %>% 
			filter(business_type=="zl") %>% 
			select(level,score)
	rlevel = with(tmp,reorder(level,score,median))
	p1 = ggplot(data=tmp,aes(x=rlevel,y=score,fill=level)) + geom_boxplot()+
		theme_bw() +  theme(text= element_text(family="STXihei")) +
		labs(x="",y="分",title=paste0("信用分职级分布: ",now_date))+
		theme(axis.text.x = element_text(angle=45))
	ggplotly(p1)
}


# 新人成长趋势分布

plot_score_new_user_box <- function(bty="mm") {
	a=finalScore %>% 
			filter(business_type==bty) %>% 
			filter(credit_date >= as.Date("2020-07-01")) %>% 
			# filter(credit_date != as.Date("2021-02-01")) %>% 			
			select(entry_month,score) %>% 
			filter(entry_month<=24)  %>% 
			ggplot(aes(x=as.factor(entry_month),y=score,fill=entry_month)) + 
			geom_boxplot(notchwidth=0)	+
			theme_bw() +  theme(text= element_text(family="STXihei")) +
			labs(x="",y="分",title=paste0("信用分成长趋势"))+
			 scale_fill_distiller(palette = "Spectral")
	ggplotly(a)		 

}




plot_score_new_user_line <- function(bty="mm") {
	s = finalScore$score[finalScore$business_type==bty & finalScore$credit_date==as.Date("2021-03-01")]
	sm = median(s)
	sm_l = quantile(s,0.25)
	sm_u = quantile(s,0.75)
	a=finalScore %>% 
			filter(business_type==bty) %>% 
			filter(credit_date >= as.Date("2020-07-01")) %>% 	
			# filter(credit_date != as.Date("2021-02-01")) %>% 						
			select(entry_month,score) %>% 
			filter(entry_month<=24)  %>% 
			group_by(entry_month) %>%
			summarise(score=median(score,na.rm=TRUE)) %>%
			ggplot(aes(x=entry_month,y=score)) + 
			geom_line()	+
			geom_hline(yintercept=sm,col=I("red"),lty=2) + 
			geom_hline(yintercept=sm_l,col=I("blue"),lty=2) + 
			geom_hline(yintercept=sm_u,col=I("blue"),lty=2) + 
			geom_point(pch=5)	+
			theme_bw() +  theme(text= element_text(family="STXihei")) +
			labs(x="",y="分",title=paste0("信用分成长趋势"))+
			 scale_fill_distiller(palette = "Spectral")
	ggplotly(a)		 

}










