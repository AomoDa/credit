library(dplyr)

# 分数匹配
credit_score <- function(id,sdat,default=0) {
	if(class(sdat)!="data.frame") return(default)
	tmp = left_join(x = data.frame(id=id),y = sdat,by="id")
	tmp$s[is.na(tmp$s)] <- default
	return(tmp$s)
}

# 打印日志
l2s <- function(rule) {
	ll = unlist(rule)
	s = "参数设置如下：\n"
	for(i in seq_len(length(ll))){
		s = paste0(s,names(ll)[i],"=",ll[i],"\n")
	}
	s = paste0(s,"\n==============================")
	return(s)
}
# 打印日志
credit_log <- function(msg,rule) {
	log_info(msg)
	log_info(l2s(rule))
}


#----------------------------------------------------------------
# 基础素质
#----------------------------------------------------------------
# 个人背景 - 教育背景分
credit_education <- function(vec,env) {
	rule=env$base$jjr_backgroup$education
	credit_log(msg="个人背景 - 教育背景分",rule)
	rlt =  rep(NA,length(vec))
	rlt[vec=="专科"] = rule$score$`大专统招`
	rlt[vec=="本科"] = rule$score$`本科及以上统招`
	rlt[vec=="高中"] = rule$score$`大专以下`
	rlt[vec=="中专"] = rule$score$`大专以下`
	return(rlt)
}

# 个人背景 - 特长
credit_hobby <- function(vec,env) {
	rule=env$base$jjr_backgroup$hobby
	return(0)
}

#  个人背景 - 政治背景
credit_political <- function(vec_party,vec_veteran,env) {
	rule = env$base$jjr_backgroup$political
	rlt_party =  rep(0,length(vec_party))
	rlt_veteran =  rep(0,length(vec_veteran))
	rlt_party[vec_party=="党员"] =  rule$score$party
	rlt_party[vec_veteran=="是"] =  rule$score$veteran
	rlt = rlt_party + rlt_veteran
	return(rlt)	
}

# 司龄
credit_entry <- function(vec,env,now_date=Sys.Date()){
	rule = env$base$enter
	vec_mon = as.numeric(str_sub(vec,1,4)) * 12 + as.numeric(str_sub(vec,6,7)) 
	now_mon = as.numeric(str_sub(now_date,1,4)) * 12 + as.numeric(str_sub(now_date,6,7)) 
	entry_mon = now_mon - vec_mon
	entry_mon[ entry_mon<0 ] <- 0 
	if(rule$score$method=="simple"){
		rlt = entry_simple(entry_mon, rule$score$simple)
	}else if(rule$score$method=="transform"){
		rlt = entry_transfrom(entry_mon, rule$score$transform)
		}
	return(rlt)	
}


# 执业认证
credit_qualification <- function(vec,env){
	rule = env$base$qualification 
	rlt =  rep(0,length(vec))
	rlt[vec=="杭州经纪人资格证"] = rule$score$`杭州经纪人证`
	rlt[vec=="全国协理证"] = rule$score$`全国协理证`
	rlt[vec=="全国经纪人资格证"] = rule$score$`全国经纪人资格证`
	return(rlt)	
}

# 每日一考
credit_exam <- function(dat,env,now_date) {
	rule = env$base$exam
	now_date <= as.Date(now_date)
	rule = env$base$exam
	dat %>% 
		filter(date == now_date) %>% 
		select(id,exam) %>% 
		mutate(s = simple_score_exam(exam,rule=rule))%>% 
		select(id,s) %>%
		as.data.frame()	
}

#----------------------------------------------------------------
# 行为规范
#----------------------------------------------------------------

# 日常行为规范 - 
credit_daily <- function(ggdat,env,now_date=Sys.Date(),last_dat=NULL) {
	rule = env$behavior$daily
	return(rule$total)
}


# 行政处罚
credit_punish <- function(ggdat,env,now_date=Sys.Date(),last_dat=NULL) {
	rule = env$behavior$punish
	# 回血判断
	rlt = data.frame(id=NA,s=NA)
	if(!is.null(last_dat)){		
		rlt =  last_dat[,c("id","punish")]
		names(rlt) = c("id","s")
		# 回血操作
		rlt$s[rlt$s < rule$total ] = rlt$s[rlt$s < rule$total] + rule$recovery
		rlt$s[rlt$s >= rule$total] = rule$total	
	}
	# 扣分
	rd = ggdat %>% 
		 	filter(class=="违规违纪") %>% 
		 	filter(year==as.numeric(str_sub(now_date,1,4))) %>% 
		 	filter(mon==as.numeric(str_sub(now_date,6,7))) %>% 
		 	mutate(s1 = if_else(type=="通报批评",rule$score$`通报批评`,0)) %>%
		 	mutate(s2 = if_else(type=="警告处分",rule$score$`警告处分`,0)) %>%
		 	mutate(s3 = if_else(type=="严重警告处分",rule$score$`严重警告处分`,0)) %>%
		 	mutate(s=s1+s2+s3) %>%
		 	group_by(id) %>%
		 	summarise(s=sum(s,na.rm=TRUE)) %>%
		 	union_all(rlt) %>%
		 	group_by(id) %>%
		 	summarise(s=sum(s,na.rm=TRUE))%>%
		 	na.omit()%>%
			as.data.frame()	
	return(rd)
}

#----------------------------------------------------------------
# 品质服务
#----------------------------------------------------------------

# 客户投诉
credit_complaint <- function(ggdat,env,now_date=Sys.Date(),last_dat=NULL){
	rule=env$service$complaint
	# 回血判断
	rlt = data.frame(id=NA,s=NA)
	if(!is.null(last_dat)){
		rlt =  last_dat[,c("id","complaint")]
		names(rlt) = c("id","s")
		# 回血操作
		rlt$s[rlt$s < rule$total ] = rlt$s[rlt$s < rule$total] + rule$recovery
		rlt$s[rlt$s >= rule$total] = rule$total	
	}
	# 扣分
	rd = ggdat %>% 
		 	filter(class=="有责投诉定级") %>% 
		 	filter(year==as.numeric(str_sub(now_date,1,4))) %>% 
		 	filter(mon==as.numeric(str_sub(now_date,6,7))) %>% 
		 	mutate(s1 = if_else(type=="一级",rule$score$`一级`,0)) %>%
		 	mutate(s2 = if_else(type=="二级",rule$score$`二级`,0)) %>%
		 	mutate(s3 = if_else(type=="三级",rule$score$`三级`,0)) %>%
		 	mutate(s4 = if_else(type=="四级",rule$score$`四级`,0)) %>%
		 	mutate(s5 = if_else(type=="五级",rule$score$`五级`,0)) %>%		 	
		 	mutate(s=s1+s2+s3+s4+s5) %>%
		 	group_by(id) %>%
		 	summarise(s=sum(s,na.rm=TRUE)) %>%
		 	union_all(rlt) %>%
		 	group_by(id) %>%
		 	summarise(s=sum(s,na.rm=TRUE))%>%
		 	na.omit()%>%
			as.data.frame()	
	# 汇总结果
	return(rd)
}

# 客户评价 
credit_praise <- function(ggdat,env,now_date=Sys.Date()) {
	rule=env$service$praise
	# 提取计算周期 
	now_date  = as.Date(now_date)
	last_date = now_date -  rule$cal$attenuation * 30
	ggdat %>% 
		filter(class=="客户表扬") %>%
		filter(date <= now_date) %>%
		filter(date >= last_date) %>%
		mutate(s1 = if_else(type=="通报表扬+奖励", rule$score$`通报表扬+奖励`,0) ) %>%
		mutate(s2 = if_else(type=="通报表扬", rule$score$`通报表扬`,0) ) %>%
		mutate(s=s1+s2)%>%
		group_by(id) %>%
		summarise(s=sum(s,na.rm=TRUE)) %>%
		as.data.frame()	
}


# NPS
credit_nps <- function(dat,nps.dat,env,now_date){
	rule=env$service$nps
	nps.tb = nps.dat[nps.dat$date == now_date,]
	if(nrow(nps.tb)==0) {
		rlt = 0 
	}else{
		rlt = dat %>% 
			filter(date == now_date) %>%
			mutate(s=simple_score_nps(nps,business,nps.tb,rule)) %>% 
			select(id,s) %>%
			as.data.frame()		
	}
	return(rlt)
}

# 经验值
credit_jingyan <- function(dat,env,now_date){
	rule=env$service$jingyan
	dat %>%
		filter(date <= now_date) %>%
		mutate(s1 = (mm_1+mm_2)* rule$score$`买卖成交单数(二手+一手)`) %>%
		mutate(s2 = (zl_cj)* rule$score$`租赁成交单`)%>%
		mutate(s3 = (zl_1)* rule$score$`租赁一手成交单`)%>%
		mutate(s = s1+s2+s3)%>%
		group_by(id) %>%
		summarise(s=sum(s,na.rm=TRUE)) %>%
		mutate(s = if_else(s >= rule$total, rule$total,s )) %>%
		select(id,s) %>%
		as.data.frame()
}

#----------------------------------------------------------------
# 参与贡献
#----------------------------------------------------------------

# 楼盘信息维护
credit_xiaoqu <- function(dat,env,now_date=Sys.Date()){
	rule=env$contribute$xiaoqu
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		group_by(id,date) %>%
		summarise(xiaoqu = rule$score * sum(xiaoqu,na.rm=TRUE),.groups = 'drop') %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(xiaoqu*coef)) %>%
		mutate(s = if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()
}


# 师徒带训
credit_teacher <- function(dat,env,now_date=Sys.Date()){
	rule=env$contribute$teacher
	return(0)
}

# 推荐入职
credit_recommend <- function(dat,env,now_date=Sys.Date()) {
	rule=env$contribute$recommend
	return(0)
}

# 社会活动参与
credit_social <- function(dat,env,now_date=Sys.Date()) {
	rule=env$contribute$social
	return(0)
}
# 其他事项
credit_other <- function(dat,env,now_date=Sys.Date()) {
	rule=env$contribute$other
	return(0)
}


#----------------------------------------------------------------
# 业务能力 - 官网指标
#----------------------------------------------------------------

# 渗透 - 
credit_app_download <- function(dat,now_date,env) {
	rule=env$business$gw$app_download
	credit_log(msg="官网指标- 渗透",rule)
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,app_download) %>%
		mutate(s_mon = sf(app_download))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()			
}



# 400接通率 -  公式转换

credit_call_answer <- function(dat,now_date,env){
	rule = env$business$gw$call_answer
	credit_log(msg="官网指标- 400接听率",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 转换率方法
	f = get_gw_lv_func(rule)
	# 得分方法
	sf = get_gw_score_func(rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%	
		select(date,id,call_400,answer_400) %>%
		mutate(lv=f(succ=answer_400,n=call_400))	%>%
		group_by(date) %>%
		mutate(rk = percent_rank(-lv))  %>%
		ungroup() %>%
		mutate(s_mon = sf(rk)) %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()			
}


# 爱聊一分钟响应率  - 公式转换

credit_al_1min <- function(dat,now_date,env){
	rule = env$business$gw$al_1min
	credit_log(msg="官网指标- 爱聊一分钟响应率",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 转换率方法
	f = get_gw_lv_func(rule)
	# 得分方法
	sf = get_gw_score_func(rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%	
		select(date,id,cnt_al,cnt_al_1min) %>%
		mutate(lv=f(succ=cnt_al_1min,n=cnt_al))	%>%
		group_by(date) %>%
		mutate(rk = percent_rank(-lv))  %>%
		ungroup() %>%
		mutate(s_mon = sf(rk)) %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()		

}



# 爱聊三日率 - 公式转换

credit_al_3day <- function(dat,now_date,env){
	rule = env$business$gw$al_3day
	credit_log(msg="官网指标- 爱聊三日复聊率",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 转换率方法
	f = get_gw_lv_func(rule)
	# 得分方法
	sf = get_gw_score_func(rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%	
		select(date,id,dcnt_al,dcnt_al_3day) %>%
		mutate(lv=f(succ=dcnt_al_3day,n=dcnt_al))	%>%
		group_by(date) %>%
		mutate(rk = percent_rank(-lv))  %>%
		ungroup() %>%
		mutate(s_mon = sf(rk)) %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()		

}


# 爱聊录入率 - 公式转换

credit_al_luru<- function(dat,now_date,env){
	rule = env$business$gw$al_luru
	credit_log(msg="官网指标- 爱聊录入率",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 转换率方法
	f = get_gw_lv_func(rule)
	# 得分方法
	sf = get_gw_score_func(rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%	
		select(date,id,dcnt_al_2,dcnt_al_luru) %>%
		mutate(lv=f(succ=dcnt_al_luru,n=dcnt_al_2))	%>%
		group_by(date) %>%
		mutate(rk = percent_rank(-lv))  %>%
		ungroup() %>%
		mutate(s_mon = sf(rk)) %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()		

}



# 爱聊转带看 - 公式转换
credit_al_daikan <- function(dat,now_date,env){
	rule = env$business$gw$al_daikan
	credit_log(msg="官网指标- 爱聊转带看率",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 转换率方法
	f = get_gw_lv_func(rule)
	# 得分方法
	sf = get_gw_score_func(rule)
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%	
		select(date,id,dcnt_al_3,dcnt_al_daikan) %>%
		mutate(lv=f(succ=dcnt_al_daikan,n=dcnt_al_3))	%>%
		group_by(date) %>%
		mutate(rk = percent_rank(-lv))  %>%
		ungroup() %>%
		mutate(s_mon = sf(rk)) %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()		
}


#----------------------------------------------------------------
# 业务能力 - 买卖
#----------------------------------------------------------------

# 成交单数
credit_mm_deal <- function(dat,now_date,env){
	rule=env$business$mm$deal
	credit_log(msg="买卖指标- 成交单数",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		group_by(id,date) %>%
		summarise(mm_2 = rule$score$mm_2 * sum(mm_2,na.rm=TRUE),
				  mm_1 = rule$score$mm_1 * sum(mm_1,na.rm=TRUE),
				  .groups = 'drop') %>%
		mutate(mm = mm_1 + mm_2) %>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(mm*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 速销 - 增长公式
credit_mm_sx <- function(dat,now_date,env){
	rule=env$business$mm$mm_sx
	credit_log(msg="买卖指标- 速销",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)	
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_sx) %>%
		mutate(s_mon = sf(mm_sx))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()		
}

# 买卖折扣 - 增长公式
credit_mm_zk <- function(dat,now_date,env){
	rule=env$business$mm$mm_zk
	credit_log(msg="买卖指标- 折扣",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_zk) %>%
		mutate(s_mon = sf(mm_zk))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	

}

# 首看客户 - 增长公式
credit_mm_csk <- function(dat,now_date,env) {
	rule=env$business$mm$mm_csk
	credit_log(msg="买卖指标- 首看客户",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_csk) %>%
		mutate(s_mon = sf(mm_csk))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 总带看客户组数 - 增长公式
credit_mm_cdk <- function(dat,now_date,env) {
	rule=env$business$mm$mm_cdk
	credit_log(msg="买卖指标- 总带看客户组数",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_cdk) %>%
		mutate(s_mon = sf(mm_cdk))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 房源新增- 增长公式
credit_mm_fxz <- function(dat,now_date,env) {
	rule=env$business$mm$mm_fxz
	credit_log(msg="买卖指标- 房源新增",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_fxz) %>%
		mutate(s_mon = sf(mm_fxz))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 认证委托- 增长公式
credit_mm_wt <- function(dat,now_date,env) {
	rule=env$business$mm$mm_wt
	credit_log(msg="买卖指标- 认证委托",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_wt) %>%
		mutate(s_mon = sf(mm_wt))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 收钥匙 - 增长公式
credit_mm_ys <- function(dat,now_date,env) {
	rule=env$business$mm$mm_ys
	credit_log(msg="买卖指标- 收钥匙",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,mm_ys) %>%
		mutate(s_mon = sf(mm_ys))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

#----------------------------------------------------------------
# 业务能力 - 租赁
#----------------------------------------------------------------

# 成交单数
credit_zl_deal <- function(dat,now_date,env){
	rule=env$business$zl$deal
	credit_log(msg="租赁指标- 成交单数",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		group_by(id,date) %>%
		summarise(zl_cj = rule$score$zl_0 * sum(zl_cj,na.rm=TRUE),
				  zl_1 = rule$score$zl_1 * sum(zl_1,na.rm=TRUE),
				  .groups = 'drop') %>%
		mutate(zl = zl_cj + zl_1) %>%
		inner_join(coef_dat,by="date")  %>%
		ungroup()%>%
		group_by(id) %>%
		summarise(s=sum(zl*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		select(id,s) %>%
		as.data.frame()	
}


# 房管收房 - 增长公式
credit_zl_sfg <- function(dat,now_date,env) {
	rule=env$business$zl$zl_sfg
	credit_log(msg="租赁指标- 房管收房",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,zl_sfg) %>%
		mutate(s_mon = sf(zl_sfg))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 房管出房- 增长公式
credit_zl_cfg <- function(dat,now_date,env) {
	rule=env$business$zl$zl_cfg
	credit_log(msg="租赁指标- 房管出房",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,zl_cfg) %>%
		mutate(s_mon = sf(zl_cfg))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 在管量  - 增长公式
credit_zl_zg <- function(dat,now_date,env ) {
	rule=env$business$zl$zl_zg
	credit_log(msg="租赁指标- 在管量",rule)	
	# 得分方法
	sf = get_grow_score_func(rule)	
	# 计算	
	dat %>% 
		filter(date == now_date) %>%
		select(id,zl_zg)%>%	
		group_by(id) %>%	
		summarise(zl_zg=sum(zl_zg,na.rm=TRUE)) %>%
		mutate(s=sf(zl_zg)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		select(id,s) %>%
		as.data.frame()	
}

# 委托续签量  - 增长公式
credit_zl_wt <- function(dat,now_date,env ) {
	rule=env$business$zl$zl_wt
	credit_log(msg="租赁指标- 委托续签量",rule)	
	# 得分方法
	sf = get_grow_score_func(rule)	
	# 计算
	dat %>% 
		filter(date == now_date) %>%
		select(id,zl_wt)%>%		
		group_by(id) %>%	
		summarise(zl_wt=sum(zl_wt,na.rm=TRUE)) %>%
		mutate(s=sf(zl_wt)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		select(id,s) %>%
		as.data.frame()	
}


# 新增客户 - 增长公式
credit_zl_cxz <- function(dat,now_date,env) {
	rule=env$business$zl$zl_cxz
	credit_log(msg="租赁指标- 新增客户",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,zl_cxz) %>%
		mutate(s_mon = sf(zl_cxz))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 房源新增- 增长公式
credit_zl_fxz <- function(dat,now_date,env) {
	rule=env$business$zl$zl_fxz
	credit_log(msg="租赁指标- 房源新增",rule)	
	# 提取计算周期
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,zl_fxz) %>%
		mutate(s_mon = sf(zl_fxz))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 普租实勘- 增长公式
credit_zl_pzsk <- function(dat,now_date,env) {
	rule=env$business$zl$zl_pzsk
	credit_log(msg="租赁指标- 普租实勘",rule)	
	# 提取计算周期 & 系数 
	coef_dat=getCoefData(dat = dat,now_date = now_date,rule = rule)
	# 得分方法
	sf = get_grow_score_func(rule)
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= min(coef_dat$date)) %>%
		select(date,id,zl_pzsk) %>%
		mutate(s_mon = sf(zl_pzsk))%>%
		inner_join(coef_dat,by="date")  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

