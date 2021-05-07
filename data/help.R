library(dplyr)


credit_score <- function(id,sdat,default=0) {
	if(class(sdat)!="data.frame") return(default)
	tmp = left_join(x = data.frame(id=id),y = sdat,by="id")
	tmp$s[is.na(tmp$s)] <- default
	return(tmp$s)
}

#----------------------------------------------------------------
# 基础素质
#----------------------------------------------------------------
# 个人背景 - 教育背景分
credit_education <- function(vec,env) {
	rule=env$base$jjr_backgroup$education
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
		print("1")
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
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(xiaoqu = rule$score * sum(xiaoqu,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
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

# 渗透量
credit_app_download <- function(dat,now_date,env) {
	rule=env$business$gw$app_download
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(app = rule$score$per * sum(app_download,na.rm=TRUE)) %>%
		mutate(app = if_else(app>=rule$score$limits,rule$score$limits,app)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(app*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()

}	

# 400接通率
credit_call_answer <- function(dat,now_date,env){
	rule = env$business$gw$call_answer
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		mutate(s_mon = simple_score_call_answer(call_answer,rule=rule)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 爱聊1分钟回复

credit_al_1min <- function(dat,now_date,env){
	rule = env$business$gw$al_1min
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		mutate(s_mon = simple_score_al_1min(al_1min,rule=rule)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 爱聊三日复聊率
credit_al_3day <- function(dat,now_date,env){
	rule = env$business$gw$al_3day
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		mutate(s_mon = simple_score_al_3day(al_3day,rule=rule)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 爱聊转录入率

credit_al_luru <- function(dat,now_date,env){
	rule = env$business$gw$al_luru
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		mutate(s_mon = simple_score_al_luru(al_luru,rule=rule)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(s_mon*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 爱聊转带看率

credit_al_daikan <- function(dat,now_date,env){
	rule = env$business$gw$al_daikan
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	# 计算
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		mutate(s_mon = simple_score_al_daikan(al_daikan,rule=rule)) %>%
		inner_join(coef_dat)  %>%
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
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_2 = rule$score$mm_2 * sum(mm_2,na.rm=TRUE),
				  mm_1 = rule$score$mm_1 * sum(mm_1,na.rm=TRUE)) %>%
		mutate(mm = mm_1 + mm_2) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 速销
credit_mm_sx <- function(dat,now_date,env){
	rule=env$business$mm$mm_sx
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_sx = rule$score * sum(mm_sx,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_sx*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()		
}

# 买卖折扣达标的量（核算单数）
credit_mm_zk <- function(dat,now_date,env){
	rule=env$business$mm$mm_zk
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_zk = rule$score * sum(mm_zk,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_zk*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	

}

# 首看客户
credit_mm_csk <- function(dat,now_date,env) {
	rule=env$business$mm$mm_csk
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_csk = rule$score$score * sum(mm_csk,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_csk*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 总带看客户组数
credit_mm_cdk <- function(dat,now_date,env) {
	rule=env$business$mm$mm_cdk
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_cdk = rule$score$score * sum(mm_cdk,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_cdk*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 房源新增

credit_mm_fxz <- function(dat,now_date,env) {
	rule=env$business$mm$mm_fxz
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_fxz = rule$score * sum(mm_fxz,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_fxz*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 认证委托

credit_mm_wt <- function(dat,now_date,env) {
	rule=env$business$mm$mm_wt
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_wt = rule$score * sum(mm_wt,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_wt*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 收钥匙
credit_mm_ys <- function(dat,now_date,env) {
	rule=env$business$mm$mm_ys
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(mm_ys = rule$score * sum(mm_ys,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(mm_ys*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

#----------------------------------------------------------------
# 业务能力 - 租赁
#----------------------------------------------------------------

# 成交单数
credit_zl_deal <- function(dat,now_date,env){
	rule=env$business$zl$deal
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(zl_cj = rule$score$zl_0 * sum(zl_cj,na.rm=TRUE),
				  zl_1 = rule$score$zl_1 * sum(zl_1,na.rm=TRUE)) %>%
		mutate(zl = zl_cj + zl_1) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(zl*coef)) %>%
		ungroup()%>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 房管收房
credit_zl_sfg <- function(dat,now_date,env) {
	rule=env$business$zl$zl_sfg
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(zl_sfg = rule$score * sum(zl_sfg,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(zl_sfg*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 房管出房
credit_zl_cfg <- function(dat,now_date,env) {
	rule=env$business$zl$zl_cfg
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(zl_cfg = rule$score * sum(zl_cfg,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(zl_cfg*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}


# 在管量

credit_zl_zg <- function(dat,now_date,env ) {
	rule=env$business$zl$zl_zg
	dat %>% 
		filter(date == now_date) %>%	
		group_by(id) %>%	
		summarise(zl_zg=sum(zl_zg,na.rm=TRUE)) %>%
		mutate(s=zl_zg * rule$score) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		select(id,s) %>%
		as.data.frame()	
}

# 委托续签量
credit_zl_wt <- function(dat,now_date,env ) {
	rule=env$business$zl$zl_wt
	dat %>% 
		filter(date == now_date) %>%	
		group_by(id) %>%	
		summarise(zl_wt=sum(zl_wt,na.rm=TRUE)) %>%
		mutate(s=zl_wt * rule$score) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		select(id,s) %>%
		as.data.frame()	
}


# 新增客户
credit_zl_cxz <- function(dat,now_date,env) {
	rule=env$business$zl$zl_cxz
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(zl_cxz = rule$score * sum(zl_cxz,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(zl_cxz*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 房源新增
credit_zl_fxz <- function(dat,now_date,env) {
	rule=env$business$zl$zl_fxz
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(zl_fxz = rule$score * sum(zl_fxz,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(zl_fxz*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

# 普租实勘
credit_zl_pzsk <- function(dat,now_date,env) {
	rule=env$business$zl$zl_pzsk
	# 提取计算周期
	now_date <- as.Date(now_date)
	order_date = sort(unique(dat$date))
	ind_right = which(order_date==now_date)
	ind_left = ind_right-rule$cal$attenuation+1
	ind_left = ifelse(ind_left>=1,ind_left,1)
	cdate = order_date[ind_left:ind_right]
	cn = length(cdate)
	# 系数
	coef_dat = data.frame(date=rev(cdate),coef= rule$cal$coef[1:cn] ) 
	dat %>% 
		filter(date <= now_date) %>%
		filter(date >= order_date[ind_left]) %>%
		group_by(id,date) %>%
		summarise(zl_pzsk = rule$score * sum(zl_pzsk,na.rm=TRUE)) %>%
		inner_join(coef_dat)  %>%
		group_by(id) %>%
		summarise(s=sum(zl_pzsk*coef)) %>%
		mutate(s=if_else(s>=rule$total,rule$total,s)) %>%
		as.data.frame()	
}

