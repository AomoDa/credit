library(logger)
# 计算一个月
run_mon <- function(kpi.dat,gg.dat,now_date,env,last_dat=NULL) {
	now_date = as.Date(now_date)
	rlt = kpi.dat[kpi.dat$date==now_date,c(1:8)]
	rlt$credit_date = now_date
	log_info("开始处理程序：","数据日期 = ",as.character(now_date))
	#----------------------------------------------------------------
	# 基础素质
	#----------------------------------------------------------------
	log_info("Start - 基础素质")
	# 个人背景 - 教育背景
	rlt$education = credit_education(vec = kpi.dat$education[kpi.dat$date==now_date],env = env)
	# # 个人背景 - 特长
	# rlt$hobby = credit_hobby(vec=NA,env=env)
	# 个人背景 - 政治背景
	rlt$political = credit_political(vec_party = kpi.dat$party[kpi.dat$date==now_date],env = env)
	# 司龄
	rlt$entry = credit_entry(vec = kpi.dat$entry_date[kpi.dat$date==now_date],env = env,now_date=now_date)
	#执业认证
	rlt$qualification = credit_qualification(vec = kpi.dat$qualification[kpi.dat$date==now_date],env = env)

	# 每日一考
	rlt$exam = credit_score(id=rlt$id,
		sdat=credit_exam(dat=kpi.dat,now_date=now_date,env=env))
	# 汇总
	rlt$base = rlt$education  + rlt$political + rlt$entry + rlt$qualification + rlt$exam
	log_info("End - 基础素质")		   
	#----------------------------------------------------------------
	# 行为规范
	#----------------------------------------------------------------
	log_info("Start - 行为规范")	
	# 日常行为规范
	rlt$daily = credit_score(id=rlt$id,
		sdat=credit_daily(kpi.dat,env=env,now_date=now_date,last_dat=last_dat),
		default=env$behavior$daily$total)
	# 行政处罚
	rlt$punish = credit_score(id=rlt$id,
		sdat=credit_punish(gg.dat,env=env,now_date=now_date,last_dat=last_dat),
		default=env$behavior$punish$total)	
	# 汇总
	rlt$behavior = rlt$daily + rlt$punish
	log_info("End - 行为规范")	
	#----------------------------------------------------------------
	# 品质服务
	#----------------------------------------------------------------	
	log_info("Start - 品质服务")		
	# 客户投诉
	rlt$complaint = credit_score(id=rlt$id,
		sdat=credit_complaint(gg.dat,env=env,now_date=now_date,last_dat=last_dat),
		default=env$service$complaint$total)		
	# 客户评价 - 客户表扬
	rlt$praise = credit_score(id=rlt$id,
		sdat=credit_praise(gg.dat,env=env,now_date=now_date))		

	# NPS
	rlt$nps = credit_score(id=rlt$id,
		sdat=credit_nps(dat=kpi.dat,nps.dat=nps.dat,env=env,now_date=now_date))	
	# 经验值
	rlt$jingyan = credit_score(id=rlt$id,
		sdat=credit_jingyan(dat=kpi.dat,env=env,now_date=now_date))	
	# 汇总
	rlt$service = rlt$complaint + rlt$praise + rlt$nps + rlt$jingyan 
	log_info("End - 品质服务")	
	#----------------------------------------------------------------
	# 参与贡献
	#----------------------------------------------------------------
	log_info("Start - 参与贡献")	
	# 楼盘信息维护
	rlt$xiaoqu = credit_score(id=rlt$id,
		sdat=credit_xiaoqu(dat=kpi.dat,env=env,now_date=now_date))
	# 师徒带训
	rlt$teacher = credit_teacher(dat=kpi.dat,env=env,now_date=now_date) 
	# 推荐入职
	rlt$recommend = credit_recommend(dat=kpi.dat,env=env,now_date=now_date)
	# 社会活动参与
	rlt$social = credit_social (dat=kpi.dat,env=env,now_date=now_date)
	# 其他事项
	rlt$other = credit_other(dat=kpi.dat,env=env,now_date=now_date)
	# 汇总
	rlt$contribute = rlt$xiaoqu + rlt$teacher + rlt$recommend + rlt$social + rlt$other
	log_info("End - 参与贡献")	
	#----------------------------------------------------------------
	# 业务能力 - 官网指标
	#----------------------------------------------------------------	
	log_info("Start - 官网指标")	
	rlt$app = credit_score(id=rlt$id,
		sdat=credit_app_download(kpi.dat,env=env,now_date=now_date))
	rlt$call_answer = credit_score(id=rlt$id,
		sdat=credit_call_answer(kpi.dat,env=env,now_date=now_date))
	rlt$al_1min = credit_score(id=rlt$id,
		sdat=credit_al_1min(kpi.dat,env=env,now_date=now_date))
	rlt$al_3day = credit_score(id=rlt$id,
		sdat=credit_al_3day(kpi.dat,env=env,now_date=now_date))
	rlt$al_luru = credit_score(id=rlt$id,
		sdat=credit_al_luru(kpi.dat,env=env,now_date=now_date))
	rlt$al_daikan = credit_score(id=rlt$id,
		sdat=credit_al_daikan(kpi.dat,env=env,now_date=now_date))
	# 汇总
	rlt$gw = rlt$app + rlt$call_answer + rlt$al_1min + rlt$al_3day +
		     rlt$al_luru + rlt$al_daikan
	log_info("End - 官网指标")			     
	#----------------------------------------------------------------
	# 业务能力 - 买卖
	#----------------------------------------------------------------	
	log_info("Start - 买卖指标")	
	# rlt$mm_deal = credit_score(id=rlt$id,
	# 	sdat=credit_mm_deal(kpi.dat,env=env,now_date=now_date))
	# 速销
	rlt$mm_s_sx = credit_score(id=rlt$id,
		sdat=credit_mm_s_sx(kpi.dat,env=env,now_date=now_date))
	rlt$mm_c_sx = credit_score(id=rlt$id,
		sdat=credit_mm_c_sx(kpi.dat,env=env,now_date=now_date))
	# 转介客源 
	rlt$mm_zjky = credit_score(id=rlt$id,
		sdat=credit_mm_zjky(kpi.dat,env=env,now_date=now_date))
	# 折扣
	rlt$mm_zk = credit_score(id=rlt$id,
		sdat=credit_mm_zk(kpi.dat,env=env,now_date=now_date))
	# 带看
	rlt$mm_cxz = credit_score(id=rlt$id,
		sdat=credit_mm_cxz(kpi.dat,env=env,now_date=now_date))
	rlt$mm_cdk = credit_score(id=rlt$id,
		sdat=credit_mm_cdk(kpi.dat,env=env,now_date=now_date))
	# 房源、委托、钥匙
	rlt$mm_fxz = credit_score(id=rlt$id,
		sdat=credit_mm_fxz(kpi.dat,env=env,now_date=now_date))
	rlt$mm_wt = credit_score(id=rlt$id,
		sdat=credit_mm_wt(kpi.dat,env=env,now_date=now_date))
	rlt$mm_ys = credit_score(id=rlt$id,
		sdat=credit_mm_ys(kpi.dat,env=env,now_date=now_date))
	# 汇总
	rlt$mm = rlt$mm_s_sx + rlt$mm_c_sx + rlt$mm_zk + rlt$mm_cxz +
             rlt$mm_cdk +rlt$mm_fxz + rlt$mm_wt + rlt$mm_ys
	log_info("End - 买卖指标")	             
	#----------------------------------------------------------------
	# 业务能力 - 租赁
	#----------------------------------------------------------------	
	log_info("Start - 租赁指标")	
	# 收房管
	rlt$zl_sfg = credit_score(id=rlt$id,
		sdat=credit_zl_sfg(kpi.dat,env=env,now_date=now_date))
	# # 合作出房
	rlt$zl_fhz = credit_score(id=rlt$id,
		sdat=credit_zl_fhz(kpi.dat,env=env,now_date=now_date))
	# 出着火房管
	rlt$zl_czh = credit_score(id=rlt$id,
		sdat=credit_zl_czh(kpi.dat,env=env,now_date=now_date))
	# 在管量 、委托
	rlt$zl_zg = credit_score(id=rlt$id,
		sdat=credit_zl_zg(kpi.dat,env=env,now_date=now_date))	
	rlt$zl_wt = credit_score(id=rlt$id,
		sdat=credit_zl_wt(kpi.dat,env=env,now_date=now_date))
	# 首看新增客户 、房源新增、普租实勘
	rlt$zl_cxz = credit_score(id=rlt$id,
		sdat=credit_zl_cxz(kpi.dat,env=env,now_date=now_date))	
	rlt$zl_fxz = credit_score(id=rlt$id,
		sdat=credit_zl_fxz(kpi.dat,env=env,now_date=now_date))
	rlt$zl_pzsk = credit_score(id=rlt$id,
		sdat=credit_zl_pzsk(kpi.dat,env=env,now_date=now_date))
	log_info("End - 租赁指标")	     
	# 汇总
	rlt$zl = rlt$zl_sfg + rlt$zl_fhz + rlt$zl_czh + rlt$zl_zg + 
	         rlt$zl_wt +  rlt$zl_cxz + rlt$zl_fxz + rlt$zl_pzsk

	return(rlt)
}


# 计算历史全部
run <- function(kpi.dat,gg.dat,env) {
	order_date = sort(unique(kpi.dat$date))
	for (i in seq_len(length(order_date))) {
		if(i==1){
			rlt = run_mon(kpi.dat,gg.dat,
				now_date=order_date[i],env=env)
		}else{
			tmp = run_mon(kpi.dat,gg.dat,
				now_date=order_date[i],env=env,
				last_dat=rlt[rlt$credit_date==order_date[i-1],])
			rlt = rbind(rlt,tmp)			
		}
	}
	# 关联经纪人身份
	rlt$business_type = ifelse(str_count(rlt$qu,pattern = "A"),"zl","mm")
	# 处理错误数据
	rlt$zl[rlt$business_type=="mm"] = 0
	rlt$mm[rlt$business_type=="zl"] = 0     
	rlt$score = rlt$base + rlt$behavior + rlt$service + 
				rlt$contribute + rlt$gw + rlt$mm + rlt$zl	
	rlt$entry_date_num = as.numeric(substr(rlt$entry_date,1,4)) * 12 + as.numeric(substr(rlt$entry_date,6,7)) 
	rlt$data_date_num = as.numeric(substr(rlt$credit_date,1,4)) * 12 + as.numeric(substr(rlt$credit_date,6,7)) 
	rlt$entry_month = rlt$data_date_num - rlt$entry_date_num
	return(rlt)
}

# 计算
# finalScore = run(kpi.dat,gg.dat,env)
# write.csv(x = finalScore,file = "finalScore.csv",row.names =FALSE)
finalScore = read.csv(file = "finalScore.csv",header = TRUE,stringsAsFactors = FALSE)
finalScore$credit_date <- as.Date(finalScore$credit_date)

















