# 计算一个月
run_mon <- function(kpi.dat,gg.dat,base.dat,nps.dat,now_date,env,last_dat=NULL) {
	now_date = as.Date(now_date)
	rlt = data.frame(credit_date = now_date,id=base.dat$id)
	#----------------------------------------------------------------
	# 基础素质
	#----------------------------------------------------------------
	# 个人背景 - 教育背景
	rlt$education = credit_education(vec = base.dat$education,env = env)
	# 个人背景 - 特长
	rlt$hobby = credit_hobby(vec=NA,env=env)
	# 个人背景 - 政治背景
	rlt$political = credit_political(vec_party = base.dat$party,
		vec_veteran = base.dat$veteran,env = env)
	# 司龄
	rlt$entry = credit_entry(vec = base.dat$entry_date,env = env,now_date=now_date)
	#执业认证
	rlt$qualification = credit_qualification(vec = base.dat$qualification,env = env)
	# 每日一考
	rlt$exam = credit_score(id=rlt$id,
		sdat=credit_exam(dat=kpi.dat,now_date=now_date,env=env))
	cat("基础素质 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$base = rlt$education + rlt$hobby  + rlt$political + 
			   rlt$entry + rlt$qualification + rlt$exam
	#----------------------------------------------------------------
	# 行为规范
	#----------------------------------------------------------------
	# 日常行为规范
	rlt$daily = credit_score(id=rlt$id,
		sdat=credit_daily(gg.dat,env=env,now_date=now_date,last_dat=last_dat),
		default=env$behavior$daily$total)
	# 行政处罚
	rlt$punish = credit_score(id=rlt$id,
		sdat=credit_punish(gg.dat,env=env,now_date=now_date,last_dat=last_dat),
		default=env$behavior$punish$total)	
	cat("行为规范 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$behavior = rlt$daily + rlt$punish
	#----------------------------------------------------------------
	# 品质服务
	#----------------------------------------------------------------	
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
	cat("品质服务 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$service = rlt$complaint + rlt$praise + rlt$nps + rlt$jingyan 
	#----------------------------------------------------------------
	# 参与贡献
	#----------------------------------------------------------------	
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
	cat("参与贡献 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$contribute = rlt$xiaoqu + rlt$teacher + rlt$recommend + rlt$social + rlt$other
	#----------------------------------------------------------------
	# 业务能力 - 官网指标
	#----------------------------------------------------------------	
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
	cat("官网指标 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$gw = rlt$app + rlt$call_answer + rlt$al_1min + rlt$al_3day +
		     rlt$al_luru + rlt$al_daikan
	#----------------------------------------------------------------
	# 业务能力 - 买卖
	#----------------------------------------------------------------	
	rlt$mm_deal = credit_score(id=rlt$id,
		sdat=credit_mm_deal(kpi.dat,env=env,now_date=now_date))
	rlt$mm_sx = credit_score(id=rlt$id,
		sdat=credit_mm_sx(kpi.dat,env=env,now_date=now_date))
	rlt$mm_zk = credit_score(id=rlt$id,
		sdat=credit_mm_zk(kpi.dat,env=env,now_date=now_date))
	rlt$mm_csk = credit_score(id=rlt$id,
		sdat=credit_mm_csk(kpi.dat,env=env,now_date=now_date))
	rlt$mm_cdk = credit_score(id=rlt$id,
		sdat=credit_mm_cdk(kpi.dat,env=env,now_date=now_date))
	rlt$mm_fxz = credit_score(id=rlt$id,
		sdat=credit_mm_fxz(kpi.dat,env=env,now_date=now_date))
	rlt$mm_wt = credit_score(id=rlt$id,
		sdat=credit_mm_wt(kpi.dat,env=env,now_date=now_date))
	rlt$mm_ys = credit_score(id=rlt$id,
		sdat=credit_mm_ys(kpi.dat,env=env,now_date=now_date))
	cat("买卖指标 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$mm = rlt$mm_deal + rlt$mm_sx + rlt$mm_zk + rlt$mm_csk +
             rlt$mm_csk  + rlt$mm_cdk +rlt$mm_fxz + rlt$mm_wt + rlt$mm_ys
	#----------------------------------------------------------------
	# 业务能力 - 租赁
	#----------------------------------------------------------------	
	rlt$zl_deal = credit_score(id=rlt$id,
		sdat=credit_zl_deal(kpi.dat,env=env,now_date=now_date))
	rlt$zl_sfg = credit_score(id=rlt$id,
		sdat=credit_zl_sfg(kpi.dat,env=env,now_date=now_date))
	rlt$zl_cfg = credit_score(id=rlt$id,
		sdat=credit_zl_cfg(kpi.dat,env=env,now_date=now_date))
	rlt$zl_zg = credit_score(id=rlt$id,
		sdat=credit_zl_zg(kpi.dat,env=env,now_date=now_date))	
	rlt$zl_wt = credit_score(id=rlt$id,
		sdat=credit_zl_wt(kpi.dat,env=env,now_date=now_date))
	rlt$zl_cxz = credit_score(id=rlt$id,
		sdat=credit_zl_cxz(kpi.dat,env=env,now_date=now_date))	
	rlt$zl_fxz = credit_score(id=rlt$id,
		sdat=credit_zl_fxz(kpi.dat,env=env,now_date=now_date))
	rlt$zl_pzsk = credit_score(id=rlt$id,
		sdat=credit_zl_pzsk(kpi.dat,env=env,now_date=now_date))
	cat("租赁指标 ---> Succ : ",as.character(now_date),"\n")
	# 汇总
	rlt$zl = rlt$zl_deal +  rlt$zl_sfg + rlt$zl_cfg + rlt$zl_zg+
	         rlt$zl_wt +  rlt$zl_cxz + rlt$zl_fxz + rlt$zl_pzsk

	return(rlt)
}

# 计算历史全部
run <- function(kpi.dat,gg.dat,base.dat,nps.dat,env) {
	order_date = sort(unique(kpi.dat$date))
	for (i in seq_len(length(order_date))) {
		if(i==1){
			rlt = run_mon(kpi.dat,gg.dat,base.dat,nps.dat,
				now_date=order_date[i],env=env)
		}else{
			tmp = run_mon(kpi.dat,gg.dat,base.dat,nps.dat,
				now_date=order_date[i],env=env,
				last_dat=rlt[rlt$credit_date==order_date[i-1],])
			rlt = rbind(rlt,tmp)			
		}
		cat("Succ : ",as.character(order_date[i]),"\n")
	}
	# 关联经纪人身份
	rlt = left_join(x = rlt,y = base.dat[,1:6],by="id")
	rlt$business_type = ifelse(str_count(rlt$qu,pattern = "A"),"zl","mm")
	rlt = rlt[rlt$entry_date <= rlt$credit_date,]
	# 处理错误数据
	rlt$zl[rlt$business_type=="mm"] = 0
	rlt$mm[rlt$business_type=="zl"] = 0     
	rlt$score = rlt$base + rlt$behavior + rlt$service + 
	               rlt$contribute + rlt$gw + rlt$mm + rlt$zl	
	return(rlt)
}

# 计算
#finalScore = run(kpi.dat,gg.dat,base.dat,nps.dat,env)
# write.csv(x = finalScore,file = "finalScore.csv",row.names =FALSE)
finalScore = read.csv(file = "finalScore.csv",header = TRUE,stringsAsFactors = FALSE)
finalScore$credit_date <- as.Date(finalScore$credit_date)

















