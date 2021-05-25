library(readxl)
library(dplyr)
library(writexl)
library(stringr)

# setwd("Documents/credit/")
# jjrDataPath = "data/积分系统测试所需员工数据5.1.xlsx"
jjrDataPath="data/data0521.xlsx"
ggDataPath="data/gg0521.xlsx"
#----------------------------------------------------------------
# 经纪人过程指标数据
#----------------------------------------------------------------
# loading data
shtFormat <- function(sht){
	s = unlist(strsplit(x = str_remove_all(str_remove_all(string = sht,pattern = " "),"月"),split = "[.]"))
	rt = paste0(s[1],"-",ifelse(nchar(s[2])==1,paste0("0",s[2]),s[2]),"-01")
	return(as.Date(rt))
}

# 读取单个SHEET
readSheet <- function(sht) {
	dat = read_xlsx(path = jjrDataPath,sheet = sht,na="无",col_types=c(rep("guess",8),rep("text",45)))
	names(dat) <- c(
					# 大区	区域	店组	岗位	姓名	职级	ERP编号	入职日期
					"daqu","qu","zu","job","name","level","id","entry_date",
					# 学历	是否党员	司龄（月）	执业认证
					"education","party","entry_month","qualification",
					# 每日一考	日常行为规范（迟到）	日常行为规范（旷工）、经验值单数	nps
					"exam",  "daily_cd","daily_kg", "jingyan", "nps_n","nps_s","nps",
					# 渗透量	400来电量	400接通量	400接通率	
					"app_download","call_400","answer_400","call_answer",
					# 爱聊客户量	爱聊1分钟回复量	爱聊1分钟回复率
					"cnt_al","cnt_al_1min","al_1min",
					# 爱聊线索量	三日复聊量	爱聊三日复聊率
					"dcnt_al","dcnt_al_3day","al_3day",
					# 爱聊线索量	爱聊客户转录入量	爱聊转录入率
					"dcnt_al_2","dcnt_al_luru","al_luru",
					# 爱聊线索量	线索转首看量	线索转首看率
					"dcnt_al_3","dcnt_al_daikan","al_daikan",
					# 小区楼盘纠错
					# "xiaoqu",
					# 买卖收速销房	买卖出速销房	转介客源（暂无数据）	买卖折扣达标量
					"mm_s_sx","mm_c_sx","mm_zjky","mm_zk",
					# 买卖新增客户	买卖总带看客户组数	 买卖房源新增	买卖认证委托量	买卖收钥匙量
					"mm_cxz","mm_cdk","mm_fxz","mm_wt","mm_ys",
					# 租赁房管收房	我收他出+他收我出,出着火房管
					"zl_sfg","zl_fhz_wstc","zl_fhz_tswc","zl_czh",
					# 租赁在管量	租赁委托续签量	租赁新增首看客户	租赁房源新增	租赁普租实勘
					"zl_zg","zl_wt","zl_cxz","zl_fxz","zl_pzsk"
					)	
	# 处理数据类型
	dat$entry_date <- as.Date(dat$entry_date)
	dat$id <- as.numeric(dat$id)
	dat$entry_month <- as.numeric(dat$entry_month)
	dat[,c(13:53)] <- as.data.frame(apply(X = dat[,c(13:53)],MARGIN = 2,FUN = as.numeric))
	dat$date <- shtFormat(sht)
	# 删除错误数据
	entry_date_num <- as.numeric(substr(dat$entry_date,1,4)) * 12 + as.numeric(substr(dat$entry_date,6,7)) 
	data_date_num <- as.numeric(substr(dat$date,1,4)) * 12 + as.numeric(substr(dat$date,6,7)) 
	dat = dat[data_date_num > entry_date_num,]
	return(as.data.frame(dat))
}

# 批量读取并合并数据
readData <- function() {
	shts <- excel_sheets(jjrDataPath)
	# shts <- shts[5:18]
	dat <- data.frame()
	for (sht in shts) {
		cat(sht,"\n")
		if(nrow(dat)==0){
			dat = readSheet(sht)
		}else{
			tmp = readSheet(sht)
			dat = rbind(dat,tmp)
		}
	}
	return(dat)
}
# miss
fill <- function(vec,default=0){
	a = vec
	a[is.na(a)] = 0
	return(a)
}

#读取数据
# kpi.dat = readData()
# kpi.dat[13:53] <- as.data.frame( apply(X = kpi.dat[13:53],MARGIN = 2,FUN = fill))
# kpi.dat$business = ifelse(str_count(kpi.dat$qu,pattern = "A"),"zl","mm")
# write.csv(x = kpi.dat,file = "kpi.csv",row.names = FALSE)

kpi.dat = read.csv("kpi.csv",stringsAsFactors = FALSE)
kpi.dat$entry_date = as.Date(kpi.dat$entry_date)
kpi.dat$date = as.Date(kpi.dat$date)

#----------------------------------------------------------------
# 经纪人基础信息数据
#----------------------------------------------------------------
# base.dat = read_xlsx(path = jjrDataPath,sheet = "基础信息",na="无")
# names(base.dat) <- c("daqu","qu","zu","id","name","entry_date","education","party","veteran","qualification")
# base.dat$id <- as.numeric(base.dat$id )
# base.dat$entry_date <- as.Date(base.dat$entry_date)

#----------------------------------------------------------------
# 经纪人宫格数据
#----------------------------------------------------------------

gg.dat = read_xlsx(path = ggDataPath,sheet = "Sheet1")
gg.dat$date = as.Date(gg.dat$date )
gg.dat$year =  as.numeric(str_sub(gg.dat$date,1,4))
gg.dat$mon =  as.numeric(str_sub(gg.dat$date,6,7))


# gg.dat = read_xlsx(path = jjrDataPath,sheet = "宫格、表扬、行政行罚",na="无")
# names(gg.dat) <- c("daqu","qu","zu","id","name","date","type","class")
# gg.dat$date <- as.Date(gg.dat$date)
# gg.dat$year =  as.numeric(str_sub(gg.dat$date,1,4))
# gg.dat$mon =  as.numeric(str_sub(gg.dat$date,6,7))

# # NPS
# nps.dat = read_xlsx(path = jjrDataPath,sheet = "NPS值平均值",na="无")
# nps.dat = as.data.frame(nps.dat)
# names(nps.dat) <- c("date","mm","zl")
# for (i in seq_len(nrow(nps.dat))) {
# 	nps.dat$date[i] =  as.character(shtFormat(nps.dat$date[i]))
# }
# nps.dat$date <- as.Date(nps.dat$date)













