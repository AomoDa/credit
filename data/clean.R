# daqu	大区
# qu	区域
# zu	店组
# id	ERP编号
# name	姓名
# entry_date	入职日期
# exam	每日一考
# nps	NPS值
# jingyan	经验值
# app_download	渗透量（当时无数据）
# call_answer	400接通率
# al_1min	爱聊1分钟回复率
# al_3day	爱聊三日复聊率 （当时无数据）
# al_luru	爱聊转录入率
# al_daikan	爱聊转带看率
# xiaoqu	楼盘纠错 无数据统计
# mm_2	买卖二手合同
# mm_1	买卖一合同
# mm_sx	买卖收速销量
# mm_zk	买卖折扣达标的量（核算单数）
# mm_csk	买卖首看客户量
# mm_cdk	买卖总带看客户组数
# mm_fxz	买卖房源新增
# mm_wt	买卖认证委托量
# mm_ys	买卖收钥匙量
# zl_cj	租赁成交单数
# zl_1	租赁一手成交单数
# zl_sfg	租赁房管收房
# zl_cfg	租赁房管出房
# zl_zg	租赁在管量
# zl_wt	租赁委托续签量
# zl_cxz	租赁新增客户
# zl_fxz	租赁房源新增
# zl_pzsk	租赁普租实勘

library(readxl)
library(dplyr)
library(writexl)
library(stringr)

# setwd("Documents/credit/")
# jjrDataPath = "data/积分系统测试所需员工数据5.1.xlsx"
jjrDataPath="data/CreditData.xlsx"

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
	dat = read_xlsx(path = jjrDataPath,sheet = sht,na="无",col_types=c(rep("guess",6),rep("text",39)))
	names(dat) <- c("daqu","qu","zu","id","name","entry_date",
					"exam","nps","jingyan","daily","app_download",
					"call_400","answer_400","call_answer",
					"cnt_al","cnt_al_1min","al_1min",
					"dcnt_al","dcnt_al_3day","al_3day",
					"dcnt_al_2","dcnt_al_luru","al_luru",
					"dcnt_al_3","dcnt_al_daikan","al_daikan",
					"xiaoqu",
					"mm_2","mm_1","mm_sx","mm_zk","mm_csk","mm_cdk","mm_fxz","mm_wt","mm_ys",
					"zl_cj","zl_1","zl_sfg","zl_cfg","zl_zg","zl_wt","zl_cxz","zl_fxz","zl_pzsk"
					)	
	# 处理数据类型
	dat$entry_date <- as.Date(dat$entry_date)
	dat[,c(7:45)] <- as.data.frame(apply(X = dat[,c(7:45)],MARGIN = 2,FUN = as.numeric))
	dat$date <- shtFormat(sht)
	return(as.data.frame(dat))
}

# 批量读取并合并数据
readData <- function() {
	shts <- excel_sheets(jjrDataPath)
	shts <- shts[5:18]
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

# 读取数据
# kpi.dat = readData()
# kpi.dat[,7:45] <- as.data.frame( apply(X = kpi.dat[,7:45],MARGIN = 2,FUN = fill))
# kpi.dat$business = ifelse(str_count(kpi.dat$qu,pattern = "A"),"zl","mm")
# write.csv(x = kpi.dat,file = "kpi.csv",row.names = FALSE)

kpi.dat = read.csv("kpi.csv",stringsAsFactors = FALSE)
kpi.dat$entry_date = as.Date(kpi.dat$entry_date)
kpi.dat$date = as.Date(kpi.dat$date)

#----------------------------------------------------------------
# 经纪人基础信息数据
#----------------------------------------------------------------

base.dat = read_xlsx(path = jjrDataPath,sheet = "基础信息",na="无")
names(base.dat) <- c("daqu","qu","zu","id","name","entry_date","education","party","veteran","qualification")
base.dat$id <- as.numeric(base.dat$id )
base.dat$entry_date <- as.Date(base.dat$entry_date)

#----------------------------------------------------------------
# 经纪人宫格数据
#----------------------------------------------------------------

gg.dat = read_xlsx(path = jjrDataPath,sheet = "宫格、表扬、行政行罚",na="无")
names(gg.dat) <- c("daqu","qu","zu","id","name","date","type","class")
gg.dat$date <- as.Date(gg.dat$date)
gg.dat$year =  as.numeric(str_sub(gg.dat$date,1,4))
gg.dat$mon =  as.numeric(str_sub(gg.dat$date,6,7))

# NPS
nps.dat = read_xlsx(path = jjrDataPath,sheet = "NPS值平均值",na="无")
nps.dat = as.data.frame(nps.dat)
names(nps.dat) <- c("date","mm","zl")
for (i in seq_len(nrow(nps.dat))) {
	nps.dat$date[i] =  as.character(shtFormat(nps.dat$date[i]))
}
nps.dat$date <- as.Date(nps.dat$date)













