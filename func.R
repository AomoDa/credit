library(plotly)
library(ggplot2)
library(dplyr)


#-------------------------------------------
# 个人背景
#-------------------------------------------

entry_transfrom <- function(x,t) {
	t = as.numeric(t)
	rt = 2/(1+exp(-4*x/t)) -1
	rt = ifelse(rt>0,rt,0)
	return(rt * env$base$enter$total)
	}

entry_simple <- Vectorize(FUN=function(x,a) {
	a=as.numeric(a)
	rt = x * a 
	rt = ifelse(rt>=env$base$enter$total,env$base$enter$total,rt)
	rt = ifelse(rt>0,rt,0)
	return(rt)
	},vectorize.args="x")


#-------------------------------------------
# 每日一考
#-------------------------------------------

simple_score_exam<- Vectorize(function(x,rule = env$base$exam){
	if(is.na(x)){
		return(rule$score$`80分以下（不含`)
	}else if(x >= 96){
		return( rule$score$`96~100分`)
	}else if (x >= 91) {
		return(rule$score$`91~95分`)
	}else if (x>=80){
		return(rule$score$`80-90分`)
	}else {
		return(rule$score$`80分以下（不含`)
	}
},vectorize.args ="x")


#-------------------------------------------
# NPS
#-------------------------------------------

simple_score_nps <- Vectorize(function(x,business,nps.tb,rule){
	nps = ifelse(business=="mm",nps.tb$mm,nps.tb$zl)
	if(is.na(x)){
		return(rule$score$`other`)
	}else if (x >= 1){
		return(rule$score$`100%`)
	}else if (x >= nps){
		return(rule$score$`平均值上`)
	}else if (x > 0){
		return(rule$score$`正分值且在平均分下`)
	} else if (x < 0){
		return(rule$score$`负分值`)
	} 
	return(rule$score$`other`)	

},vectorize.args =c("x","business"))


#-------------------------------------------
# 400接听率
#-------------------------------------------
simple_score_call_answer <- Vectorize(function(x,rule = env$business$gw$call_answer){
	if(is.na(x)){
		return(0)
	}else if(x > 0.9){
		return( rule$score$`91-100%`)
	}else if (x >0.8) {
		return(rule$score$`81-90%`)
	}else if (x>0.7){
		return(rule$score$`71-80%`)
	}else if (x>0.6){
		return(rule$score$`61-70%`)
	}else {
		return(rule$score$`60下(含)`)
	}
},vectorize.args ="x")


#-------------------------------------------
# 爱聊1min响应率
#-------------------------------------------

simple_score_al_1min <- Vectorize(function(x,rule = env$business$gw$al_1min){
	if(is.na(x)){
		return(0)
	}else if(x > 0.9){
		return( rule$score$`91-100%`)
	}else if (x >0.8) {
		return(rule$score$`81-90%`)
	}else if (x>0.7){
		return(rule$score$`71-80%`)
	}else if (x>0.6){
		return(rule$score$`61-70%`)
	}else {
		return(rule$score$`60下(含)`)
	}
},vectorize.args ="x")




#-------------------------------------------
# 爱聊3日复聊率
#-------------------------------------------

simple_score_al_3day <- Vectorize(function(x,rule = env$business$gw$al_3day){
	if(is.na(x)){
		return(0)
	}else if(x > 0.5){
		return( rule$score$`51%以上`)
	}else if (x >0.4) {
		return(rule$score$`41-50%`)
	}else if (x>0.3){
		return(rule$score$`31-40%`)
	}else if (x>0.1){
		return(rule$score$`11-30%`)
	}else if (x>0){
		return(rule$score$`10%下(含)`)		
	}else {
		return(rule$score$`0`)
	}
},vectorize.args ="x")


#-------------------------------------------
# 爱聊录入率
#-------------------------------------------

simple_score_al_luru <- Vectorize(function(x,rule = env$business$gw$al_luru){
	if(is.na(x)){
		return(0)
	}else if(x > 0.3){
		return( rule$score$`31%以上`)
	}else if (x >0.2) {
		return(rule$score$`21-30%`)
	}else if (x>0.1){
		return(rule$score$`11-20%`)
	}else if (x>0){
		return(rule$score$`10%下(含)`)		
	}else {
		return(rule$score$`0`)
	}
},vectorize.args ="x")

#-------------------------------------------
# 爱聊转带看
#-------------------------------------------



simple_score_al_daikan <- Vectorize(function(x,rule = env$business$gw$al_daikan){
	if(is.na(x)){
		return(0)
	}else if(x > 0.1){
		return( rule$score$`10%以上`)
	}else if (x >0.07) {
		return(rule$score$`7-10%`)
	}else if (x>0.05){
		return(rule$score$`5-7%`)
	}else if (x>0){
		return(rule$score$`4%下(含)`)		
	}else {
		return(rule$score$`0`)
	}
},vectorize.args ="x")



#-------------------------------------------
# 增长曲线
#-------------------------------------------

growS <- Vectorize(function(x,alpha,beta,c_alpha=0.9,c_beta=0.9,gamma=1){
	# a1 = (c_alpha ) / (exp(alpha/gamma) - exp(0))
	# b1 = c_alpha - a1 * exp( alpha/gamma)
	a1 = c_alpha / (alpha/gamma)^2
	
	a2 = ( c_beta - c_alpha ) / log( beta / alpha )
	b2 = c_beta - a2 * log( beta/gamma )

	a3 = (1 - c_beta) * 2 
	b3 = 1 - a3 
	if (x <= 0){
		return(0)
	} else if(x <= alpha){
		rlt = a1 * (x/gamma)^2
	}else if ( x <= beta){
		rlt = a2 * log(x/gamma) + b2
	}else{
		rlt = a3 / (1 + exp((beta - x/gamma)/ (alpha +beta))) + b3
	}
},vectorize.args="x")


# 增长计算得分方法
get_grow_score_func <- function(rule){
	aa = ifelse( is.numeric(rule$score),rule$score,rule$score$limit)
	function(x) aa * growS(x,alpha=rule$para$alpha,
						beta=rule$para$beta,
						c_alpha=rule$para$y_alpha,
						c_beta=rule$para$y_beta)
}


# 线性模型
get_grow_score_linear_func <- function(rule) {
	Vectorize(function(x) {
		x1 = rule$score$score * x 
		x2 = rule$score$limit
		return(min(x1,x2))
	})
}


#-------------------------------------------
# 衰减系数
#-------------------------------------------

getCoefData <- function(dat,now_date,rule){
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
	return(coef_dat)
}



#----------------------------------------
# 官网指标计算
#----------------------------------------

# 转换秩
gw_transform <- Vectorize(function(succ,n,alpha,beta) {
	alpha = as.numeric(alpha)
	beta = as.numeric(beta)
  z = qnorm(0.975)
  p = log((succ+alpha)) / log((n+beta))
  r = ( p  + z^2 / ( 2 * (n+beta)) ) / (1 + z^2 / (n+beta))
  return(r)
},vectorize.args=c("succ","n"))

# 官网计算方法
get_gw_lv_func <- function(rule){
	function(succ,n) gw_transform(succ,n,alpha=rule$para$alpha,beta=rule$para$beta)
}

# 官网得分计算
gw_score <- Vectorize(function(rk,rule) {
	# 排名0-1%（含）计 1
	# 排名1%-5%（含）计0.9
	# 排名5%-10%（含）计0.8
	# 排名10%-20%（含）计0.7
	# 排名20%-30%（含）计0.6
	# 排名30%-40%（含）计0.5
	# 排名40%-50%（含）计0.4
	# 排名50-60%（含）计0.2
	# 排名60%-80% （含）计0.1
	# 排名前80%-100%（含）计0分
	if(rk <= 0.01){
		p =1 
	}else if (rk <=0.05){
		p = 0.9
	}else if (rk <= 0.1){
		p = 0.8
	}else if (rk <= 0.2){
		p = 0.7
	}else if (rk <= 0.3){
		p = 0.6
	}else if (rk <= 0.4){
		p = 0.5
	}else if (rk <= 0.5){
		p = 0.4
	}else if (rk <= 0.6){
		p = 0.2
	}else if (rk <= 0.8){
		p = 0.1
	}else{
		p = 0
	}
	return(p * rule$score$limit)
},,vectorize.args="rk")

# 官网计算方法
get_gw_score_func <- function(rule) {
	function(rk) gw_score(rk,rule=rule)
}







