library(plotly)
library(ggplot2)
library(dplyr)


#-------------------------------------------
# 司龄
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
		return(rule$score$`e4`)
	}else if(x >= 95){
		return( rule$score$`e1`)
	}else if (x >= 90) {
		return(rule$score$`e2`)
	}else if (x>=80){
		return(rule$score$`e3`)
	}else {
		return(rule$score$`e4`)
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
		x1 = rule$para$a * x 
		x2 = ifelse(is.numeric(rule$score),rule$score,rule$score$limit)
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
	aa = ifelse(is.numeric(rule$score),rule$score,rule$score$limit)
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
	return(p * aa)
},,vectorize.args="rk")

# 官网计算方法
get_gw_score_func <- function(rule) {
	function(rk) gw_score(rk,rule=rule)
}



# 折扣率计算分数
mm_zk_score <- Vectorize(function(rk,rule) {
	aa = ifelse(is.numeric(rule$score),rule$score,rule$score$limit)
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
		p = 0.5
	}else if (rk <= 0.4){
		p = 0.4
	}else if (rk <= 0.5){
		p = 0.2
	}else if (rk <= 0.6){
		p = 0.1
	}else if (rk <= 0.8){
		p = 0
	}else{
		p = 0
	}
	return(p * aa)
},,vectorize.args="rk")

# 官网计算方法
get_zk_score_func <- function(rule) {
	function(rk) mm_zk_score(rk,rule=rule)
}





