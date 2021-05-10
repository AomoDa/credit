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

entry_para_plot_1 <- function(a,t) {
	d = data.frame(x=1:120,"线性增加"=NA,"平滑曲线"=NA)
	d[,2] = entry_simple(1:120,a=a)
	d[,3] = entry_transfrom(1:120,t=t)
	ggplot(data=d,aes(x=x)) + 
		geom_line(aes(y=`线性增加`,col="线性增加"),lty=2,lwd=2) + 
		geom_line(aes(y=`平滑曲线`,col="平滑曲线"),lty=1,lwd=1) +
		theme_bw()+theme(text = element_text(family = "STXihei"))+
		labs(x="司龄(月)",y="信用分",title="司龄信用积分 By 计算方法") 

}

entry_para_plot_2 <- function(a,t) {
	d = data.frame(x=1:120,"线性增加"=NA,"平滑曲线"=NA)
	d[,2] = entry_simple(1:120,a=a)
	d[,2] = c(NA,diff(d[,2]))
	d[,3] = entry_transfrom(1:120,t=t)
	d[,3] = c(NA,diff(d[,3]))
	ggplot(data=d[-1,],aes(x=x)) + 
		geom_line(aes(y=`线性增加`,col="线性增加"),lty=2,lwd=2) + 
		geom_line(aes(y=`平滑曲线`,col="平滑曲线"),lty=1,lwd=1) +
		theme_bw()+theme(text = element_text(family = "STXihei"))+
		labs(x="司龄(月)",y="新增信用分",title="新增司龄信用积分 By 计算方法") 

}




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
		return(rule$score$`样本量少于5个`)
	}else if (x == 1){
		return(rule$score$`100%`)
	}else if (x >= nps){
		return(rule$score$`平均值上`)
	}else if (x >=0){
		return(rule$score$`正分值且在平均分下`)
	} else if (x <0){
		return(rule$score$`负分值`)
	} 
	return(rule$score$`样本量少于5个`)	

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
# 激励曲线
#-------------------------------------------
growS <- Vectorize(function(x,alpha,beta,c_alpha=0.9,c_beta=0.98,intercept=0){
	a1 = (c_alpha ) / (exp(alpha) - exp(0))
	b1 = c_alpha - a1 * exp( alpha)
	a2 = ( c_beta - c_alpha ) / log( beta / alpha )
	b2 = c_beta - a2 * log( beta )
	a3 = (1 - c_beta) * 2 
	b3 = 1 - a3 
	if (x <= intercept){
		return(0)
	} else if(x <= alpha){
		rlt = a1 * exp(x) + b1 
	}else if ( x <= beta){
		rlt = a2 * log(x) + b2
	}else{
		rlt = a3 / (1 +exp((beta - x)/ (alpha +beta))) + b3
	}
},vectorize.args="x")



