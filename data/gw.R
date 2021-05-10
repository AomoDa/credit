
gw_score <- function(rk) {

}

gw_transform <- Vectorize(function(succ,n,alpha,beta) {
  z = qnorm(0.975)
  p = log((succ+alpha)) / log((n+beta))
  r = ( p  + z^2 / ( 2 * (n+beta)) ) / (1 + z^2 / (n+beta))
  return(r)
},vectorize.args=c("succ","n"))


#----------------------------------------
# 400接通率
#----------------------------------------


# 
credit_call_answer_transform <- function(dat,now_date,env){

}
# 简单计算
credit_call_answer_simple <- function(dat,now_date,env){
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