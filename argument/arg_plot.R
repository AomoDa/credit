# 400接听率
plot_zh_gw_simple <- function(alpha,beta,s=10,n=10){
  s = as.numeric(s)
  n = as.numeric(n)
  alpha = as.numeric(alpha)
  beta = as.numeric(beta)  
  tmp_400 = expand_grid(S=0:s,N=0:n)
  tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
  tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
  tmp_400$transform = round(gw_transform(tmp_400$S,tmp_400$N,alpha,beta),2)
  ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
    geom_raster(aes(fill = simple), show.legend =FALSE) + 
    geom_label(aes(label=simple))+theme_bw() +
    theme(text= element_text(family="STXihei")) + 
    labs(x="呼叫量",y="接听量") +
    scale_fill_distiller(palette = "Spectral")    

}


plot_zh_gw_transform <- function(alpha,beta,s=10,n=10){
  s = as.numeric(s)
  n = as.numeric(n)
  alpha = as.numeric(alpha)
  beta = as.numeric(beta)  
  tmp_400 = expand_grid(S=0:s,N=0:n)
  tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
  tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
  tmp_400$transform = round(gw_transform(tmp_400$S,tmp_400$N,alpha,beta),2)
  ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
    geom_raster(aes(fill = transform), show.legend =FALSE) + 
    geom_label(aes(label=transform))+theme_bw() +
    theme(text= element_text(family="STXihei")) + 
    labs(x="呼叫量",y="接听量") +
    scale_fill_distiller(palette = "Spectral")
}

tb_zh_gw_in <- function(alpha,beta,s=10,n=10){
  s = as.numeric(s)
  n = as.numeric(n)
  alpha = as.numeric(alpha)
  beta = as.numeric(beta)  
  tmp_400 = expand_grid(S=0:s,N=0:n)
  tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
  tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
  tmp_400$transform = round(gw_transform(tmp_400$S,tmp_400$N,alpha,beta),2)
  names(tmp_400) = c("S","N","简单计算","转化计算")
  return(as.data.frame(tmp_400))
}



# wilson_score_call_answer <- function(succ,n) gw_transform(succ,n,alpha=2,beta=3)
# plot_400_simple <- function() {
#   tmp_400 = expand_grid(S=0:10,N=0:10)
#   tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
#   tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
#   tmp_400$transform = round(wilson_score_call_answer(tmp_400$S,tmp_400$N),2)
#   ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
#     geom_raster(aes(fill = simple), show.legend =FALSE) + 
#     geom_label(aes(label=simple))+theme_bw() +
#     theme(text= element_text(family="STXihei")) + 
#     labs(x="呼叫量",y="接听量") +
#     scale_fill_distiller(palette = "Spectral")
# }

# plot_400_transform <- function() {
#   tmp_400 = expand_grid(S=0:10,N=0:10)
#   tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
#   tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
#   tmp_400$transform = round(wilson_score_call_answer(tmp_400$S,tmp_400$N),2)
#   ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
#     geom_raster(aes(fill = transform), show.legend =FALSE) + 
#     geom_label(aes(label=transform))+theme_bw() +
#     theme(text= element_text(family="STXihei")) + 
#     labs(x="呼叫量",y="接听量") +
#     scale_fill_distiller(palette = "Spectral")

# }


#爱聊录入率

# wilson_score_al_luru <- Vectorize(function(succ,n) {
#   z = qnorm(0.975)
#   p = (succ+1) / (n+10)
#   # se = sqrt( p * (1-p) + z^2 / (4 * n^2)  )
#   r = ( p  + z^2 / ( 2 * (n+10)) ) / (1 + z^2 / (n+10))
#   return(r)
# })


# plot_al_luru_simple <- function() {
#   tmp_400 = expand_grid(S=0:10,N=0:10)
#   tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
#   tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
#   tmp_400$transform = round(wilson_score_al_luru(tmp_400$S,tmp_400$N),2)
#   ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
#     geom_raster(aes(fill = simple), show.legend =FALSE) + 
#     geom_label(aes(label=simple))+theme_bw() +
#     theme(text= element_text(family="STXihei")) + 
#     labs(x="呼叫量",y="接听量") +
#     scale_fill_distiller(palette = "Spectral")

# }

# plot_al_luru_transform <- function() {
#   tmp_400 = expand_grid(S=0:10,N=0:30)
#   tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
#   tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
#   tmp_400$transform = round(wilson_score_al_luru(tmp_400$S,tmp_400$N),2)
#   ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
#     geom_raster(aes(fill = transform), show.legend =FALSE) + 
#     geom_label(aes(label=transform))+theme_bw() +
#     theme(text= element_text(family="STXihei")) + 
#     labs(x="呼叫量",y="接听量") +
#     scale_fill_distiller(palette = "Spectral")

# }


#-----------------------------------
#激励曲线
#-----------------------------------

plot_jili_sim_example <- function() {
  alpha = 3
  beta = 10 
  x = seq(from=0, to=30, by=0.1)
  y = growS(x,alpha = alpha,beta = beta,c_alpha=0.6,c_beta=0.9)
  sm = smooth.spline(x, y, spar=0.4)
  s_alpha = sm$x[which(sm$y>=0.6)[1]]
  s_beta = sm$x[which(sm$y>=0.9)[1]]
  s_c_alpha = sm$y[sm$x==s_alpha]
  s_c_beta = sm$y[sm$x==s_beta]
  plot(sm,type="l",xlab="",ylab="",axes = FALSE,ylim=c(0,1),xlim=c(0,30),family="STXihei")
  axis(1, c(0,s_alpha,s_beta),c(0,expression(alpha),expression(beta)))
  axis(2, c(0,s_c_alpha,s_c_beta,1),c(0,expression('S'[alpha]) ,expression('S'[beta]),1),las=1)
  graphics::box()
  segments(x0 = s_alpha,y0 = -1,x1 = s_alpha,y1 = s_c_alpha,col="red",lty=4)
  segments(x0 = -1,y0 = s_c_alpha,x1 = s_alpha,y1 = s_c_alpha,col="red",lty=4)
  segments(x0 = s_beta,y0 = -1,x1 = s_beta,y1 = s_c_beta,col="blue",lty=4)
  segments(x0 = -1,y0 = s_c_beta,x1 = s_beta,y1 = s_c_beta,col="blue",lty=4)
  abline(h=0,lty=2,col="orange")
  abline(h=1,lty=2,col="orange")
  text(x = alpha/2,y = 0.5,labels = "A阶段",family="STXihei")
  text(x = alpha+(beta-alpha)/2,y = 0.5,labels = "B阶段",family="STXihei")
  text(x = 20,y = 0.5,labels = "C阶段",family="STXihei")
}



plot_jili_sim_test <- function(alpha,beta,c_alpha,c_beta,gamma=1){
  alpha = as.numeric(alpha)
  beta = as.numeric(beta)
  c_alpha = as.numeric(c_alpha)
  c_beta = as.numeric(c_beta)
  gamma = as.numeric(gamma)
  x=seq(from=0,to=beta*2,by=0.1)
  y=growS(x,alpha,beta,c_alpha,c_beta,gamma)
  plot(x,y,type="l",xlab="",ylab="",ylim=c(0,1))
  abline(h=0,lty=2,col="orange")
  abline(h=1,lty=2,col="orange")
  segments(x0 = alpha,y0 = -1,x1 = alpha,y1 = c_alpha,col="red",lty=4)
  segments(x0 = -1,y0 = c_alpha,x1 = alpha,y1 = c_alpha,col="red",lty=4)
  segments(x0 = beta,y0 = -1,x1 = beta,y1 = c_beta,col="blue",lty=4)
  segments(x0 = -1,y0 = c_beta,x1 = beta,y1 = c_beta,col="blue",lty=4) 
}


plot_jili_sim_test_trend <- function(alpha,beta,c_alpha,c_beta,gamma=1){
  alpha = as.numeric(alpha)
  beta = as.numeric(beta)
  c_alpha = as.numeric(c_alpha)
  c_beta = as.numeric(c_beta)
  gamma = as.numeric(gamma)
  x=seq(from=1,to=beta*2,by=0.5)
  y0 = growS(x,alpha,beta,c_alpha,c_beta,gamma)
  y1 = growS(x-1,alpha,beta,c_alpha,c_beta)
  y = y0 - y1
  plot(x,y,type="l",xlab="",ylab="")
  ta =  y[x==alpha]
  tb =  y[x==beta]
  segments(x0 = alpha,y0 = -1,x1 = alpha,y1 = ta,col="red",lty=4)
  segments(x0 = -1,y0 = ta,x1 = alpha,y1 = ta,col="red",lty=4)
  segments(x0 = beta,y0 = -1,x1 = beta,y1 = tb,col="blue",lty=4)
  segments(x0 = -1,y0 = tb,x1 = beta,y1 = tb,col="blue",lty=4)   
}


tb_jili_in <- function(alpha,beta,c_alpha,c_beta,gamma=1){
    alpha = as.numeric(alpha)
    beta = as.numeric(beta)
    c_alpha = as.numeric(c_alpha)
    c_beta = as.numeric(c_beta)
    gamma = as.numeric(gamma)
    x=seq(from=1,to=beta*2,by=1)
    y=growS(x,alpha,beta,c_alpha,c_beta,gamma)
    rlt=data.frame(
      "变量" = x,
      "得分比"=round(y,4), 
      "环比"=round(c(NA,diff(y)),4)
      )
    return(rlt)
}



#-----------------------------------
# 司龄
#-----------------------------------

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




