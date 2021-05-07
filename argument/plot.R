# 400接听率
wilson_score_call_answer <- Vectorize(function(succ,n) {
  z = qnorm(0.975)
  p = log((succ+2)) / log((n+2))
  # se = sqrt( p * (1-p) + z^2 / (4 * n^2)  )
  r = ( p  + z^2 / ( 2 * (n+2)) ) / (1 + z^2 / (n+2))
  return(r)
})



plot_400_simple <- function() {
  tmp_400 = expand_grid(S=0:10,N=0:10)
  tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
  tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
  tmp_400$transform = round(wilson_score_call_answer(tmp_400$S,tmp_400$N),2)
  ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
    geom_raster(aes(fill = simple), show.legend =FALSE) + 
    geom_label(aes(label=simple))+theme_bw() +
    theme(text= element_text(family="STXihei")) + 
    labs(x="呼叫量",y="接听量") +
    scale_fill_distiller(palette = "Spectral")

}

plot_400_transform <- function() {
  tmp_400 = expand_grid(S=0:10,N=0:10)
  tmp_400 = tmp_400[tmp_400$S <= tmp_400$N,]
  tmp_400$simple = round(tmp_400$S / tmp_400$N,2)
  tmp_400$transform = round(wilson_score_call_answer(tmp_400$S,tmp_400$N),2)
  ggplot(data=tmp_400, aes(x=as.factor(N), y=as.factor(S))) + 
    geom_raster(aes(fill = transform), show.legend =FALSE) + 
    geom_label(aes(label=transform))+theme_bw() +
    theme(text= element_text(family="STXihei")) + 
    labs(x="呼叫量",y="接听量") +
    scale_fill_distiller(palette = "Spectral")

}