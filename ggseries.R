
# Paquetes
require(tseries)
require(ggplot2)
require(cowplot)

# Fu

# Plot ACP / ACF with IC
# How to compute IC for ACF and PACF :
# https://stats.stackexchange.com/questions/211628/how-is-the-confidence-interval-calculated-for-the-acf-function
ic_alpha= function(alpha, acf_res){
  return(qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used))
}

ggplot_acf_pacf= function(res_, lag, label, alpha= 0.05){
  df_= with(res_, data.frame(lag, acf))
  
  # IC alpha
  lim1= ic_alpha(alpha, res_)
  lim0= -lim1
  
  
  ggplot(data = df_, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y= label) +
    geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
    geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue') + 
    theme_bw()
}

graf_ts = function(ts,
                   salida=1,
                   tiempo=NULL,
                   namex=NULL,
                   namey=NULL,
                   lags=NULL,
                   colors="#000000",
                   org=1){
  
  if(is.null(tiempo))tiempo = 1:length(ts)
  if(is.null(namex))namex = "tiempo"
  if(is.null(namey))namey = "valores de la serie"
  if(is.null(lags))lags = length(ts)
  
  g1 = ggplot(mapping = aes(x=tiempo,
                            y=ts)) +
    geom_line(linetype = 1,
              lwd = 1,
              color = "#00AFBB") +
    xlab(namex)+
    ylab(namey)+
    scale_y_continuous(limits = c(min(ts),max(ts)))+
    scale_x_continuous(limits = c(min(tiempo),max(tiempo)))+
    theme_bw()+
    labs(title = "")
  
  g2 = ggplot_acf_pacf(res_ = acf(ts, lag = lags,plot = F),
                       lags,
                       label = "ACF")
  
  g3 = ggplot_acf_pacf(res_ = pacf(ts , lag = lags, plot = F),
                       lags,
                       label = "PACF")
  if(org==1){plot = plot_grid(g1,g2,g3,ncol=3,nrow = 1)}
  
  if(org==2){plot = plot_grid(g1,g2,g3,ncol=1,nrow = 3)}
  
  if(salida==1){return(plot)}
  if(salida==2){return(g1)}
  if(salida==3){return(g2)}
  if(salida==4){return(g3)}

}

