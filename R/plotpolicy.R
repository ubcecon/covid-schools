pfigure <- function(df, pol, stlab=1, outcome="dlogdc", L=14) {
  df$p <- panellag(df[,pol],df$fips, df$date, L)
  df$p <- ceiling(df$p)
  df$y <- df[,outcome]
  #df$week <- week(df$date)
  m <- lm(y ~ as.factor(date) + as.factor(date):p, data=df)
  adf <- rbind(data.frame(date=m$xlevels$`as.factor(date)`, p=0),
               data.frame(date=m$xlevels$`as.factor(date)`, p=1))
  #adf$week <- week(as.Date(adf$date))
  pred <- predict(m, newdata=adf, interval="confidence")
  adf$hat <- pred[,"fit"]
  adf$lo <- pred[,"lwr"]
  adf$hi <- pred[,"upr"]
  adf$date <- as.Date(adf$date)
  if (stlab==1) {
    pdf <- subset(df, df$p<=0.01)
    ldf <- subset(df, df$p>0.01)
  } else if (stlab==0) {
    pdf <- subset(df, df$p>=0.99)
    ldf <- subset(df, df$p<0.99)
  } else {
    pdf <- df
    ldf <- NULL #data.frame(date=NA,y=NA,ST=NA,p=NA)
  }
  clrs <- solarized_pal(2)(2)
  fig <- ggplot(df,aes(color=p)) +
    geom_point(data=pdf,aes(x=date, y=y, color=p),alpha=0.3, size=0.5) +
    xlim(c(max(c(as.Date("2020-04-10"), min(df$date))), max(df$date))) +
    geom_line(data=adf, aes(x=date, y=hat, group=p), size=2) +
    figtheme + scale_color_gradient(low=clrs[1], high=clrs[2]) + #colors_grad(palette="Green-Gold") +
    #theme(legend.position="none") +
    xlab("") +
    ylim(c(-0.65,0.4))
  if (!is.null(ldf))
    fig <- fig +
      geom_text(data=ldf,
                aes(x=date, y=y, label=ST, color=p), alpha=1.0, size=5)
  return(fig)
}
library(latex2exp)
sdf$ps <- sdf$pschoolfull + 0.5*sdf$pschoolhybrid
fig <- pfigure(subset(sdf, date>="2020-07-01"), "ps", stlab=-1,L=10) + ggtitle(TeX(relabel("dlogdc given pschoolremote"))) + ylab(TeX(relabel("dlogdc")))

#p <- "pmask"
#figdev(sprintf("%s/tex/tables_and_figures/%s-cases-%s.pdf", rootdir, p, L.c), width=4, height=3)
#print(fig)
#dev.off()
fig
