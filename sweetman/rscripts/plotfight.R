library(ggplot2)
set.seed(10)
d.plot <- data.frame(x=seq(1,20),y=runif(20),
                     subject=c(rep('case',11),rep('control',9)))
# false positives
fp <- d.plot[which(d.plot$x>11 & d.plot$y > 0.5),]
# false negatives
fn <- d.plot[which(d.plot$x<12 & d.plot$y <= 0.5),]

sz = 4
delta.x = 0.25
delta.y = 0.005



ggplot(data=d.plot,aes(x=x,y=y,colour=subject)) + geom_point(cex=sz) + 
  geom_hline(yintercept=0.5,linetype='dotted') + 
  #  geom_vline(xintercept=11.5,linetype='dotted') + 
  labs(x='participant',y="Predicted probability of having the disease") + 
  scale_color_manual(values=c("firebrick2", "darkgreen")) +
  scale_x_continuous(breaks=seq(1,20),minor_breaks = seq(1,20)) +
  geom_line(data=data.frame(x=c(fp$x[1]-delta.x,fp$x[1]+delta.x),
                            y=c(fp$y[1]+delta.y,fp$y[1]-delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(fp$x[2]-delta.x,fp$x[2]+delta.x),
                            y=c(fp$y[2]+delta.y,fp$y[2]-delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(fp$x[3]-delta.x,fp$x[3]+delta.x),
                            y=c(fp$y[3]+delta.y,fp$y[3]-delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(fp$x[1]-delta.x,fp$x[1]+delta.x),
                          y=c(fp$y[1]-delta.y,fp$y[1]+delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(fp$x[2]-delta.x,fp$x[2]+delta.x),
                          y=c(fp$y[2]-delta.y,fp$y[2]+delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(fp$x[3]-delta.x,fp$x[3]+delta.x),
                          y=c(fp$y[3]-delta.y,fp$y[3]+delta.y)),colour='firebrick1')


             
             
             
             # geom_text(data=data.frame(x=fp$x,
             #                           y=fp$y,
             #                           subject='case'),aes(label='x'), ,size=sz+1,vjust='middle',hjust='middle',nudge_x=0.01,nudge_y=0.003,colour='firebrick2') #+
             # geom_text(data=data.frame(x=fn$x,
             #                           y=fn$y,
             #                           subject='control'),aes(label='x'),size=sz+1,vjust='middle',hjust='middle',nudge_x=0.01,nudge_y=0.003,colour='darkgreen')


## legend

d=0.2
d.l <- data.frame(x=c(0,0,0,0),
                  y=c(1+d,1+2*d,1+3*d,1+4*d),
                  x1=c(3,3,3,3),                 
                  label=c( 'False negative', 'True negative','False positive','True positive'),
                  col=c('a','a','b','b'),
                  col2=rep('black',4)
                  )

delta.y = 0.018
ggplot(data=d.l,aes(x=x,y=y,colour=col)) + geom_point(cex=sz) +
  coord_cartesian(xlim =c(0, 14), ylim = c(0, 3)) +
  geom_text(aes(label=label,x=x1,y=y),colour=d.l$col2) +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values=c("darkgreen","firebrick2" )) +
  theme(legend.position="none") +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank()) +
  geom_line(data=data.frame(x=c(d.l$x[1] + delta.x,0 + d.l$x[1] -delta.x),
                            y=c(d.l$y[1] + delta.y, d.l$y[1]-delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(d.l$x[1] + delta.x,0 + d.l$x[1] -delta.x),
                            y=c(d.l$y[1] - delta.y, d.l$y[1]+delta.y)),colour='firebrick1') +
  geom_line(data=data.frame(x=c(d.l$x[1] + delta.x,0 + d.l$x[1] -delta.x),
                            y=c(d.l$y[3] + delta.y, d.l$y[3]-delta.y)),colour='darkgreen') +
  geom_line(data=data.frame(x=c(d.l$x[1] + delta.x,0 + d.l$x[1] -delta.x),
                            y=c(d.l$y[3] - delta.y, d.l$y[3]+delta.y)),colour='darkgreen') 

