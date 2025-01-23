# Plotting posterior distribution of RR and prior distribution
# Jan, 2023

# rr is vector of draws from posterior distribution

d_rr<-data.frame(rr=as.vector(rr),logrr=as.vector(log(rr)))

p1<- d_rr %>%
  ggplot()+
  stat_halfeye(aes(x = logrr,fill = stat((x) > 0)),adjust=3,
               .width=.95,limits=c(-1.5,1.5))+
  
  # stat_slab(aes(x=y1),fill = NA, color = "mediumblue",limits=c(-1.2,.69),
  #           adjust=3,n=10000) +
  # scale_thickness_shared()+
  theme_classic() + ylab("Probability Density")+
  ggtitle("Posterior probability of intervention effect on primary outcome")+
  theme(plot.title = element_text(hjust = 0.3))+
  xlab("\n Risk Ratio ")+
  theme(axis.ticks.y = element_blank(),axis.line.y=element_blank(),
        axis.text.y = element_blank())+
  # scale_x_continuous(limits=c(.1,2))+
  scale_x_continuous(breaks=c(-1.2, -0.69,-.356, -.10, 0,.18,0.41, 0.69), 
                     # limits=c(-1.4, 1),
                     
                     labels = c(0.30, 0.50,0.7,0.9, 1, 1.2, 1.5, 2)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), n = 10000, 
                colour = "mediumblue",xlim=c(-1,1.5)) +
  
  coord_cartesian(xlim = c(-.69, 1))+
  
  theme(legend.position="none")+
  annotate(geom="text", x=0.15, y=.05,  
           label=paste("INTERVENTION \n BENEFIT, ", round(mean(rr>1)*100),"%",sep=""),
           color="black",size=2.5,fontface=2)+
  annotate(geom="text", x=-.1, y=.05, 
           label=paste("INTERVENTION \n HARM, ", round(mean(rr<1)*100),"%",sep=""),
           color="black",size=2.5,fontface=2) +
  annotate(geom="text", x=.86, y=.33, colour = "mediumblue",
           label="Neutral Prior")

png(filename = "Prior_Post_plot.png",width=1800,
    height=1000,
    units="px",res=200)
p1
dev.off()


# Code from Katie  -----
p1<-ggplot(test2, aes(x=x, y=y)) + 
  theme_bw() +
  geom_line() +
  theme(panel.border = element_blank()) +
  removeGridX() +
  removeGridY() +
  xlab("\n Risk Ratio") + 
  ylab("Probability Density") +
  geom_ribbon(data=subset(test2 , x >= 0), aes(ymax=y),ymin=0,
              fill="red", colour=NA, alpha = 0.5) +
  geom_ribbon(data=subset(test2 , x <= 0), aes(ymax=y),ymin=0,
              fill="blue", colour=NA, alpha = 0.3) +
  geom_errorbarh(aes(xmax = log(qt[3]), xmin = log(qt[2]), y = 0), 
                 alpha=0.9, size=0.3, height = 0.2) +
  theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12, face="bold"),
    axis.text.y = element_blank(),
    axis.ticks.y=element_blank()
  ) +
  scale_x_continuous(breaks=c(-0.8, -0.4, 0, 0.4, 0.8), limits=c(-1, 0.8),
                     labels = c(0.44, 0.67, 1, 1.49, 2.22)) +
  geom_vline(xintercept = 0, color = "black", size = 0.5, linetype="dashed") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 0.7), n = 10000, colour = "#009E73") +
  annotate("text", x = -0.22, y = 2.5, label= "Posterior probability\n of intervention effect", 
           size = 4.5, fontface = 2, hjust = 1) +
  annotate("text", x = 0.6, y = 0.6, label= "Neutral prior", 
           size = 4.5, colour = "#009E73", fontface =2) +
  annotate("text", x = -0.025, y = 0.6, label= "Treatment\n benefit", 
           size = 3.5, colour = "blue", fontface = 2, hjust = 1) +
  annotate("text", x = 0.025, y = 0.6, label= "Treatment\n harm", 
           size = 3.5, colour = "red", fontface = 2, hjust = 0) +
  annotate("text", x = -0.43, y = 0.18, label= "95% credible interval", 
           size = 3.5, fontface = 2, hjust = 0)


png(filename = "Prior_Post_plot.png",width=1800,
    height=1000,
    units="px",res=200)
p1
dev.off()
