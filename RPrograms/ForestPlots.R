# Sample code to create Forest plot  --------
# Draw a forestplot of cross-sectional, linear associations
# (Note that the df variable names 'name', 'beta' and 'se' do not really have to be
# explictly defined, as these are the default input values of these parameters.)
library(grid)
library(forestplot)
# library(forestploter)
library(ggplot2)
library(Unicode)

d1<-data.frame(Subgroup=c("Gestational age","< 28 weeks",">= 28 weeks","Bronchopulmonary dysplasia","Yes","No","Surgical approach", "Laparoscopic","Open repair","Overall"),n.early=c("","33/102 (32)","11/57 (19)","", "26/81 (32)","18/78 (23)","", "16/62 (26)","26/88 (30)","44/159 (28)"),n.late=c("", "16/99 (16)","11/50 (22)","", "10/73 (14)","17/76 (22)","", "6/47 (13)","17/79 (22)","27/149 (18)"),rr.ci=c("","0.61 (0.39-0.94)","0.92 (0.47-1.75)","","0.50 (0.27-0.87)","0.85 (0.51-1.37)","","0.54 (0.25-1.05)","0.74 (0.46-1.16)","0.68 (0.45-1.01)"),rd.ci=c("","-0.11 (-0.21, -0.01)","-0.02 (-0.14, 0.11)","","-0.14 (-0.26, -0.03)","-0.04 (-0.14, 0.07)","", "-0.10 (-0.23, 0.01)","-0.07 (-0.18, 0.03)","-0.08 (-0.17,  0.002)" ),
               prob=c("","99","61","","99","75","","96","90","97"),
               RR=c(NA,0.613,0.921,NA,0.501,0.844, NA,.543,0.741,0.683),
               lb=c(NA,0.389,0.477,NA,0.268,0.516, NA,0.253,0.458,0.449),
               ub=c(NA,0.943,1.716,NA,0.872,1.363,
                    NA,1.066,1.166,1.01))

d1$Subgroup <- ifelse(is.na(d1$RR), 
                      d1$Subgroup,
                      paste0("      ", d1$Subgroup))
d1$Subgroup[nrow(d1)]<-"Overall"


p1<-d1 |>
  forestplot(labeltext = c(Subgroup,n.early,n.late,rd.ci,rr.ci,prob),mean=RR,lower=lb,
             upper=ub,
             boxsize = 0.15,
             is.summary=c(rep(FALSE,12),TRUE),
             clip = c(0.1, 2),
             align="lccccc",
             vertices = TRUE,
             xlog = T,
             zero=1,
             xticks=log(c(.25,.5,1,2)),
             xlab="      Relative risk (95% CrI)",
             lwd.zero=1.5,
             lwd.ci=1,
             txt_gp = fpTxtGp(summary = list(
               gpar(fontfamily = "", fontface=1,cex=.9), 
               gpar(fontface=1,cex=.5)
             )
             ),
             col=fpColors(lines="black",box="black",summary="black"),
             fn.ci_sum=function(col, size, ...) {
               fpDrawNormalCI(clr.line = col, clr.marker = col, size=.3, lwd=2,...)
             },) |> 
  
  fp_add_header(Subgroup=c("", "Subgroup"),n.early=c("", " Early repair "),n.late=c("", "Late repair "),rr.ci=c("", "RR (95% CrI)"),prob=c("", "Probability of \n benefit*, %"),rd.ci=c("", "RD (95% CrI)"),is.summary = T) |>
  fp_add_lines(h_3 = gpar(lwd=1.5,col="black",columns = 1:5), 
               h_2 = gpar(lwd = 1.5, columns = 2:3, col = "gray")) |> 
  fp_set_zebra_style("#EFEFEF",ignore_subheaders = T) |>
  fp_set_style(txt_gp = fpTxtGp(label = list(gpar(fontfamily = "",cex=.78),
                                             gpar(fontfamily = "",cex=.78)),
                                ticks = gpar(fontfamily = "", cex = .65),
                                xlab  = gpar(fontfamily = "sans",
                                             cex = .7))) |> 
  fp_decorate_graph(
    graph.pos = 6) 


# png(file = "./Results/ForestSubgroups_Jan2024_2.png",width=2200,
#     height=1200,
#     units="px",res=200)
# p1
# 
# dev.off()
# 
x <- unit(.75, 'npc')
y <- unit(.76, 'npc')

x2 <- unit(.845, 'npc')

x3<-unit(.3,'npc')
y3<-unit(.92,'npc')

# svg("./Results/ForestSubgroups_Jan2024.svg", width = 10, height = 5)
# p1
# grid.text('Favors Late Repair', x, y, gp = gpar(fontsize=7))
# grid.text('Favors Early Repair', x2, y, gp = gpar(fontsize=7))
# 
# dev.off()

pdf("./Results/ForestSubgroups_Jan2024_2.pdf", width = 10, height = 5)
p1
grid.text('Favors Late Repair', x, y, gp = gpar(fontsize=7))
grid.text('Favors Early Repair', x2, y, gp = gpar(fontsize=7))

grid.text("No. of events/total patients (%)",x3,y3,gp=gpar(fontfamily = "", fontface=2,cex=.8))

dev.off()
