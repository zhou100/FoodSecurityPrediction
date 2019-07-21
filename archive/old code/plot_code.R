rm(list=ls())

library("reshape2")
library("ggplot2")
library("zoo")

setwd("~/Box Sync/Research/Malawi_FewS/")
malawi <- read.csv("malawi_data_new.csv")


# plot the coefficents 
RCSI_ln = lm (RCSI~0 + as.factor(month)+reside* as.factor(month), data=malawi)
summary(RCSI_ln)
unlist(RCSI_ln$coefficients[1])


data<-cbind(scale(malawi$RCSI),scale(malawi$FCS),scale(malawi$HDDS),scale(malawi$MAHFP),malawi$month)


            colnames(data)<-c("RCSI","FCS","HDDS","MAHFP","month")
            data<-as.data.frame(data)
            long_data<- melt(data,na.rm=TRUE,id.vars = "month",measure.vars = c("RCSI","FCS","HDDS","MAHFP"))
            
            colnames(long_data)<-c("month","measure","Normalized_value")
            long_data$months<-as.factor(long_data$month)
            long_data$FS_measures<-as.factor(long_data$measure)
            ggplot(as.data.frame(long_data),aes(x=months, color=FS_measures,y=Normalized_value))  
            
            ggplot(as.data.frame(long_data),aes(x=months, color=FS_measures,y=Normalized_value))  + geom_smooth(model=lm)
            
            # start here 
            data<-cbind(malawi$RCSI,(malawi$FCS),(malawi$HDDS),malawi$month)
            colnames(data)<-c("RCSI","FCS","HDDS","month")
            data<-as.data.frame(data)
            
            short_data<-aggregate(cbind(RCSI,FCS,HDDS)~month,data=data, mean)
            
            long_data<- melt(short_data,na.rm=TRUE,id.vars = "month",measure.vars = c("RCSI","FCS","HDDS"))
            
            colnames(long_data)<-c("month","measure","Food_security")
            long_data$months<-as.factor(long_data$month)
            long_data$FS_measures<-as.factor(long_data$measure)
            
            library(ggplot2)
            library(gtable)
            library(grid)
            plot1<-ggplot(as.data.frame(long_data[long_data$measure!="FCS",]),aes(x=months,color=FS_measures,y=Food_security)) + theme_bw() + geom_point() 
            plot2<-ggplot(as.data.frame(long_data[long_data$measure=="FCS",]),aes(x=months, color=FS_measures,y=Food_security))   + geom_point(color="dark green")+ theme_bw() %+replace% 
              theme(panel.background = element_rect(fill = NA)) 
        
            
              # extract gtable
            g1 <- ggplot_gtable(ggplot_build(plot1))
            g2 <- ggplot_gtable(ggplot_build(plot2))
            
            # overlap the panel of 2nd plot on that of 1st plot
            pp <- c(subset(g1$layout, name == "panel", se = t:r))
            g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                                 pp$l, pp$b, pp$l)
            
            # axis tweaks
            ia <- which(g2$layout$name == "axis-l")
            ga <- g2$grobs[[ia]]
            ax <- ga$children[[2]]
            ax$widths <- rev(ax$widths)
            ax$grobs <- rev(ax$grobs)
            ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
            g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
            g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
            
            # draw it
            grid.draw(g)

            p <- ggplot(as.data.frame(long_data[long_data$measure!="FCS",]),aes(x=months,color=FS_measures,y=Food_security,shape = measure)) + theme_bw() + geom_point() 
            # adding the relative humidity data, transformed to match roughly the range of the temperature
            p <- p + geom_point(data=as.data.frame(long_data[long_data$measure=="FCS",]), aes(y = Food_security/12.3, colour = "FCS")) 
            
            # now adding the secondary axis, following the example in the help file ?scale_y_continuous
            # and, very important, reverting the above transformation
            p <- p + scale_y_continuous(sec.axis = sec_axis(~.*12.3, name = "Food consumption scores"))
            
            # modifying colours and theme options
          #  p <- p + scale_colour_manual(values = c("blue", "red"))
            p <- p + labs(y = "Food Security Measures",
                          x = "Month",
                          colour = "Food Security Measures",
                          shape= " ")
          #  p <- p + theme(legend.position = c(0.8, 0.9))
            p <- p+ theme_bw() 

            
            
            data<-cbind(scale(malawi$RCSI),scale(malawi$FCS),scale(malawi$HDDS),(malawi$month))
            colnames(data)<-c("RCSI","FCS","HDDS","month")
            data<-as.data.frame(data)
            
            short_data<-aggregate(cbind(RCSI,FCS,HDDS)~month,data=data, mean)
            
            long_data<- melt(short_data,na.rm=TRUE,id.vars = "month",measure.vars = c("RCSI","FCS","HDDS"))
            
            colnames(long_data)<-c("month","measure","Normalized_value")
            long_data$months<-as.factor(long_data$month)
            long_data$FS_measures<-as.factor(long_data$measure)
            
            ggplot(as.data.frame(long_data),aes(x=months, color=FS_measures,y=Normalized_value))  + geom_point()
            
            
            data<-cbind(scale(malawi$RCSI),scale(malawi$FCS),scale(malawi$HDDS),malawi$month)
            colnames(data)<-c("RCSI","FCS","HDDS","month")
            data<-as.data.frame(data)
            long_data<- melt(data,na.rm=TRUE,id.vars = "month",measure.vars = c("RCSI","FCS","HDDS"))
            
            colnames(long_data)<-c("month","measure","Normalized_value")
            long_data$months<-as.factor(long_data$month)
            long_data$FS_measures<-as.factor(long_data$measure)
            ggplot(as.data.frame(long_data),aes(x=months, color=FS_measures,y=Normalized_value))  + geom_point()
            
            
