library(ordinal)
library(tidyverse)
library(ggplot2)
library(ggpmisc)


#1.1
eng.clmm.graph <- clmm(Values ~ University + Gender + (1|NO), data = ana.data.1.1)
summary(eng.clmm.graph)

t1.1 <- cbind("Fixed effect terms"=c("University**","Male**"), Estimate=c("0.82", "0.74"), SE=c("0.27", "0.26"), "z value"=c("3.02", "2.84"))

vlines <- c(0,
            eng.clmm.graph$beta[[1]],
            eng.clmm.graph$beta[[2]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")

g1.1 <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Other affiliation - Female",
                   "University",
                   "Male"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Engament with OS aspects") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

t1.1 <- as.data.frame(t1.1)

t1.1
g1.1 +                                               # Add table to ggplot2 plot
  annotate(geom = "table",
           x = 0.2,
           y = 1.2,
           label = list(t1.1))

#1.2a

eng.clmm.graph <- clmm(Values ~ Domain + Gender + (1|NO), data = ana.data.1.2a)  
summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]],
            eng.clmm.graph$beta[[2]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Minimally" = "orangered",
            "Somewhat" = "orange2",
            "Very" = "yellow",
            "Extremely" = "green")

tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Minimally=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Somewhat=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Very=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Extremely=1-(plogis(eng.clmm.graph$Theta[3] - xaxis))) %>%
          
  gather(Values, probability, 3:6) %>% 
  mutate(Values=factor(Values,levels=c("Minimally","Somewhat", "Very", "Extremely"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Research - Female",
                   "Teaching",
                   "Male"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Importance of data sharing") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#1.3

eng.clmm.graph <- clmm(Values ~ Action + Gender + (1|NO), data = ana.data.1.3)

summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]],
            eng.clmm.graph$beta[[2]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")


tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Sharing - Female",
                   "Using",
                   "Male"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Using and sharing open data and code") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#3.2a

eng.clmm.graph <- clm(use.engage.Data ~ Gender, data = ana.data.3.2a)  

summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")


tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Female",
                   "Male"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Using open data based on experience") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#3.2b

eng.clmm.graph <- clm(use.engage.Code ~ learnt.use.Code, data = ana.data.3.2b)

summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")


tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Learned No",
                   "Learned Yes"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Using open code based on experience") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#3.2d

eng.clmm.graph <- clm(share.engage.Data ~ Gender, data = ana.data.3.2d)

summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")


tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Female",
                   "Male"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Sharing open data based on experience") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#3.2e

eng.clmm.graph <- clm(share.engage.Code ~ learnt.share.Code, data = ana.data.3.2e) 

summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")


tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Learned No",
                   "Learned Yes"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Sharing open code based on experience") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background

#3.2g

eng.clmm.graph <- clm(use.engage.EduTool ~ learnt.use.EduTool + Gender, data = ana.data.3.2g)  

summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Never" = "orangered",
            "Rarely" = "orange2",
            "Yearly" = "yellow",
            "Monthly" = "green",
            "Weekly" = "turquoise")


tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Never=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Rarely=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Yearly=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Monthly=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
         Weekly=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
  
  gather(Values, probability, 3:7) %>% 
  mutate(Values=factor(Values,levels=c("Never","Rarely", "Yearly", "Monthly","Weekly"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Learned No",
                   "Learned Yes"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Using open educational tools based on experience") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background


#3.3
eng.clmm.graph <- clmm(Values ~ Domain + Aspect + (1|NO), data = ana.data.3.3)
summary(eng.clmm.graph)

vlines <- c(0,
            eng.clmm.graph$beta[[1]],
            eng.clmm.graph$beta[[2]],
            eng.clmm.graph$beta[[3]],
            eng.clmm.graph$beta[[4]],
            eng.clmm.graph$beta[[5]],
            eng.clmm.graph$beta[[6]],
            eng.clmm.graph$beta[[7]],
            eng.clmm.graph$beta[[8]])
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("Minimally" = "orangered",
            "Somewhat" = "orange2",
            "Very" = "yellow",
            "Extremely" = "green")

tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(Minimally=plogis(eng.clmm.graph$Theta[1] - xaxis), 
         Somewhat=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
         Very=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
         Extremely=1-(plogis(eng.clmm.graph$Theta[3] - xaxis))) %>%
  
  gather(Values, probability, 3:6) %>% 
  mutate(Values=factor(Values,levels=c("Minimally","Somewhat", "Very", "Extremely"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("Research - Code sharing",
                   "Supervision",
                   "Teaching",
                   "Communication",
                   "Data sharing",
                   "Method sharing",
                   "Open publication",
                   "Research reproducibility",
                   "Research transparency"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("Predicted curves - Importance of OS aspects") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() # improve visibility with white background


#####ALTERNATIVE


pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
  Theta <- c(-1000, theta, 1000)
  sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}


plot.probabilities<-function(grid, model, leg, draw.these=NULL, main="", xlab="", legend.pos="topleft", ylim=NULL, col=NULL, lty=NULL) {
  co <- model$coefficients[1:length(model$y.levels)-1]
  pre.mat <- pred(eta=rowSums(grid), theta=co)
  n.total<-length(pre.mat)
  n.rows<-length(pre.mat[1,])
  n <- n.total/n.rows
  ylim <- if(is.null(ylim)) c(0,1)
  else ylim
  draw.these <- if(is.null(draw.these)) 1:n
  else draw.these
  
  plot(model$y.levels, pre.mat[draw.these[1],], lty=1, type="l", ylim=ylim, xlab=xlab, axes=FALSE, ylab="Probability", las=1, main=main)
  axis(1)
  axis(2)
  i <- 1
  for(k in draw.these) {
    draw_color <- if(is.null(col)) "black"
    else col[i]
    curr_lty <- if(is.null(lty)) i
    else lty[i]
    lines(model$y.levels, pre.mat[k,], lty=curr_lty, col=draw_color)
    i <- i + 1 
  }
  if(is.null(lty)) { 
    legend(legend.pos, leg, lty=1:i, bty="n")
  }
  else {
    legend(legend.pos, leg, lty=lty, bty="n")
  }
}



eng.clmm.f <- clmm(Values ~ University + Gender + (1|NO), data = ana.data.1.1)
mat_0_Female <- expand.grid(NO = qnorm(0.95) * c(-1, 0, 1) * eng.clmm.f$stDev, University = c(0), Gender=c(0))
mat_1_Female <- expand.grid(NO = qnorm(0.95) * c(-1, 0, 1) * eng.clmm.f$stDev, University = c(eng.clmm.f$beta[2]), Gender=c(0))
mat_0_Male <- expand.grid(NO = qnorm(0.95) * c(-1, 0, 1) * eng.clmm.f$stDev, Gender = c(0), University=c(eng.clmm.f$beta[1]))
mat_1_Male <- expand.grid(NO = qnorm(0.95) * c(-1, 0, 1) * eng.clmm.f$stDev, Gender = c(eng.clmm.f$beta[2]), University=c(eng.clmm.f$beta[1]))
par(mfrow = c(2,2))
plot.probabilities(mat_0_Female, eng.clmm.f, c("5th %-tile respondent","avg. respondent","95th %-tile respondent"), main="Affiliation=Other, Gender=Female")
plot.probabilities(mat_1_Female, eng.clmm.f, c("5th %-tile respondent%","avg. respondent","95th %-tile respondent"), main="Affiliation=University, Gender=Female")
plot.probabilities(mat_0_Male, eng.clmm.f, c("5th %-tile respondent","avg. respondent","95th %-tile respondent"), main="Affiliation=Other, Gender=Male")
plot.probabilities(mat_1_Male, eng.clmm.f, c("5th %-tile respondent","avg. respondent","95th %-tile respondent"), main="Affiliation=University, Gender=Female")


