library(ordinal)
library(tidyverse)
library(ggpmisc)
library(broom)
library(viridis)
library(gridExtra)
library(grid)
library(ggmosaic)
library(ggtext)
library(cowplot)
library(forcats)


plot_plan <- drake_plan(
  
  #Probability distributions
  
  #1.1
  eng.clmm.graph <- clmm(Values ~ University + Gender + (1|NO), data = ana.data.1.1),
  summary(eng.clmm.graph),
  
  t1.1 <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "University1", "University **", term),
           term = if_else(term == "GenderMale", "Male **", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5:6),
  
  t1.1 <- as.data.frame(t1.1),
  is.num <- sapply(t1.1, is.numeric),
  t1.1[is.num] <- lapply(t1.1[is.num], round, 2),
  t1.1,
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]],
              eng.clmm.graph$beta[[2]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  g1.1 <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g1.1 +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 2.2,
             y = 1,
             label = list(t1.1),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 1.3)),
  
  
  #1.2a
  
  eng.clmm.graph <- clmm(Values ~ Domain + Gender + (1|NO), data = ana.data.1.2a)  ,
  summary(eng.clmm.graph),
  
  t1.2a <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "DomainT", "Teaching *", term),
           term = if_else(term == "GenderMale", "Male *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(4:5),
  
  t1.2a <- as.data.frame(t1.2a),
  is.num <- sapply(t1.2a, is.numeric),
  t1.2a[is.num] <- lapply(t1.2a[is.num], round, 2),
  t1.2a,
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]],
              eng.clmm.graph$beta[[2]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  
  g1.2a <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Minimally important"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Somewhat important"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Very important"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Extremely important"=1-(plogis(eng.clmm.graph$Theta[3] - xaxis))) %>%
    
    gather(Values, probability, 3:6) %>% 
    mutate(Values=factor(Values,levels=c("Minimally important","Somewhat important", "Very important", "Extremely important"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g1.2a +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 4.7,
             y = 1,
             label = list(t1.2a),
             fill="white")+
    coord_cartesian(xlim = c(-2.2, 2.5)),
  
  #1.3
  
  eng.clmm.graph <- clmm(Values ~ Gender + (1|NO), data = ana.data.1.3),
  
  summary(eng.clmm.graph),
  
  t1.3 <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "GenderMale", "Male *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5:5),
  
  t1.3 <- as.data.frame(t1.3),
  is.num <- sapply(t1.3, is.numeric),
  t1.3[is.num] <- lapply(t1.3[is.num], round, 2),
  t1.3,
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  g1.3 <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    ylab("Probability") + xlab("") + ggtitle("Predicted curves - Using and sharing open data and code") + # label plot properties
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g1.3 +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 2.5,
             y = 1,
             label = list(t1.3),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 1.5)),
  
  
  #3.2a
  
  eng.clmm.graph <- clm(use.engage.Data ~ Gender, data = ana.data.3.2a) , 
  
  summary(eng.clmm.graph),
  
  
  t3.2a <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "GenderMale", "Male *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5),
  
  t3.2a <- as.data.frame(t3.2a),
  is.num <- sapply(t3.2a, is.numeric),
  t3.2a[is.num] <- lapply(t3.2a[is.num], round, 2),
  t3.2a,
  
  
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  
  
  g3.2a <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g3.2a +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 2.5,
             y = 1,
             label = list(t3.2a),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 1.5)),
  
  #3.2b
  
  eng.clmm.graph <- clm(use.engage.Code ~ learnt.use.Code, data = ana.data.3.2b),
  
  summary(eng.clmm.graph),
  
  t3.2b <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "learnt.use.CodeTRUE", "Learned Yes *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5),
  
  t3.2b <- as.data.frame(t3.2b),
  is.num <- sapply(t3.2b, is.numeric),
  t3.2b[is.num] <- lapply(t3.2b[is.num], round, 2),
  t3.2b,
  
  
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  
  
  g3.2b <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g3.2b +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 2.5,
             y = 1,
             label = list(t3.2b),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 1.5)),
  
  #3.2d
  
  eng.clmm.graph <- clm(share.engage.Data ~ Gender, data = ana.data.3.2d),
  
  summary(eng.clmm.graph),
  
  t3.2d <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "GenderMale", "Male *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5),
  
  t3.2d <- as.data.frame(t3.2d),
  is.num <- sapply(t3.2d, is.numeric),
  t3.2d[is.num] <- lapply(t3.2d[is.num], round, 2),
  t3.2d,
  
  
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  
  
  g3.2d <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g3.2d +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 2.5,
             y = 1,
             label = list(t3.2d),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 1.5)),
  
  #3.2e
  
  eng.clmm.graph <- clm(share.engage.Code ~ learnt.share.Code, data = ana.data.3.2e), 
  
  summary(eng.clmm.graph),
  
  t3.2e <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "learnt.share.CodeTRUE", "Learned Yes *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5),
  
  t3.2e <- as.data.frame(t3.2e),
  is.num <- sapply(t3.2e, is.numeric),
  t3.2e[is.num] <- lapply(t3.2e[is.num], round, 2),
  t3.2e,
  
  
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  
  
  g3.2e <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g3.2e +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 2.5,
             y = 1,
             label = list(t3.2e),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 1.5)),
  
  #3.2g
  
  eng.clmm.graph <- clm(use.engage.EduTool ~ learnt.use.EduTool, data = ana.data.3.2g),  
  
  summary(eng.clmm.graph),
  
  
  t3.2g <- tidy(eng.clmm.graph) %>%
    mutate(term = if_else(term == "learnt.use.EduToolTRUE", "Learned Yes *", term)) %>% 
    select(-p.value, -coef.type,
           "Fixed effect terms" = term,
           Estimate = estimate,
           SE = std.error,
           "z-value" = statistic) %>%
    slice(5),
  
  t3.2g <- as.data.frame(t3.2g),
  is.num <- sapply(t3.2g, is.numeric),
  t3.2g[is.num] <- lapply(t3.2g[is.num], round, 2),
  t3.2g,
  
  
  
  
  vlines <- c(0,
              eng.clmm.graph$beta[[1]]),
  xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  
  
  
  g3.2g <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
    mutate("Never"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
           "Rarely"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
           "Several times a year"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
           "Several times a month"=plogis(eng.clmm.graph$Theta[4] - xaxis) - plogis(eng.clmm.graph$Theta[3] - xaxis),
           "Several times a week"=1-(plogis(eng.clmm.graph$Theta[4] - xaxis))) %>% 
    gather(Values, probability, 3:7) %>% 
    mutate(Values=factor(Values,levels=c("Never","Rarely", "Several times a year", "Several times a month","Several times a week"))) %>% # make factor and relevel
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
    scale_colour_viridis_d() + # apply colours manually
    theme_bw()+
    theme(axis.line = element_line(color = 'black'),
          legend.title = element_blank(),
          panel.border = element_blank()
    ),
  
  g3.2g +                                               # Add table to ggplot2 plot
    annotate(geom = "table",
             x = 3.25,
             y = 1,
             label = list(t3.2g),
             fill="white")+
    coord_cartesian(xlim = c(-0.5, 2)),
  
  
  #3.3
  
 # eng.clmm.graph <- clmm(Values ~ Domain + Aspect + (1|NO), data = ana.data.3.3),
#  summary(eng.clmm.graph),
  
  
 # t3.3 <- tidy(eng.clmm.graph) %>%
  #  mutate(term = if_else(term == "DomainS", "Supervision", term),
   #        term = if_else(term == "DomainT", "Teaching ***", term),
    #       term = if_else(term == "AspectCommunication", "Communication", term),
     #      term = if_else(term == "AspectData", "Data sharing", term),
      #     term = if_else(term == "AspectMethod", "Method sharing", term),
       #    term = if_else(term == "AspectPublish", "Publishing", term),
        #   term = if_else(term == "AspectReproducibility", "Reproducibility **", term),
         #  term = if_else(term == "AspectTransparency", "Transparency ***", term)) %>% 
    #select(-p.value, -coef.type,
     #      "Fixed effect terms" = term,
      #     Estimate = estimate,
       #    SE = std.error,
        #   "z-value" = statistic) %>%
    #slice(4:11),
  
  #t3.3 <- as.data.frame(t3.3),
  #is.num <- sapply(t3.3, is.numeric),
  #t3.3[is.num] <- lapply(t3.3[is.num], round, 2),
  #t3.3,
  
  #png("test.png")
  #p<-tableGrob(t3.3, theme_bw())
  #grid.arrange(p)
  #dev.off()
  
  
 # vlines <- c(0,
  #            eng.clmm.graph$beta[[1]],
   #           eng.clmm.graph$beta[[2]],
    #          eng.clmm.graph$beta[[3]],
     #         eng.clmm.graph$beta[[4]],
      #        eng.clmm.graph$beta[[5]],
       #       eng.clmm.graph$beta[[6]],
        #      eng.clmm.graph$beta[[7]],
         #     eng.clmm.graph$beta[[8]]),
  #xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100), # create 100 steps
  #yaxis <- rep(c(0,1),50), # fill in 0s and 1s for y-axis
  #colors <- c("Minimally" = "orangered",
       #       "Somewhat" = "orange2",
      #        "Very" = "yellow",
     #         "Extremely" = "green"),
  
  #g3.3 <- tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
   # mutate("Minimally important"=plogis(eng.clmm.graph$Theta[1] - xaxis), 
    #       "Somewhat important"=plogis(eng.clmm.graph$Theta[2] - xaxis) - plogis(eng.clmm.graph$Theta[1] - xaxis),
     #      "Very important"=plogis(eng.clmm.graph$Theta[3] - xaxis) - plogis(eng.clmm.graph$Theta[2] - xaxis),
      #     "Extremely important"=1-(plogis(eng.clmm.graph$Theta[3] - xaxis))) %>%
    
    #gather(Values, probability, 3:6) %>% 
    #mutate(Values=factor(Values,levels=c("Minimally important","Somewhat important", "Very important", "Extremely important"))) %>% # make factor and relevel
    #ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
    #geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
    #geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
    #geom_line(aes(y=probability,colour=Values),lwd=1,alpha=.8) + # add predicted curves
    #annotate("segment", # type of annotation = line segments
    #         x=vlines, y=0, xend=vlines, yend=1, # add estimates
    #         lty="solid", alpha=.75) + # visual properties of vertical lines
    #annotate("text", # type of annotation = text
    #         x=vlines,y=.75, # location of labels
    #         label=c("Research - Code sharing",
    #                 "Supervision",
    #                 "Teaching",
    #                 "Communication",
    #                 "Data sharing",
    #                 "Method sharing",
    #                 "Open publication",
    #                 "Research reproducibility",
    #                 "Research transparency"), # label names aligned with vlines[1:4]
    #         angle=90,vjust=-0.2) + # visual properties of text labels
    #scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
    #scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
    #ylab("Probability") + xlab("") + ggtitle("Predicted curves - Importance of Open Science aspects") + # label plot properties
    #scale_colour_viridis_d() + # apply colours manually
    #theme_bw()+
    #theme(axis.line = element_line(color = 'black'),
    #      legend.title = element_blank(),
    #      panel.border = element_blank()
    #),
  
#  g3.3 +                                               # Add table to ggplot2 plot
#    annotate(geom = "table",
#             x = 3,
#             y = -0.2,
#             label = list(t3.3),
#             fill="white")+
#    coord_cartesian(xlim = c(-1.5, 1.5)),
  
  
  #Scales
  
  
  scale1 = tibble( numeric_scale = c( 1, 2, 3, 4, 5, 6 ),
                   text_scale = c( "Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know" )),
  scale2 = tibble( numeric_scale = c(2, 3, 4, 5, 1 ),
                   text_scale = c("Minimally important", "Somewhat important", "Very important", "Extremely important", "Not applicable to my work" )),
  scale3 = tibble( numeric_scale = c( 1, 2, 3, 4, 5 ),
                   text_scale = c( "Not applicable to my work", "Minimally useful", "Somewhat useful", "Very useful", "Extremely useful" )),
  scale4 = tibble (numeric_scale = c(0, 1),
                   text_scale= c("No", "Yes")),
  scale5 = tibble (numeric_scale = c(0, 1, 2),
                   text_scale= c("No", "Yes", "Don't_know")),
  scale6 = tibble (numeric_scale = c(1, 2),
                   text_scale= c("Yes", "No")),
  scale7 = tibble (numeric_scale = c(1,2,3,4),
                   text_scale = c ("High_school", "Bsc", "Msc", "PhD")),
  scale8 = tibble (numeric_scale = c(1,2,3,4,5,6,7),
                   text_scale = c ("Bachelor_student", "Master_student", "PhD_student", "Researcher(temporary)", "Researcher(permanent)", "Associate_professor/Professor", "Other" )),
  
  
  
  
  # Mosaic plots
  
  ##1.1
  plotdata.1.1 <- ana.data.1.1 %>% 
    mutate(University = if_else(University == "1", "University", "Other"),
           University = factor(University, levels = c("University", "Other"))),
  
  mosaic_activity = ggplot(data = plotdata.1.1) +
    geom_mosaic(aes(x = product(Gender, Values, University), fill = Values), offset = 0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank()),
  
  # scale_fill_discrete(breaks = c(1:6), 
  #                     labels = scale1$text_scale) +
  # theme(axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       panel.background = element_blank())
  mosaic_activity ,
  
  
  #axis label frequency, text scale y axis, increase offset
  
  
  ##1.2a
  plotdata1.2a = ana.data.1.2a %>% 
    mutate(Domain = if_else(Domain == "R", "Research", "Teaching")),
  
  mosaic_importance = ggplot(data=plotdata1.2a)+
    geom_mosaic(aes(x = product(Gender, Values, Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels = c("Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank()),
  mosaic_importance ,
  
  
  ##1.3
  plotdata1.3 = ana.data.1.3 ,
  
  mosaic_action = ggplot(data=plotdata1.3)+
    geom_mosaic(aes(x=product(Gender), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank()),
  
  mosaic_action ,
  
  ##3.3
  plotdata3.3 = ana.data.3.3 %>% 
    mutate(Domain = fct_recode(Domain,
                               "Research" = "R",
                               "Supervision" = "S",
                               "Teaching" = "T")),
  
  aspect.labs = c( "Data \n sharing", "Code \n sharing",  "Method/protocol \n sharing", "Publishing \n papers \n open access", "Science \n communication \n through open \n channels",  "Research \n reproducibility", "Research \n transparency"),
  names(aspect.labs) = c("Data", "Code", "Method", "Publish", "Communication", "Reproducibility", "Transparency"),
  
  mosaic_all = ggplot(data=plotdata3.3)+
    geom_mosaic(aes( x=product(Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    facet_grid(~Aspect, labeller = labeller(Aspect = aspect.labs)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title=""),
  
  mosaic_all,
  
  #1
  plotdata3.3.1 = plotdata3.3 %>% 
    filter(Aspect=="Data"),
  
  
  
  mosaic_data = ggplot(data=plotdata3.3.1)+
    geom_mosaic(aes( x=product(Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Data sharing"),
  
  mosaic_data,
  
  ##3.3
  #2
  plotdata3.3.2 = plotdata3.3 %>% 
    filter(Aspect=="Code") %>% 
    
    
    mosaic_code = ggplot(data=plotdata3.3.2)+
    geom_mosaic(aes( x=product(Values,Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Code sharing"),
  
  mosaic_code,
  
  #3
  plotdata3.3.3 = plotdata3.3 %>% 
    filter(Aspect=="Method"),
  
  mosaic_method = ggplot(data=plotdata3.3.3)+
    geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Method sharing"),
  
  mosaic_method,
  
  
  
  #4
  plotdata3.3.4 = plotdata3.3 %>% 
    filter(Aspect=="Publish"),
  
  mosaic_publish = ggplot(data=plotdata3.3.4)+
    geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Publishing open access"),
  
  mosaic_publish,
  
  
  #5
  plotdata3.3.5 = plotdata3.3 %>% 
    filter(Aspect=="Communication"),
  
  mosaic_communication = ggplot(data=plotdata3.3.5)+
    geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Communicating science through open channels"),
  
  mosaic_communication,
  
  #6
  plotdata3.3.6 = plotdata3.3 %>% 
    filter(Aspect=="Reproducibility"),
  
  mosaic_reproducibility = ggplot(data=plotdata3.3.6)+
    geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Research reproducibility"),
  
  mosaic_reproducibility,
  
  #7
  plotdata3.3.7 = plotdata3.3 %>% 
    filter(Aspect=="Transparency"),
  
  mosaic_transparency = ggplot(data=plotdata3.3.7)+
    geom_mosaic(aes( x=product(Values, Domain), fill = Values), offset=0.02) + 
    scale_fill_viridis_d() +
    scale_y_productlist(labels=c("Not applicable to my work", "Minimally important", "Somewhat important", "Very important", "Extremely important")) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank())+
    labs(title="Research transparency")+
    
    
    mosaic_transparency,
  
  grid.newpage(),
  pushViewport(viewport(layout = grid.layout(4,2))),
  
  print(mosaic_data, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)),
  print(mosaic_code, vp = viewport(layout.pos.row = 1, layout.pos.col = 2)),
  print(mosaic_method, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)),
  print(mosaic_publish, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)),
  print(mosaic_communication, vp = viewport(layout.pos.row = 3, layout.pos.col = 1)),
  print(mosaic_reproducibility, vp = viewport(layout.pos.row = 3, layout.pos.col = 2)),
  print(mosaic_transparency, vp = viewport(layout.pos.row = 4, layout.pos.col = 1)),
  
  
  
  
  
  #3.2
  plotdata3.2 <- bndata %>% 
    select(NO, Gender, learnt.use.Data, learnt.share.Data, learnt.use.Code, learnt.share.Code,learnt.use.Publish, learnt.share.Publish,learnt.use.EduTool, learnt.do.Review, learnt.do.Outreach, learnt.principle.Reproducibility, learnt.principle.Transparency) %>% 
    pivot_longer( cols = c(learnt.use.Data, learnt.share.Data, learnt.use.Code, learnt.share.Code,learnt.use.Publish, learnt.share.Publish,learnt.use.EduTool, learnt.do.Review, learnt.do.Outreach, learnt.principle.Reproducibility, learnt.principle.Transparency), 
                  names_to = c("Action", "Domain", "Aspect"), values_to = "Values", names_sep = "[.]") %>%
    mutate(Values = if_else(Values == "0", "No", "Yes"),
           Values = factor(Values, levels = c("No", "Yes"))) %>%
    filter(!is.na(Values)),
  #filtering out use.engage.data = 6 ("I don't know")
  
  
  #earning.labs = c( "Open data", "Open code", "Open learning tools", "Open publications", "Open review", "Open science \n communication")
  #names(learning.labs) = c("Data", "Code", "EduTool", "Publish", "Review", "Outreach")
  
  ## using
  
  
  
  learning_use_stackplot = plotdata3.2 %>% 
    filter(Domain == "use") %>%
    count(Aspect, Values) %>% 
    group_by(Aspect) %>% 
    mutate(Freq = prop.table(n)) %>% 
    ggplot(aes(fill = Values, y = Freq, x = Aspect)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "Proportion of respondents") +
    scale_fill_manual(values = c("#31688EFF", "#FDE725FF"))+
    scale_x_discrete(labels = c("Open code", "Open data", "Open \n learning tools", "Read open \n publications"))+
    theme_minimal() +
    theme(legend.title = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(title="Using open resources in own education"),
  
  learning_use_stackplot,
  
  ## sharing
  
  
  learning_share_stackplot = plotdata3.2 %>% 
    filter(Domain == "share") %>%
    count(Aspect, Values) %>% 
    group_by(Aspect) %>% 
    mutate(Freq = prop.table(n)) %>% 
    ggplot(aes(fill = Values, y = Freq, x = Aspect)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "") +
    scale_fill_manual(values = c("#31688EFF", "#FDE725FF"))+
    scale_x_discrete(labels = c("Own code", "Own data","Publish papers \n or results \n openly"))+
    theme_minimal() +
    theme(legend.title = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(title="Sharing of resources openly in own education"),
  
  learning_share_stackplot,
  
  
  
  ## Principles
  
  
  learning_principles_stackplot = plotdata3.2 %>% 
    filter( Domain == "principle") %>%
    count(Aspect, Values) %>% 
    group_by(Aspect) %>% 
    mutate(Freq = prop.table(n)) %>% 
    ggplot(aes(fill = Values, y = Freq, x = Aspect)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "Proportion of respondents") +
    scale_fill_manual(values = c("#31688EFF", "#FDE725FF"))+
    #scale_x_discrete(labels = c("Own code", "Own data","Publish papers \n or results \n openly"))+
    theme_minimal() +
    theme(legend.title = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(title="Learned principles in own education"),
  
  
  learning_principles_stackplot,
  
  ## Review and outreach
  
  
  learning_other_stackplot = plotdata3.2 %>% 
    filter( Domain == "do") %>%
    count(Aspect, Values) %>% 
    group_by(Aspect) %>% 
    mutate(Freq = prop.table(n)) %>% 
    ggplot(aes(fill = Values, y = Freq, x = Aspect)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "") +
    scale_fill_manual(values = c("#31688EFF", "#FDE725FF"))+
    scale_x_discrete(labels = c("Outreach or \n science communication", "Open peer \n review"))+
    theme_minimal() +
    theme(legend.title = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(title="Open engamement in own learning"),
  
  
  learning_other_stackplot,
  
  
  
  grid.newpage(),
  pushViewport(viewport(layout = grid.layout(2,2))),
  
  print(learning_use_stackplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)),
  print(learning_share_stackplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2)),
  print(learning_principles_stackplot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)),
  print(learning_other_stackplot, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)),
  
  affiliation_plot = plotdata %>% 
    filter(Category == "Background") %>% 
    filter(Value != 0) %>%
    select(NO, Question, Value) %>% 
    mutate(Value = as.numeric(Value)) %>%
    group_by(Question) %>% 
    #summarise(Freq = sum(Value)) %>% 
    ggplot() +
    geom_bar(aes(Question)) +
    #scale_fill_viridis()+
    labs(x = "Affiliation", y = "Number of respondents") +
    scale_x_discrete(labels = c("Government agency", "Institute", "Other", "University"))+
    ylim(0, 50) +
    theme_minimal() +
    theme(legend.title = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()),
  
  
  
  country_plot = plotdata %>% 
    filter(Category == "Country") %>% 
    filter(Value != 0) %>% 
    select(NO, Question, Value) %>% 
    mutate(Value = as.numeric(Value)) %>%
    group_by(Question) %>% 
    ggplot() +
    geom_bar(aes(Question)) +
    #scale_fill_viridis()+
    labs(x = "Country of affiliation", y = "") +
    scale_x_discrete(labels = c("EU", "Non-EU", "Norway"))+
    ylim(0, 50) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          legend.title = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()),
  
  
  grid.newpage(),
  pushViewport(viewport(layout = grid.layout(1,2))),
  
  print(affiliation_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)),
  print(country_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  
)
