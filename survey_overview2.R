library("tidyverse")
library("igraph") 
library("network") 
library("sna")
library("ggraph")
library("visNetwork")
library("threejs")
library("networkD3")
library("ndtv")

questions_num<- c("OS_aspects_engagement",  "Activities","Importance_OS_aspects_R", "Importance_OS_aspects_T", "Importance_OS_aspects_S", "Affiliation", "Country", "Position", "Degree", "Degree_year", "Teaching_types", "Academic_activities", "Gender", "Workshop_participation", "OS_in_teaching", "OS_in_supervision", "OS_in_own_edu",  "Workshop_utility_research", "Overall_utility")

levels_num<- c(11, 4, 7, 7, 7, 5, 3, 1, 1, 1, 7, 6, 3, 3, 4, 4, 14, 6, 4)

question_number_num<-sprintf("q%s", seq(1:19))

part_num<- c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3)

#add variable type column

nodes_num<- data.frame(question_number_num, part_num, questions_num, levels_num) 

from_num<-sprintf("q%s", c(6, 6, 8, 1, 1 ,1, 2, 2, 2, 12, 12, 12, 1, 4, 6, 8, 1, 3, 6, 8, 17, 17  ))

to_num<-sprintf("q%s", c(8, 10, 10, 3, 4, 5, 3, 4, 5, 3, 4, 5, 4, 5, 4, 4, 5, 5, 5, 5, 3, 4))




links_num<-data.frame(from_num, to_num)

links_num

nodes_linked_num<-nodes_num[-c(11, 13, 15, 18),]


overview_num<-graph_from_data_frame(d = links_num, vertices = nodes_linked_num, directed = FALSE)
E(overview_num)$edge.color <- "gray80"


#graph_attr(overview_num, "layout") <- layout_with_lglcolrs <- c("gold")
plot(overview_num, edge.curved = .1, edge.arrow.size = 0.5, edge.arrow.width = 0.5,  edge.width = 1, vertex.label=V(overview_num)$questions_num, vertex.label.dist = 1.5, vertex.size = 5, vertex.label.cex = 0.8,  asp = 0.6)



#levels_qual<- c(1, 1, 1, 1, 1, 2, 1, 1,)
#part_qual<- c(1, 1, 1, 1, 3, 3, 3, 3)