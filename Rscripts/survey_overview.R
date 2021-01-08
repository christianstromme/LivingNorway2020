library("tidyverse")
library("igraph") 
library("network") 
library("sna")
library("ggraph")
library("visNetwork")
library("threejs")
library("networkD3")
library("ndtv")

questions<- c("Definition1", "OS_aspects_engagement", "Hindrance", "Help", "Workshop_motiv", "Activities","Importance_OS_aspects_R", "Importance_OS_aspects_T", "Importance_OS_aspects_S", "Affiliation", "Country", "Position", "Degree", "Degree_year", "Teaching_types", "Academic_activities", "Gender", "Workshop_participation", "OS_in_teaching", "OS_in_supervision", "OS_in_own_edu", "Definition2", "Workshop_change","Future_OS","OS_comments", "Workshop_utility_research", "Overall_utility")

levels<- c(1, 11, 1, 1, 1, 4, 7, 7, 7, 5, 3, 1, 1, 1, 7, 6, 3, 3, 4, 4, 14, 1, 2, 1, 1, 6, 4)

question_number<-sprintf("q%s", seq(1:27))

part<- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)

#add variable type column

nodes<- data.frame(question_number, part, questions, levels) %>%
  mutate(Type = case_when( question_number %in% c("q1", "q3", "q4", "q5", "q22", "q23", "q24", "q25") ~ "Qualitative",
  question_number %in% c("q2", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14", "q15", "q16", "q17", "q18", "q19", "q20", "q21", "q26", "q27") ~ "Numerical"))


from<-sprintf("q%s", c(10, 10, 12, 2, 2, 2, 6, 6, 6, 16, 16, 16, 1, 2, 2, 2, 19, 20, 21, 23, 26, 27, 2, 7, 10, 12, 2, 7, 10, 12, 21, 21, 2, 10, 17, 2, 10, 17, 3, 4, 7, 8, 9, 3, 4, 7, 8, 9))

to<-sprintf("q%s", c(12, 14, 14, 7, 8, 9, 7, 8, 9, 7, 8, 9, 22, 1, 22, 5, 5, 5, 5, 24, 24, 24, 8, 8, 8, 8, 9, 9, 9, 9, 7, 8, 3, 3, 3, 4, 4, 4, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25))

weight<-c(5,5,1,11,11,11,4,4,4,6,6,6,1,11,11,11,4,4,14,2,6,4,11,7,5,1,11,7,5,1,14,14,11,5,3,11,5,3,1,1,7,7,7,1,1,7,7,7)


links<-data.frame(from, to, weight)

links

nodes_linked<-nodes[-c(11, 13, 15, 18),]


overview<-graph_from_data_frame(d = links, vertices = nodes_linked, directed = FALSE)
E(overview)$width <- E(overview)$weight
V(overview)$color <- ifelse(V(overview)$Type == "Qualitative", "darkorchid1", "cyan")
E(overview)$edge.color <- "gray80"




#deg<-degree.distribution(overview, mode="in")

#V(overview)$size<-deg*50

graph_attr(overview, "layout") <- layout_with_lgl

plot(overview, edge.curved = .1, edge.arrow.size = 0.5, edge.arrow.width = 0.5,  edge.width = 1, vertex.label=V(overview)$questions, vertex.label.dist = 1.5, vertex.size = 5, vertex.label.cex = 0.8,  asp = 0.6)
legend(x=-1, y=-1, c("Qualitative","Numerical"), pch=21,
       col="#777777", pt.bg= c("darkorchid1", "cyan"), pt.cex=2, cex=.8, bty="n", ncol=1)



