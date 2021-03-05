#loadpackages
library("drake")

#plan
trait_driver_plan <- "Rscripts/1_living_norway_drake.R"

#Build the right things
r_make(source = trait_driver_plan)
drake_failed()

# show methods results
if(length(drake_failed()) == 0){
  fs::file_show("manuscript.pdf")#display pdf
}


#view dependency graph
r_vis_drake_graph(source = trait_driver_plan, targets_only = TRUE)
