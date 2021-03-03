#drake plan for living norway manuscript

#load packages
library("drake")
library("tidyverse")
library("broom")
library("wesanderson")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#source subplans
source("Rscripts/import_surveydata2020_plan.R")
source("Rscripts/plot_plan.R")



#drake plan
# manuscript plan
manuscript_plan <- drake_plan(
  # #add extra packages to bibliography
  # biblio2 = package_citations(
  #   packages = c("e1071", "traitstrap", "drake", "tidyverse", "rmarkdown", "renv"), 
  #   old_bib = file_in("Rmd/TDT.bib"), 
  #   new_bib = file_out("Rmd/TDT2.bib")),
  
  #knit manuscript
  manuscript = {
    file_in("Rmd/elsevier-harvard_rjt.csl")
    file_in("Rmd/Paperpile - Nov 02 BibTeX Export.bib")
    rmarkdown::render(
      input = knitr_in("Manuscript.Rmd"), 
      clean = FALSE)
  }
)

#### combine plans ####
living_norway_plan <- bind_plans(import_plan,
                         # analysis_plan,
                         plot_plan)
                         #manuscript_plan)
#quick plot
plot(living_norway_plan)

#### configure drake plan ####
trait_config <- drake_config(plan = living_norway_plan, keep_going = TRUE)
trait_config
