#drake plan for living norway manuscript

#load packages
library(drake)
library(tidyverse)
library(broom)
#library(wesanderson)
library(ordinal)
#library(nlme)
#library(lme4)
library(MuMIn)
library(ggpmisc)
library(viridis)
library(gridExtra)
library(ggmosaic)
library(ggtext)
library(patchwork)
#library(cowplot)
library(forcats)
library(kableExtra)



#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#source subplans
source("Rscripts/import_surveydata2020_plan.R")
source("Rscripts/analysis_plan.R")
source("Rscripts/results_plan.R")
source("Rscripts/plot_plan.R")
source("Rscripts/table_plan.R")
source("Rscripts/SI_plot_plan.R")



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
  },
  
  si = {
    file_in("Rmd/elsevier-harvard_rjt.csl")
    file_in("Rmd/Paperpile - Nov 02 BibTeX Export.bib")
    rmarkdown::render(
      input = knitr_in("SI.Rmd"), 
      clean = FALSE)
  }
  
)

#### combine plans ####
living_norway_plan <- bind_plans(import_plan,
                                 analysis_plan,
                                 results_plan,
                                 manuscript_plan,
                                 plot_plan,
                                 table_plan,
                                 SI_plot_plan)
                         
#quick plot
plot(living_norway_plan)

#### configure drake plan ####
trait_config <- drake_config(plan = living_norway_plan, keep_going = TRUE)
trait_config

