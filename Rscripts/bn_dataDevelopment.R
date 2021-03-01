# Bayesian network analysis ---

# Background
# draft survey form https://docs.google.com/document/d/1_24DQGPrbvieNBiJuvlq-pVkl6zY41YwVgYR_LGJW3s/edit#heading=h.ivck3znq3pw4 

# Libraries ---
library(tidyverse)
library(bnlearn)

# Data import ---

loadd(bndata)
   
# Now we can start to identify the key nodes and the links between them
   # we can split up the major nodes as:
    # TAUGHT, USE,  IMPORTANCE, SHARE
    # And we can differentiate these by: 
      # ACTIVITY: Data, Code, Publish, ( Method,  ( Edutool, Review, Outreach, Other)) brackets denote not all nodes have this
      # PRINCIPLE: Communication, Reproducability, Transparency
   # In addition, we have, as potential end nodes:
      # prior use in teaching, supervision (and research)
      # future use in research, teaching, supervision.
   # Finally we have Workshop participation and workshop value, which we hope contributes to future use.
   
 [Activity]  # Data, Code, Publish, ( Method,  ( Edutool, Review, Outreach, Other)) 
 [Principle]   # Communication, Reproducability, Transparency
 [OtherReasons]  # does Research, Synthesis, Policy, Outreach ; Academic, position, degree, year, gender
 [ImportanceArea] # Important to Research, Teaching, or Supervision
 [TaughtActivity | Activity]
 [TaughtPrinciple | Principle] 
 [Use | TaughtActivity : TaughtPrinciple : OtherReasons]   
 [ImportanceActivity | ImportanceArea : TaughtActivity : TaughtPrinciple : Use : OtherReasons]  
 [ImportancePrinciple | ImportanceArea : TaughtActivity : TaughtPrinciple : Use : OtherReasons]   
 [Share | ImportanceActivity : ImportancePrinciple : ImportanceArea]   
 [TeachPrior |  ImportanceArea : ImportanceActivity : ImportancePrinciple : Use]   
 #[TeachFuture | ImportanceArea : ImportanceActivity : ImportancePrinciple : TeachPrior : WorkshopValue : WorkshopParticipation]
 #[UseFuture | ImportanceArea : ImportanceActivity : ImportancePrinciple : Use : WorkshopValue : WorkshopParticipation] 
   
# So, we need columns for all these nodes, for each 
   
   
   
   
   