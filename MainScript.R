#looking at the difference in yes no responses vs 5 point likert scale
library('tidyverse')
library('dplyr')
working_directory <- getwd()

x <- read.csv("Response Scales_Pilot April 2023.csv")
y <- names(x)
x_reduced <- x%>%
  select(PartNo, Age, Gender, CounterCode,
         avgSSOSH,       # - Average SSOSH score
         dassSTRESS,     # - DASS - Stress score
         dassDEPRESSION, # - DASS - Depression score
         dassANXIETY,    # - DASS - Anxiety score
         spqCOGPERCEP,   # - SPQ - Cognitive Perceptual subscale scores
         spqINTERPERSON, # - SPQ - Interpersonal
         spqDISORGANIZED,# - SPQ - Disorganisation
         spqTOTAL,       # - SPG - Total
         UE_YN, CD_YN, IA_YN, IN_YN, # OLIFE Yes No
         UE_5P, CD_5P, IA_5P, IN_5P, # OLIFE 5 Likert
         olife_5SUM,    # OLIFE Sum 5 point scale when answer == Yes 
         olife_5nosum,  # OLIFE Sum 5 point scale when answer == No 
         ue_Ysum, ue_Nsum,  # Unusual experiences 5 point scale when answer == Yes/No 
         cd_Ysum, cd_Nsum,  # CD 5 point scale when answer == Yes/No
         ia_Ysum, ia_Nsum,  # introvert anademia 5 point scale when answer == Yes/No
         in_Ysum, in_Nsum,  # in 5 point scale when answer == Yes/No
         avgUE_Y, avgCD_Y, avgIA_Y, avgIN_Y, # Avg 5 point when answer was yes (for each OLIFE subscale)
         avgUE_N, avgCD_N, avgIA_N, avgIN_N) # Avg 5 point when answer was no  (for each OLIFE subscale)
