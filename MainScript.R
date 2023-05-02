#looking at the difference in yes no responses vs 5 point likert scale

#SPSS dataset all checked and labelled (except for the raw questions in some cases
#because it’s easier to see what’s what when conducting analyses via point and click using just the names).
#Unless you spot something of course! Updated ANOVA interaction is below based on the
#corrected scores due to reverse scoring. Both main Effects still sig. and an
#interaction effect still sig. but now it looks like this is driven by UE and IN being sig.
#lower than CD and IA, which is interesting, but that’s still something for later on (possibly a study 2).

#If you could have a crack at reproducing this in R starting from the raw OLIFE scores that would be super.
#The end goal then is a 4x2 repeated measures ANOVA on the average 5-point scores 
#when participants say yes or no on the standard 2-point scale. Let me know if you want more detail on that.

library('tidyverse')
library('dplyr')
working_directory <- getwd()

x <- read.csv("Response Scales_Pilot April 2023.csv")
y <- names(x)

Dass_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         DASS1,DASS2,DASS3,DASS4,DASS5,DASS6,DASS7,
         DASS8,DASS9,DASS10,DASS11,DASS12,DASS13,DASS14,
         DASS15,DASS16,DASS17,DASS18,DASS19,DASS20,DASS21)
#dassSTRESS, dassDEPRESSION, dassANXIETY

SPQ_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         SPQ1,SPQ2,SPQ3,SPQ4,SPQ5,SPQ6,SPQ7,SPQ8,SPQ9,SPQ10,
         SPQ11,SPQ12,SPQ13,SPQ14,SPQ15,SPQ16,SPQ17,SPQ18,SPQ19,SPQ20,
         SPQ21,SPQ22)
#spgCOGPERCEP, spqINTERPERSON, spqINTERPERSON, spqTOTAL

SSOH_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         SSOSH1,SSOSH2,SSOSH3,SSOSH4,SSOSH5,
         SSOSH6,SSOSH7,SSOSH8,SSOSH9,SSOSH10)
#reverse score 2,4,5,7,9
#average SSOSH
SSOH_raw <- SSOH_raw%>% # add in the reverse scores columns
  dplyr::mutate(SSOSH2_reverse = case_when(SSOSH2 == 1~5,
                                           SSOSH2 == 2~4,
                                           SSOSH2 == 4~2,
                                           SSOSH2 == 5~1,
                                           TRUE~SSOSH2),
                SSOSH4_reverse = case_when(SSOSH4 == 1~5,
                                         SSOSH4 == 2~4,
                                         SSOSH4 == 4~2,
                                         SSOSH4 == 5~1,
                                         TRUE~SSOSH4),
                SSOSH5_reverse = case_when(SSOSH5 == 1~5,
                                         SSOSH5 == 2~4,
                                         SSOSH5 == 4~2,
                                         SSOSH5 == 5~1,
                                         TRUE~SSOSH5),
                SSOSH7_reverse = case_when(SSOSH7 == 1~5,
                                         SSOSH7 == 2~4,
                                         SSOSH7 == 4~2,
                                         SSOSH7 == 5~1,
                                         TRUE~SSOSH7),
               SSOSH9_reverse = case_when(SSOSH9 == 1~5,
                                           SSOSH9 == 2~4,
                                           SSOSH9 == 4~2,
                                           SSOSH9 == 5~1,
                                           TRUE~SSOSH9))%>%
  dplyr::mutate(avgSSOSH = sum(SSOSH1, SSOSH2_reverse,SSOSH3,SSOSH4_reverse,
                               SSOSH5_reverse, SSOSH6,SSOSH7_reverse,SSOSH8,
                               SSOSH9_reverse,SSOSH10)/10)
  


OLIFE_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         OLIFE1_YN,OLIFE2_YN,OLIFE3_YN,OLIFE4_YN,OLIFE5_YN,OLIFE6_YN,OLIFE7_YN,OLIFE8_YN,OLIFE9_YN,OLIFE10_YN,
         OLIFE11_YN,OLIFE12_YN,OLIFE13_YN,OLIFE14_YN,OLIFE15_YN,OLIFE16_YN,OLIFE17_YN,OLIFE18_YN,OLIFE19_YN,OLIFE20_YN,
         OLIFE21_YN,OLIFE22_YN,OLIFE23_YN,OLIFE24_YN,OLIFE25_YN,OLIFE26_YN,OLIFE27_YN,OLIFE28_YN,OLIFE29_YN,OLIFE30_YN,
         OLIFE31_YN,OLIFE32_YN,OLIFE33_YN,OLIFE34_YN,OLIFE35_YN,OLIFE36_YN,OLIFE37_YN,OLIFE38_YN,OLIFE39_YN,OLIFE40_YN,
         OLIFE41_YN,OLIFE42_YN,
         OLIFE1_5P,OLIFE2_5P,OLIFE3_5P,OLIFE4_5P,OLIFE5_5P,OLIFE6_5P,OLIFE7_5P,OLIFE8_5P,OLIFE9_5P,OLIFE10_5P,
         OLIFE11_5P,OLIFE12_5P,OLIFE13_5P,OLIFE14_5P,OLIFE15_5P,OLIFE16_5P,OLIFE17_5P,OLIFE18_5P,OLIFE19_5P,OLIFE20_5P,
         OLIFE21_5P,OLIFE22_5P,OLIFE23_5P,OLIFE24_5P,OLIFE25_5P,OLIFE26_5P,OLIFE27_5P,OLIFE28_5P,OLIFE29_5P,OLIFE30_5P,
         OLIFE31_5P,OLIFE32_5P,OLIFE33_5P,OLIFE34_5P,OLIFE35_5P,OLIFE36_5P,OLIFE37_5P,OLIFE38_5P,OLIFE39_5P,OLIFE40_5P,
         OLIFE41_5P,OLIFE42_5P)
#reverse score 4IA,23IA,27IA,31IA,32IA,43IN,14IN,12IN
OLIFE_raw <- OLIFE_raw%>%
  dplyr::mutate(OLIFE4_YN_reversed_IA = case_when(OLIFE4_YN == 1~0,OLIFE4_YN == 0~1,TRUE~OLIFE4_YN),
                OLIFE4_5P_reversed_IA = case_when(OLIFE4_5P == 0~4,OLIFE4_5P == 1~3,OLIFE4_5P == 3~1,OLIFE4_5P == 4~0,TRUE~OLIFE4_5P),
                OLIFE23_YN_reversed_IA = case_when(OLIFE23_YN == 1~0,OLIFE23_YN == 0~1,TRUE~OLIFE23_YN),
                OLIFE23_5P_reversed_IA = case_when(OLIFE23_5P == 0~4,OLIFE23_5P == 1~3,OLIFE23_5P == 3~1,OLIFE23_5P == 4~0,TRUE~OLIFE23_5P),
                OLIFE27_YN_reversed_IA = case_when(OLIFE27_YN == 1~0,OLIFE27_YN == 0~1,TRUE~OLIFE27_YN),
                OLIFE27_5P_reversed_IA = case_when(OLIFE27_5P == 0~4,OLIFE27_5P == 1~3,OLIFE27_5P == 3~1,OLIFE27_5P == 4~0,TRUE~OLIFE27_5P),
                OLIFE31_YN_reversed_IA = case_when(OLIFE31_YN == 1~0,OLIFE31_YN == 0~1,TRUE~OLIFE31_YN),
                OLIFE31_5P_reversed_IA = case_when(OLIFE31_5P == 0~4,OLIFE31_5P == 1~3,OLIFE31_5P == 3~1,OLIFE31_5P == 4~0,TRUE~OLIFE31_5P),
                OLIFE32_YN_reversed_IA = case_when(OLIFE32_YN == 1~0,OLIFE32_YN == 0~1,TRUE~OLIFE32_YN),
                OLIFE32_5P_reversed_IA = case_when(OLIFE32_5P == 0~4,OLIFE32_5P == 1~3,OLIFE32_5P == 3~1,OLIFE32_5P == 4~0,TRUE~OLIFE32_5P),
                OLIFE43_YN_reversed_IN = case_when(OLIFE43_YN == 1~0,OLIFE43_YN == 0~1,TRUE~OLIFE43_YN),
                OLIFE43_5P_reversed_IN = case_when(OLIFE43_5P == 0~4,OLIFE43_5P == 1~3,OLIFE43_5P == 3~1,OLIFE43_5P == 4~0,TRUE~OLIFE43_5P),
                OLIFE14_YN_reversed_IN = case_when(OLIFE14_YN == 1~0,OLIFE14_YN == 0~1,TRUE~OLIFE14_YN),
                OLIFE14_5P_reversed_IN = case_when(OLIFE14_5P == 0~4,OLIFE14_5P == 1~3,OLIFE14_5P == 3~1,OLIFE14_5P == 4~30,TRUE~OLIFE14_5P),
                OLIFE12_YN_reversed_IN = case_when(OLIFE12_YN == 1~0,OLIFE12_YN == 0~1,TRUE~OLIFE12_YN),
                OLIFE12_5P_reversed_IN = case_when(OLIFE12_5P == 0~4,OLIFE12_5P == 1~3,OLIFE12_5P == 3~1,OLIFE12_5P == 4~30,TRUE~OLIFE12_5P))
#UE_YN, CD_YN, IA_YN, IN_YN,
#UE_5P, CD_5P, IA_5P, IN_5P
#OLIFE_5P_Y_SUM
#OLIFE_5P_N_SUM
#UE_5P_Y_SUM, CD_5P_Y_SUM, IA_5P_Y_SUM, IN_5P_Y_SUM
#UE_5P_N_SUM, CD_5P_N_SUM, IA_5P_N_SUM, IN_5P_N_SUM
#avg_UE_5P_Y, avg_CD_5P_Y, avg_IA_5P_Y, avg_IN_5P_Y
#avg_UE_5P_N, avg_CD_5P_N, avg_IA_5P_N, avg_IN_5P_N



x_reduced <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
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
         cd_Ysum, cd_Nsum,  # Cognitive D 5 point scale when answer == Yes/No
         ia_Ysum, ia_Nsum,  # introvert anademia 5 point scale when answer == Yes/No
         in_Ysum, in_Nsum,  # in 5 point scale when answer == Yes/No
         avgUE_Y, avgCD_Y, avgIA_Y, avgIN_Y, # Avg 5 point when answer was yes (for each OLIFE subscale)
         avgUE_N, avgCD_N, avgIA_N, avgIN_N) # Avg 5 point when answer was no  (for each OLIFE subscale)


