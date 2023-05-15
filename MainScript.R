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
library('ggplot2')
library('data.table')
working_directory <- getwd()

#x <- read.csv("Response Scales_Pilot April 2023.csv")
x <- read.csv("Response Scales_2023 For Stew.csv")
y <- names(x)

Dass_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         DASS1,DASS2,DASS3,DASS4,DASS5,DASS6,DASS7,
         DASS8,DASS9,DASS10,DASS11,DASS12,DASS13,DASS14,
         DASS15,DASS16,DASS17,DASS18,DASS19,DASS20,DASS21)
Dass_raw <- Dass_raw%>%
  dplyr::mutate(dassDEPRESSION = (DASS3 + DASS5 + DASS10 + DASS13 + DASS16 + DASS17 + DASS21)*2,
                dassANXIETY = (DASS2 + DASS4 + DASS7 + DASS9 + DASS15 + DASS19 + DASS20)*2,
                dassSTRESS = (DASS1 + DASS6 + DASS8 + DASS11 + DASS12 + DASS14 + DASS18)*2)
#Depression: 3, 5, 10, 13, 16, 17, 21
#Anxiety:  2, 4, 7, 9, 15, 19, 20
#Stress:  1, 6, 8, 11, 12, 14, 18


SPQ_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         SPQ1,SPQ2,SPQ3,SPQ4,SPQ5,SPQ6,SPQ7,SPQ8,SPQ9,SPQ10,
         SPQ11,SPQ12,SPQ13,SPQ14,SPQ15,SPQ16,SPQ17,SPQ18,SPQ19,SPQ20,
         SPQ21,SPQ22)
SPQ_raw <- SPQ_raw%>%
  dplyr::mutate(spqCOGPERCEP = (SPQ2 + SPQ4 + SPQ5 + SPQ9 + SPQ10 + SPQ12 + SPQ16 + SPQ17),
                spqINTERPERSON = (SPQ1 + SPQ7 + SPQ11 + +SPQ14 + SPQ15 + SPQ18 + SPQ21 + SPQ22),
                spqDISORGANISED = (SPQ3 + SPQ6 + SPQ8 + SPQ13 + SPQ19 + SPQ20),
                spqTOTAL = (spqINTERPERSON + spqDISORGANISED + spqCOGPERCEP))

#spgCOGPERCEP, spqINTERPERSON, spqDISORGANISED, spqTOTAL
#spqINTERPERSONAL: 1,7,11,14,15,18,21,22
#spqDISORGANISED : 3,6,8,13,19,20
#spqCOGPERCEP : 2,4,5,9,10,12,16,17

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
  dplyr::mutate(avgSSOSH = (SSOSH1 + SSOSH2_reverse + SSOSH3 + SSOSH4_reverse +
                               SSOSH5_reverse + SSOSH6 + SSOSH7_reverse + SSOSH8 +
                               SSOSH9_reverse + SSOSH10)/10)

OLIFE_raw <- x%>%
  dplyr::select(PartNo, Age, Gender, CounterCode,
         OLIFE1_YN,OLIFE2_YN,OLIFE3_YN,OLIFE4_YN,OLIFE5_YN,OLIFE6_YN,OLIFE7_YN,OLIFE8_YN,OLIFE9_YN,OLIFE10_YN,
         OLIFE11_YN,OLIFE12_YN,OLIFE13_YN,OLIFE14_YN,OLIFE15_YN,OLIFE16_YN,OLIFE17_YN,OLIFE18_YN,OLIFE19_YN,OLIFE20_YN,
         OLIFE21_YN,OLIFE22_YN,OLIFE23_YN,OLIFE24_YN,OLIFE25_YN,OLIFE26_YN,OLIFE27_YN,OLIFE28_YN,OLIFE29_YN,OLIFE30_YN,
         OLIFE31_YN,OLIFE32_YN,OLIFE33_YN,OLIFE34_YN,OLIFE35_YN,OLIFE36_YN,OLIFE37_YN,OLIFE38_YN,OLIFE39_YN,OLIFE40_YN,
         OLIFE41_YN,OLIFE42_YN,OLIFE43_YN,
         OLIFE1_5P,OLIFE2_5P,OLIFE3_5P,OLIFE4_5P,OLIFE5_5P,OLIFE6_5P,OLIFE7_5P,OLIFE8_5P,OLIFE9_5P,OLIFE10_5P,
         OLIFE11_5P,OLIFE12_5P,OLIFE13_5P,OLIFE14_5P,OLIFE15_5P,OLIFE16_5P,OLIFE17_5P,OLIFE18_5P,OLIFE19_5P,OLIFE20_5P,
         OLIFE21_5P,OLIFE22_5P,OLIFE23_5P,OLIFE24_5P,OLIFE25_5P,OLIFE26_5P,OLIFE27_5P,OLIFE28_5P,OLIFE29_5P,OLIFE30_5P,
         OLIFE31_5P,OLIFE32_5P,OLIFE33_5P,OLIFE34_5P,OLIFE35_5P,OLIFE36_5P,OLIFE37_5P,OLIFE38_5P,OLIFE39_5P,OLIFE40_5P,
         OLIFE41_5P,OLIFE42_5P, OLIFE43_5P)
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
                OLIFE14_5P_reversed_IN = case_when(OLIFE14_5P == 0~4,OLIFE14_5P == 1~3,OLIFE14_5P == 3~1,OLIFE14_5P == 4~0,TRUE~OLIFE14_5P),
                OLIFE12_YN_reversed_IN = case_when(OLIFE12_YN == 1~0,OLIFE12_YN == 0~1,TRUE~OLIFE12_YN),
                OLIFE12_5P_reversed_IN = case_when(OLIFE12_5P == 0~4,OLIFE12_5P == 1~3,OLIFE12_5P == 3~1,OLIFE12_5P == 4~0,TRUE~OLIFE12_5P))
OLIFE_raw <- OLIFE_raw%>%
  dplyr::mutate(UE_YN = (OLIFE1_YN + OLIFE5_YN + OLIFE8_YN + OLIFE11_YN + OLIFE20_YN + OLIFE21_YN + 
                           OLIFE28_YN + OLIFE30_YN + OLIFE35_YN + OLIFE37_YN + OLIFE40_YN + OLIFE41_YN),
                CD_YN = (OLIFE2_YN + OLIFE15_YN + OLIFE22_YN + OLIFE24_YN + OLIFE25_YN + OLIFE26_YN + 
                           OLIFE29_YN + OLIFE34_YN + OLIFE36_YN + OLIFE38_YN + OLIFE39_YN),
                IA_YN = (OLIFE3_YN + OLIFE4_YN_reversed_IA + OLIFE7_YN + OLIFE17_YN + OLIFE18_YN +
                         OLIFE23_YN_reversed_IA + OLIFE27_YN_reversed_IA + OLIFE31_YN_reversed_IA + OLIFE32_YN_reversed_IA + OLIFE33_YN),
                IN_YN = (OLIFE6_YN + OLIFE9_YN + OLIFE10_YN + OLIFE12_YN_reversed_IN + OLIFE13_YN + 
                         OLIFE14_YN_reversed_IN  + OLIFE16_YN + OLIFE19_YN + OLIFE42_YN + OLIFE43_YN_reversed_IN))%>%
  dplyr::mutate(UE_5P = (OLIFE1_5P + OLIFE5_5P + OLIFE8_5P + OLIFE11_5P + OLIFE20_5P + OLIFE21_5P + 
                         OLIFE28_5P + OLIFE30_5P + OLIFE35_5P + OLIFE37_5P + OLIFE40_5P + OLIFE41_5P),
                CD_5P = (OLIFE2_5P + OLIFE15_5P + OLIFE22_5P + OLIFE24_5P + OLIFE25_5P + OLIFE26_5P + 
                         OLIFE29_5P + OLIFE34_5P + OLIFE36_5P + OLIFE38_5P + OLIFE39_5P),
                IA_5P = (OLIFE3_5P + OLIFE4_5P_reversed_IA + OLIFE7_5P + OLIFE17_5P + OLIFE18_5P +
                         OLIFE23_5P_reversed_IA + OLIFE27_5P_reversed_IA + OLIFE31_5P_reversed_IA + OLIFE32_5P_reversed_IA + OLIFE33_5P),
                IN_5P = (OLIFE6_5P + OLIFE9_5P + OLIFE10_5P + OLIFE12_5P_reversed_IN + OLIFE13_5P + 
                         OLIFE14_5P_reversed_IN  + OLIFE16_5P + OLIFE19_5P + OLIFE42_5P + OLIFE43_5P_reversed_IN))
#UE_YN, UE_5P 1,5,8,11,20,21,28,30,35,37,40,41
#CD_YN, CD_5P 2,15,22,24,25,26,29,34,36,38,39
#IA_YN, IA_5P 3,4R,7,17,18,23R,27R,31R,32R,33
#IN_YN, IN_5P 6,9,10,12R,13,14R,16,19,42,43R

#OLIFE_5P_Y_SUM
OLIFE_raw <- OLIFE_raw%>%
  dplyr::mutate(OL1Y5P = case_when(is.na(OLIFE1_5P)~0,OLIFE1_YN == 1 ~ OLIFE1_5P, OLIFE1_YN == 0 ~ 0),
         OL2Y5P = case_when(is.na(OLIFE2_5P)~0,OLIFE2_YN == 1 ~ OLIFE2_5P, OLIFE2_YN == 0 ~ 0),
         OL3Y5P = case_when(is.na(OLIFE3_5P)~0,OLIFE3_YN == 1 ~ OLIFE3_5P, OLIFE3_YN == 0 ~ 0),
         OL4Y5P = case_when(is.na(OLIFE4_5P)~0,OLIFE4_YN_reversed_IA == 1 ~ OLIFE4_5P_reversed_IA, OLIFE4_YN_reversed_IA == 0 ~ 0),
         OL5Y5P = case_when(is.na(OLIFE5_5P)~0,OLIFE5_YN == 1 ~ OLIFE5_5P, OLIFE5_YN == 0 ~ 0),
         OL6Y5P = case_when(is.na(OLIFE6_5P)~0,OLIFE6_YN == 1 ~ OLIFE6_5P, OLIFE6_YN == 0 ~ 0),
         OL7Y5P = case_when(is.na(OLIFE7_5P)~0,OLIFE7_YN == 1 ~ OLIFE7_5P, OLIFE7_YN == 0 ~ 0),
         OL8Y5P = case_when(is.na(OLIFE8_5P)~0,OLIFE8_YN == 1 ~ OLIFE8_5P, OLIFE8_YN == 0 ~ 0),
         OL9Y5P = case_when(is.na(OLIFE9_5P)~0,OLIFE9_YN == 1 ~ OLIFE9_5P, OLIFE9_YN == 0 ~ 0),
         OL10Y5P = case_when(is.na(OLIFE10_5P)~0,OLIFE10_YN == 1 ~ OLIFE10_5P, OLIFE10_YN == 0 ~ 0),
         OL11Y5P = case_when(is.na(OLIFE11_5P)~0,OLIFE11_YN == 1 ~ OLIFE11_5P, OLIFE11_YN == 0 ~ 0),
         OL12Y5P = case_when(is.na(OLIFE12_5P)~0,OLIFE12_YN_reversed_IN == 1 ~ OLIFE12_5P_reversed_IN, OLIFE12_YN_reversed_IN == 0 ~ 0),
         OL13Y5P = case_when(is.na(OLIFE13_5P)~0,OLIFE13_YN == 1 ~ OLIFE13_5P, OLIFE13_YN == 0 ~ 0),
         OL14Y5P = case_when(is.na(OLIFE14_5P)~0,OLIFE14_YN_reversed_IN == 1 ~ OLIFE14_5P_reversed_IN, OLIFE14_YN_reversed_IN == 0 ~ 0),
         OL15Y5P = case_when(is.na(OLIFE15_5P)~0,OLIFE15_YN == 1 ~ OLIFE15_5P, OLIFE15_YN == 0 ~ 0),
         OL16Y5P = case_when(is.na(OLIFE16_5P)~0, OLIFE16_YN == 1 ~ OLIFE16_5P, OLIFE16_YN == 0 ~ 0),
         OL17Y5P = case_when(is.na(OLIFE17_5P)~0,OLIFE17_YN == 1 ~ OLIFE17_5P, OLIFE17_YN == 0 ~ 0),
         OL18Y5P = case_when(is.na(OLIFE18_5P)~0,OLIFE18_YN == 1 ~ OLIFE18_5P, OLIFE18_YN == 0 ~ 0),
         OL19Y5P = case_when(is.na(OLIFE19_5P)~0,OLIFE19_YN == 1 ~ OLIFE19_5P, OLIFE19_YN == 0 ~ 0),
         OL20Y5P = case_when(is.na(OLIFE20_5P)~0,OLIFE20_YN == 1 ~ OLIFE20_5P, OLIFE20_YN == 0 ~ 0),
         OL21Y5P = case_when(is.na(OLIFE21_5P)~0,OLIFE21_YN == 1 ~ OLIFE21_5P, OLIFE21_YN == 0 ~ 0),
         OL22Y5P = case_when(is.na(OLIFE22_5P)~0,OLIFE22_YN == 1 ~ OLIFE22_5P, OLIFE22_YN == 0 ~ 0),
         OL23Y5P = case_when(is.na(OLIFE23_5P)~0,OLIFE23_YN_reversed_IA == 1 ~ OLIFE23_5P_reversed_IA, OLIFE23_YN_reversed_IA == 0 ~ 0),
         OL24Y5P = case_when(is.na(OLIFE24_5P)~0,OLIFE24_YN == 1 ~ OLIFE24_5P, OLIFE24_YN == 0 ~ 0),
         OL25Y5P = case_when(is.na(OLIFE25_5P)~0,OLIFE25_YN == 1 ~ OLIFE25_5P, OLIFE25_YN == 0 ~ 0),
         OL26Y5P = case_when(is.na(OLIFE26_5P)~0,OLIFE26_YN == 1 ~ OLIFE26_5P, OLIFE26_YN == 0 ~ 0),
         OL27Y5P = case_when(is.na(OLIFE27_5P)~0,OLIFE27_YN_reversed_IA == 1 ~ OLIFE27_5P_reversed_IA, OLIFE27_YN_reversed_IA == 0 ~ 0),
         OL28Y5P = case_when(is.na(OLIFE28_5P)~0,OLIFE28_YN == 1 ~ OLIFE28_5P, OLIFE28_YN == 0 ~ 0),
         OL29Y5P = case_when(is.na(OLIFE29_5P)~0,OLIFE29_YN == 1 ~ OLIFE29_5P, OLIFE29_YN == 0 ~ 0),
         OL30Y5P = case_when(is.na(OLIFE30_5P)~0,OLIFE30_YN == 1 ~ OLIFE30_5P, OLIFE30_YN == 0 ~ 0),
         OL31Y5P = case_when(is.na(OLIFE31_5P)~0,OLIFE31_YN_reversed_IA == 1 ~ OLIFE31_5P_reversed_IA, OLIFE31_YN_reversed_IA == 0 ~ 0),
         OL32Y5P = case_when(is.na(OLIFE32_5P)~0,OLIFE32_YN_reversed_IA == 1 ~ OLIFE32_5P_reversed_IA, OLIFE32_YN_reversed_IA == 0 ~ 0),
         OL33Y5P = case_when(is.na(OLIFE33_5P)~0,OLIFE33_YN == 1 ~ OLIFE33_5P, OLIFE33_YN == 0 ~ 0),
         OL34Y5P = case_when(is.na(OLIFE34_5P)~0,OLIFE34_YN == 1 ~ OLIFE34_5P, OLIFE34_YN == 0 ~ 0),
         OL35Y5P = case_when(is.na(OLIFE35_5P)~0,OLIFE35_YN == 1 ~ OLIFE35_5P, OLIFE35_YN == 0 ~ 0),
         OL36Y5P = case_when(is.na(OLIFE36_5P)~0,OLIFE36_YN == 1 ~ OLIFE36_5P, OLIFE36_YN == 0 ~ 0),
         OL37Y5P = case_when(is.na(OLIFE37_5P)~0,OLIFE37_YN == 1 ~ OLIFE37_5P, OLIFE37_YN == 0 ~ 0),
         OL38Y5P = case_when(is.na(OLIFE38_5P)~0,OLIFE38_YN == 1 ~ OLIFE38_5P, OLIFE38_YN == 0 ~ 0),
         OL39Y5P = case_when(is.na(OLIFE39_5P)~0,OLIFE39_YN == 1 ~ OLIFE39_5P, OLIFE39_YN == 0 ~ 0),
         OL40Y5P = case_when(is.na(OLIFE40_5P)~0,OLIFE40_YN == 1 ~ OLIFE40_5P, OLIFE40_YN == 0 ~ 0),
         OL41Y5P = case_when(is.na(OLIFE41_5P)~0,is.na(OLIFE16_5P)~0,OLIFE41_YN == 1 ~ OLIFE41_5P, OLIFE41_YN == 0 ~ 0),
         OL42Y5P = case_when(is.na(OLIFE42_5P)~0,OLIFE42_YN == 1 ~ OLIFE42_5P, OLIFE42_YN == 0 ~ 0),
         OL43Y5P = case_when(is.na(OLIFE43_5P)~0,OLIFE43_YN_reversed_IN == 1 ~ OLIFE43_5P_reversed_IN, OLIFE43_YN_reversed_IN == 0 ~ 0))%>%
  dplyr::mutate(OLY5P_SUM = (OL1Y5P + OL2Y5P + OL3Y5P + OL4Y5P + OL5Y5P + 
                      OL6Y5P + OL7Y5P + OL8Y5P + OL9Y5P + OL10Y5P+
                      OL11Y5P + OL12Y5P + OL13Y5P + OL14Y5P + OL15Y5P + 
                      OL16Y5P + OL17Y5P + OL18Y5P + OL19Y5P + OL20Y5P +
                      OL21Y5P + OL22Y5P + OL23Y5P + OL24Y5P + OL25Y5P + 
                      OL26Y5P + OL27Y5P + OL28Y5P + OL29Y5P + OL30Y5P +
                      OL31Y5P + OL32Y5P + OL33Y5P + OL34Y5P + OL35Y5P +
                      OL36Y5P + OL37Y5P + OL38Y5P + OL39Y5P + OL40Y5P + 
                      OL41Y5P + OL42Y5P + OL43Y5P))%>%
  dplyr::mutate(UE_5P_Y_SUM = (OL1Y5P + OL5Y5P + OL8Y5P + OL11Y5P + OL20Y5P + OL21Y5P + OL28Y5P + OL30Y5P + OL35Y5P + OL37Y5P + OL40Y5P + OL41Y5P),
                CD_5P_Y_SUM = (OL2Y5P + OL15Y5P + OL22Y5P + OL24Y5P + OL25Y5P + OL26Y5P + OL29Y5P + OL34Y5P + OL36Y5P + OL38Y5P + OL39Y5P),
                IA_5P_Y_SUM = (OL3Y5P + OL4Y5P + OL7Y5P + OL17Y5P + OL18Y5P + OL23Y5P + OL27Y5P + OL31Y5P + OL32Y5P + OL33Y5P),
                IN_5P_Y_SUM = (OL6Y5P + OL9Y5P + OL10Y5P + OL12Y5P + OL13Y5P + OL14Y5P + OL16Y5P + OL19Y5P + OL42Y5P + OL43Y5P))%>%
  dplyr::mutate(avg_UE_5P_Y = UE_5P_Y_SUM/UE_YN,#12
                avg_CD_5P_Y = CD_5P_Y_SUM/CD_YN,#11
                avg_IA_5P_Y = IA_5P_Y_SUM/IA_YN,#10
                avg_IN_5P_Y = IN_5P_Y_SUM/IN_YN)#10

#UE_5P_Y_SUM, CD_5P_Y_SUM, IA_5P_Y_SUM, IN_5P_Y_SUM
#avg_UE_5P_Y, avg_CD_5P_Y, avg_IA_5P_Y, avg_IN_5P_Y

#UE_YN, UE_5P 1,5,8,11,20,21,28,30,35,37,40,41 (12)
#CD_YN, CD_5P 2,15,22,24,25,26,29,34,36,38,39 (11)
#IA_YN, IA_5P 3,4R,7,17,18,23R,27R,31R,32R,33 (10)
#IN_YN, IN_5P 6,9,10,12R,13,14R,16,19,42,43R (10)

#OLIFE_5P_N_SUM
OLIFE_raw <- OLIFE_raw%>%
  mutate(OL1N5P = case_when(is.na(OLIFE1_5P)~0,OLIFE1_YN == 0 ~ OLIFE1_5P, OLIFE1_YN == 1 ~ 0),
         OL2N5P = case_when(is.na(OLIFE2_5P)~0,OLIFE2_YN == 0 ~ OLIFE2_5P, OLIFE2_YN == 1 ~ 0),
         OL3N5P = case_when(is.na(OLIFE3_5P)~0,OLIFE3_YN == 0 ~ OLIFE3_5P, OLIFE3_YN == 1 ~ 0),
         OL4N5P = case_when(is.na(OLIFE4_5P)~0,OLIFE4_YN_reversed_IA == 0 ~ OLIFE4_5P_reversed_IA, OLIFE4_YN_reversed_IA == 1 ~ 0),
         OL5N5P = case_when(is.na(OLIFE5_5P)~0,OLIFE5_YN == 0 ~ OLIFE5_5P, OLIFE5_YN == 1 ~ 0),
         OL6N5P = case_when(is.na(OLIFE6_5P)~0,OLIFE6_YN == 0 ~ OLIFE6_5P, OLIFE6_YN == 1 ~ 0),
         OL7N5P = case_when(is.na(OLIFE7_5P)~0,OLIFE7_YN == 0 ~ OLIFE7_5P, OLIFE7_YN == 1 ~ 0),
         OL8N5P = case_when(is.na(OLIFE8_5P)~0,OLIFE8_YN == 0 ~ OLIFE8_5P, OLIFE8_YN == 1 ~ 0),
         OL9N5P = case_when(is.na(OLIFE9_5P)~0,OLIFE9_YN == 0 ~ OLIFE9_5P, OLIFE9_YN == 1 ~ 0),
         OL10N5P = case_when(is.na(OLIFE10_5P)~0,OLIFE10_YN == 0 ~ OLIFE10_5P, OLIFE10_YN == 1 ~ 0),
         OL11N5P = case_when(is.na(OLIFE11_5P)~0,OLIFE11_YN == 0 ~ OLIFE11_5P, OLIFE11_YN == 1 ~ 0),
         OL12N5P = case_when(is.na(OLIFE12_5P)~0,OLIFE12_YN_reversed_IN == 0 ~ OLIFE12_5P_reversed_IN, OLIFE12_YN_reversed_IN == 1 ~ 0),
         OL13N5P = case_when(is.na(OLIFE13_5P)~0,OLIFE13_YN == 0 ~ OLIFE13_5P, OLIFE13_YN == 1 ~ 0),
         OL14N5P = case_when(is.na(OLIFE14_5P)~0,OLIFE14_YN_reversed_IN == 0 ~ OLIFE14_5P_reversed_IN, OLIFE14_YN_reversed_IN == 1 ~ 0),
         OL15N5P = case_when(is.na(OLIFE15_5P)~0,OLIFE15_YN == 0 ~ OLIFE15_5P, OLIFE15_YN == 1 ~ 0),
         OL16N5P = case_when(is.na(OLIFE16_5P)~0, OLIFE16_YN == 0 ~ OLIFE16_5P, OLIFE16_YN == 1 ~ 0),
         OL17N5P = case_when(is.na(OLIFE17_5P)~0,OLIFE17_YN == 0 ~ OLIFE17_5P, OLIFE17_YN == 1 ~ 0),
         OL18N5P = case_when(is.na(OLIFE18_5P)~0,OLIFE18_YN == 0 ~ OLIFE18_5P, OLIFE18_YN == 1 ~ 0),
         OL19N5P = case_when(is.na(OLIFE19_5P)~0,OLIFE19_YN == 0 ~ OLIFE19_5P, OLIFE19_YN == 1 ~ 0),
         OL20N5P = case_when(is.na(OLIFE20_5P)~0,OLIFE20_YN == 0 ~ OLIFE20_5P, OLIFE20_YN == 1 ~ 0),
         OL21N5P = case_when(is.na(OLIFE21_5P)~0,OLIFE21_YN == 0 ~ OLIFE21_5P, OLIFE21_YN == 1 ~ 0),
         OL22N5P = case_when(is.na(OLIFE22_5P)~0,OLIFE22_YN == 0 ~ OLIFE22_5P, OLIFE22_YN == 1 ~ 0),
         OL23N5P = case_when(is.na(OLIFE23_5P)~0,OLIFE23_YN_reversed_IA == 0 ~ OLIFE23_5P_reversed_IA, OLIFE23_YN_reversed_IA == 1 ~ 0),
         OL24N5P = case_when(is.na(OLIFE24_5P)~0,OLIFE24_YN == 0 ~ OLIFE24_5P, OLIFE24_YN == 1 ~ 0),
         OL25N5P = case_when(is.na(OLIFE25_5P)~0,OLIFE25_YN == 0 ~ OLIFE25_5P, OLIFE25_YN == 1 ~ 0),
         OL26N5P = case_when(is.na(OLIFE26_5P)~0,OLIFE26_YN == 0 ~ OLIFE26_5P, OLIFE26_YN == 1 ~ 0),
         OL27N5P = case_when(is.na(OLIFE27_5P)~0,OLIFE27_YN_reversed_IA == 0 ~ OLIFE27_5P_reversed_IA, OLIFE27_YN_reversed_IA == 1 ~ 0),
         OL28N5P = case_when(is.na(OLIFE28_5P)~0,OLIFE28_YN == 0 ~ OLIFE28_5P, OLIFE28_YN == 1 ~ 0),
         OL29N5P = case_when(is.na(OLIFE29_5P)~0,OLIFE29_YN == 0 ~ OLIFE29_5P, OLIFE29_YN == 1 ~ 0),
         OL30N5P = case_when(is.na(OLIFE30_5P)~0,OLIFE30_YN == 0 ~ OLIFE30_5P, OLIFE30_YN == 1 ~ 0),
         OL31N5P = case_when(is.na(OLIFE31_5P)~0,OLIFE31_YN_reversed_IA == 0 ~ OLIFE31_5P_reversed_IA, OLIFE31_YN_reversed_IA == 1 ~ 0),
         OL32N5P = case_when(is.na(OLIFE32_5P)~0,OLIFE32_YN_reversed_IA == 0 ~ OLIFE32_5P_reversed_IA, OLIFE32_YN_reversed_IA == 1 ~ 0),
         OL33N5P = case_when(is.na(OLIFE33_5P)~0,OLIFE33_YN == 0 ~ OLIFE33_5P, OLIFE33_YN == 1 ~ 0),
         OL34N5P = case_when(is.na(OLIFE34_5P)~0,OLIFE34_YN == 0 ~ OLIFE34_5P, OLIFE34_YN == 1 ~ 0),
         OL35N5P = case_when(is.na(OLIFE35_5P)~0,OLIFE35_YN == 0 ~ OLIFE35_5P, OLIFE35_YN == 1 ~ 0),
         OL36N5P = case_when(is.na(OLIFE36_5P)~0,OLIFE36_YN == 0 ~ OLIFE36_5P, OLIFE36_YN == 1 ~ 0),
         OL37N5P = case_when(is.na(OLIFE37_5P)~0,OLIFE37_YN == 0 ~ OLIFE37_5P, OLIFE37_YN == 1 ~ 0),
         OL38N5P = case_when(is.na(OLIFE38_5P)~0,OLIFE38_YN == 0 ~ OLIFE38_5P, OLIFE38_YN == 1 ~ 0),
         OL39N5P = case_when(is.na(OLIFE39_5P)~0,OLIFE39_YN == 0 ~ OLIFE39_5P, OLIFE39_YN == 1 ~ 0),
         OL40N5P = case_when(is.na(OLIFE40_5P)~0,OLIFE40_YN == 0 ~ OLIFE40_5P, OLIFE40_YN == 1 ~ 0),
         OL41N5P = case_when(is.na(OLIFE41_5P)~0,OLIFE41_YN == 0 ~ OLIFE41_5P, OLIFE41_YN == 1 ~ 0),
         OL42N5P = case_when(is.na(OLIFE42_5P)~0,OLIFE42_YN == 0 ~ OLIFE42_5P, OLIFE42_YN == 1 ~ 0),
         OL43N5P = case_when(is.na(OLIFE43_5P)~0,OLIFE43_YN_reversed_IN == 0 ~ OLIFE43_5P_reversed_IN, OLIFE43_YN_reversed_IN == 1 ~ 0))%>%
  mutate(OLN5P_SUM = (OL1N5P + OL2N5P + OL3N5P + OL4N5P + OL5N5P + 
                      OL6N5P + OL7N5P + OL8N5P + OL9N5P + OL10N5P+
                      OL11N5P + OL12N5P + OL13N5P + OL14N5P + OL15N5P + 
                      OL16N5P + OL17N5P + OL18N5P + OL19N5P + OL20N5P +
                      OL21N5P + OL22N5P + OL23N5P + OL24N5P + OL25N5P + 
                      OL26N5P + OL27N5P + OL28N5P + OL29N5P + OL30N5P +
                      OL31N5P + OL32N5P + OL33N5P + OL34N5P + OL35N5P +
                      OL36N5P + OL37N5P + OL38N5P + OL39N5P + OL40N5P + 
                      OL41N5P + OL42N5P + OL43N5P))%>%
  dplyr::mutate(UE_5P_N_SUM = (OL1N5P + OL5N5P + OL8N5P + OL11N5P + OL20N5P + OL21N5P + OL28N5P + OL30N5P + OL35N5P + OL37N5P + OL40N5P + OL41N5P),
                CD_5P_N_SUM = (OL2N5P + OL15N5P + OL22N5P + OL24N5P + OL25N5P + OL26N5P + OL29N5P + OL34N5P + OL36N5P + OL38N5P + OL39N5P),
                IA_5P_N_SUM = (OL3N5P + OL4N5P + OL7N5P + OL17N5P + OL18N5P + OL23N5P + OL27N5P + OL31N5P + OL32N5P + OL33N5P),
                IN_5P_N_SUM = (OL6N5P + OL9N5P + OL10N5P + OL12N5P + OL13N5P + OL14N5P + OL16N5P + OL19N5P + OL42N5P + OL43N5P))%>%
  dplyr::mutate(avg_UE_5P_N = UE_5P_N_SUM/(12-UE_YN),#12
                avg_CD_5P_N = CD_5P_N_SUM/(11-CD_YN),#11
                avg_IA_5P_N = IA_5P_N_SUM/(10-IA_YN),#10
                avg_IN_5P_N = IN_5P_N_SUM/(10-IN_YN))#10  
#UE_5P_N_SUM, CD_5P_N_SUM, IA_5P_N_SUM, IN_5P_N_SUM
#avg_UE_5P_N, avg_CD_5P_N, avg_IA_5P_N, avg_IN_5P_N

#UE_YN, UE_5P 1,5,8,11,20,21,28,30,35,37,40,41 (12)
#CD_YN, CD_5P 2,15,22,24,25,26,29,34,36,38,39 (11)
#IA_YN, IA_5P 3,4R,7,17,18,23R,27R,31R,32R,33 (10)
#IN_YN, IN_5P 6,9,10,12R,13,14R,16,19,42,43R (10)

#'''

#OLIFE_raw <- OLIFE_raw%>%
#  mutate(OL1Y5P = case_when(is.na(OLIFE1_5P)~0,OLIFE1_YN == 1 ~ OLIFE1_5P, OLIFE1_YN == 0 ~ 0),
#         OL2Y5P = case_when(is.na(OLIFE2_5P)~0,OLIFE2_YN == 1 ~ OLIFE2_5P, OLIFE2_YN == 0 ~ 0),
#         OL3Y5P = case_when(is.na(OLIFE3_5P)~0,OLIFE3_YN == 1 ~ OLIFE3_5P, OLIFE3_YN == 0 ~ 0),
#         OL4Y5P = case_when(is.na(OLIFE4_5P)~0,OLIFE4_YN == 1 ~ OLIFE4_5P, OLIFE4_YN == 0 ~ 0),
#         OL5Y5P = case_when(is.na(OLIFE5_5P)~0,OLIFE5_YN == 1 ~ OLIFE5_5P, OLIFE5_YN == 0 ~ 0),
#         OL6Y5P = case_when(is.na(OLIFE6_5P)~0,OLIFE6_YN == 1 ~ OLIFE6_5P, OLIFE6_YN == 0 ~ 0),
#         OL7Y5P = case_when(is.na(OLIFE7_5P)~0,OLIFE7_YN == 1 ~ OLIFE7_5P, OLIFE7_YN == 0 ~ 0),
#         OL8Y5P = case_when(is.na(OLIFE8_5P)~0,OLIFE8_YN == 1 ~ OLIFE8_5P, OLIFE8_YN == 0 ~ 0),
#         OL9Y5P = case_when(is.na(OLIFE9_5P)~0,OLIFE9_YN == 1 ~ OLIFE9_5P, OLIFE9_YN == 0 ~ 0),
#         OL10Y5P = case_when(is.na(OLIFE10_5P)~0,OLIFE10_YN == 1 ~ OLIFE10_5P, OLIFE10_YN == 0 ~ 0),
#         OL11Y5P = case_when(is.na(OLIFE11_5P)~0,OLIFE11_YN == 1 ~ OLIFE11_5P, OLIFE11_YN == 0 ~ 0),
#         OL12Y5P = case_when(is.na(OLIFE12_5P)~0,OLIFE12_YN == 1 ~ OLIFE12_5P, OLIFE12_YN == 0 ~ 0),
#         OL13Y5P = case_when(is.na(OLIFE13_5P)~0,OLIFE13_YN == 1 ~ OLIFE13_5P, OLIFE13_YN == 0 ~ 0),
#         OL14Y5P = case_when(is.na(OLIFE14_5P)~0,OLIFE14_YN == 1 ~ OLIFE14_5P, OLIFE14_YN == 0 ~ 0),
#         OL15Y5P = case_when(is.na(OLIFE15_5P)~0,OLIFE15_YN == 1 ~ OLIFE15_5P, OLIFE15_YN == 0 ~ 0),
#         OL16Y5P = case_when(is.na(OLIFE16_5P)~0,OLIFE16_YN == 1 ~ OLIFE16_5P, OLIFE16_YN == 0 ~ 0),
#         OL17Y5P = case_when(is.na(OLIFE17_5P)~0,OLIFE17_YN == 1 ~ OLIFE17_5P, OLIFE17_YN == 0 ~ 0),
#         OL18Y5P = case_when(is.na(OLIFE18_5P)~0,OLIFE18_YN == 1 ~ OLIFE18_5P, OLIFE18_YN == 0 ~ 0),
#         OL19Y5P = case_when(is.na(OLIFE19_5P)~0,OLIFE19_YN == 1 ~ OLIFE19_5P, OLIFE19_YN == 0 ~ 0),
#         OL20Y5P = case_when(is.na(OLIFE20_5P)~0,OLIFE20_YN == 1 ~ OLIFE20_5P, OLIFE20_YN == 0 ~ 0),
#         OL21Y5P = case_when(is.na(OLIFE21_5P)~0,OLIFE21_YN == 1 ~ OLIFE21_5P, OLIFE21_YN == 0 ~ 0),
#         OL22Y5P = case_when(is.na(OLIFE22_5P)~0,OLIFE22_YN == 1 ~ OLIFE22_5P, OLIFE22_YN == 0 ~ 0),
#         OL23Y5P = case_when(is.na(OLIFE23_5P)~0,OLIFE23_YN == 1 ~ OLIFE23_5P, OLIFE23_YN == 0 ~ 0),
#         OL24Y5P = case_when(is.na(OLIFE24_5P)~0,OLIFE24_YN == 1 ~ OLIFE24_5P, OLIFE24_YN == 0 ~ 0),
#         OL25Y5P = case_when(is.na(OLIFE25_5P)~0,OLIFE25_YN == 1 ~ OLIFE25_5P, OLIFE25_YN == 0 ~ 0),
#         OL26Y5P = case_when(is.na(OLIFE26_5P)~0,OLIFE26_YN == 1 ~ OLIFE26_5P, OLIFE26_YN == 0 ~ 0),
#         OL27Y5P = case_when(is.na(OLIFE27_5P)~0,OLIFE27_YN == 1 ~ OLIFE27_5P, OLIFE27_YN == 0 ~ 0),
#         OL28Y5P = case_when(is.na(OLIFE28_5P)~0,OLIFE28_YN == 1 ~ OLIFE28_5P, OLIFE28_YN == 0 ~ 0),
#         OL29Y5P = case_when(is.na(OLIFE29_5P)~0,OLIFE29_YN == 1 ~ OLIFE29_5P, OLIFE29_YN == 0 ~ 0),
#         OL30Y5P = case_when(is.na(OLIFE30_5P)~0,OLIFE30_YN == 1 ~ OLIFE30_5P, OLIFE30_YN == 0 ~ 0),
#         OL31Y5P = case_when(is.na(OLIFE31_5P)~0,OLIFE31_YN == 1 ~ OLIFE31_5P, OLIFE31_YN == 0 ~ 0),
#         OL32Y5P = case_when(is.na(OLIFE32_5P)~0,OLIFE32_YN == 1 ~ OLIFE32_5P, OLIFE32_YN == 0 ~ 0),
#         OL33Y5P = case_when(is.na(OLIFE33_5P)~0,OLIFE33_YN == 1 ~ OLIFE33_5P, OLIFE33_YN == 0 ~ 0),
#         OL34Y5P = case_when(is.na(OLIFE34_5P)~0,OLIFE34_YN == 1 ~ OLIFE34_5P, OLIFE34_YN == 0 ~ 0),
#         OL35Y5P = case_when(is.na(OLIFE35_5P)~0,OLIFE35_YN == 1 ~ OLIFE35_5P, OLIFE35_YN == 0 ~ 0),
#         OL36Y5P = case_when(is.na(OLIFE36_5P)~0,OLIFE36_YN == 1 ~ OLIFE36_5P, OLIFE36_YN == 0 ~ 0),
#         OL37Y5P = case_when(is.na(OLIFE37_5P)~0,OLIFE37_YN == 1 ~ OLIFE37_5P, OLIFE37_YN == 0 ~ 0),
#         OL38Y5P = case_when(is.na(OLIFE38_5P)~0,OLIFE38_YN == 1 ~ OLIFE38_5P, OLIFE38_YN == 0 ~ 0),
#         OL39Y5P = case_when(is.na(OLIFE39_5P)~0,OLIFE39_YN == 1 ~ OLIFE39_5P, OLIFE39_YN == 0 ~ 0),
#         OL40Y5P = case_when(is.na(OLIFE40_5P)~0,OLIFE40_YN == 1 ~ OLIFE40_5P, OLIFE40_YN == 0 ~ 0),
#         OL41Y5P = case_when(is.na(OLIFE41_5P)~0,OLIFE41_YN == 1 ~ OLIFE41_5P, OLIFE41_YN == 0 ~ 0),
#         OL42Y5P = case_when(is.na(OLIFE42_5P)~0,OLIFE42_YN == 1 ~ OLIFE42_5P, OLIFE42_YN == 0 ~ 0),
#         OL43Y5P = case_when(is.na(OLIFE43_5P)~0,OLIFE43_YN == 1 ~ OLIFE43_5P, OLIFE43_YN == 0 ~ 0))%>%
#  mutate(OLY5P_SUM = (OL1Y5P + OL2Y5P + OL3Y5P + OL4Y5P + OL5Y5P + 
#                        OL6Y5P + OL7Y5P + OL8Y5P + OL9Y5P + OL10Y5P+
#                        OL11Y5P + OL12Y5P + OL13Y5P + OL14Y5P + OL15Y5P + 
#                        OL16Y5P + OL17Y5P + OL18Y5P + OL19Y5P + OL20Y5P +
#                        OL21Y5P + OL22Y5P + OL23Y5P + OL24Y5P + OL25Y5P + 
#                        OL26Y5P + OL27Y5P + OL28Y5P + OL29Y5P + OL30Y5P +
#                        OL31Y5P + OL32Y5P + OL33Y5P + OL34Y5P + OL35Y5P +
#                        OL36Y5P + OL37Y5P + OL38Y5P + OL39Y5P + OL40Y5P + 
#                        OL41Y5P + OL42Y5P + OL43Y5P))
#'''


Dass_mod <- Dass_raw%>%
  select(PartNo, Age, Gender, CounterCode, dassDEPRESSION, dassANXIETY, dassSTRESS)
SPQ_mod <- SPQ_raw%>%
  select(PartNo, Age, Gender, CounterCode, spqCOGPERCEP, spqINTERPERSON, spqDISORGANISED, spqTOTAL)
SSOH_mod <- SSOH_raw%>%
  select(PartNo,Age,Gender,CounterCode, avgSSOSH)
OLIFE_mod <- OLIFE_raw%>%
  select(PartNo,Age,Gender, CounterCode,OLY5P_SUM, OLN5P_SUM,
         IA_YN, UE_YN, CD_YN, IN_YN,
         IA_5P_N_SUM, UE_5P_N_SUM, CD_5P_N_SUM, IN_5P_N_SUM,
         IA_5P_Y_SUM, UE_5P_Y_SUM, CD_5P_Y_SUM, IN_5P_Y_SUM,
         avg_IA_5P_N, avg_UE_5P_N, avg_CD_5P_N, avg_IN_5P_N,
         avg_IA_5P_Y, avg_UE_5P_Y, avg_CD_5P_Y, avg_IN_5P_Y)
MainDF <- left_join(Dass_mod, SPQ_mod)
MainDF <- left_join(MainDF, SSOH_mod)
MainDF <- left_join(MainDF, OLIFE_mod)

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

p1 <- MainDF%>%
  summarise(IA_N = mean(avg_IA_5P_N, na.rm = TRUE),
            IN_N = mean(avg_IN_5P_N, na.rm = TRUE),
            CD_N = mean(avg_CD_5P_N, na.rm = TRUE),
            UE_N = mean(avg_UE_5P_N, na.rm = TRUE))
p1b <- transpose(p1)
p1b <- p1b%>%
  mutate(Row_Num = row_number(), colourColumn = 'Ns')%>%
  rename(Values = V1)


p2 <- MainDF%>%
  summarise(IA_Y = mean(avg_IA_5P_Y, na.rm = TRUE),
            IN_Y = mean(avg_IN_5P_Y, na.rm = TRUE),
            CD_Y = mean(avg_CD_5P_Y, na.rm = TRUE),
            UE_Y = mean(avg_UE_5P_Y, na.rm = TRUE))
p2b <- transpose(p2)
p2b <- p2b%>%
  mutate(Row_Num = row_number(), colourColumn = 'Ys')%>%
  rename(Values = V1)

P <- ggplot(p1b, aes(x = Row_Num, y = Values, colour = colourColumn))+
  geom_line()+
  geom_point()

P + geom_line(data = p2b, aes(x = Row_Num, y = Values, colour = colourColumn))+
    geom_point(data = p2b, aes(x = Row_Num, y = Values, colour = colourColumn))+
    scale_x_continuous(labels = c('IA','IN','CD','UE'))+
    scale_y_continuous(limits = c(0,3))


