# Cleaning data :)

                                   
# 1.  Load data 

library(readr)
library(readxl)
library(dplyr)


                #Inventory data
CFI1970 <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/HWFCFI1970Treev1-1.xlsx")
CFI1981 <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/HWFCFI1981Treev1-1.xlsx")
CFI1991 <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/HWFCFI1991Treev1-1.xlsx")
CFI2001 <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/HWFCFI2001Treev1-1.xlsx")
CFI2011 <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/HWFCFI2011Treev1-1.xlsx")
CFI2021 <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/HWFCFI2021Treev1-1.xlsx")
               #Code of the species to match 
Eq <- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/SpeciesToMatch.xlsx")

#names(CFI1970)
                #View(Eq)
                #View(CFI1970)


                                          
                                            # 1970 #

# 2. Remove not neccesary data and colunms 

CFI1970 <- CFI1970 %>%
  select(-MERCHCL, -SAWHT, -DOBSHT_70, -TOTHT, -YRS_70, -HTMEASURE, -REMEAS_70) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(HISTORY %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%  
  mutate(
    DBHcm = DBH * 2.54,
    Year = "1970"
  )


CFI1970 <- CFI1970 %>%
  mutate(
    PLOT = sub("^1+", "", PLOT),  
    PLOT_SIZE = as.numeric(PLOT_SIZE),         # Convertir PLOT_SIZE a numérico
    PlotSize = case_when(
      PLOT_SIZE == 20 ~ "POLE",
      PLOT_SIZE == 5  ~ "SAW",
      TRUE ~ NA_character_
    )
  )




# 3. as numeric :) 

CFI1970$SPECIES <- as.numeric(CFI1970$SPECIES)

# 4. Join 

CFI1970 <- CFI1970 %>%
  left_join(Eq%>% select(Code, Equation), by = c("SPECIES" = "Code"))


View(CFI1970)

# 5. RENAME 


CFI1970 <- dplyr::rename(CFI1970, Species = SPECIES)


CFI1970 <- CFI1970 %>%
  select(-PLOT_SIZE)


# 5. Save the data :) 

write.csv(CFI1970, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI1970.csv", row.names = FALSE)


View(CFI1970)

#############



# 1980 x2 #
names(CFI1981)

# 2. Remove not neccesary data and colunms 

CFI1981 <- CFI1981 %>%
  select(-DEAD_76, -MERCHCL, -SAWHT, -BOLEHT4, -CROWNCL, -DISEASE_81, -VIGOR, -HARVEST_81, -QUAD, -YRS_81, -HTMEASURE, -REMEAS_81, -CUT_YR_81, -TYP_CUT_81) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(HISTORY %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%  
  mutate(
    DBHcm = DBH * 2.54,
    Year = "1980"
  )


CFI1981 <- CFI1981 %>%
  mutate(
    PLOT= sub("^1+", "", PLOT),  
    PLOT_SIZE = as.numeric(PLOT_SIZE),         # Convertir PLOT_SIZE a numérico
    PlotSize = case_when(
      PLOT_SIZE == 20 ~ "POLE",
      PLOT_SIZE == 5  ~ "SAW",
      TRUE ~ NA_character_
    )
  )




# 3. as numeric :) 

CFI1981$SPECIES <- as.numeric(CFI1981$SPECIES)

# 4. Join 

CFI1981 <- CFI1981 %>%
  left_join(Eq%>% select(Code, Equation), by = c("SPECIES" = "Code"))


View(CFI1981)

# 5. RENAME 

CFI1981 <- dplyr::rename(CFI1981, Plot = PLOT)

View(CFI1981)

# 5. Save the data :) 

write.csv(CFI1981, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI1981.csv", row.names = FALSE)
















#############









                                        # 1981 #

View(CFI1981)
names(CFI1981)
# 2. Remove not neccesary data and colunms 

CFI1981 <- CFI1981 %>%
  select(-DEAD_76, -MERCHCL, -SAWHT, -BOLEHT4, -CROWNCL, -DISEASE_81, -VIGOR, -HARVEST_81, -QUAD, -YRS_81, -HTMEASURE, -REMEAS_81, -CUT_YR_81, -TYP_CUT_81) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(HISTORY %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%
  mutate(
    DBHcm = DBH * 2.54,
    Year = "1981"
  )


CFI1981 <- CFI1981 %>%
  mutate(
    Plot = as.numeric(Plot)^1,  # Elevar a la 1 no cambia el valor, pero asegura que sea numérico
    PLOT_SIZE = as.numeric(PLOT_SIZE),
    PlotSize = case_when(
      PLOT_SIZE == 20 ~ "POLE",
      PLOT_SIZE == 5  ~ "SAW",
      TRUE ~ NA_character_
    )
  )



# 3. as numeric :) 

CFI1981$SPECIES <- as.numeric(CFI1981$SPECIES)

# 4. Join 

CFI1981 <- CFI1981 %>%
  left_join(Eq%>% select(Code, Equation), by = c("SPECIES" = "Code"))


View(CFI1981)

# 5. RENAME 


CFI1981 <- dplyr::rename(CFI1981, Plot = PLOT)






# 5. Save the data :) 

write.csv(CFI1981, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI1981.csv", row.names = FALSE)




                                          # 1991 #

View(CFI1991)
names(CFI1991)

# 1. RENAME 


CFI1991 <- dplyr::rename(CFI1991, Plot = PLOT)

# 2. Remove not neccesary data and colunms 

CFI1991 <- CFI1991 %>%
  select(-GRIDX, -GRIDY, -MERCHCL, -HTMEASURE, -TOTHT, -BOLEHT4, -SAWHT, -CROWNCL, -VIGOR, -AZIMUTH, -H_DIST, -NOTES_TREE, -DESCRIPTION, -YRS_91, -REMEAS_91 ) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(HISTORY %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%
  mutate(
    DBHcm = DBH * 2.54,
    Year = "1991"
  )

  
CFI1991 <- CFI1991 %>%
  mutate(
    Plot = as.numeric(Plot)^1,  
    PLOT_SIZE = as.numeric(PLOT_SIZE),
    PlotSize = case_when(
      PLOT_SIZE == 20 ~ "POLE",
      PLOT_SIZE == 5  ~ "SAW",
      TRUE ~ NA_character_
    )
  )


# 3. as numeric :) 

CFI1991$SPECIES <- as.numeric(CFI1991$SPECIES)

# 3. Join 

CFI1991 <- CFI1991 %>%
  left_join(Eq%>% select(Code, Equation), by = c("SPECIES" = "Code"))


View(CFI1991)

# 4. Save the data :) 

write.csv(CFI1991, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI1991.csv", row.names = FALSE)

 
                                            # 2001 #
View(CFI2001)
names(CFI2001)

#Renanem some columns xdxd 

names(CFI2001)[names(CFI2001) == "DBH_01"] <- "DBH"
names(CFI2001)[names(CFI2001) == "HIST_01"] <- "HISTORY"
names(CFI2001)[names(CFI2001) == "PlotSize"] <- "PLOTSIZE"



# 2. Remove not neccesary data and colunms 

CFI2001 <- CFI2001 %>%
  select(-LET_LIN, -NUM_LIN, -MEASURE_01, -TOTHT_01, -BOLEHT4_01, -SAWHT_01, -MCLASS_01, -CROWN_01, -VIGOR_01, -AZIMUTH, -DIST_FEET, -TRNOTES_01, -DESCRIP_01, -YRS_01, -REMEAS_01) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(HISTORY %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%
  mutate(
    DBHcm = DBH * 2.54,
    Year = "2001"
  )


CFI2001 <- CFI2001 %>%
  mutate(
    PLOT = sub("^0+", "", PLOT),  
    PLOT_SIZE = as.numeric(PLOT_SIZE),         # Convertir PLOT_SIZE a numérico
    PlotSize = case_when(
      PLOT_SIZE == 20 ~ "POLE",
      PLOT_SIZE == 5  ~ "SAW",
      TRUE ~ NA_character_
    )
  )


View(CFI2001)


# 3. as numeric :) 

CFI2001$SPECIES <- as.numeric(CFI2001$SPECIES)

# 4. Join 

CFI2001 <- CFI2001 %>%
  left_join(Eq%>% select(Code, Equation), by = c("SPECIES" = "Code"))


View(CFI2001)

# 5. Save the data :) 

write.csv(CFI2001, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI2001.csv", row.names = FALSE)


                                            # 2011 #
names(CFI2011)


# 2. Remove not neccesary data and colunms 

CFI2011 <- CFI2011 %>%
  select(-SawHt,-Measure,-TotHt,-BoleHt, -MerchCl, -CrownCl, -Vigor, -Azi, -HDist, -TreeNotes, -Desc) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(TrHist %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%
  mutate(
    DBHcm = DBH * 2.54,
    Year = "2011"
  )

View(CFI2011)
# 3. as numeric :) 

CFI2011$Species <- as.numeric(CFI2011$Species)

# 4. Join 

CFI2011 <- CFI2011 %>%
  left_join(Eq%>% select(Code, Equation), by = c("Species" = "Code"))


View(CFI2011)

# 5. Save the data :) 

write.csv(CFI2011, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI2011.csv", row.names = FALSE)
 



                                           # 2021 #
names(CFI2021)

# 1. Rename :) 

CFI2021 <- CFI2021 %>%
  rename(
    DBHcm = `DBH cm`  # si la columna original tiene espacio
  )

# 2. Remove not neccesary data and colunms 

CFI2021 <- CFI2021 %>%
  select(-`B^0`,-`B^1`,-CF,-SawHt, -Measure, -NewTree, -TotHt, -BoleHt, -MerchCl, -CrownCl, -Vigor,-Azi,-HDist, -TreeNotes,-Description) %>%
  filter(
    !is.na(DBH),
    DBH != 0,
    !(TrHist %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  ) %>%
  mutate(
    DBHcm = DBH * 2.54,
    Year = "2011"
  )

View(CFI2021)
# 3. as numeric :) 

CFI2021$Species <- as.numeric(CFI2021$Species)

# 4. Join 

CFI2011 <- CFI2011 %>%
  left_join(Eq%>% select(Code, Equation), by = c("Species" = "Code"))


View(CFI2011)

# 5. Save the data :) 

write.csv(CFI2011, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI2021.csv", row.names = FALSE)





CFI1970_2<- read_excel("C:/Users/vanco/Desktop/ResearchR/Research/Raw_data/CFI1970Excel.xlsx")

                                                # 1970 # Take 2

CFI1970_2 <- CFI1970_2 %>%
    filter(
    !is.na(DBH),
    DBH != 0,
    !(TrHist %in% c("36", "37", "38", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "80", "81"))
  )


# 2. Save the data :) 

write.csv(CFI1970_2, file = "C:/Users/vanco/Desktop/ResearchR/Research/Process_data/CFI1970_2.csv", row.names = FALSE)



