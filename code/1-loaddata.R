rm(list = ls())


################################################################
## Load all required packages

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-packages.R"))
source(paste0(mainDir,"/code/0-config.R"))

### Double Check that you have the last version
#source("https://raw.githubusercontent.com/Edouard-Legoupil/koboloadeR/master/inst/script/install_github.R")
#install.packages("devtools")
#library("devtools")
#install_github("Edouard-Legoupil/koboloadeR")

library(koboloadeR)

## kobo_projectinit()

##############################################
## Load form


cat("\n\n Build dictionnary from the xlsform \n")

#rm(form)
#form <- "form.xls"
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
rm(form)


#################################################################################################################################
## Load all frames
#################################################################################################################################
library(readr)
household <- read_csv("data/BaslineFirst2017.csv")
CaseInformation <- read_csv("data/BaslineFirst2017_CaseInformation.csv")
IndividaulBioData <- read_csv("data/BaslineFirst2017_IndividaulBioData.csv")
InformationNotRegFamilies <- read_csv("data/BaslineFirst2017_InformationNotRegFamilies.csv")

##################################################################
###### Restore links between frame
## household - CaseInformation
## household - CaseInformation - IndividaulBioData

#################################################################################################################################
## Household
## fEW CHECK
#names(household)
#table(household$case_reachable-reachable)
# nrow(as.data.frame(unique(household$meta-instanceID)))
# nrow(as.data.frame(unique(household$KEY)))

cat("\n\nCheck Household\n")
household1 <- household
datalabel <- as.data.frame( names(household))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
names(household) <- datalabel[, 2]

## Remove rows for "not reachable"
#table(household[ ,8])
#str(household)
#names(household)
#str(household$case_reachable.reachable)
#levels(as.factor(household$VolunteerInformation.CaseStatus))
household <- household[which(household$VolunteerInformation.CaseStatus == "Available"), ]
# names(household)



#################################################################################################################################
## Case

cat("\n\nCheck cases\n")
CaseInformation1 <- CaseInformation
datalabel <- as.data.frame( names(CaseInformation))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
#datalabel$namenew<- paste("section2.CaseInformation.", datalabel$namenew, sep="")
names(CaseInformation) <- datalabel[, 2]

## merge
#names(CaseInformation)
#levels(as.factor(household$SET.OF.section2.CaseInformation))
#CaseInformation$SET.OF.CaseInformation <- CaseInformation$section2.CaseInformation.SET.OF.CaseInformation
CaseInformation <- join(y = household, x = CaseInformation, by="SET.OF.CaseInformation", type="left")

#################################################################################################################################
## Bio Data
# names(IndividaulBioData)
cat("\n\nCheck individuals\n")
IndividaulBioData2 <- IndividaulBioData
datalabel <- as.data.frame( names(IndividaulBioData))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
names(IndividaulBioData) <- datalabel[, 2]

#names(CaseInformation)
#names(IndividaulBioData)
#names(IndividaulBioData2)
#IndividaulBioData$section2.case_number_details.SET.OF.IndividaulBioData <- IndividaulBioData$section2.case_number_details.CaseInformation.IndividaulBioData.SET.OF.IndividaulBioData
IndividaulBioData <- join(y= CaseInformation, x = IndividaulBioData, by="SET.OF.IndividaulBioData", type="left")
#names(IndividaulBioData)
#levels(as.factor(IndividaulBioData$section7.communication.social_media))

#################################################################################################################################
## InformationNotRegFamilies

cat("\n\nCheck InformationNotRegFamilies\n")
InformationNotRegFamilies1 <- InformationNotRegFamilies
datalabel <- as.data.frame( names(InformationNotRegFamilies))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
datalabel$namenew <- str_replace_all(datalabel$nameor, "-", ".")
#datalabel$namenew<- paste("section2.InformationNotRegFamilies.", datalabel$namenew, sep="")
names(InformationNotRegFamilies) <- datalabel[, 2]

## merge
#names(InformationNotRegFamilies)
#levels(as.factor(household$SET.OF.section2.InformationNotRegFamilies))
#InformationNotRegFamilies$SET.OF.InformationNotRegFamilies <- InformationNotRegFamilies$section2.InformationNotRegFamilies.SET.OF.InformationNotRegFamilies
InformationNotRegFamilies <- join(y = household, x = InformationNotRegFamilies, by="SET.OF.InformationNotRegFamilies", type="left")

###################################################################################
##### Re-encode correctly the dataset
cat("\n\n\nNow re-encode data and label variables \n\n\n\n")


cat("\n\n\n Household \n\n\n\n")
# household1 <- kobo_split_multiple(household, dico)
household <- kobo_split_multiple(household, dico)
household <- kobo_encode(household, dico)
household <- kobo_label(household , dico)


cat("\n\n\n Case \n\n\n\n")
CaseInformation <- kobo_split_multiple(CaseInformation, dico)
CaseInformation <- kobo_encode(CaseInformation, dico)
CaseInformation <- kobo_label(CaseInformation , dico)


cat("\n\n\n Individuals \n\n\n\n")
IndividaulBioData <- kobo_split_multiple(IndividaulBioData, dico)
IndividaulBioData <- kobo_encode(IndividaulBioData, dico)
IndividaulBioData <- kobo_label(IndividaulBioData , dico)


cat("\n\n\n Case \n\n\n\n")
InformationNotRegFamilies <- kobo_split_multiple(InformationNotRegFamilies, dico)
InformationNotRegFamilies <- kobo_encode(InformationNotRegFamilies, dico)
InformationNotRegFamilies <- kobo_label(InformationNotRegFamilies , dico)




#######################################################################
### Edit data-sets before creating indicators #########################
#######################################################################
## Create standard HH and Case identification variables

IndividaulBioData$HH_KEY <- as.character(lapply(strsplit(as.character(IndividaulBioData$PARENT_KEY), split = "/"), "[", 1))
IndividaulBioData$Case_Key <- IndividaulBioData$PARENT_KEY

InformationNotRegFamilies$count <- rownames(InformationNotRegFamilies)
InformationNotRegFamilies$HH_KEY <- InformationNotRegFamilies$PARENT_KEY

# No case_key because they're not part of a registered case!
CaseInformation$HH_KEY <- CaseInformation$PARENT_KEY
CaseInformation$Case_Key <- CaseInformation$KEY
household$HH_KEY <- household$KEY

## Merge key variables from HH data onto case data for mapping
FoodSecurityShared <- household$FoodSecurityHH
CopingStrategiesShared <- household$IstheHouseHoldProvetyCoping
ExpendituresShared <- household$FinancialSituation.AsHouseHoldExp
household$HH.TotalReg <- dcast(IndividaulBioData, IndividaulBioData$HH_KEY ~  (IndividaulBioData$IndiviualInformation.AgeOfIndividual  >= 0))[, c("TRUE")]
HH.TotalReg <- dcast(IndividaulBioData, IndividaulBioData$HH_KEY ~  (IndividaulBioData$IndiviualInformation.AgeOfIndividual  >= 0))[, c("TRUE")]
HH_KEY <- household$HH_KEY
Map <- data.frame(cbind(FoodSecurityShared, CopingStrategiesShared, ExpendituresShared, HH.TotalReg, HH_KEY))
CaseInformation <- join(CaseInformation, Map, by = 'HH_KEY', type = 'left', match = 'all')

## Create same sorting order for each data set
household <- household[order(household$HH_KEY),]
CaseInformation <- CaseInformation[order(CaseInformation$HH_KEY, CaseInformation$Case_Key),]
IndividaulBioData <- IndividaulBioData[order(IndividaulBioData$HH_KEY, IndividaulBioData$Case_Key),]


## Clean numeric variables with NAs
IndividaulBioData$IndiviualFinancialSituation.Wages.HowMuchProceedsFromHome[is.na(IndividaulBioData$IndiviualFinancialSituation.Wages.HowMuchProceedsFromHome)] <- 0
IndividaulBioData$IndiviualFinancialSituation.Wages.IrregularEmploymentAmount[is.na(IndividaulBioData$IndiviualFinancialSituation.Wages.IrregularEmploymentAmount)] <- 0

## IndividaulBioData$IndiviualFinancialSituation.Wages.RegularEmploymentAmount replaced by IndiviualFinancialSituation.Wages.RegularEmployment
IndividaulBioData$IndiviualFinancialSituation.Wages.RegularEmployment[is.na(IndividaulBioData$IndiviualFinancialSituation.Wages.RegularEmployment)] <- 0


############################################################
cat("\n\nWrite backup\n")

write.csv(household, "data/household.csv", row.names = FALSE)
write.csv(CaseInformation, "data/CaseInformation.csv", row.names = FALSE)
write.csv(IndividaulBioData , "data/IndividaulBioData.csv", row.names = FALSE)
write.csv(InformationNotRegFamilies, "data/InformationNotRegFamilies.csv", row.names = FALSE)

