#### post stratification of the sample

## cf tuto here: http://www.andrew.cmu.edu/user/jsmurray/teaching/303/files/lab.html
## https://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
## http://sdaza.com/survey/2012/08/25/raking/


rm(list = ls())
## Load the form

mainDir <- getwd()
## Load all required packages
source(paste0(mainDir,"/code/0-config.R"))
source(paste0(mainDir,"/code/0-packages.R"))
library(koboloadeR)


### Load the data
cat("\n\n Loading data. It is assumed that the cleaning, weighting & re-encoding has been done previously \n")


household <- read.csv("data/household.csv", encoding="UTF-8", na.strings="NA")
CaseInformation <- read.csv("data/CaseInformation.csv", encoding="UTF-8", na.strings="NA")
IndividaulBioData <- read.csv("data/IndividaulBioData.csv", encoding="UTF-8", na.strings="NA")
InformationNotRegFamilies <- read.csv("data/InformationNotRegFamilies.csv", encoding="UTF-8", na.strings="NA")

data <- household
names(household)
library(survey)

#The survey package provides a survey.design object, which is a container for a dataset and the
# sampling design information, including sampling scheme, weights, population sizes (and more).

# The svydesign function is used to create survey.design objects.
#It has a number of arguments, but the most important for you are:

###  ids: Name of variable in the dataframe that contains cluster ids
##  ids = ~1 means there is no clustering.

###  strata: Names of stratification variables, as a formula: ~var1 + var2 + var3
## strata = NULL means there was no stratification.

## weights	: Formula or vector specifying sampling weights as an alternative to prob
# probs: Formula or data frame specifying cluster sampling probabilities

###  fpc (finite population correction) : A vector the same length as the data, giving the stratum population size for each observation.
##The name is confusing, since you don’t actually supply the finite population correction factor.
## fpc = rep(N, n): The function call rep(N, n) generates a vector of length n where each entry is
## N (the population size).

## “Independent sampling design” means that the sampling design is an SRS - Stratified Random Sample.
## When the population size is specified (via the fpc argument) it is assumed that the SRS is without replacement.

###  data: Dataframe containing the raw survey data
## data = dat tells svydesign where to find the actual data.

#################################################################
## First load the universe
##loading  case profile from progres
progrescase <- read_csv("data/progrescase-1.csv")

universe <- progrescase[progrescase$CountryAsylum %in% c("JOR"), ]
N <- nrow(universe)
n <- nrow(data)

rm(progrescase)


######################################################################
## Doing poststratification
## We will build 2 stratum - corresponding to the 2 dependent variable to intention as per the chi square test.
## Area of asylum
## Family Size

## Relative frequencies for each of these levels from the population data frames
#names(universe)

## Now Area of Asylum
# We need first to recategorise Gov
#levels(as.factor(universe$coal1Cat))

data$key <- data$HouseholdInformation.Governorate
#levels(as.factor(data$key))
coal1cat <- levels(as.factor(data$key))
universe$COA_L1 <- as.character(universe$coal1)
universe$key <- as.character(universe$coal1)

universe$key[universe$key == "Tafiela"] <- "Tafileh"
universe$key[universe$key == "Maan"] <- "Ma’an"

levels(as.factor(universe$COA_L1))
universe$COA_L1[!(universe$key %in% coal1cat)] <- "Other"

## Delte records with wrong admin 1

universe$COA_L1[universe$COA_L1 == "Tafiela"] <- "Tafileh"
universe$COA_L1[universe$COA_L1 == "Maan"] <- "Ma’an"
levels(as.factor(universe$COA_L1))

universe <- universe[universe$COA_L1 %in% coal1cat, ]



universe.COA_L1 <- as.data.frame(table(universe$COA_L1))
names(universe.COA_L1)[1] <- "COA_L1"

universe.key <- as.data.frame(table(universe$key))
names(universe.key)[1] <- "key"
write.csv(universe.key, "out/universekey.csv", row.names=FALSE)

cat("create the unweighted survey object\n")
## create the unweighted survey object
data.svy.unweighted <- svydesign(ids =  ~ 1,
                                 data = data)

## Post stratify on those relative frequency
cat("post stratification on ctr and on area of Origin\n")
## Try post stratification on ctr and on area of Origin
data.svy.rake.coa <- rake(
  design = data.svy.unweighted,
  #sample.margins = list( ~ ctr,  ~ COO_L1),
  #population.margins = list(universe.ctr, universe.COO_L1)
  sample.margins = list( ~ key),
  population.margins = list(universe.key)
)

cat("Trim weight for area of Asylumn\n")
data.svy.rake.trim <- trimWeights(data.svy.rake.coa,
                                  lower = 0.3,
                                  upper = 3,
                                  strict = TRUE)

### Add Case size stata

#data$Case.size2 <- data$size
#levels(as.factor(data$Case.size2))
#prop.table(table(data$Case.size, useNA = "ifany"))
#check <- data[is.na(data$Case.size2),]

#data$Case.size <- car::recode(data$Case.size2,"'Case.size.1'='Case.size.1';
#                                 'Case.size.2'='Case.size.2';
#                                 'Case.size.3'='Case.size.3.to.5';
#                                 'Case.size.4'='Case.size.3.to.5';
#                                 'Case.size.5'='Case.size.3.to.5';
#                                 'Case.size.over.5'='Case.size.6.and.more';
#                                 'Case.size.over.5'='Case.size.6.and.more'")
#universe$Case.size2 <- universe$Case.size
#universe$Case.size <- car::recode(universe$Case.size,"'Case.size.1'='Case.size.1';
#                                 'Case.size.2'='Case.size.2';
#                                 'Case.size.3'='Case.size.3.to.5';
#                                 'Case.size.4'='Case.size.3.to.5';
#                                 'Case.size.5'='Case.size.3.to.5';
#                                 'Case.size.6'='Case.size.6.and.more';
#                                 'Case.size.7.and.more'='Case.size.6.and.more'")#


#data$key2 <- paste(data$ctr,data$COO_L1,data$Case.size,sep="-")
#universe$key2 <- paste(universe$ctr,universe$COO_L1,universe$Case.size,sep="-")

#prop.table(table(data$key2, useNA = "ifany"))
#prop.table(table(universe$key2, useNA = "ifany"))
#universe.key2 <- as.data.frame(table(universe$key2))
#names(universe.key2)[1] <- "key2"
#data.key2 <- as.data.frame(table(data$key2))
cat("post stratification on area of Origin & case size\n")
## Try post stratification on ctr, area of Origin & case size
data.svy.rake.coa.size <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ key2),
  population.margins = list(universe.key2)
)
cat("Trim weight for Asylum area & Case size\n")
data.svy.rake.trim2 <- trimWeights(data.svy.rake.coa.size,
                                  lower = 0.3,
                                  upper = 3,
                                  strict = TRUE)



###################################################
### Compile a table to show impact of wieighting approach

cat("Compile a matrix of comparison between weighting\n")
compare.weight <- t(
  cbind(
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.unweighted,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.coa,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.coa.size,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.trim,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.trim2,
      FUN = svymean
    )))


