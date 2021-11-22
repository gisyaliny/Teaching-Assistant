##
## Working with count data of nominal/ordinal scaled variables
##

data(UCBAdmissions, package = "datasets")

UCBAdmissions                                   # Data come as cross-tabulation
ftable(UCBAdmissions)                           # reformat table into a "flat" table

Admiss <- as.data.frame(UCBAdmissions)          # Convert table into aggregated dataframe with "Freq" counts


##
## Work with simple pairwise cross-tabulations
##
AdmissByGen <- xtabs(Freq ~  Admit + Gender, data=Admiss)


## Make table with row percentages
AdmissByGenPropMat <- prop.table(AdmissByGen,2)
round(AdmissByGenPropMat,2)

##
## Redo pairwise cross-tabulations for males and females
##

## Males
AdmissMale <- Admiss[Admiss$Gender=="Male", ]
AdmissByDeptMale <- xtabs(Freq ~  Admit + Dept, data=AdmissMale)

round(prop.table(AdmissByDeptMale,2), 2)


## Females
AdmissFem <- Admiss[Admiss$Gender=="Female", ]
AdmissByDeptFem <- xtabs(Freq ~  Admit + Dept, data=AdmissFem)

round(prop.table(AdmissByDeptFem,2),2)

