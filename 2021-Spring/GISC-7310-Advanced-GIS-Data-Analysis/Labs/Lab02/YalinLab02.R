getwd()

setwd('G:\\UTD_Classes\\2020Spring\\GISC7310_AdvancedDataAnalysis\\Lab02')
concord <-  data.frame(foreign::read.spss('Concord1.sav'))

model.1 <- lm(water81~income+water80+educat,data = concord)
summary(model.1)

model.2 <- lm(water81~income+water80,data = concord)
summary(model.2)
sum(r1 <- residuals(model.2))
model.3 <- lm(educat~income+water80,data = concord)
sum(r2 <- residuals(model.3))




scatterplot(r1~r2, xlab=bquote(hat(bold(e))[" Education | Income,Water80"]),       
            ylab=bquote(hat(bold(e))[" Water81| Income,Water80"]))

summary(lm(r1~r2))

province <- foreign::read.dbf('PROVINCES.DBF')
province$TOTFERTRAT
car::scatterplotMatrix(~TOTFERTRAT+ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM+REGION , data=province)

model.1 <- lm(TOTFERTRAT~ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, data=province)
summary(model.1)

col_lst <-  c('TOTFERTRAT','ILLITERRAT','FEMMARAGE','DIVORCERAT','TELEPERFAM')
province_new <- province[,col_lst]
province_scale <- as.data.frame(scale(province_new))
model.2 <- lm(TOTFERTRAT~-1+ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM, data=province_scale)
summary(model.2)

library (coefplot)                                                      # needs to be downloaded
coefplot(model.2,xlab = 'Coefficient',ylab = 'Variables',decreasing = TRUE,sort = "magnitude")

model.combine <- lm(cbind(TOTFERTRAT,ILLITERRAT,FEMMARAGE,DIVORCERAT,TELEPERFAM)~REGION,data=province)
summary(model.combine)

model.3 <- lm(TOTFERTRAT~+ILLITERRAT+FEMMARAGE+DIVORCERAT+TELEPERFAM+REGION, data=province)
summary(model.3)
anova(model.1,model.3)
a$adj.r.squared
?get
load('ModelSpecs.Rdata')
a <- summary(mod1)

summary_model <- function(mod){
  
  full.model <- lm(y~g*x, data=mod)
  Intercept.model <- lm(y~g+x, data=mod)
  slope.model <- lm(y~g:x, data=mod)
  means.model <- lm(y~g,data=mod)
  plain.model <- lm(y~x, data=mod)
  model_lst <- list(full.model,Intercept.model,slope.model,means.model,plain.model)
  model_summary <- lapply(model_lst, summary)
  r_square_lst <- function(a) {return(a$adj.r.squared)}
  model_summary[[6]] <- as.data.frame(lapply(model_summary, r_square_lst), 
    col.names = c('full.model', 'Intercept.model','slope.model','means.model','plain.model'))
  model_summary[[7]] <- model_lst
  return(model_summary)
}


car::scatterplot(y~x|g,smoother=F,boxplots="xy",data=mod1,main="Model_1")
mod1_summary <- summary_model(mod1)
mod1_summary
mod1_summary[[7]][[1]]
anova(mod1_summary[[7]][[3]],mod1_summary[[7]][[1]])


car::scatterplot(y~x|g,smoother=F,boxplots="xy",data=mod2,main="Model_2")
mod1_summary <- summary_model(mod2)
mod1_summary[[6]]
mod1_summary
anova(mod1_summary[[7]][[1]],mod1_summary[[7]][[2]])

car::scatterplot(y~x|g,smoother=F,boxplots="xy",data=mod3,main="Model_3")
mod1_summary <- summary_model(mod3)
mod1_summary[[6]]
mod1_summary
anova(mod1_summary[[7]][[1]],mod1_summary[[7]][[4]])

car::scatterplot(y~x|g,smoother=F,boxplots="xy",data=mod5,main="Model_5")
mod1_summary <- summary_model(mod5)
mod1_summary[[6]]
mod1_summary
anova(mod1_summary[[7]][[3]],mod1_summary[[7]][[1]])
anova(mod1_summary[[7]][[2]],mod1_summary[[7]][[1]])

car::scatterplot(y~x|g,smoother=F,boxplots="xy",data=mod6,main="Model_6")
mod1_summary <- summary_model(mod6)
mod1_summary[[6]]
mod1_summary
anova(mod1_summary[[7]][[3]],mod1_summary[[7]][[1]])
anova(mod1_summary[[7]][[2]],mod1_summary[[7]][[1]])
