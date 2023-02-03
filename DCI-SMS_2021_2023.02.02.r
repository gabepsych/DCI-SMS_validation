gc(rm(list = ls()))
library(haven)
library(dplyr)
library(sjlabelled)
library(corrplot)
library(psych)
library(knitr)
library(ggplot2)
library(lavaan)
library(car)
library(Hmisc)
library(xtable)
library(correlation)
library(sjstats)
library(semTools)
library(QuantPsyc)

# Data Preparation ####
data <- read_sav("DCI-SMS_cleaned_6.26.sav")

#Remove SPSS labels because they impede R functions
data <- remove_all_labels(data)
#Someone forgot to reverse code this one
data$GLRS8r <- recode(data$GLRS8, '1=7; 2=6; 3=5; 4=4')
#Verify sum scores are computed accurately
data <- data %>% mutate(GLSTOT = GLRS1r + GLRS2r + GLRS3 + GLRS4 + GLRS5 +
                                GLRS6 + GLRS7 + GLRS8r + GLRS9r + GLRS10 +
                                GLRS11 + GLRS12 + GLRS13 + GLRS14r + GLRS15 + GLRS16r)
data <- data %>% mutate(ConEngage = CRSI1 + CRSI5 + CRSI9 + CRSI13,
                        Stresscoms = DCISMS1 + DCISMS2,
                        sdcemots = DCISMS13 + DCISMS14,
                        sdcprobs = DCISMS16 + DCISMS20,
                        ndcs = DCISMS15 + DCISMS17 + DCISMS18 + DCISMS19,
                        Stresscomp = DCISMS11 + DCISMS12,
                        sdcemotp = DCISMS3 + DCISMS4,
                        sdcprobp = DCISMS6 + DCISMS9,
                        ndcp = DCISMS5 + DCISMS7 + DCISMS8 + DCISMS10,
                        emotcdc = DCISMS24 + DCISMS25,
                        probcdc = DCISMS21 + DCISMS22 + DCISMS23,
                        DCeval = DCISMS26 + DCISMS27,
                        DCtot = DCISMS1 + DCISMS2 + DCISMS3 + DCISMS4 + DCISMS5r +
                                DCISMS6 + DCISMS7r + DCISMS8r + DCISMS9 + 
                                DCISMS10r + DCISMS11 + DCISMS12 + DCISMS13 +
                                DCISMS14 + DCISMS15r + DCISMS16 + DCISMS17r +
                                DCISMS18r + DCISMS19r + DCISMS20 + DCISMS21 +
                                DCISMS22 + DCISMS23 + DCISMS24 + DCISMS25)



#Check frequency table for Relationship gender type
table(data$reltype_gender)

#Create new relationship gender type variable
#1 = Same-gender
#2 = Different-gender
#3 = Other relationship type
data$reltype_gender_1 <- ifelse(data$reltype_gender == 2, 1,
                                data$reltype_gender)
data$reltype_gender_1 <- ifelse(data$reltype_gender_1 == 3, 2,
                                data$reltype_gender_1)
data$reltype_gender_1 <- ifelse(data$reltype_gender_1 == 4, 3,
                                data$reltype_gender_1)

table(data$reltype_gender_1)

data$reltype_gender_1 <- factor(data$reltype_gender_1,
                          levels = c(1,2,3),
                          labels = c("Same-Gend", "Dif-Gend", "Other"))
table(data$reltype_gender_1)

#Isolate DCI-SMS items for multigroup CFA
data_DCISMS <- dplyr::select(data, ID, reltype_gender_1, DCISMS1:DCISMS27)
data_DCISMS <- dplyr::select(data_DCISMS, -DCISMS5r,
                      -DCISMS7r, -DCISMS8r, -DCISMS10r,
                      -DCISMS15r, -ATTCHK1, -DCISMS17r,
                      -DCISMS18r,-DCISMS19r)

#Create a dataset for CFA
table(data_DCISMS$reltype_gender_1)

data_DCISMS <- data_DCISMS %>% filter(reltype_gender_1 == "Same-Gend" |
                                          reltype_gender_1 == "Dif-Gend")

data_DCISMS$reltype_gender_1 <- factor(data_DCISMS$reltype_gender_1,
                             levels = c("Same-Gend", "Dif-Gend"))

#Remove IDs that failed validity screenings####
# IDs that are invalid include
invalid_ids <- c(457,456,454,455,457,461,460,459, #Failed a screening item
                60,452, #Reported an asexual identity in screen but bisexual in demographics
                450,451) #These have no data available for the DCI
                #247,435,316,450,245,393) # These have missing data but will be retained because we are using FIML
invalid_ids <- as.numeric(invalid_ids)

`%notin%` <- Negate(`%in%`)
data_DCISMS_clean <- data_DCISMS[data_DCISMS$ID %notin% invalid_ids, ]

clean_ids <- dplyr::select(data_DCISMS_clean,ID)
data_FINAL_SAMPLE_w_baseline <- left_join(clean_ids, data, by = "ID")
table(data_DCISMS$reltype_gender_1)
table(data_DCISMS_clean$reltype_gender_1)

data_FINAL_SAMPLE_w_baseline %>% filter(reltype_gender_1 == "Same-Gend") %>% count(gender)


write.csv(data_FINAL_SAMPLE_w_baseline, "data_DCISMS_8.28.2022.csv")
write_sav(data_FINAL_SAMPLE_w_baseline, "data_DCISMS_8.28.2022.sav")

#Check missing data
data_noNA <- na.omit(data_DCISMS_clean)
nrow(data_DCISMS_clean) - nrow(data_noNA)
mean(is.na(data_DCISMS_clean))

rows_w_missing <- data_DCISMS_clean[rowSums(is.na(data_DCISMS_clean))>0,]
mean(is.na(rows_w_missing))

mean(rowSums(is.na(rows_w_missing)))/29

#Isolate DCI-SMS subscales, GLSTOT, and ConEngage for correlation table
data_corr <- dplyr::select(data[data$ID %notin% invalid_ids, ],reltype_gender_1,
                    Stresscoms:DCeval, DCtot,
                    GLSTOT, ConEngage)

data_same <- filter(data_DCISMS_clean, reltype_gender_1 == "Same-Gend")
data_dif <- filter(data_DCISMS_clean, reltype_gender_1 == "Dif-Gend")

data_same_cor <- dplyr::select(data_same, DCISMS1:DCISMS27)
data_dif_cor <- dplyr::select(data_dif, DCISMS1:DCISMS27)

cor_same_items <- rcorr(as.matrix(data_same_cor))
cor_dif_items <- rcorr(as.matrix(data_dif_cor))

write.csv(cor_same_items$r, "corr.matrix.DCI.items.SAME.csv")
write.csv(cor_dif_items$r, "corr.matrix.DCI.items.DIFF.csv")


#Confirmatory Factor Analysis####

#Define models

self.model <- '
Stresscom.s =~ DCISMS1 + DCISMS2
Emotsupp.s =~ DCISMS13 + DCISMS14
Probsupp.s =~ DCISMS16 + DCISMS20
NegDC.s =~ DCISMS15 + DCISMS17 + DCISMS18 + DCISMS19
'

partner.model <- '
Stresscom.p =~ DCISMS11 + DCISMS12
Emotsupp.p =~ DCISMS3 + DCISMS4
Probsupp.p =~ DCISMS6 + DCISMS9
NegDC.p =~ DCISMS5 + DCISMS7 + DCISMS8 + DCISMS10
'

cdc.model <-'
emot.cdc =~ DCISMS24 + DCISMS25
prob.cdc =~ DCISMS21 + DCISMS22 + DCISMS23
'

#Full DCI-SMS (11 factors)
full.model <- '
Stresscom.s =~ DCISMS1 + DCISMS2
Emotsupp.s =~ DCISMS13 + DCISMS14
Probsupp.s =~ DCISMS16 + DCISMS20
NegDC.s =~ DCISMS15 + DCISMS17 + DCISMS18 + DCISMS19

Stresscom.p =~ DCISMS11 + DCISMS12
Emotsupp.p =~ DCISMS3 + DCISMS4
Probsupp.p =~ DCISMS6 + DCISMS9
NegDC.p =~ DCISMS5 + DCISMS7 + DCISMS8 + DCISMS10

emot.cdc =~ DCISMS24 + DCISMS25
prob.cdc =~ DCISMS21 + DCISMS22 + DCISMS23

DC_eval =~ DCISMS26 + DCISMS27
'

d <- data_DCISMS_clean
same <- data_same
dif <- data_dif

#Tests of multivariate non-normality
QuantPsyc::mult.norm(same[,c(-1,-2)])
QuantPsyc::mult.norm(dif[,c(-1,-2)])


#DC by oneself (Same-gender)
self.same.fit <- cfa(model = self.model,
                   data = same,
                   meanstructure = TRUE,
                estimator = "MLR",
                missing = "FIML")
summary(self.same.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)


#Fit is acceptable

#DC by oneself (Diff-gender)
self.dif.fit <- cfa(model = self.model,
                     data = dif,
                     meanstructure = TRUE,
                    estimator = "MLR",
                    missing = "FIML")
summary(self.dif.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#Fit is good

#DC by ones partner (Same-gender)
partner.same.fit <- cfa(model = partner.model,
                data = same,
                meanstructure = TRUE,
                estimator = "MLR",
                missing = "FIML")
summary(partner.same.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#Fit is acceptable. RMSEA is kind of high
modindices(partner.same.fit, sort. = TRUE)

#DC by ones partner (Diff-gender)
partner.dif.fit <- cfa(model = partner.model,
                        data = dif,
                        meanstructure = TRUE,
                       estimator = "MLR",
                       missing = "FIML")
summary(partner.dif.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#Fit is acceptable
modindices(partner.dif.fit, sort. = TRUE)

#Common dyadic coping (Same-gender)
cdc.same.fit <- cfa(model = cdc.model,
                   data = same,
                   meanstructure = TRUE,
                   estimator = "MLR",
                   missing = "FIML")
summary(cdc.same.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#RMSEA is high... may be due to low sample size.
#Fit is acceptable

#Common dyadic coping (Dif-gender)
cdc.dif.fit <- cfa(model = cdc.model,
                    data = dif,
                    meanstructure = TRUE,
                   estimator = "MLR",
                   missing = "FIML")
summary(cdc.dif.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#Bad fit. RMSEA too high
#Chi-square/df ratio = 6.526
modindices(cdc.dif.fit, sort. = TRUE)

#Try correlating items to increase RMSEA
cdc.model.1 <-'
emot.cdc =~ DCISMS24 + DCISMS25
prob.cdc =~ DCISMS21 + DCISMS22 + DCISMS23
DCISMS22 ~~ DCISMS23
DCISMS21 ~~ DCISMS24
'


cdc.dif.fit.1 <- cfa(model = cdc.model.1,
               data = dif,
               meanstructure = TRUE,
               estimator = "MLR",
               missing = "FIML")
summary(cdc.dif.fit.1,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)


lavTestLRT(cdc.dif.fit.1, cdc.dif.fit)
#Model fit improved

#Therefore, fit is acceptable

#Run model again for Common DC Same-Gender with new model
#Common dyadic coping (Same-gender)
cdc.same.fit.1 <- cfa(model = cdc.model.1,
                    data = same,
                    meanstructure = TRUE,
                    estimator = "MLR",
                    missing = "FIML")
summary(cdc.same.fit.1,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)


lavTestLRT(cdc.same.fit.1, cdc.same.fit)
#Model fit improved...barely!

#Full Model (Same gender)
full.same.fit <- cfa(model = full.model,
               data = same,
               meanstructure = TRUE,
               estimator = "MLR",
               missing = "FIML")
#Kicked back an error
lavInspect(full.same.fit, "vcov")


# Full Model (same-gender)

summary(full.same.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#Fit is acceptable. CFI is borderline

#Full Model (different gender)

full.dif.fit <- cfa(model = full.model,
                data = dif,
                meanstructure = TRUE,
                estimator = "MLR",
                missing = "FIML")
summary(full.dif.fit,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)
#Fit is acceptable

# Multi-group analysis (Same-Gend/Dif-Gend) ####

#Self model ####

# Self-model - Configural ####
self.MI.fit.conf <- cfa(
        model = self.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML"
)
summary(self.MI.fit.conf, fit.measures = TRUE, standardized = TRUE)

# Self-model - Metric ####
self.MI.fit.metr <- cfa(
        model = self.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings")
)
summary(self.MI.fit.metr, fit.measures = TRUE, standardized = TRUE)

# Self-model - Scalar ####
self.MI.fit.scalr <- cfa(
        model = self.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts")
)
summary(self.MI.fit.scalr, fit.measures = TRUE, standardized = TRUE)

# Self-model - Strict ####
self.MI.fit.strict <- cfa(
        model = self.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts","residuals")
)
summary(self.MI.fit.strict, fit.measures = TRUE, standardized = TRUE)

self.compare <- compareFit(self.MI.fit.conf, 
                           self.MI.fit.metr,
                           self.MI.fit.scalr,
                           self.MI.fit.strict)
summary(self.compare)
        
#partner model ####

# partner-model - Configural ####
partner.MI.fit.conf <- cfa(
        model = partner.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML"
)
summary(partner.MI.fit.conf, fit.measures = TRUE, standardized = TRUE)

# partner-model - Metric ####
partner.MI.fit.metr <- cfa(
        model = partner.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings")
)
summary(partner.MI.fit.metr, fit.measures = TRUE, standardized = TRUE)

# partner-model - Scalar ####
partner.MI.fit.scalr <- cfa(
        model = partner.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts")
)
summary(partner.MI.fit.scalr, fit.measures = TRUE, standardized = TRUE)

# partner-model - Strict ####
partner.MI.fit.strict <- cfa(
        model = partner.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts","residuals")
)
summary(partner.MI.fit.strict, fit.measures = TRUE, standardized = TRUE)

partner.compare <- compareFit(partner.MI.fit.conf, 
                              partner.MI.fit.metr,
                              partner.MI.fit.scalr,
                              partner.MI.fit.strict)
summary(partner.compare)

#Common Dyadic Coping ####

# CDC-model - Configural ####
cdc.MI.fit.conf <- cfa(
        model = cdc.model.1,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML"
)
summary(cdc.MI.fit.conf, fit.measures = TRUE, standardized = TRUE)

# CDC-model - Metric ####
cdc.MI.fit.metr <- cfa(
        model = cdc.model.1,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings")
)
summary(cdc.MI.fit.metr, fit.measures = TRUE, standardized = TRUE)

# CDC-model - Scalar ####
cdc.MI.fit.scalr <- cfa(
        model = cdc.model.1,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts")
)
summary(cdc.MI.fit.scalr, fit.measures = TRUE, standardized = TRUE)

# CDC-model - Strict ####
cdc.MI.fit.strict <- cfa(
        model = cdc.model.1,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts","residuals")
)
summary(cdc.MI.fit.strict, fit.measures = TRUE, standardized = TRUE)

cdc.compare <- compareFit(cdc.MI.fit.conf, 
                              cdc.MI.fit.metr,
                              cdc.MI.fit.scalr,
                              cdc.MI.fit.strict)
summary(cdc.compare)


#Full Model (11 factors) ####

# full-model - Configural ####
full.MI.fit.conf <- cfa(
        model = full.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML"
)
summary(full.MI.fit.conf, fit.measures = TRUE, standardized = TRUE)

# full-model - Metric ####
full.MI.fit.metr <- cfa(
        model = full.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings")
)
summary(full.MI.fit.metr, fit.measures = TRUE, standardized = TRUE)

# full-model - Scalar ####
full.MI.fit.scalr <- cfa(
        model = full.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts")
)
summary(full.MI.fit.scalr, fit.measures = TRUE, standardized = TRUE)

# full-model - Strict ####
full.MI.fit.strict <- cfa(
        model = full.model,
        data = d,
        group = "reltype_gender_1",
        meanstructure = TRUE,
        estimator = "MLR",
        missing = "FIML",
        group.equal = c("loadings","intercepts","residuals")
)
summary(full.MI.fit.strict, fit.measures = TRUE, standardized = TRUE)

full.compare <- compareFit(full.MI.fit.conf, 
                              full.MI.fit.metr,
                              full.MI.fit.scalr,
                              full.MI.fit.strict)
summary(full.compare)


# Correlation between subscales and GLRS and ConEngage ####

#Same gender correlation matrix
data_corr_same <- filter(data_corr, reltype_gender_1 == "Same-Gend")
data_corr_same <- dplyr::select(data_corr_same, -reltype_gender_1)
data_corr_same <- as.matrix(data_corr_same)

cor_same <- rcorr(data_corr_same,
                 type = "spearman")
cor_same_rho <- cor_same$r
cor_same <- round(cor_same_rho, 2)
upper <- cor_same
upper[upper.tri(cor_same, diag = FALSE)] <- ""

# Diff gender correlation matrix
data_corr_dif <- filter(data_corr, reltype_gender_1 == "Dif-Gend")
data_corr_dif <- dplyr::select(data_corr_dif, -reltype_gender_1)
data_corr_dif <- as.matrix(data_corr_dif)

cor_dif <- rcorr(data_corr_dif,
                 type = "spearman")
cor_dif_rho <- cor_dif$r
cor_dif <- round(cor_dif_rho, 2)
lower <- cor_dif
lower[lower.tri(cor_dif, diag = FALSE)] <- ""

#Combine correlation matrix. Same-gend = Lower, Dif-Gend = Upper
lower[lower.tri(cor_dif, diag = TRUE)] <- upper[lower.tri(cor_same, diag = TRUE)]

both <- as.data.frame(lower)
print(xtable(both),type = "html", file = "cor.matrix.html")

#Isolate individual correlations

data_corr_same <- as.data.frame(data_corr_same)
data_corr_dif <- as.data.frame(data_corr_dif)

correlation(data_corr_same, method = "spearman", digits = 5, p_adjust = "none")

subscale_cor_same<-correlation(data_corr_same %>% dplyr::select(-GLSTOT, -ConEngage), method = "spearman", digits = 5, p_adjust = "none")
sum((subscale_cor_same[,8]<.05)) #total number of significant correlations among subscales 
length(subscale_cor_same[,8]) #total number of possible correlations

correlation(data_corr_dif, method = "spearman", digits = 5, p_adjust = "none")

subscale_cor_diff<-correlation(data_corr_dif %>% dplyr::select(-GLSTOT, -ConEngage), method = "spearman", digits = 5, p_adjust = "none")
sum((subscale_cor_diff[,8]<.05)) #total number of significant correlations among subscales 
length(subscale_cor_diff[,8]) #total number of possible 

nrow(data_same)
nrow(data_dif)


names_same <- c("Stresscoms.s", "sdcemots.s", "sdcprobs.s", "ndcs.s", "Stresscomp.s", 
            "sdcemotp.s", "sdcprobp.s", "ndcp.s", "emotcdc.s", "probcdc.s", "DCeval.s", 
            "DCtot.s", "GLSTOT.s", "ConEngage.s")
names_dif <- c("Stresscoms.d", "sdcemots.d", "sdcprobs.d", "ndcs.d", "Stresscomp.d", 
            "sdcemotp.d", "sdcprobp.d", "ndcp.d", "emotcdc.d", "probcdc.d", "DCeval.d", 
            "DCtot.d", "GLSTOT.d", "ConEngage.d")

x <- data_corr_same
names(x)<-names_same
y <- data_corr_dif
names(y)<-names_dif

z <- cbind(x,y)
correlation(z, method = "pearson", digits = 5, p_adjust = "none")


#Descriptive statistics
df1 <- psych::describe(x)
df2 <- psych::describe(y)


df1$mean

#T-tests #### 

w_same <- dplyr::filter(data_corr, reltype_gender_1 == "Same-Gend")
w_diff <- dplyr::filter(data_corr, reltype_gender_1 == "Dif-Gend")
names(w_same) <- paste(names(w_same), "s", sep = "_")
names(w_diff) <- paste(names(w_diff), "d", sep = "_")


# Screen for outliers (t-tests)


outliers_same <- list()
outliers_diff <- list()
for(i in 2:length(w_diff)){
        outliers_same[[i]] <- boxplot.stats(as.numeric(w_same[,i]), coef = 3)$out
        outliers_diff[[i]] <- boxplot.stats(as.numeric(w_diff[,i]), coef = 3)$out
}
outliers_same
outliers_diff

t.tests <- list()
t.test_table <- matrix(NA, length(w_same), 7)
for(i in 2:length(w_diff)){
  t.tests[[i]] <- t.test(
    w_same[,i], w_diff[,i]
  )
  t.test_table[i,1] <- t.tests[[i]][[1]]
  t.test_table[i,2] <- t.tests[[i]][[2]]
  t.test_table[i,3] <- t.tests[[i]][[3]]
  t.test_table[i,4] <- t.tests[[i]][[4]][1]
  t.test_table[i,5] <- t.tests[[i]][[4]][2]
  t.test_table[i,6] <- t.tests[[i]][[5]][1]
  t.test_table[i,7] <- t.tests[[i]][[5]][2]
}

t.test_table <- t.test_table[-1,]
t.test_table <- as.data.frame(t.test_table)
names(t.test_table) <- c("t","df","p","lower","upper","same_M","diff_M")
rownames(t.test_table) <- names(w_diff)[-1]

t.test_table
format(t.test_table[,3], scientific = F)


mann_u <- list()
mann_u_table <- matrix(NA, length(w_same), 2)
for(i in 2:length(w_diff)){
  mann_u[[i]] <- wilcox.test(
    as.numeric(w_same[,i]), as.numeric(w_diff[,i])
  )
  mann_u_table[i,1] <- mann_u[[i]][[1]]
  mann_u_table[i,2] <- mann_u[[i]][[3]]
}

mann_u_table <- mann_u_table[-1,]
mann_u_table <- as.data.frame(mann_u_table)
names(mann_u_table) <- c("u","p")
rownames(mann_u_table) <- names(w_diff)[-1]

mann_u_table
format(mann_u_table[,2], scientific = F)

#Alpha Reliabilities

matrix2 <- matrix(NA, 14, 3)
colnames(matrix2) <- c("Variable", "Alpha.Same", "Alpha.Dif")

get_alpha <- function(x){
        test <- psych::alpha(x)
        test$total[1,1]
}

#same gender
Stresscoms.s = dplyr::select(data_same, DCISMS1, DCISMS2)
sdcemots.s = dplyr::select(data_same, DCISMS13, DCISMS14)
sdcprobs.s = dplyr::select(data_same, DCISMS16, DCISMS20)
ndcs.s = dplyr::select(data_same, DCISMS15, DCISMS17, DCISMS18, DCISMS19)
Stresscomp.s = dplyr::select(data_same, DCISMS11, DCISMS12)
sdcemotp.s = dplyr::select(data_same, DCISMS3, DCISMS4)
sdcprobp.s = dplyr::select(data_same, DCISMS6, DCISMS9)
ndcp.s = dplyr::select(data_same, DCISMS5, DCISMS7, DCISMS8, DCISMS10)
emotcdc.s = dplyr::select(data_same, DCISMS24, DCISMS25)
probcdc.s = dplyr::select(data_same, DCISMS21, DCISMS22, DCISMS23)
DCeval.s = dplyr::select(data_same, DCISMS26, DCISMS27)
DCtot.s = dplyr::select(dplyr::filter(data, reltype_gender_1 == "Same-Gend"),
                 DCISMS1,DCISMS2, DCISMS3, DCISMS4, DCISMS5r,
        DCISMS6,DCISMS7r , DCISMS8r , DCISMS9 , 
        DCISMS10r, DCISMS11 , DCISMS12 , DCISMS13 ,
        DCISMS14 , DCISMS15r , DCISMS16 , DCISMS17r ,
        DCISMS18r , DCISMS19r , DCISMS20 , DCISMS21 ,
        DCISMS22 , DCISMS23 , DCISMS24 , DCISMS25)
GLSTOT.s = dplyr::select(dplyr::filter(data, reltype_gender_1 == "Same-Gend"), GLRS1r, GLRS2r, GLRS3,GLRS4, GLRS5,
               GLRS6 , GLRS7 , GLRS8r , GLRS9r , GLRS10 ,
               GLRS11 , GLRS12 , GLRS13 , GLRS14r , GLRS15 , GLRS16r)
ConEngage.s = dplyr::select(dplyr::filter(data, reltype_gender_1 == "Same-Gend"), CRSI1, CRSI5, CRSI9, CRSI13)

#different gender
Stresscoms.d = dplyr::select(data_dif, DCISMS1, DCISMS2)
sdcemots.d = dplyr::select(data_dif, DCISMS13, DCISMS14)
sdcprobs.d = dplyr::select(data_dif, DCISMS16, DCISMS20)
ndcs.d = dplyr::select(data_dif, DCISMS15, DCISMS17, DCISMS18, DCISMS19)
Stresscomp.d = dplyr::select(data_dif, DCISMS11, DCISMS12)
sdcemotp.d = dplyr::select(data_dif, DCISMS3, DCISMS4)
sdcprobp.d = dplyr::select(data_dif, DCISMS6, DCISMS9)
ndcp.d = dplyr::select(data_dif, DCISMS5, DCISMS7, DCISMS8, DCISMS10)
emotcdc.d = dplyr::select(data_dif, DCISMS24, DCISMS25)
probcdc.d = dplyr::select(data_dif, DCISMS21, DCISMS22, DCISMS23)
DCeval.d = dplyr::select(data_dif, DCISMS26, DCISMS27)
DCtot.d = dplyr::select(dplyr::filter(data, reltype_gender_1 == "Dif-Gend"),
                 DCISMS1,DCISMS2, DCISMS3, DCISMS4, DCISMS5r,
                 DCISMS6,DCISMS7r , DCISMS8r , DCISMS9 , 
                 DCISMS10r, DCISMS11 , DCISMS12 , DCISMS13 ,
                 DCISMS14 , DCISMS15r , DCISMS16 , DCISMS17r ,
                 DCISMS18r , DCISMS19r , DCISMS20 , DCISMS21 ,
                 DCISMS22 , DCISMS23 , DCISMS24 , DCISMS25)
GLSTOT.d = dplyr::select(dplyr::filter(data, reltype_gender_1 == "Dif-Gend"), GLRS1r, GLRS2r, GLRS3,GLRS4, GLRS5,
                  GLRS6 , GLRS7 , GLRS8r , GLRS9r , GLRS10 ,
                  GLRS11 , GLRS12 , GLRS13 , GLRS14r , GLRS15 , GLRS16r)
ConEngage.d = dplyr::select(dplyr::filter(data, reltype_gender_1 == "Dif-Gend"), CRSI1, CRSI5, CRSI9, CRSI13)

n <- names(w_diff)[-1]

#assemble table of reliabilities
matrix2[1,] <- c(n[1], get_alpha(Stresscoms.s), get_alpha(Stresscoms.d))
matrix2[2,] <- c(n[2], get_alpha(sdcemots.s), get_alpha(sdcemots.d))
matrix2[3,] <- c(n[3], get_alpha(sdcprobs.s), get_alpha(sdcprobs.d))
matrix2[4,] <- c(n[4], get_alpha(ndcs.s), get_alpha(ndcs.d))
matrix2[5,] <- c(n[5], get_alpha(Stresscomp.s), get_alpha(Stresscomp.d))
matrix2[6,] <- c(n[6], get_alpha(sdcemotp.s), get_alpha(sdcemotp.d))
matrix2[7,] <- c(n[7], get_alpha(sdcprobp.s), get_alpha(sdcprobp.d))
matrix2[8,] <- c(n[8], get_alpha(ndcp.s), get_alpha(ndcp.d))
matrix2[9,] <- c(n[9], get_alpha(emotcdc.s), get_alpha(emotcdc.d))
matrix2[10,] <- c(n[10], get_alpha(probcdc.s), get_alpha(probcdc.d))
matrix2[11,] <- c(n[11], get_alpha(DCeval.s), get_alpha(DCeval.d))
matrix2[12,] <- c(n[12], get_alpha(DCtot.s), get_alpha(DCtot.d))
matrix2[13,] <- c(n[13], get_alpha(GLSTOT.s), get_alpha(GLSTOT.d))
matrix2[14,] <- c(n[14], get_alpha(ConEngage.s), get_alpha(ConEngage.d))

kable(matrix2)

#construct final table

matrix3 <- matrix(NA, 14, 9)
colnames(matrix3) <- c("Variable", "Mean.Same-Gender", "SD.Same-Gender", "n.Same-Gender", "alpha.Same-Gender",
                       "Mean.Diff-Gender", "SD.Diff-Gender", "n.Diff-Gender", "alpha.Diff-Gender")

matrix3[,1] <- n
matrix3[,2] <- df1$mean
matrix3[,3] <- df1$sd
matrix3[,4] <- matrix1[,3]
matrix3[,5] <- matrix2[,2]
matrix3[,6] <- df2$mean
matrix3[,7] <- df2$sd
matrix3[,8] <- matrix1[,5]
matrix3[,9] <- matrix2[,3]
matrix3[,10] <- matrix1[,6]
matrix3[,11] <- matrix1[,8]
matrix3[,12] <- matrix1[,10]

kable(matrix3)

descriptives <- as.data.frame(matrix3)
