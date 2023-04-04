# ==============================================================================
#
# SCRIPT FOR DELIVERABLE 4.2
# Obesity's contribution to diabetes outcomes in people with T1D
#
# Author: Sofia Pazmino (SP)
# Date: 2022-10-26
#
# NOTE:
# The data is in wide format: one ID per row, one measurement per column!
# If there are multiple measurements, as in longitudinal data, then append
# the variable name with _t1, _t2, _t3, ... for each timepoint.

# NOTE:
# Use underscores only for separating timepoints. If you have variable names
# containing several words, use dots, for example:
# age.at.consent or date.of.diagnosis

# NOTE:
# The data frame must be named 'df'

# (1) Install/Upload packages ####
if (!require(stats)) install.packages('stats')
library(stats)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(gtsummary)) install.packages('gtsummary')
library(gtsummary)
if (!require(survival)) install.packages('survival')
library (survival)
if (!require(survminer)) install.packages('survminer')
library (survminer)
if (!require(ggplot2)) install.packages('ggplot2')
library (ggplot2)

# (2) Set working directory ####
# Manually: Session -> Set_Working Directory -> Choose Directory #I did it like this
# OR  Ctrl + Shift + H

# (3) Definitons/variables of interest ####

# sex = female ==0, male == 1
# disdur = disease duration, time since diagnosis in years
# sbp = systolic blood pressure
# dbp = diastolic blood pressure

# cat.hba1c = Yes==1/No==0 : hba1c <7% | hba1c <53mmol/mol
# time.hba1c = time in years to first hba1c>=7% after having cat.hba1c ==1
# cont.hba1c = Yes==1/No==0 : hba1c >=7% | hba1c >=53mmol/mol
# hypoglyc1 = Yes==1/No==0 : glucose >=54mg/dL(3mmol/L) & glucose <70mg/dL(3.9mmol/L)
# hypoglyc2 = Yes==1/No==0 : glucose <54mg/dL(3mmol/L)
# cat.lipid = Yes==1/No==0 : chol<=190mg/dL &
#             LDL <100mg/dL if <35 years of age | <116 mg/dL if age >=35 years &
#             HDL >=50mg/dL for women | >=40mg/dL for men &
#             TG<150mg/dL

# Complications:

# card = Yes==1/No==0 : cardiovascular death OR all cause death |
#        non-fatal myocardial infarction (ICD-10: I21 | I22 | I23 | I25) |
#        non-fatal stroke (ICD-10: I60| I61| I62| I63|G45) |
# card.mi = Yes==1/No==0 --> ICD-10: I21 | I22 | I23 | I25
# card.stroke = Yes==1/No==0 --> ICD-10: I60| I61| I62| I63|G45
# card.angina = Yes==1/No==0 --> ICD-10: I20.0
# death = Yes==1/No==0: all cause mortality
# time.card = time in years to first card | last available visit if no event happened
# time.card.mi = time in years to first card.mi | last available visit if no event happened
# time.card.stroke = time in years to first card.stroke | last available visit if no event happened
# time.card.angina = time in years to first card.angina | last available visit if no event happened
# time.death = time in years to death | last available visit if no event happened

# neuro = Yes==1/No==0 --> ICD-10: E10.4
# time.neuro = time in years to first neuropathy | last available visit if no event happened

# nefro = Yes==1/No==0 --> ICD-10: E10.2 | EGFR<60 | microalbuminuria
# time.nefro = time in years to first nephropathy | last available visit if no event happened

# ret = Yes==1/No==0 --> ICD-10: E10.3
# time.ret = time in years to first retinopathy | last available visit if no event happened

# foot = Yes==1/No==0 --> ICD-10: E10.621
# time.foot = time in years to first diabetic foot (ulcer) | last available visit if no event happened

# dka = Yes==1/No==0 --> ICD-10: E10.10 | ketones in urine >=5
# time.dka = time in years to first diabetic ketoacidosis | last available visit if no event happened

# (4) Logistic regressions ####

## 4.1 Glycaemic control ####
mod1 <- glm(cat.hba1c ~ age + sex + bmi + disdur,#,# + insulin,
            data = df,
            family = binomial)
# mod1 %>%
#   tbl_regression(exponentiate = TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p(q = TRUE)

mod2 <- glm(hypoglyc1 ~ age + sex + bmi + disdur,#,# + insulin,
            data = df,
            family = binomial)
# mod2 %>%
#   tbl_regression(exponentiate = TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p(q = TRUE)

mod3 <- glm(hypoglyc2 ~ age + sex + bmi + disdur,# + insulin,
            data = df,
            family = binomial)
# mod3 %>%
#   tbl_regression(exponentiate = TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p(q = TRUE)

# ## 4.2 Metabolic/lipid control ####
# mod4 <- glm(cat.lipid ~ age + sex + bmi + disdur,# + insulin,
#             data=df,
#             family=binomial)
# mod4 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

## 3.3 Complications ####
mod5 <- glm(card ~ age + sex + bmi + disdur + #insulin +
              sbp + dbp + hba1c,
            data=df,
            family=binomial)
# mod5 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod6 <- glm(card.mi ~ age + sex + bmi + disdur + #insulin +
              sbp + dbp + hba1c,
            data=df,
            family=binomial)
# mod6 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod7 <- glm(card.stroke ~ age + sex + bmi + disdur + #insulin +
              sbp + dbp + hba1c,
            data=df,
            family=binomial)
# mod7 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod8 <- glm(card.angina ~ age + sex + bmi + disdur + #insulin +
              sbp + dbp + hba1c,
            data=df,
            family=binomial)
# mod8 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod9 <- glm(death ~ age + sex + bmi + disdur + #insulin +
              sbp + dbp + hba1c,
            data=df,
            family=binomial)
# mod9 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod10 <- glm(neuro ~ age + sex + bmi + disdur + #insulin +
              sbp + dbp + hba1c,
            data=df,
            family=binomial)
# mod10 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod11 <- glm(nefro ~ age + sex + bmi + disdur + #insulin +
               sbp + dbp + hba1c,
             data=df,
             family=binomial)
# mod11 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod12 <- glm(ret ~ age + sex + bmi + disdur + #insulin +
               sbp + dbp + hba1c,
             data=df,
             family=binomial)
# mod12 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod13 <- glm(foot ~ age + sex + bmi + disdur + #insulin +
               sbp + dbp + hba1c,
             data=df,
             family=binomial)
# mod13 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

mod14 <- glm(dka ~ age + sex + bmi + disdur + #insulin +
               sbp + dbp + hba1c,
             data=df,
             family=binomial)
# mod14 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   add_q() %>%
#   bold_labels() %>%
#   bold_p (q=TRUE)

# (5) Survival analyses ####

## Check levels for cat.bmi
df$cat.bmi <- factor(df$cat.bmi,
                     levels = c("Normal weight", "Overweight", "Obesity"))
col.pal <- c("#42b540","#00468b", "#ed0000")

## 5.1 Stratified Kaplan Meier ####

### 5.1.1 Glycaemic control #####
km1<-survfit(Surv(time.hba1c, cont.hba1c==1)~ cat.bmi, data=df)
plot1 <- ggsurvplot(km1, data = df,
                         surv.median.line = "hv",
                         risk.table = "abs_pct",
                         palette = col.pal,
                         pval = TRUE, pval.method = TRUE,
                         conf.int = TRUE, ggtheme = theme_minimal(),
                         xlab = "Time in years", ylab = "Probability of survival",
                         title = "Time to loss of glycaemic control",
                         legend="bottom", legend.title= "BMI",
                         legend.labs= c("Normal weight", "Overweight", "Obesity"),
                         font.legend =14,
                         font.title= c(20, "bold", "darkblue"),
                         xlim = c(0,12), caption = "CAT-DM1",
                         break.time.by= 2,
                         font.x=14, font.y=14, font.caption=14,
                         fontsize = 4,
                         font.tickslab=14,
                         censor = T, censor.shape = "|")

png("hba1c.png",
    width=30, height=20, units = "cm", res=300)
plot1
dev.off()

### 5.1.2 Complications #####
km2<-survfit(Surv(time.card, card==1)~ cat.bmi, data=df)
plot2 <- ggsurvplot(km2, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to MACE",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")

png("cardiovascular.png",
    width=30, height=20, units = "cm", res=300)
plot2
dev.off()

km3<-survfit(Surv(time.card.mi, card.mi==1)~ cat.bmi, data=df)
plot3 <- ggsurvplot(km3, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to myocardial infarction",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")

png("myocardial_infarction.png",
    width=30, height=20, units = "cm", res=300)
plot3
dev.off()


km4<-survfit(Surv(time.card.stroke, card.stroke==1)~ cat.bmi, data=df)
plot4 <- ggsurvplot(km4, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to stroke",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")

png("stroke.png",
    width=30, height=20, units = "cm", res=300)
plot4
dev.off()


km5<-survfit(Surv(time.card.angina, card.angina==1)~ cat.bmi, data=df)
plot5 <- ggsurvplot(km5, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to unstable angina",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")

png("unstable_angina.png",
    width=30, height=20, units = "cm", res=300)
plot5
dev.off()

km6<-survfit(Surv(time.death, death==1)~ cat.bmi, data=df)
plot6 <- ggsurvplot(km6, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to death",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")
png("death.png",
    width=30, height=20, units = "cm", res=300)
plot6
dev.off()

km7<-survfit(Surv(time.neuro, neuro==1)~ cat.bmi, data=df)
plot7 <- ggsurvplot(km7, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to neuropathy",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")
png("neuropathy.png",
    width=30, height=20, units = "cm", res=300)
plot7
dev.off()

km8<-survfit(Surv(time.nefro, nefro==1)~ cat.bmi, data=df)
plot8 <- ggsurvplot(km8, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to nephropathy",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")
png("nephropathy.png",
    width=30, height=20, units = "cm", res=300)
plot8
dev.off()

km9<-survfit(Surv(time.ret, ret==1)~ cat.bmi, data=df)
plot9 <- ggsurvplot(km9, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to retinopathy",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")
png("retinopathy.png",
    width=30, height=20, units = "cm", res=300)
plot9
dev.off()

km10<-survfit(Surv(time.foot, foot==1)~ cat.bmi, data=df)
plot10 <- ggsurvplot(km10, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to diabetic foot",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")
png("diabetic_foot.png",
    width=30, height=20, units = "cm", res=300)
plot10
dev.off()

km11<-survfit(Surv(time.dka, dka==1)~ cat.bmi, data=df)
plot11 <- ggsurvplot(km11, data = df,
                    surv.median.line = "hv",
                    risk.table = "abs_pct",
                    palette = col.pal,
                    pval = TRUE, pval.method = TRUE,
                    conf.int = TRUE, ggtheme = theme_minimal(),
                    xlab = "Time in years", ylab = "Probability of survival",
                    title = "Time to diabetic ketoacidosis",
                    legend="bottom", legend.title= "BMI",
                    legend.labs= c("Normal weight", "Overweight", "Obesity"),
                    font.legend =14,
                    font.title= c(20, "bold", "darkblue"),
                    xlim = c(0,12), caption = "CAT-DM1",
                    break.time.by= 2,
                    font.x=14, font.y=14, font.caption=14,
                    fontsize = 4,
                    font.tickslab=14,
                    censor = T, censor.shape = "|")
png("dka.png",
    width=30, height=20, units = "cm", res=300)
plot11
dev.off()

## 4.2 Cox regressions ####

### 4.2.1 Glycaemic control #####
cox1=coxph(Surv(time.hba1c, cont.hba1c==1) ~ age + sex + bmi + disdur,# + insulin,
           data=df,method="breslow")
# cox1 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

### 4.2.2 Complications #####
cox2=coxph(Surv(time.card, card==1) ~ age + sex + bmi + disdur,# + insulin,
           data=df,method="breslow")
# cox2 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox3=coxph(Surv(time.card.mi, card.mi==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox3 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox4=coxph(Surv(time.card.stroke, card.stroke==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox4 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox5=coxph(Surv(time.card.angina, card.angina==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox5 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox6=coxph(Surv(time.death, death==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox6 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox7=coxph(Surv(time.neuro, neuro==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox7 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox8=coxph(Surv(time.nefro, nefro==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox8 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox9=coxph(Surv(time.ret, ret==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox9 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p

cox10=coxph(Surv(time.foot, foot==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox10 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()

cox11=coxph(Surv(time.dka, dka==1) ~ age + sex + bmi + disdur + #insulin +
             sbp + dbp + hba1c,
           data=df,method="breslow")
# cox11 %>%
#   tbl_regression(exponentiate=TRUE) %>%
#   add_nevent() %>%
#   bold_labels() %>%
#   bold_p ()
