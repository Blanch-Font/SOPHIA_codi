###################################################################################################
# Needed Packages
###################################################################################################
library(tidyverse)
library(mice)

###################################################################################################
# Load data
###################################################################################################
load(file)

###################################################################################################
# Pre-MICE
###################################################################################################
# Major event included in the MICE process
aalen <- survfit(coxph(Surv(time.card, card) ~ 1,
                       data = df),
                 type = 'aalen')
df[, na.event := approx(aalen$time,
                        -log(aalen$surv),
                        xout = as.numeric(time.card))$y]

# Transform continuos variables
df <- df %>%
  mutate(ln_hba1c = log(hba1c),
         ln_fglucose = log(fglucose),
         ln_bmi = log(bmi),
         ln_chol = log(chol),
         ln_hdl = log(hdl),
         ln_ldl = log(ldl),
         ln_tg = log(tg),
         ln_cpep = log(cpep),
         ln_creatinine = log(creatinine),
         ln_albumin = log(albumin),
         ln_ast = log(ast),
         ln_alt = log(alt),
         ln_gamma.gt = log(gamma.gt),
         ln_vit.d = log(vit.d),
         ln_sbp = log(sbp),
         ln_pp = log(sbp - dbp))

df_imp <- df %>%
  select(id, ln_hba1c, ln_fglucose, dka_t0, hypoglyc, sex, smoking, age, ln_bmi, ln_chol, ln_hdl,
         ln_tg, ln_cpep, ln_creatinine, ln_albumin, ln_ast, ln_alt, ln_gamma.gt, ln_vit.d, disdur,
         ln_sbp, ln_pp, na.event)

###################################################################################################
# MICE
###################################################################################################
imp <- mice(data = df_imp,
            m = 5,
            method = "pmm",
            maxit = 60,
            printFlag = FALSE)

###################################################################################################
# Post-MICE
###################################################################################################
df_imputed <- complete(imp, action = 'long', include = FALSE)
df_imputed <- df_imputed %>%
  mutate(hba1c = exp(ln_hba1c),
         fglucose = exp(ln_fglucose),
         bmi = exp(ln_bmi),
         chol = exp(ln_chol),
         hdl = exp(ln_hdl),
         ldl = exp(ln_ldl),
         tg = exp(ln_tg),
         cpep = exp(ln_cpep),
         creatinine = exp(ln_creatinine),
         albumin = exp(ln_albumin),
         ast = exp(ln_ast),
         alt = exp(ln_alt),
         gamma.gt = exp(ln_gamma.gt),
         vit.d = exp(ln_vit.d),
         sbp = exp(ln_sbp),
         dbp = sbp - exp(ln_pp))
# The variables that depend on one of the imputed variables should also be calculated
