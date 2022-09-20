# 0. Einlesen + librarys----
## 0.1 librarys----
library(dplyr)
library(mgcv)
library(gamm4)
library(glmmTMB)

## 0.2 Einlesen----
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/data.RData")

## 0.3 Für Betaverteilung: setzen von quality = 0 auf quality = 0.00001, da x \in (0,1) (gleiches bei 1)
df$quality_adj <- df$quality
df$quality_adj[df$quality == 0] <-  .00001
df$quality_adj[df$quality == 1] <- 0.99999
df$Study[which(!(df$Study %in% c(paste("ESS Round",1:7))))] <- "Other"

# 1. Modelle----
## 1.1 LMMs ----
### 1.1.1 lme4----
##### 1.1.1.1 Mit allen Kovariablen----
mod_lme4_gauss_full_reml <- lme4::lmer(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df, REML = T, weight = sample_size, 
                                       control = lmerControl(optCtrl = list(maxfun = 100000)))
mod_lme4_gauss_full_non.reml <- lme4::lmer(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df, REML = F, weight = sample_size,
                                           control = lmerControl(optCtrl = list(maxfun = 100000, maxit = 100000), optimizer = "bobyqa"))

##### Fuer weitere Berechnungen mittels anderen Schätzern:
#lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#     control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))

##### 1.1.1.2 Ohne dichotome GIFI Variablen----
mod_lme4_gauss_sub_reml <- lme4::lmer(as.formula(paste("quality ~", paste(model_names_nondich[-52], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df, REML = T, weight = sample_size,
                                      control = lmerControl(optCtrl = list(maxfun = 100000)))
mod_lme4_gauss_sub_non.reml <- lme4::lmer(as.formula(paste("quality ~", paste(model_names_nondich[-52], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df, REML = F, weight = sample_size,
                                          control = lmerControl(optCtrl = list(maxfun = 100000)))

### 1.1.2 glmmTMB----
#### 1.1.2.1 Mit allen Kovariablen----
mod_glmmTMB_gauss_full_reml <- glmmTMB::glmmTMB(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                       control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size, REML = T)

mod_glmmTMB_gauss_full_non.reml <- glmmTMB::glmmTMB(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                                control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size, REML = F)

#### 1.1.2.2 Ohne dichotome GIGI Variablen----
mod_glmmTMB_gauss_sub_reml <- glmmTMB::glmmTMB(as.formula(paste("quality ~", paste(model_names_nondich[-52], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                                    control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size, REML = T)

mod_glmmTMB_gauss_sub_non.reml <- glmmTMB::glmmTMB(as.formula(paste("quality ~", paste(model_names_nondich[-52], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                               control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size, REML = F)

## 1.2 GLMMs----
### 1.2.1 Beta----
#### 1.2.1.1 glmmTMB----
#### 1.1.2.1 Mit allen Kovariablen----
mod_glmmTMB_beta_full_reml <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                                control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size, 
                                               family = glmmTMB::beta_family(link = "logit"))

mod_glmmTMB_beta_full_non.reml <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                                    control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size, REML = F,
                                                   family = glmmTMB::beta_family(link = "logit"))

#### 1.1.2.2 Ohne dichotome GIFI Variablen----
mod_glmmTMB_beta_sub_reml <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names_nondich[-52], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                               control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size, REML = T,
                                              family = glmmTMB::beta_family(link = "logit"))

mod_glmmTMB_beta_sub_non.reml <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names_nondich[-52], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df,
                                                   control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size, REML = F,
                                                  family = glmmTMB::beta_family(link = "logit"))


# 2. Speichern----
save(mod_glmmTMB_beta_full_non.reml, mod_glmmTMB_beta_sub_non.reml, file = "C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Modelle/modelle.RData")

# 3. Alte Version----
#lmer_model_nloptwrap <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#                             control = lmerControl(optCtrl = list(maxfun = 100000)))
#
#lmer_model_nerlder <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#                           control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))
#
#lmer_model_bobyqa <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#                          control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))
#
#lmer_model_nlminbwrap <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#                              control = lmerControl(optCtrl = list(optimizer = "nlminbwrap", maxfun = 100000)))
#
#
#### 0.5.2 glmer
#glmer_bin.model_nloptwrap <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
#                                   weights = sample_size, control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 100000)))
#
#
#glmer_bin.model_nelder <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
#                                weights = sample_size, control = glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000)))
#
#glmer_bin.model_bobyqa <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
#                                weights = sample_size, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
#
#glmer_bin.model_nlminbwrap <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
#                                    weights = sample_size, control = glmerControl(optimizer = "nlminbwrap", optCtrl = list(maxfun = 100000)))
#
### 0.5.3 glmmTMB
#df$quality_adj <- NA
#df$quality_adj <- ifelse(df$quality == 0, .0001, df$quality)
#df$quality_adj[df$quality == 1] <- 0.999
#glmmTMB_beta.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df, family = glmmTMB::beta_family(link = "logit"),
#                                       control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size)
#
#glmmTMB_normal.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1 | Country) + (1 | Study/experiment)")), data = df, family = gaussian(),
#                                         control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size)
#
#glmmTMB_beta.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1 | Language) + (1 | Study) + (1|experiment)")), data = df, family = glmmTMB::beta_family(link = "logit"),
#                                       control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), profile = TRUE, collect = FALSE))
#
#glmmTMB_normal.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1 | Language) + (1 | Study) + (1|experiment)")), data = df, family = gaussian(),
#                                         control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), profile = TRUE, collect = FALSE))
#
#DHARMa::simulateResiduals(glmmTMB_beta.model, plot = T)
#DHARMa::simulateResiduals(glmmTMB_normal.model, plot = T)
#
### 0.5.2 GLMMadaptive
#GLMMadaptive::mixed_model(as.formula(paste("quality ~", paste(model_names, collapse = " + "))), random = ~ 1 | Country / Study / experiment, family = GLMMadaptive::beta.fam(), optimizer = "nlminb",
#                          iter_qN_incr = 20, nAGQ = 21, data = df)
#
#
## Reproduzierbares Beispiel
#set.seed(12345)
#a <- sample(c(0,1), 1000, replace = T)
#df1 <- data.frame(a = a,
#                  b = ifelse(a == 1, sample(c(0,1)), NA))
#df1$b_gifi_eins <- ifelse(!is.na(df1$b) & df1$b == 1, 1, 0)
#df1$b_gifi_null <- ifelse(!is.na(df1$b) & df1$b == 0, 1, 0)
#df1$out <- rbeta(1000, 1, 1)
#
#m <- glmmTMB::glmmTMB(out ~ b_gifi_eins + b_gifi_null + a, data = df1, family = glmmTMB::beta_family(link = "logit"))
#m1 <- glmmTMB::glmmTMB(out ~ b_gifi_null + a, data = df1, family = glmmTMB::beta_family(link = "logit")) 
#m2 <- glmmTMB::glmmTMB(out ~ b_gifi_null + a, data = df1, family = gaussian())










#random = ~1 | Study,family = beta.fam(),iter_EM = 0, optimizer = "nlminb",
#initial_values = list(betas = GLMMadaptive_question1$coefficients, D = matrix(c(0.5, 0, 0, 0.1), 2, 2)),iter_qN_incr = 20,nAGQ = 21, data = df



