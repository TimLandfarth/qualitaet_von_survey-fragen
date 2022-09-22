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
### Seed zur Reproduzierbarkeit
set.seed(12345)

### Kovariablen und Einfuehrungen in Filter
a.f <- sample(c(0,1), 1000, replace = T) # "normale" kovariable
b.f <- sample(c(0,1), 1000, replace = T) # Einfuehrung in ersten Filter (1. Filter)
c.f <- sample(0:3, 1000, replace = T) # Einfuehrung in zweiten Filter (2. Filter)
d <- sample(0:5, 1000, replace = T) # Random Effekt
e <- sample(0:10, 1000, replace = T) # genesteter Random Effekt in d ( d/e )

df1 <- data.frame(a.f = a.f,
                  b.f = b.f,
                  b.1 = ifelse(b.f == 1, sample(c(0,1)), NA),  # Kovariable im ersten Filter
                  c.f = c.f,
                  c.1 = ifelse(c.f == 2 | c.f == 3, sample(c(0,1)), NA), # Kovariable im zweiten Filter
                  d = d)

### GIFI kodierung
##### Fuer Kovariable im ersten Filter
df1$b.1_1 <- ifelse(!is.na(df1$b.1) & df1$b.1 == 1, 1, 0)
df1$b.1_0 <- ifelse(!is.na(df1$b.1) & df1$b.1 == 0, 1, 0)

##### Fuer Kovariable im zweiten Filter
df1$c.1_1 <- ifelse(!is.na(df1$c.1) & df1$c.1 == 1, 1, 0)
df1$c.1_0 <- ifelse(!is.na(df1$c.1) & df1$c.1 == 0, 1, 0)

### Outcome
df1$out <- rbeta(1000, 1, 1)

### Modell mit GIFI-kodierung
m <- glmmTMB::glmmTMB(out ~ a.f + b.f + c.f + b.1_1 + b.1_0 + c.1_1 + c.1_0 + (1|d/e),
                      data = df1, family = glmmTMB::beta_family(link = "logit"))

summary(m)
TMB::sdreport(m,getJointPrecision=TRUE)







#random = ~1 | Study,family = beta.fam(),iter_EM = 0, optimizer = "nlminb",
#initial_values = list(betas = GLMMadaptive_question1$coefficients, D = matrix(c(0.5, 0, 0, 0.1), 2, 2)),iter_qN_incr = 20,nAGQ = 21, data = df



