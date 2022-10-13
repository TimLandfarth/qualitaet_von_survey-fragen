# 0. Einlesen + librarys----
## 0.1 librarys----
library(dplyr)
library(mgcv)
library(gamm4)
library(glmmTMB)

## 0.2 Einlesen----
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/data.RData")

## 0.3 Für Betaverteilung: setzen von quality = 0 auf quality = 0.00001, da x \in (0,1) (gleiches bei 1)

# 1. Qualitaet----
## 1.1 LMMs ----
### 1.1.1 lme4----
mod_lme4_gauss_q <- lme4::lmer(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df, REML = F, weight = sample_size_stand, 
                                       control = lmerControl(optCtrl = list(maxfun = 100000)))

plot(mod_lme4_gauss_q)
##### Fuer weitere Berechnungen mittels anderen Schätzern:
#lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#     control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))

### 1.1.2 glmmTMB----
mod_glmmTMB_gauss_q <- glmmTMB::glmmTMB(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df,
                                                control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size_stand, REML = F)

DHARMa::plotQQunif(mod_glmmTMB_gauss_q, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)
## 1.2 GLMMs----
### 1.2.1 Beta----
#### 1.2.1.1 glmmTMB----
mod_glmmTMB_beta_q <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df,
                                                   control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size_stand, REML = F,
                                                   family = glmmTMB::beta_family(link = "logit"))


DHARMa::plotQQunif(mod_glmmTMB_beta_q, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)

# 2. Validitaet----
## 2.1 LMMs ----
### 2.1.1 lme4----
mod_lme4_gauss_v <- lme4::lmer(as.formula(paste("validity ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df, REML = F, weight = sample_size_stand, 
                             control = lmerControl(optCtrl = list(maxfun = 100000)))

##### Fuer weitere Berechnungen mittels anderen Schätzern:
#lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#     control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))

### 2.1.2 glmmTMB----
mod_glmmTMB_gauss_v <- glmmTMB::glmmTMB(as.formula(paste("validity ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df,
                                      control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size_stand, REML = F)

## 2.2 GLMMs----
### 2.2.1 Beta----
#### 2.2.1.1 glmmTMB----
mod_glmmTMB_beta_v <- glmmTMB::glmmTMB(as.formula(paste("validity_adj ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df,
                                     control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size_stand, REML = F,
                                     family = glmmTMB::beta_family(link = "logit"))

# 3. Reliabilitaet----
## 3.1 LMMs ----
### 3.1.1 lme4----
mod_lme4_gauss_r <- lme4::lmer(as.formula(paste("reliability ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df, REML = F, weight = sample_size_stand, 
                               control = lmerControl(optCtrl = list(maxfun = 100000)))

##### Fuer weitere Berechnungen mittels anderen Schätzern:
#lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
#     control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))

### 3.1.2 glmmTMB----
mod_glmmTMB_gauss_r <- glmmTMB::glmmTMB(as.formula(paste("reliability ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df,
                                        control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size_stand, REML = F)

## 3.2 GLMMs----
### 3.2.1 Beta----
#### 3.2.1.1 glmmTMB----
mod_glmmTMB_beta_r <- glmmTMB::glmmTMB(as.formula(paste("reliability_adj ~", paste(model_names[-55], collapse = " + "), " + (1|Study/Language/experiment)")), data = df,
                                       control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 200000,eval.max = 200000), collect = FALSE), weights = sample_size_stand, REML = F,
                                       family = glmmTMB::beta_family(link = "logit"))



# 2. Speichern----
save(mod_glmmTMB_beta_q, mod_glmmTMB_beta_r, mod_glmmTMB_beta_v,
     mod_glmmTMB_gauss_q, mod_glmmTMB_gauss_r, mod_glmmTMB_gauss_v,
     mod_lme4_gauss_q, mod_lme4_gauss_r, mod_lme4_gauss_v,
     file = "C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Modelle/modelle.RData")

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

glmmTMB_question1 <-
  glmmTMB(
    quality ~ Domain + Concept + Social.Desirability + Centrality + Reference.period +
      Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used +
      Request.for.an.answer.type_Declar. + Request.for.an.answer.type_Imper. +
      Request.for.an.answer.type_Inter. + Use.of.gradation_No +
      Use.of.gradation_Yes + Balance.of.the.request_Balanced +
      Presence.of.encouragement.to.answer_No +
      Emphasis.on.subjective.opinion.in.request_No +
      Use.of.stimulus.or.statement.in.the.request +
      Absolute.or.comparative.judgment + Response.scale..basic.choice +
      Number.of.categories + Theoretical.range.of.the.concept.bipolar.unipolar_bipolar +
      Range.of.the.used.scale.bipolar.unipolar_Bipolar +
      Symmetry.of.response.scale_Asymmetric +
      Neutral.category_Not.present +
      Number.of.fixed.reference.points + Don.t.know.option + Interviewer.instruction + Respondent.instruction +
      Extra.information.or.definition + Knowledge.provided_Definitions + Knowledge.provided_Other +
      Knowledge.provided_No + Knowledge.provided_def..and.other + Introduction.available. +
      Request.present.in.the.introduction_present +
      Number.of.sentences.in.introduction + Number.of.words.in.introduction + Number.of.sentences.in.introduction +
      Number.of.sentences.in.the.request + Number.of.words.in.request + Total.number.of.nouns.in.request.for.an.answer +
      Total.number.of.abstract.nouns.in.request.for.an.answer + Total.number.of.syllables.in.request +
      Number.of.subordinate.clauses.in.request + Number.of.syllables.in.answer.scale + Total.number.of.nouns.in.answer.scale +
      Total.number.of.abstract.nouns.in.answer.scale +
      Showcard.or.other.visual.aids.used +
      Horizontal.or.vertical.scale_Horizontal +
      Overlap.of.scale.labels.and.categories_clearly.connected +
      Numbers.or.letters.before.the.answer.categories_Neither +
      Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes + Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers +
      Start.of.the.response.sentence.on.the.visual.aid_No +
      Request.on.the.visual.aid_No + Request.on.the.visual.aid_Yes + Picture.provided._No +
      Computer.assisted + Interviewer + Visual.or.oral.presentation + Position + (1 | Language) + (1 | Study / experiment),
    family = beta_family(link = "logit"),
    control = glmmTMBControl(
      optCtrl = list(iter.max = 20000, eval.max = 20000),
      profile = TRUE,
      collect = FALSE
    ),
    weights = sample_size,
    data = df
  )

t <- c("Domain" , "Concept" , "Social.Desirability" , "Centrality" , "Reference.period" ,
  "Formulation.of.the.request.for.an.answer..basic.choice" , "WH.word.used.in.the.request_used" ,
  "Request.for.an.answer.type_Declar." , "Request.for.an.answer.type_Imper." ,
  "Request.for.an.answer.type_Inter." , "Use.of.gradation_No" ,
  "Use.of.gradation_Yes" , "Balance.of.the.request_Balanced" ,
  "Presence.of.encouragement.to.answer_No" ,
  "Emphasis.on.subjective.opinion.in.request_No" ,
  "Use.of.stimulus.or.statement.in.the.request" ,
  "Absolute.or.comparative.judgment" , "Response.scale..basic.choice" ,
  "Number.of.categories" , "Theoretical.range.of.the.concept.bipolar.unipolar_bipolar" ,
  "Range.of.the.used.scale.bipolar.unipolar_Bipolar" ,
  "Symmetry.of.response.scale_Asymmetric" ,
  "Neutral.category_Not.present" ,
  "Number.of.fixed.reference.points" , "Don.t.know.option" , "Interviewer.instruction" , "Respondent.instruction" ,
  "Extra.information.or.definition" , "Knowledge.provided_Definitions", "Knowledge.provided_Other" ,
  "Knowledge.provided_No" , "Knowledge.provided_def..and.other" , "Introduction.available." ,
  "Request.present.in.the.introduction_present" ,
  "Number.of.sentences.in.introduction" , "Number.of.words.in.introduction" , "Number.of.sentences.in.introduction" ,
  "Number.of.sentences.in.the.request" , "Number.of.words.in.request" , "Total.number.of.nouns.in.request.for.an.answer" ,
  "Total.number.of.abstract.nouns.in.request.for.an.answer" , "Total.number.of.syllables.in.request" ,
  "Number.of.subordinate.clauses.in.request" , "Number.of.syllables.in.answer.scale" , "Total.number.of.nouns.in.answer.scale" ,
  "Total.number.of.abstract.nouns.in.answer.scale" ,
  "Showcard.or.other.visual.aids.used" ,
  "Horizontal.or.vertical.scale_Horizontal" ,
  "Overlap.of.scale.labels.and.categories_clearly.connected" ,
  "Numbers.or.letters.before.the.answer.categories_Neither" ,
  "Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes" , "Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers" ,
  "Start.of.the.response.sentence.on.the.visual.aid_No" ,
  "Request.on.the.visual.aid_No" , "Request.on.the.visual.aid_Yes" , "Picture.provided._No" ,
  "Computer.assisted" , "Interviewer", "Visual.or.oral.presentation", "Position")
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
m <- glmmTMB::glmmTMB(out ~ a.f + b.f + c.f + b.1_1 + b.1_0 + c.1_1 + c.1_0 + (1|e) + (1|d),
                      data = df1, family = glmmTMB::beta_family(link = "logit"))

predict(m, df1, re.form = NULL)[1:10]


summary(m)
TMB::sdreport(m,getJointPrecision=TRUE)







#random = ~1 | Study,family = beta.fam(),iter_EM = 0, optimizer = "nlminb",
#initial_values = list(betas = GLMMadaptive_question1$coefficients, D = matrix(c(0.5, 0, 0, 0.1), 2, 2)),iter_qN_incr = 20,nAGQ = 21, data = df



