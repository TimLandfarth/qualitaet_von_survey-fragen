############# Modelle ################

library(stringr)
library(dplyr)
library(data.table)
library(readxl)
library(olsrr)
library(betareg)
###### Hier werden 2 Datensätze eingelesen und modifiziert. Einmal der Gifi Datensatz bei dem alle Labels eine eigene binäre Variable wurden und der "normale" Datensatz


#### einlesen von modifizierten Daten (GIFI) #####

source("C:/Users/mvett/Desktop/Studium_Statistik/SS22/Consulting-Projekt/R_dateien_und_co/Aufbearbeitungsfile_Gifisystem.R")


### loADING Tims dataset 

load("C:/Users/mvett/Desktop/Studium_Statistik/SS22/Consulting-Projekt/qualitaet_von_survery-fragen/Aufbereitung/data.RData")

### dropping non-existing levels
DF_factor <- DF[,which(sapply(DF,is.factor))]
DF_factor <- sapply(DF_factor,droplevels)
C:\Users\mvett\Desktop\Studium_Statistik\SS22\Consulting-Projekt\qualitaet_von_survey-fragen\Aufbereitung#### first model - linear model - way 1 by the Gifi-System

limo1 <- lm(quality.r.2. ~ Domain + Concept   + 
              Social.Desirability + Centrality + Reference.period + 
              Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + WH.word.used.in.the.request_notused + X.WH..word_who + 
              X.WH..word_which + X.WH..word_what + X.WH..word_whentime + X.WH..word_whereplace + X.WH..word_howProcedure + X.WH..word_howRelationship + 
              X.WH..word_howOpinion + X.WH..word_howQuantity + X.WH..word_howExtremity + X.WH..word_how_Intensity + X.WH..word_why + Request.for.an.answer.type_interrogative + 
              Request.for.an.answer.type_imperative + Request.for.an.answer.type_declarative + Request.for.an.answer.type_NoneOfThese + Use.of.gradation_nogradation + 
              Use.of.gradation_gradation + Balance.of.the.request_balanced + Balance.of.the.request_unbalanced + Presence.of.encouragement.to.answer_noEncour + 
              Presence.of.encouragement.to.answer_Encour + Emphasis.on.subjective.opinion.in.request_noEmphasis + Emphasis.on.subjective.opinion.in.request_emphasis + 
              Information.about.the.opinion.of.other.people_noInfo + Information.about.the.opinion.of.other.people_info + Use.of.stimulus.or.statement.in.the.request_notUsed + 
              Use.of.stimulus.or.statement.in.the.request_used + Absolute.or.comparative.judgment + Response.scale..basic.choice_morethan2cat + Response.scale..basic.choice_twoCatScale + 
              Response.scale..basic.choice_openEnded + Response.scale..basic.choice_notAvailableMagnEst + Response.scale..basic.choice_NotavailableLineProd + 
              Response.scale..basic.choice_MoreStepsProcedure + Number.of.categories_number + Labels.of.categories_noLabels + Labels.of.categories_partiallyLabeled + 
              Labels.of.categories_fully_Labeled + Labels.with.short.text.or.complete.sentences_shortText + Labels.with.short.text.or.complete.sentences_completeSentences + 
              Order.of.the.labels_firstNegativeOrNonApplicable + Order.of.the.labels_firstPositive + Correspondence.between.labels.and.numbers.of.the.scale_highCorrespondence + 
              Correspondence.between.labels.and.numbers.of.the.scale_mediumCorrespondence + Correspondence.between.labels.and.numbers.of.the.scale_lowCorrespondence + 
              Correspondence.between.labels.and.numbers.of.the.scale_notApplicable + Theoretical.range.of.the.concept.bipolar.unipolar_unipolar + Theoretical.range.of.the.concept.bipolar.unipolar_bipolar + 
              Range.of.the.used.scale.bipolar.unipolar_unipolar + Range.of.the.used.scale.bipolar.unipolar_bipolar + Symmetry.of.response.scale_asymmetric + 
              Symmetry.of.response.scale_symmetric + Neutral.category_present + Neutral.category_notPresent + Number.of.fixed.reference.points_number + Don.t.know.option_present + 
              Don.t.know.option_onlyRegistered + Don.t.know.option_notPresent + Interviewer.instruction + Respondent.instruction + Extra.information.or.definition + 
              Knowledge.provided_noExtraInfo + Knowledge.provided_definitionsOnly + Knowledge.provided_otherExplanations + Knowledge.provided_bothDefAndExpl + Introduction.available. + 
              Request.present.in.the.introduction_notPresent + Request.present.in.the.introduction_present + Number.of.sentences.in.introduction_number + Number.of.words.in.introduction_number + 
              Number.of.subordinated.clauses.in.introduction_number + Number.of.sentences.in.the.request + Number.of.words.in.request + Total.number.of.nouns.in.request.for.an.answer + 
              Total.number.of.abstract.nouns.in.request.for.an.answer + Total.number.of.syllables.in.request + Number.of.subordinate.clauses.in.request_number + Number.of.syllables.in.answer.scale + 
              Total.number.of.nouns.in.answer.scale + Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used + Horizontal.or.vertical.scale_horizontal + 
              Horizontal.or.vertical.scale_vertical + Overlap.of.scale.labels.and.categories_present + Overlap.of.scale.labels.and.categories_notPresent + 
              Numbers.or.letters.before.the.answer.categories_numbers + Numbers.or.letters.before.the.answer.categories_letters, data = DF)
summary(limo1)          
ols_step_all_possible(limo1)  ### does not work, too many variables, hence too big
forw <- ols_step_forward_p(limo1)
back <- ols_step_backward_p(limo1)
both <- ols_step_both_p(limo1)

#### second model - linear model - way 2 by including the nested variables only as interaction terms   ###############################
#### here the original data  is used (not Gifi) here called DF_original

source("C:/Users/mvett/Desktop/Studium_Statistik/SS22/Consulting-Projekt/R_dateien_und_co/Aufbereitung_factors.R")
## Regression model - multi level (with gamm4 package)

library(mgcv)
library(gamm4)
library(lme4)
library(glmmTMB)
library(glmmLasso)
library(glmer)
library(brms)
library(GLMMadaptive)

### lme with gifi data

### removing na rows
NA_rows<- which(!complete.cases(DF))
DF <- DF[-NA_rows,]
DF <- DF[,!names(DF) %in% "X.WH..word_why", drop = F]

### lme model
lmer_question2 <- lmer(quality ~ Language + Domain + Concept + Social.Desirability + Centrality + Reference.period +
                         Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + 
                         WH.word.used.in.the.request_without + Request.for.an.answer.type_Declar. + Request.for.an.answer.type_Imper. + 
                         Request.for.an.answer.type_Inter. + Request.for.an.answer.type_None + Use.of.gradation_No + 
                         Use.of.gradation_Yes + Balance.of.the.request_Balanced + Balance.of.the.request_Unbalanced + 
                         Presence.of.encouragement.to.answer_No + Presence.of.encouragement.to.answer_Yes + 
                         Emphasis.on.subjective.opinion.in.request_No + Emphasis.on.subjective.opinion.in.request_Yes + 
                         Use.of.stimulus.or.statement.in.the.request + Absolute.or.comparative.judgment + Response.scale..basic.choice + 
                         Number.of.categories + Theoretical.range.of.the.concept.bipolar.unipolar_bipolar + 
                         Theoretical.range.of.the.concept.bipolar.unipolar_unipolar + Range.of.the.used.scale.bipolar.unipolar_Bipolar + 
                         Range.of.the.used.scale.bipolar.unipolar_Unipolar + Symmetry.of.response.scale_Asymmetric + 
                         Symmetry.of.response.scale_Symmetric + Neutral.category_Not.present + Neutral.category_Present + 
                         Number.of.fixed.reference.points + Don.t.know.option + Interviewer.instruction + Respondent.instruction + 
                         Extra.information.or.definition + Knowledge.provided_Definitions + Knowledge.provided_Other + 
                         Knowledge.provided_No + Knowledge.provided_def..and.other + Introduction.available. + 
                         Request.present.in.the.introduction_present + Request.present.in.the.introduction_not.present + 
                         Number.of.sentences.in.introduction + Number.of.words.in.introduction + Number.of.sentences.in.introduction +
                         Number.of.sentences.in.the.request + Number.of.words.in.request + Total.number.of.nouns.in.request.for.an.answer + 
                         Total.number.of.abstract.nouns.in.request.for.an.answer + Total.number.of.syllables.in.request + 
                         Number.of.subordinate.clauses.in.request + Number.of.syllables.in.answer.scale + Total.number.of.nouns.in.answer.scale + 
                         Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used +
                         Horizontal.or.vertical.scale_Horizontal + Horizontal.or.vertical.scale_Vertical + 
                         Overlap.of.scale.labels.and.categories_clearly.connected + Overlap.of.scale.labels.and.categories_Overlap.present + 
                         Numbers.or.letters.before.the.answer.categories_Neither + Numbers.or.letters.before.the.answer.categories_Numbers + 
                         Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes + Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers + 
                         Start.of.the.response.sentence.on.the.visual.aid_No + Start.of.the.response.sentence.on.the.visual.aid_Yes + 
                         Request.on.the.visual.aid_No + Request.on.the.visual.aid_Yes + Picture.provided._No + Picture.provided._Yes + 
                         Computer.assisted + Interviewer + Visual.or.oral.presentation + Position + (1|Study),family = "binomial", nAGQ = 2,data = df,na.action = na.exclude)

summary(lmer_question2)


#### list of variables that got kicked out: 
### 1.) WH.word.used.in.the.request_without
### 2.) Request for an answer type_none
### 3.) Balance of the request_unbalanced
### 4.) Presence of encouragement to answer_yes
### 5.) emphasis on subjective opinion_yes
### 6.) Theoretical.range.of.the.concept.bipolar.unipolar_unipolar
### 7.) Range.of.the.used.scale.bipolar.unipolar_Unipolar
### 8.) Symmetry.of.response.scale_Symmetric
### 9.) Neutral.category_Present
### 10.) Request.present.in.the.introduction_not.present
### 11.) Horizontal.or.vertical.scale_Vertical
### 12.) Overlap.of.scale.labels.and.categories_Overlap.present
### 13.) Numbers.or.letters.before.the.answer.categories_Numbers
### 14.) Start.of.the.response.sentence.on.the.visual.aid_Yes
### 15.)Picture.provided._Yes

### trying the glmmLasso package for variable selection in glm context

glmer_question1 <- glmer(quality ~ Domain + Concept + Social.Desirability + Centrality + Reference.period +
                           Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + 
                           Request.for.an.answer.type_Declar. + Request.for.an.answer.type_Imper. + 
                           Request.for.an.answer.type_Inter. + Use.of.gradation_No + 
                           Use.of.gradation_Yes + Balance.of.the.request_Balanced + 
                           Presence.of.encouragement.to.answer_No + 
                           Emphasis.on.subjective.opinion.in.request_No + 
                           Use.of.stimulus.or.statement.in.the.request + Absolute.or.comparative.judgment + Response.scale..basic.choice + 
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
                           Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used +
                           Horizontal.or.vertical.scale_Horizontal + 
                           Overlap.of.scale.labels.and.categories_clearly.connected + 
                           Numbers.or.letters.before.the.answer.categories_Neither + 
                           Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes + Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers + 
                           Start.of.the.response.sentence.on.the.visual.aid_No + 
                           Request.on.the.visual.aid_No + Request.on.the.visual.aid_Yes + Picture.provided._No + 
                           Computer.assisted + Interviewer + Visual.or.oral.presentation + Position + (1|Study),family = binomial(link = "logit"),data = df)
#### produces following error message :Warning messages:
###1: In eval(family$initialize, rho) :
###  non-integer #successes in a binomial glm!
###2: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
###failure to converge in 10000 evaluations
### 3: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
###convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
###4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
### Model failed to converge with max|grad| = 50.0554 (tol = 0.002, component 1)
###5: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
###Model is nearly unidentifiable: very large eigenvalue
### - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
### - Rescale variables?

summary(glmer_question1)



#### beta distribution with glmmTMB

glmmTMB_question1 <- glmmTMB(quality ~ Domain + Concept + Social.Desirability + Centrality + Reference.period +
                               Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + 
                               Request.for.an.answer.type_Declar. + Request.for.an.answer.type_Imper. + 
                               Request.for.an.answer.type_Inter. + Use.of.gradation_No + 
                               Use.of.gradation_Yes + Balance.of.the.request_Balanced + 
                               Presence.of.encouragement.to.answer_No + 
                               Emphasis.on.subjective.opinion.in.request_No + 
                               Use.of.stimulus.or.statement.in.the.request + Absolute.or.comparative.judgment + Response.scale..basic.choice + 
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
                               Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used +
                               Horizontal.or.vertical.scale_Horizontal + 
                               Overlap.of.scale.labels.and.categories_clearly.connected + 
                               Numbers.or.letters.before.the.answer.categories_Neither + 
                               Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes + Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers + 
                               Start.of.the.response.sentence.on.the.visual.aid_No + 
                               Request.on.the.visual.aid_No + Request.on.the.visual.aid_Yes + Picture.provided._No + 
                               Computer.assisted + Interviewer + Visual.or.oral.presentation + Position + (1|Language/Study),family = beta_family(link = "logit"),control = glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), profile = TRUE, collect = FALSE), data = df)


##### GLMM mit GLMMadaptive package. Funktionier an sich gut, problematisch ist leider dass man keine 3-level random effects einfügen kann.

GLMMadaptive_question1 <- mixed_model(quality ~ Domain + Concept + Social.Desirability + Centrality + Reference.period +
                                        Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + 
                                        Request.for.an.answer.type_Declar. + Request.for.an.answer.type_Imper. + 
                                        Request.for.an.answer.type_Inter. + Use.of.gradation_No + 
                                        Use.of.gradation_Yes + Balance.of.the.request_Balanced + 
                                        Presence.of.encouragement.to.answer_No + 
                                        Emphasis.on.subjective.opinion.in.request_No + 
                                        Use.of.stimulus.or.statement.in.the.request + Absolute.or.comparative.judgment + Response.scale..basic.choice + 
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
                                        Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used +
                                        Horizontal.or.vertical.scale_Horizontal + 
                                        Overlap.of.scale.labels.and.categories_clearly.connected + 
                                        Numbers.or.letters.before.the.answer.categories_Neither + 
                                        Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes + Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers + 
                                        Start.of.the.response.sentence.on.the.visual.aid_No + 
                                        Request.on.the.visual.aid_No + Request.on.the.visual.aid_Yes + Picture.provided._No + 
                                        Computer.assisted + Interviewer + Visual.or.oral.presentation + Position,random = ~1 | Study,family = beta.fam(),iter_EM = 0, optimizer = "nlminb",initial_values = list(betas = GLMMadaptive_question1$coefficients, D = matrix(c(0.5, 0, 0, 0.1), 2, 2)),iter_qN_incr = 20,nAGQ = 21, data = df)


### Hier schaue ich nach wie gut die Koeffizienten konvergieren, in dem ich die Anzahl der Quadraturpunkte variere 

GLMMadaptive_question2 <- update(GLMMadaptive_question1,nAGQ = 11)
GLMMadaptive_question3 <- update(GLMMadaptive_question2, nAGQ = 15)
GLMMadaptive_question4 <- update(GLMMadaptive_question2, nAGQ = 28)
models <- list("nAGQ-11 "=GLMMadaptive_question2,"nAGQ-15"=GLMMadaptive_question3,"nAGQ-21" = GLMMadaptive_question1,"nAGQ-28" = GLMMadaptive_question4)

## Die Funktion gibt die Varianz und Likelihood für ein Model zurück (zum Modelvergleich)
extract <- function(obj){
  c(fixef(obj),"var_(intercept)" = obj$D[1, 1], "logLik"= logLik(obj))
}

vergleich_glmm_adaptive <- sapply(models,extract)

summ_glmmadaptive <- summary(GLMMadaptive_question1)

### getting the siginificant covariates
length(summ_glmmadaptive$coef_table[which(summ_glmmadaptive$coef_table[,4] < 0.05),])

### Hier versuche ich den Unterschied der Koeffizienten  zu bestimmen
model_differences <- (fixef(glmmTMB_question1)$cond - GLMMadaptive_question1$coefficients)/fixef(glmmTMB_question1)$cond
comparison <- cbind(fixef(glmmTMB_question1)$cond,GLMMadaptive_question1$coefficients)

##### trying the DHARMA package
library(DHARMa)
Resi_dharma <- simulateResiduals((glmmTMB_question1))
plot(Resi_dharma)

###look at residuals per group
Resi_dharma_groups = recalculateResiduals(Resi_dharma, group = df$Study)
plot(Resi_dharma_groups)
###Dispersion test
test_disp <- testDispersion(glmmTMB_question1)
testUniformity(Resi_dharma)



fitted_glmmTMB <- exp(predict(glmmTMB_question1,df))/(1+predict(glmmTMB_question1,df))
pearson_res_TMB <- residuals(glmmTMB_question1, type = c("response","pearson","working"))
deviance_res_TMB <- residuals(glmmTMB_question1, type = "deviance")
plot(res_TMB ~ fitted_glmmTMB)
plot(res_TMB ~ c(1:6074))
length(1:res_TMB)
sum_glmmTMB <- summary(glmmTMB_question1)
p_values <- sum_glmmTMB$coefficients$cond
p_values$
  ### glmmLasso
  
  glmmLasso_question1 <- glmmLasso(quality ~ Domain + Concept + Social.Desirability + Centrality + Reference.period +
                                     Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + 
                                     Request.for.an.answer.type_Declar. + Request.for.an.answer.type_Imper. + 
                                     Request.for.an.answer.type_Inter. + Use.of.gradation_No + 
                                     Use.of.gradation_Yes + Balance.of.the.request_Balanced + 
                                     Presence.of.encouragement.to.answer_No + 
                                     Emphasis.on.subjective.opinion.in.request_No + 
                                     Use.of.stimulus.or.statement.in.the.request + Absolute.or.comparative.judgment + Response.scale..basic.choice + 
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
                                     Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used +
                                     Horizontal.or.vertical.scale_Horizontal + 
                                     Overlap.of.scale.labels.and.categories_clearly.connected + 
                                     Numbers.or.letters.before.the.answer.categories_Neither + 
                                     Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes + Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers + 
                                     Start.of.the.response.sentence.on.the.visual.aid_No + 
                                     Request.on.the.visual.aid_No + Request.on.the.visual.aid_Yes + Picture.provided._No + 
                                     Computer.assisted + Interviewer + Visual.or.oral.presentation + Position,rnd = list(Study = ~1),lambda = 3,family = beta_family(),data = df)
### produces following warning/error:Error in est.glmmLasso(fix, rnd, data = data, lambda = lambda, family = family,  : 
###argument "lambda" is missing, with no default
###In addition: Warning messages:
###  1: In split.default((1:ncol(X))[-inotpen.which], ipen) :
###  data length is not a multiple of split variable
###2: In est.glmmLasso.RE(fix = fix, rnd = rnd, data = data, lambda = lambda,  :
###                         Cluster variable should be specified as a factor variable!


summary(glmmLasso_question1)


##### here I am trying to deal with the error message in the models: " Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
##### Kontraste können nur auf Faktoren mit 2 oder mehr Stufen angewendet werden""
einsen <- which(df$quality==1)
nullen <- which(df$quality ==0)
df$quality <- ifelse(df$quality ==1,0.9999999,df$quality)
df$quality <- ifelse(df$quality ==0,0.000000001,df$quality)

######## getting counts of the groups 
nests <- DF %>% count(study,experiment)

