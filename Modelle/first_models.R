############# Modelle ################

library(stringr)
library(dplyr)
library(data.table)
library(readxl)

#### einlesen von modifizierten Daten (GIFI) #####


DF <- read.csv2("Gifi-Datensatz.csv")

####### lineares Model

##### switching numerical to factors

num_cols2            <- unlist(lapply(DF,is.numeric))

new_data2             <- DF
new_data2[,num_cols2] <- lapply(DF[,num_cols2], factor)

#### checking if it worked #######

classes_question2 <- lapply(new_data2,class)

### creating vector of predictor variables ###

predictors <- colnames(DF)[11:110]

pred_formula <- paste(predictors, collapse = " + ")


#### first model - linear model - way 1 by the Gifi-System

limo1 <- lm(quality.r.2. ~ Domain + Concept + Concept..other.simple.concepts_TRUE + Concept..complex.concept_importOfAJudgement + Concept..complex.concept_CertaintyOfAJudgement + Concept..complex.concept_Other + Social.Desirability + Centrality + Reference.period + Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + WH.word.used.in.the.request_notused + X.WH..word_who + X.WH..word_which + X.WH..word_what + X.WH..word_whentime + X.WH..word_whereplace + X.WH..word_howProcedure + X.WH..word_howRelationship + X.WH..word_howOpinion + X.WH..word_howQuantity + X.WH..word_howExtremity + X.WH..word_how_Intensity + X.WH..word_why + Request.for.an.answer.type_interrogative + Request.for.an.answer.type_imperative + Request.for.an.answer.type_declarative + Request.for.an.answer.type_NoneOfThese + Use.of.gradation_nogradation + Use.of.gradation_gradation + Balance.of.the.request_balanced + Balance.of.the.request_unbalanced + Presence.of.encouragement.to.answer_noEncour + Presence.of.encouragement.to.answer_Encour + Emphasis.on.subjective.opinion.in.request_noEmphasis + Emphasis.on.subjective.opinion.in.request_emphasis + Information.about.the.opinion.of.other.people_noInfo + Information.about.the.opinion.of.other.people_info + Use.of.stimulus.or.statement.in.the.request_notUsed + Use.of.stimulus.or.statement.in.the.request_used + Absolute.or.comparative.judgment + Response.scale..basic.choice_morethan2cat + Response.scale..basic.choice_twoCatScale + Response.scale..basic.choice_openEnded + Response.scale..basic.choice_notAvailableMagnEst + Response.scale..basic.choice_NotavailableLineProd + Response.scale..basic.choice_MoreStepsProcedure + Number.of.categories_number + Labels.of.categories_noLabels + Labels.of.categories_partiallyLabeled + Labels.of.categories_fully_Labeled + Labels.with.short.text.or.complete.sentences_shortText + Labels.with.short.text.or.complete.sentences_completeSentences + Order.of.the.labels_firstNegativeOrNonApplicable + Order.of.the.labels_firstPositive + Correspondence.between.labels.and.numbers.of.the.scale_highCorrespondence + Correspondence.between.labels.and.numbers.of.the.scale_mediumCorrespondence + Correspondence.between.labels.and.numbers.of.the.scale_lowCorrespondence + Correspondence.between.labels.and.numbers.of.the.scale_notApplicable + Theoretical.range.of.the.concept.bipolar.unipolar_unipolar + Theoretical.range.of.the.concept.bipolar.unipolar_bipolar + Range.of.the.used.scale.bipolar.unipolar_unipolar + Range.of.the.used.scale.bipolar.unipolar_bipolar + Symmetry.of.response.scale_asymmetric + Symmetry.of.response.scale_symmetric + Neutral.category_present + Neutral.category_notPresent + Number.of.fixed.reference.points_number + Don.t.know.option_present + Don.t.know.option_onlyRegistered + Don.t.know.option_notPresent + Interviewer.instruction + Respondent.instruction + Extra.information.or.definition + Knowledge.provided_noExtraInfo + Knowledge.provided_definitionsOnly + Knowledge.provided_otherExplanations + Knowledge.provided_bothDefAndExpl + Introduction.available. + Request.present.in.the.introduction_notPresent + Request.present.in.the.introduction_present + Number.of.sentences.in.introduction_number + Number.of.words.in.introduction_number + Number.of.subordinated.clauses.in.introduction_number + Number.of.sentences.in.the.request + Number.of.words.in.request + Total.number.of.nouns.in.request.for.an.answer + Total.number.of.abstract.nouns.in.request.for.an.answer + Total.number.of.syllables.in.request + Number.of.subordinate.clauses.in.request_number + Number.of.syllables.in.answer.scale + Total.number.of.nouns.in.answer.scale + Total.number.of.abstract.nouns.in.answer.scale + Showcard.or.other.visual.aids.used + Horizontal.or.vertical.scale_horizontal + Horizontal.or.vertical.scale_vertical + Overlap.of.scale.labels.and.categories_present + Overlap.of.scale.labels.and.categories_notPresent + Numbers.or.letters.before.the.answer.categories_numbers + Numbers.or.letters.before.the.answer.categories_letters, data = DF
            
            
            
#####################    linear model - way 2 by including the nested variables only as interaction terms   ###############################




DF_original <- read_excel("updated_data1706.xlsx")

DF_original <- question

write.csv2(DF_original, "actual.data.csv")

DF_original <- DF_original %>% mutate(quality.r.2. = `reliability(r^2)`*`validity(v^2)`)

### getting the class distribution of the datase

classes_question <- lapply(new_data,class)

### changing all numerical variables to factors and save that new data.frame in new_data

num_cols   <- unlist(lapply(DF_original,is.numeric))

new_data <- DF_original
new_data[,num_cols] <- lapply(DF_original[,num_cols], factor)







## select non-nested variables

preds <- NULL
for (i in 4:length(colnames(DF_original))){
  
  if (sum(is.na(DF_original[,i])) == 0)
    
  preds = append(preds,colnames(DF_original[,i]))
}
### create regression formula
preds_formula <- paste(preds, collapse = " + ")
DF_original$`Use of gradation`


limo2 <- lm(quality.r.2. ~  Language + Concept + `Social Desirability` + Centrality + `Reference period` + `Formulation of the request for an answer: basic choice` + `Formulation of the request for an answer: basic choice`:`WH word used in the request` + 
                            `Formulation of the request for an answer: basic choice`:`Request for an answer type` + `Formulation of the request for an answer: basic choice`:`Use of gradation` + 
                            `Formulation of the request for an answer: basic choice`:`Balance of the request` + `Formulation of the request for an answer: basic choice`:`Presence of encouragement to answer` + `Formulation of the request for an answer: basic choice`:`Emphasis on subjective opinion in request` + 
                            `Formulation of the request for an answer: basic choice`:`Information about the opinion of other people` + `Absolute or comparative judgment` + `Response scale: basic choice` + `Response scale: basic choice`:`Number of categories` + 
                            `Response scale: basic choice`:`Labels of categories` + `Response scale: basic choice`:`Labels with short text or complete sentences` + `Response scale: basic choice`:`Order of the labels` + 
                            `Response scale: basic choice`:`Correspondence between labels and numbers of the scale` + `Response scale: basic choice`:`Theoretical range of the concept bipolar/unipolar` + `Response scale: basic choice`:`Range of the used scale bipolar/unipolar` +  
                            `Response scale: basic choice`:`Symmetry of response scale` + `Response scale: basic choice`:`Neutral category`  + `Response scale: basic choice`:`Number of fixed reference points` +  `Don't know option` + `Interviewer instruction` + `Respondent instruction` + 
                            `Extra information or definition` + `Extra information or definition`:`Knowledge provided` + `Introduction available?` + `Introduction available?`:`Request present in the introduction` + 
                            `Introduction available?`:`Number of sentences in introduction` + `Introduction available?`:`Number of words in introduction` + `Introduction available?`:`Number of subordinated clauses in introduction` +
                            `Number of sentences in the request` + `Number of words in request` + `Total number of nouns in request for an answer` + `Total number of abstract nouns in request for an answer` + `Total number of syllables in request` + 
                            `Number of subordinate clauses in request` + `Number of syllables in answer scale` + `Total number of nouns in answer scale` + `Total number of abstract nouns in answer scale` + `Showcard or other visual aids used` + 
                            `Showcard or other visual aids used`:`Horizontal or vertical scale` + `Showcard or other visual aids used`:`Horizontal or vertical scale`:`Overlap of scale labels and categories` + 
                            `Showcard or other visual aids used`:`Numbers or letters before the answer categories` + `Showcard or other visual aids used`:`Scale with only numbers or numbers in boxes` + 
                            `Showcard or other visual aids used`:`Start of the response sentence on the visual aid` + `Showcard or other visual aids used`:`Request on the visual aid` + `Computer assisted` + 
                            `Showcard or other visual aids used`:`Picture provided?` + `Visual or oral presentation` + Position, data = DF_original)
summary(limo2)



#############           Generalized additive model with mgcv   #####################


library(mgcv)

gam_question <- gamm(quality.r.2. ~  Language + Concept + `Social Desirability` + Centrality + `Reference period` + `Formulation of the request for an answer: basic choice` + 
                       `Formulation of the request for an answer: basic choice`:`WH word used in the request` + 
                       `Formulation of the request for an answer: basic choice`:`Request for an answer type` + `Formulation of the request for an answer: basic choice`:`Use of gradation` + 
                       `Formulation of the request for an answer: basic choice`:`Balance of the request` + `Formulation of the request for an answer: basic choice`:`Presence of encouragement to answer` + 
                       `Formulation of the request for an answer: basic choice`:`Emphasis on subjective opinion in request` + 
                       `Formulation of the request for an answer: basic choice`:`Information about the opinion of other people` + `Absolute or comparative judgment` + `Response scale: basic choice` + 
                       `Response scale: basic choice`:`Number of categories` + `Response scale: basic choice`:`Labels of categories` + `Response scale: basic choice`:`Labels with short text or complete sentences` + 
                       `Response scale: basic choice`:`Order of the labels` + `Response scale: basic choice`:`Correspondence between labels and numbers of the scale` + 
                       `Response scale: basic choice`:`Theoretical range of the concept bipolar/unipolar` + `Response scale: basic choice`:`Range of the used scale bipolar/unipolar` +  
                       `Response scale: basic choice`:`Symmetry of response scale` + `Response scale: basic choice`:`Neutral category`  + `Response scale: basic choice`:`Number of fixed reference points` +  
                       `Don't know option` + `Interviewer instruction` + `Respondent instruction` + `Extra information or definition` + `Extra information or definition`:`Knowledge provided` + 
                       `Introduction available?` + `Introduction available?`:`Request present in the introduction` + `Introduction available?`:`Number of sentences in introduction` + 
                       `Introduction available?`:`Number of words in introduction` + `Introduction available?`:`Number of subordinated clauses in introduction` +
                       `Number of sentences in the request` + `Number of words in request` + `Total number of nouns in request for an answer` + `Total number of abstract nouns in request for an answer` + `Total number of syllables in request` + 
                       `Number of subordinate clauses in request` + `Number of syllables in answer scale` + `Total number of nouns in answer scale` + `Total number of abstract nouns in answer scale` + `Showcard or other visual aids used` + 
                       `Showcard or other visual aids used`:`Horizontal or vertical scale` + `Showcard or other visual aids used`:`Horizontal or vertical scale`:`Overlap of scale labels and categories` + 
                       `Showcard or other visual aids used`:`Numbers or letters before the answer categories` + `Showcard or other visual aids used`:`Scale with only numbers or numbers in boxes` + 
                       `Showcard or other visual aids used`:`Start of the response sentence on the visual aid` + `Showcard or other visual aids used`:`Request on the visual aid` + `Computer assisted` + 
                       `Showcard or other visual aids used`:`Picture provided?` + `Visual or oral presentation` + Position, 
                        random = ,data = DF_original)
)
