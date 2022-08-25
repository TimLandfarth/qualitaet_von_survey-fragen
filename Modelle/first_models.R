############# Modelle ################

library(stringr)
library(dplyr)
library(data.table)
library(readxl)
###### Hier werden 2 Datensätze eingelesen und modifiziert. Einmal der Gifi Datensatz bei dem alle Labels eine eigene binäre Variable wurden und der "normale" Datensatz


#### einlesen von modifizierten Daten (GIFI) #####


DF <- read.csv2("Gifi-Datensatz2.csv")
#### first model - linear model - way 1 by the Gifi-System

limo1 <- lm(quality.r.2. ~ Domain + Concept + Concept..other.simple.concepts_TRUE + Concept..complex.concept_importOfAJudgement + 
              Concept..complex.concept_CertaintyOfAJudgement + Concept..complex.concept_Other + Social.Desirability + Centrality + Reference.period + 
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


#### second model - linear model - way 2 by including the nested variables only as interaction terms   ###############################

#### here the original data  is used (not Gifi) here called DF_original



DF_original <- read_excel("actual.data.csv")
DF_original <- question #### I use question for the prep file of Barbara for getting the experiment variable

### DF_nospaces: to make it easier to deal with I made a data frame without spaces and other signs in the name

DF_nospaces <- read.csv2("actual.data_nospaces.csv")
write.csv2(DF_nospaces, file = "actual.data_nospaces.csv")
## Regression model - linear   /// leave "information about the opinion about other people out (only one level)


limo2 <- lm(quality~ Language+Concept+Social.Desirability+Centrality+Reference.period+
              Formulation.of.the.request.for.an.answer..basic.choice+
              Formulation.of.the.request.for.an.answer..basic.choice:WH.word.used.in.the.request+
              Formulation.of.the.request.for.an.answer..basic.choice:Request.for.an.answer.type+
              Formulation.of.the.request.for.an.answer..basic.choice:Use.of.gradation+
              Formulation.of.the.request.for.an.answer..basic.choice:Balance.of.the.request+
              Formulation.of.the.request.for.an.answer..basic.choice:Presence.of.encouragement.to.answer+
              Formulation.of.the.request.for.an.answer..basic.choice:Emphasis.on.subjective.opinion.in.request+
              Absolute.or.comparative.judgment+
              Response.scale..basic.choice+Response.scale..basic.choice:Number.of.categories+
              Response.scale..basic.choice:Labels.of.categories+
              Response.scale..basic.choice:Labels.with.short.text.or.complete.sentences+
              Response.scale..basic.choice:Order.of.the.labels+
              Response.scale..basic.choice:Correspondence.between.labels.and.numbers.of.the.scale+
              Response.scale..basic.choice:Theoretical.range.of.the.concept.bipolar.unipolar+
              Response.scale..basic.choice:Range.of.the.used.scale.bipolar.unipolar+
              Response.scale..basic.choice:Symmetry.of.response.scale+Response.scale..basic.choice:Neutral.category+
              Response.scale..basic.choice:Number.of.fixed.reference.points+
              Don.t.know.option+Interviewer.instruction+Respondent.instruction+
              Extra.information.or.definition+Extra.information.or.definition:Knowledge.provided+
              Introduction.available.+Introduction.available.:Request.present.in.the.introduction+
              Introduction.available.:Number.of.sentences.in.introduction+Introduction.available.:Number.of.words.in.introduction+
              Introduction.available.:Number.of.subordinated.clauses.in.introduction +
              Number.of.sentences.in.the.request+Number.of.words.in.request+Total.number.of.nouns.in.request.for.an.answer+
              Total.number.of.abstract.nouns.in.request.for.an.answer+Total.number.of.syllables.in.request+
              Number.of.subordinate.clauses.in.request+Number.of.syllables.in.answer.scale+Total.number.of.nouns.in.answer.scale+
              Total.number.of.abstract.nouns.in.answer.scale+
              Showcard.or.other.visual.aids.used+
              Showcard.or.other.visual.aids.used:Horizontal.or.vertical.scale+
              Showcard.or.other.visual.aids.used:Overlap.of.scale.labels.and.categories+
              Showcard.or.other.visual.aids.used:Numbers.or.letters.before.the.answer.categories+
              Showcard.or.other.visual.aids.used:Scale.with.only.numbers.or.numbers.in.boxes+
              Showcard.or.other.visual.aids.used:Start.of.the.response.sentence.on.the.visual.aid+
              Showcard.or.other.visual.aids.used:Request.on.the.visual.aid+Computer.assisted+
              Showcard.or.other.visual.aids.used:Picture.provided.+
              Visual.or.oral.presentation+Position, 
            data = DF_nospaces)
summary(limo2)



## Regression model - multi level (with gamm4 package)

library(mgcv)
library(gamm4)

### lme with normal data

lme_question <- lme(quality ~ Language+Concept+Social.Desirability+Centrality+Reference.period+
                      Formulation.of.the.request.for.an.answer..basic.choice+
                      Formulation.of.the.request.for.an.answer..basic.choice:WH.word.used.in.the.request+
                      Formulation.of.the.request.for.an.answer..basic.choice:Request.for.an.answer.type+
                      Formulation.of.the.request.for.an.answer..basic.choice:Use.of.gradation+
                      Formulation.of.the.request.for.an.answer..basic.choice:Balance.of.the.request+
                      Formulation.of.the.request.for.an.answer..basic.choice:Presence.of.encouragement.to.answer+
                      Formulation.of.the.request.for.an.answer..basic.choice:Emphasis.on.subjective.opinion.in.request+
                      Absolute.or.comparative.judgment+
                      Response.scale..basic.choice+Response.scale..basic.choice:Number.of.categories+
                      Response.scale..basic.choice:Labels.of.categories+
                      Response.scale..basic.choice:Labels.with.short.text.or.complete.sentences+
                      Response.scale..basic.choice:Order.of.the.labels+
                      Response.scale..basic.choice:Correspondence.between.labels.and.numbers.of.the.scale+
                      Response.scale..basic.choice:Theoretical.range.of.the.concept.bipolar.unipolar+
                      Response.scale..basic.choice:Range.of.the.used.scale.bipolar.unipolar+
                      Response.scale..basic.choice:Symmetry.of.response.scale+Response.scale..basic.choice:Neutral.category+
                      Response.scale..basic.choice:Number.of.fixed.reference.points+
                      Don.t.know.option+Interviewer.instruction+Respondent.instruction+
                      Extra.information.or.definition+Extra.information.or.definition:Knowledge.provided+
                      Introduction.available.+Introduction.available.:Request.present.in.the.introduction+
                      Introduction.available.:Number.of.sentences.in.introduction+Introduction.available.:Number.of.words.in.introduction+
                      Introduction.available.:Number.of.subordinated.clauses.in.introduction +
                      Number.of.sentences.in.the.request+Number.of.words.in.request+Total.number.of.nouns.in.request.for.an.answer+
                      Total.number.of.abstract.nouns.in.request.for.an.answer+Total.number.of.syllables.in.request+
                      Number.of.subordinate.clauses.in.request+Number.of.syllables.in.answer.scale+Total.number.of.nouns.in.answer.scale+
                      Total.number.of.abstract.nouns.in.answer.scale+
                      Showcard.or.other.visual.aids.used+
                      Showcard.or.other.visual.aids.used:Horizontal.or.vertical.scale+
                      Showcard.or.other.visual.aids.used:Overlap.of.scale.labels.and.categories+
                      Showcard.or.other.visual.aids.used:Numbers.or.letters.before.the.answer.categories+
                      Showcard.or.other.visual.aids.used:Scale.with.only.numbers.or.numbers.in.boxes+
                      Showcard.or.other.visual.aids.used:Start.of.the.response.sentence.on.the.visual.aid+
                      Showcard.or.other.visual.aids.used:Request.on.the.visual.aid+Computer.assisted+
                      Showcard.or.other.visual.aids.used:Picture.provided.+
                      Visual.or.oral.presentation+Position, 
                    random = ~1|study,data= DF_nospaces,na.action = na.exclude)

### lme with gifi data

### removing na rows
NA_rows<- which(!complete.cases(DF))
DF <- DF[-NA_rows,]
DF <- DF[,!names(DF) %in% "X.WH..word_why", drop = F]

### lme model
lme_question2 <- lme(quality.r.2. ~ Domain + Concept + 
                       Concept..complex.concept_Other + Social.Desirability + Centrality + Reference.period + 
                       Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request_used + WH.word.used.in.the.request_notused + X.WH..word_who + 
                       X.WH..word_which + X.WH..word_what + X.WH..word_whentime + X.WH..word_whereplace + X.WH..word_howProcedure + X.WH..word_howRelationship + 
                       X.WH..word_howOpinion + X.WH..word_howQuantity + X.WH..word_howExtremity + X.WH..word_how_Intensity + Request.for.an.answer.type_interrogative + 
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
                       Numbers.or.letters.before.the.answer.categories_numbers + Numbers.or.letters.before.the.answer.categories_letters,random = ~1|study,data = DF,na.action = na.exclude)

##### here I am trying to deal with the error message in the models: " Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
##### Kontraste können nur auf Faktoren mit 2 oder mehr Stufen angewendet werden""

#### I try to include NA as a level to overcome the problem ---- didn´t work so far

#Check if variables have NAs, in case yes, include NA as a factor level

which(sapply(DF_nospaces, is.factor) & (sapply(DF_nospaces, anyNA)))

DF_nospaces$Picture_provided <- addNA(DF_nospaces$Picture_provided) 
DF_nospaces$WH_word_used_in_the_request <- addNA(DF_nospaces$WH_word_used_in_the_request)
DF_nospaces$Request_for_an_answer_type <- addNA(DF_nospaces$Request_for_an_answer_type)
DF_nospaces$Balance_of_the_request <- addNA(DF_nospaces$Balance_of_the_request)
DF_nospaces$Emphasis_on_subjective_opinion_in_request <- addNA(DF_nospaces$Emphasis_on_subjective_opinion_in_request)
DF_nospaces$Labels_with_short_text_or_complete_sentences <- addNA(DF_nospaces$Labels_with_short_text_or_complete_sentences)
DF_nospaces$Correspondence_between_labels_and_numbers_of_the_scale <- addNA(DF_nospaces$Correspondence_between_labels_and_numbers_of_the_scale)
DF_nospaces$Range_of_the_used_scale_bipolarunipolar <- addNA(DF_nospaces$Range_of_the_used_scale_bipolarunipolar)
DF_nospaces$Neutral_category <- addNA(DF_nospaces$Neutral_category)
DF_nospaces$Request_present_in_the_introduction <- addNA(DF_nospaces$Request_present_in_the_introduction)
DF_nospaces$Number_of_words_in_introduction <- addNA(DF_nospaces$Number_of_words_in_introduction)
DF_nospaces$Horizontal_or_vertical_scale <- addNA(DF_nospaces$Horizontal_or_vertical_scale)
DF_nospaces$Numbers_or_letters_before_the_answer_categories <- addNA(DF_nospaces$Numbers_or_letters_before_the_answer_categories)
DF_nospaces$Start_of_the_response_sentence_on_the_visual_aid <- addNA(DF_nospaces$Start_of_the_response_sentence_on_the_visual_aid)
DF_nospaces$WH_word <- DF_nospaces$addNA(DF_nospaces$WH_word)
DF_nospaces$Use_of_gradation <- addNA(DF_nospaces$Use_of_gradation)
DF_nospaces$Presence_of_encouragement_to_answer <- addNA(DF_nospaces$Presence_of_encouragement_to_answer)
DF_nospaces$Labels_of_categories <- addNA(DF_nospaces$Labels_of_categories)
DF_nospaces$Order_of_the_labels <- addNA(DF_nospaces$Order_of_the_labels)
DF_nospaces$Theoretical_range_of_the_concept_bipolarunipolar <- addNA(DF_nospaces$Theoretical_range_of_the_concept_bipolarunipolar)
DF_nospaces$Symmetry_of_response_scale <- addNA(DF_nospaces$Symmetry_of_response_scale)
DF_nospaces$Knowledge_provided <- addNA(DF_nospaces$Knowledge_provided)
DF_nospaces$Number_of_sentences_in_introduction <- addNA(DF_nospaces$Number_of_sentences_in_introduction)
DF_nospaces$Number_of_subordinated_clauses_in_introduction <- addNA(DF_nospaces$Number_of_subordinated_clauses_in_introduction)
DF_nospaces$Overlap_of_scale_labels_and_categories <- addNA(DF_nospaces$Overlap_of_scale_labels_and_categories)
DF_nospaces$Scale_with_only_numbers_or_numbers_in_boxes <- addNA(DF_nospaces$Scale_with_only_numbers_or_numbers_in_boxes)#
DF_nospaces$Request_on_the_visual_aid <- addNA(DF_nospaces$Request_on_the_visual_aid)

######## subsetting data for the model
nests <- DF %>% count(study,experiment)
unique(DF$study)
subset1 <- DF[which(DF$study=="ESS Round 2"),]

table(subset1$experiment)
