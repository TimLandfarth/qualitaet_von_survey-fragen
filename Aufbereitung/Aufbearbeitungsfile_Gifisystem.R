################# binarizing the filtervariables  ########################

### Ich habe die ganze Filtervariablen in 5 Blöcke unterteilt. Für jede Katergorie einer Variablen wurde eine separate Variable erstellt, inklusive der NA´s.


library(stringr)
library(dplyr)
library(data.table)
library(readxl)

#### Datensatz einlesen 

question <- read_excel("updated_data1706.xlsx") 

##### Domain Variablen und die beiden Maximum Variablen filtern (fast nur NA´s deshalb kommen die raus)
question <- question %>% select(-c(`Domain: national politics`:`Domain: other beliefs`),-`Maximum possible value_75`,-`Maximum possible value_76`)


##### checken wie viele NA´s noch vorhanden sind #########

nonvalues <- as.data.frame(matrix(nrow=1,ncol=159))
colnames(nonvalues) <- colnames(question)

for(i in 1:dim(question)[2]){
  nonvalues[,i] <- sum(is.na(question[,i]))
}

### Ausgabe der Variablen die immernoch NA´s haben

nonvalues <- nonvalues[,which(colSums(nonvalues)>0)]

#### löschen der Zeilen mit NA´s in rel und val

question <- question[which(!is.na(question$`reliability(r^2)`) & !is.na(question$`validity(v^2)`)),]


#### speichern des neuen Dataframes

write.csv2(question, file = "Gifi-Datensatz.csv")


################  Block 1 Concepts    ##############





### Concept: other simple concepts - 2 Kategorien, TRUE und NA


question$`Concept: other simple concepts_TRUE`           <- ifelse(!is.na(question$`Concept: other simple concepts`) & question$`Concept: other simple concepts` == TRUE,1,0)

### relocate new variable 

question <- question %>% relocate(`Concept: other simple concepts_TRUE`, .before = `Concept: other simple concepts`) %>% select(-`Concept: other simple concepts`)

                                                                                                                                  
### Concept:complex concept

question$`Concept: complex concept_importOfAJudgement`    <- ifelse(!is.na(question$`Concept: complex concept`) & question$`Concept: complex concept`=="1",1,0)
question$`Concept: complex concept_CertaintyOfAJudgement` <- ifelse(!is.na(question$`Concept: complex concept`) & question$`Concept: complex concept`=="2",1,0)
question$`Concept: complex concept_Other`                 <- ifelse(!is.na(question$`Concept: complex concept`) & question$`Concept: complex concept`=="3",1,0)

### relocate new variables

question <- question %>% relocate(c(`Concept: complex concept_importOfAJudgement`,`Concept: complex concept_CertaintyOfAJudgement`,`Concept: complex concept_Other`), .before = `Concept: complex concept`) %>%
            select(-`Concept: complex concept`)




###########            Block 2 Formulation of the request             ####################





### WH word used in the request ###

question$`WH word used in the request_used`     <- ifelse(!is.na(question$`WH word used in the request`) & question$`WH word used in the request`== "1",1,0)
question$`WH word used in the request_notused`  <- ifelse(!is.na(question$`WH word used in the request`) & question$`WH word used in the request`== "0",1,0)

### relocate new variables


question <- question %>% relocate(c(`WH word used in the request_used`,`WH word used in the request_notused`), .before = `WH word used in the request`) %>% select(-`WH word used in the request`)


### WH word ###

question$`'WH' word_who`             <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "1",1,0)
question$`'WH' word_which`           <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "2",1,0)
question$`'WH' word_what`            <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "3",1,0)
question$`'WH' word_whentime`        <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "4",1,0)
question$`'WH' word_whereplace`      <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "5",1,0)
question$`'WH' word_howProcedure`    <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "6",1,0)
question$`'WH' word_howRelationship` <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "7",1,0)
question$`'WH' word_howOpinion`      <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "8",1,0)
question$`'WH' word_howQuantity`     <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "9",1,0)
question$`'WH' word_howExtremity`    <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "10",1,0)
question$`'WH' word_how_Intensity`   <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "11",1,0)
question$`'WH' word_why`             <- ifelse(!is.na(question$`'WH' word`) & question$`'WH' word`== "12",1,0)


### relocate new variables


question <- question %>% relocate(c(`'WH' word_who`,`'WH' word_which`,`'WH' word_what`,`'WH' word_whentime`,`'WH' word_whereplace`,`'WH' word_howProcedure`,
                                    `'WH' word_howRelationship`,`'WH' word_howOpinion`,`'WH' word_howQuantity`,`'WH' word_howExtremity`,`'WH' word_how_Intensity`,`'WH' word_why`), .before = `'WH' word`) %>% 
                          select(-`'WH' word`)


### Request for an answer type

question$`Request for an answer type_interrogative` <- ifelse(!is.na(question$`Request for an answer type`) & question$`Request for an answer type`=="1",1,0)
question$`Request for an answer type_imperative`    <- ifelse(!is.na(question$`Request for an answer type`) & question$`Request for an answer type`=="2",1,0)
question$`Request for an answer type_declarative`   <- ifelse(!is.na(question$`Request for an answer type`) & question$`Request for an answer type`=="3",1,0)
question$`Request for an answer type_NoneOfThese`   <- ifelse(!is.na(question$`Request for an answer type`) & question$`Request for an answer type`=="0",1,0)


### relocate new variables

question <-  question %>% relocate(c(`Request for an answer type_interrogative`,`Request for an answer type_imperative`,`Request for an answer type_declarative`,`Request for an answer type_NoneOfThese`), .before = `Request for an answer type`) %>%
              select(-`Request for an answer type`)



### use of gradation ###

question$`Use of gradation_nogradation` <- ifelse(!is.na(question$`Use of gradation`) & question$`Use of gradation`=="0",1,0)
question$`Use of gradation_gradation`   <- ifelse(!is.na(question$`Use of gradation`) & question$`Use of gradation`=="1",1,0)


### relocate new variables

question <- question %>% relocate(c(`Use of gradation_nogradation`,`Use of gradation_gradation`), .before = `Use of gradation`) %>% 
                          select(-`Use of gradation`)



### Balance of the request ###

question$`Balance of the request_balanced`    <- ifelse(!is.na(question$`Balance of the request`) & question$`Balance of the request`=="0",1,0)
question$`Balance of the request_unbalanced`  <- ifelse(!is.na(question$`Balance of the request`) & question$`Balance of the request`=="1",1,0)


### relocate new variables

question <- question %>% relocate(c(`Balance of the request_balanced`,`Balance of the request_unbalanced`), .before = `Balance of the request`) %>% 
                          select(-`Balance of the request`)


### presence of encouragement to answer ###      

question$`Presence of encouragement to answer_noEncour` <- ifelse(!is.na(question$`Presence of encouragement to answer`) & question$`Presence of encouragement to answer`=="0",1,0)
question$`Presence of encouragement to answer_Encour`   <- ifelse(!is.na(question$`Presence of encouragement to answer`) & question$`Presence of encouragement to answer`=="1",1,0)


#### relocate variables

question <- question %>% relocate(c(`Presence of encouragement to answer_noEncour`,`Presence of encouragement to answer_Encour`), .before = `Presence of encouragement to answer`) %>%
                          select(-`Presence of encouragement to answer`)

### Emphasis on subjective opinion in request ###

question$`Emphasis on subjective opinion in request_noEmphasis` <- ifelse(!is.na(question$`Emphasis on subjective opinion in request`) & question$`Emphasis on subjective opinion in request`=="0",1,0)
question$`Emphasis on subjective opinion in request_emphasis`   <- ifelse(!is.na(question$`Emphasis on subjective opinion in request`) & question$`Emphasis on subjective opinion in request`=="1",1,0)


### relocate variables

question <- question %>% relocate(c(`Emphasis on subjective opinion in request_noEmphasis`,`Emphasis on subjective opinion in request_emphasis`), .before = `Emphasis on subjective opinion in request`)  %>%
            select(-`Emphasis on subjective opinion in request`)



### Information about the opinion of other people ###
question$`Information about the opinion of other people_noInfo`<- ifelse(!is.na(question$`Information about the opinion of other people`) & question$`Information about the opinion of other people`=="0",1,0)
question$`Information about the opinion of other people_info`  <- ifelse(!is.na(question$`Information about the opinion of other people`) & question$`Information about the opinion of other people`=="1",1,0)


### relocate variables


question <- question %>% relocate(c(`Information about the opinion of other people_noInfo`,`Information about the opinion of other people_info`), .before = `Information about the opinion of other people`) %>%
            select(-`Information about the opinion of other people`)

### Use of stimulus or statement in the request

question$`Use of stimulus or statement in the request_notUsed`  <- ifelse(!is.na(question$`Use of stimulus or statement in the request`) & question$`Use of stimulus or statement in the request`=="0",1,0)
question$`Use of stimulus or statement in the request_used`     <- ifelse(!is.na(question$`Use of stimulus or statement in the request`) & question$`Use of stimulus or statement in the request`=="1",1,0)


### relocate variables

question <- question %>% relocate(c(`Use of stimulus or statement in the request_notUsed`,`Use of stimulus or statement in the request_used`), .before = `Use of stimulus or statement in the request`) %>%
            select(-`Use of stimulus or statement in the request`)






##############     3. Block  Response Scale       ################






####  Response Scale ####


question$`Response scale: basic choice_morethan2cat`         <- ifelse(!is.na(question$`Response scale: basic choice`) & question$`Response scale: basic choice`=="0",1,0)
question$`Response scale: basic choice_twoCatScale`          <- ifelse(!is.na(question$`Response scale: basic choice`) & question$`Response scale: basic choice`=="1",1,0)
question$`Response scale: basic choice_openEnded`            <- ifelse(!is.na(question$`Response scale: basic choice`) & question$`Response scale: basic choice`=="2",1,0)
question$`Response scale: basic choice_notAvailableMagnEst`  <- ifelse(!is.na(question$`Response scale: basic choice`) & question$`Response scale: basic choice`=="3",1,0)
question$`Response scale: basic choice_NotavailableLineProd` <- ifelse(!is.na(question$`Response scale: basic choice`) & question$`Response scale: basic choice`=="4",1,0)
question$`Response scale: basic choice_MoreStepsProcedure`   <- ifelse(!is.na(question$`Response scale: basic choice`) & question$`Response scale: basic choice`=="5",1,0)

### relocate variables

question <- question %>% relocate(c(`Response scale: basic choice_morethan2cat`,`Response scale: basic choice_twoCatScale`,
                                    `Response scale: basic choice_openEnded`,`Response scale: basic choice_notAvailableMagnEst`,
                                    `Response scale: basic choice_NotavailableLineProd`,`Response scale: basic choice_MoreStepsProcedure`
                                    ), .before = `Response scale: basic choice`) %>% 
                         select(-`Response scale: basic choice`)
            
            
### number of categories

question$`Number of categories_number` <- ifelse(!is.na(question$`Number of categories`),question$`Number of categories`,0)

### relocate variables ####

question <- question %>% relocate(c(`Number of categories_number`), .before = `Number of categories`) %>%
                          select(-`Number of categories`)

#### Labels of categories


question$`Labels of categories_noLabels`         <- ifelse(!is.na(question$`Labels of categories`) & question$`Labels of categories`=="1",1,0)
question$`Labels of categories_partiallyLabeled` <- ifelse(!is.na(question$`Labels of categories`) & question$`Labels of categories`=="2",1,0)
question$`Labels of categories_fully_Labeled`    <- ifelse(!is.na(question$`Labels of categories`) & question$`Labels of categories`=="3",1,0)

### relocate variables

question <- question %>% relocate(c(`Labels of categories_noLabels`,`Labels of categories_partiallyLabeled`,`Labels of categories_fully_Labeled`), .before = `Labels of categories`) %>%
            select(-`Labels of categories`)



#### Labels with short text or complete sentences ####


question$`Labels with short text or complete sentences_shortText`         <- ifelse(!is.na(question$`Labels with short text or complete sentences`) & question$`Labels with short text or complete sentences`=="0",1,0)
question$`Labels with short text or complete sentences_completeSentences`         <- ifelse(!is.na(question$`Labels with short text or complete sentences`) & question$`Labels with short text or complete sentences`=="1",1,0)


### relocate variables


question <- question %>% relocate(c(`Labels with short text or complete sentences_shortText`,`Labels with short text or complete sentences_completeSentences`), .before = `Labels with short text or complete sentences`) %>%
            select(-`Labels with short text or complete sentences`)


#### Order of the labels #####

question$`Order of the labels_firstNegativeOrNonApplicable` <- ifelse(!is.na(question$`Order of the labels`) & question$`Order of the labels`=="1",1,0)
question$`Order of the labels_firstPositive`                <- ifelse(!is.na(question$`Order of the labels`) & question$`Order of the labels`=="2",1,0)


### relocate variables

question <- question %>% relocate(c(`Order of the labels_firstNegativeOrNonApplicable`,`Order of the labels_firstPositive`), .before = `Order of the labels`) %>%
            select(-`Order of the labels`)




##### Correspondence between labels and numbers of the scale #####


question$`Correspondence between labels and numbers of the scale_highCorrespondence`   <- ifelse(!is.na(question$`Correspondence between labels and numbers of the scale`) & question$`Correspondence between labels and numbers of the scale`=="1",1,0)
question$`Correspondence between labels and numbers of the scale_mediumCorrespondence` <- ifelse(!is.na(question$`Correspondence between labels and numbers of the scale`) & question$`Correspondence between labels and numbers of the scale`=="2",1,0)
question$`Correspondence between labels and numbers of the scale_lowCorrespondence`    <- ifelse(!is.na(question$`Correspondence between labels and numbers of the scale`) & question$`Correspondence between labels and numbers of the scale`=="3",1,0)
question$`Correspondence between labels and numbers of the scale_notApplicable`        <- ifelse(!is.na(question$`Correspondence between labels and numbers of the scale`) & question$`Correspondence between labels and numbers of the scale`=="0",1,0)


### relocate variables

question <- question %>% relocate(c(`Correspondence between labels and numbers of the scale_highCorrespondence`,`Correspondence between labels and numbers of the scale_mediumCorrespondence`,
                                    `Correspondence between labels and numbers of the scale_lowCorrespondence`,`Correspondence between labels and numbers of the scale_notApplicable`
                                    ), .before = `Correspondence between labels and numbers of the scale`) %>%
                                    select(-`Correspondence between labels and numbers of the scale`)



#### Theoretical range of the concept bipolar/unipolar ####


question$`Theoretical range of the concept bipolar/unipolar_unipolar` <- ifelse(!is.na(question$`Theoretical range of the concept bipolar/unipolar`) & question$`Theoretical range of the concept bipolar/unipolar`=="0",1,0)
question$`Theoretical range of the concept bipolar/unipolar_bipolar`  <- ifelse(!is.na(question$`Theoretical range of the concept bipolar/unipolar`) & question$`Theoretical range of the concept bipolar/unipolar`=="1",1,0)

### relocate variables

question <- question %>% relocate(c(`Theoretical range of the concept bipolar/unipolar_unipolar`,`Theoretical range of the concept bipolar/unipolar_bipolar`
                                    ), .before = `Theoretical range of the concept bipolar/unipolar`) %>%
                                    select(-`Theoretical range of the concept bipolar/unipolar`)



#### Range of the used scale bipolar/unipolar #####

question$`Range of the used scale bipolar/unipolar_unipolar` <- ifelse(!is.na(question$`Range of the used scale bipolar/unipolar`) & question$`Range of the used scale bipolar/unipolar`=="0",1,0)
question$`Range of the used scale bipolar/unipolar_bipolar`  <- ifelse(!is.na(question$`Range of the used scale bipolar/unipolar`) & question$`Range of the used scale bipolar/unipolar`=="1",1,0)

### relocate variables

question <- question %>% relocate(c(`Range of the used scale bipolar/unipolar_unipolar`,`Range of the used scale bipolar/unipolar_bipolar`
                                    ), .before = `Range of the used scale bipolar/unipolar`) %>%
                                    select(-`Range of the used scale bipolar/unipolar`)



#### Symmetrie of response scale ####

question$`Symmetry of response scale_asymmetric` <- ifelse(!is.na(question$`Symmetry of response scale`) & question$`Symmetry of response scale`=="0",1,0)
question$`Symmetry of response scale_symmetric`  <- ifelse(!is.na(question$`Symmetry of response scale`) & question$`Symmetry of response scale`=="1",1,0)

### relocate variables

question <- question %>% relocate(c(`Symmetry of response scale_asymmetric`,`Symmetry of response scale_symmetric`
                                    ), .before = `Symmetry of response scale`) %>%
                                    select(-`Symmetry of response scale`)


##### Neutral Category #####

question$`Neutral category_present`    <- ifelse(!is.na(question$`Neutral category`) & question$`Neutral category`=="1",1,0)
question$`Neutral category_notPresent` <- ifelse(!is.na(question$`Neutral category`) & question$`Neutral category`=="3",1,0)

### relocate variables

question <- question %>% relocate(c(`Neutral category_present`,`Neutral category_notPresent`), .before = `Neutral category`) %>%
                          select(-`Neutral category`)


### number of fixed reference points ####

question$`Number of fixed reference points_number` <- ifelse(!is.na(question$`Number of fixed reference points`),question$`Number of fixed reference points`,0)


### relocate variables

question <- question %>% relocate(c(`Number of fixed reference points_number`), .before = `Number of fixed reference points`) %>%
            select(-`Number of fixed reference points`)


#### Dont know option #####


question$`Don't know option_present`        <- ifelse(!is.na(question$`Don't know option`) & question$`Don't know option`=="1",1,0)
question$`Don't know option_onlyRegistered` <- ifelse(!is.na(question$`Don't know option`) & question$`Don't know option`=="2",1,0)
question$`Don't know option_notPresent`     <- ifelse(!is.na(question$`Don't know option`) & question$`Don't know option`=="3",1,0)


### relocate variables

question <- question %>% relocate(c(`Don't know option_present`,`Don't know option_onlyRegistered`,`Don't know option_notPresent`), .before = `Don't know option`) %>%
            select(-`Don't know option`)


##### knowledge provided ######


question$`Knowledge provided_noExtraInfo`       <- ifelse(!is.na(question$`Knowledge provided`) & question$`Knowledge provided`=="1",1,0)
question$`Knowledge provided_definitionsOnly`   <- ifelse(!is.na(question$`Knowledge provided`) & question$`Knowledge provided`=="2",1,0)
question$`Knowledge provided_otherExplanations` <- ifelse(!is.na(question$`Knowledge provided`) & question$`Knowledge provided`=="3",1,0)
question$`Knowledge provided_bothDefAndExpl`    <- ifelse(!is.na(question$`Knowledge provided`) & question$`Knowledge provided`=="4",1,0)

### relocate variables

question <- question %>% relocate(c(`Knowledge provided_noExtraInfo`,`Knowledge provided_definitionsOnly`,`Knowledge provided_otherExplanations`,`Knowledge provided_bothDefAndExpl`
                                    ), .before = `Knowledge provided`) %>%
                          select(-`Knowledge provided`)




#########      4. Block Introduction available       ##########




#### request present in the intro ####

question$`Request present in the introduction_notPresent` <- ifelse(!is.na(question$`Request present in the introduction`) & question$`Request present in the introduction`=="0",1,0)
question$`Request present in the introduction_present`    <- ifelse(!is.na(question$`Request present in the introduction`) & question$`Request present in the introduction`=="1",1,0)

### relocate variables

question <- question %>% relocate(c(`Request present in the introduction_notPresent`,`Request present in the introduction_present`), .before = `Request present in the introduction`) %>%
            select(-`Request present in the introduction`)


#### number of sentences in introduction ####

question$`Number of sentences in introduction_number` <- ifelse(!is.na(question$`Number of sentences in introduction`),question$`Number of sentences in introduction`,0)


### relocate variables ###

question <- question %>% relocate(c(`Number of sentences in introduction_number`), .before = `Number of sentences in introduction`) %>%
            select(-`Number of sentences in introduction`)


#### number of words in introduction #####

question$`Number of words in introduction_number` <- ifelse(!is.na(question$`Number of words in introduction`),question$`Number of words in introduction`,0)

### relocate variables

question <- question %>% relocate(c(`Number of words in introduction_number`), .before = `Number of words in introduction`) %>%
            select(-`Number of words in introduction`)


#### number of subordinate clauses in request ####

question$`Number of subordinate clauses in request_number` <- ifelse(!is.na(question$`Number of subordinate clauses in request`),question$`Number of subordinate clauses in request`,0)

### relocate variables


question <- question %>% relocate(c(`Number of subordinate clauses in request_number`), .before = `Number of subordinate clauses in request`) %>%
            select(-`Number of subordinate clauses in request`)


##### number of subordinate clauses in introduction

question$`Number of subordinated clauses in introduction_number`   <- ifelse(!is.na(question$`Number of subordinated clauses in introduction`),question$`Number of subordinated clauses in introduction`,0)

#### relocate variables ####

question <- question %>% relocate(c(`Number of subordinated clauses in introduction_number`), .before = `Number of subordinated clauses in introduction`) %>%
                          select(-`Number of subordinated clauses in introduction`)

#####  5. Block Showcard or other visual aids used



##### Horizontal or vertical scale ######


question$`Horizontal or vertical scale_horizontal` <- ifelse(!is.na(question$`Horizontal or vertical scale`) & question$`Horizontal or vertical scale`=="1",1,0)
question$`Horizontal or vertical scale_vertical`   <- ifelse(!is.na(question$`Horizontal or vertical scale`) & question$`Horizontal or vertical scale`=="0",1,0)

### relocate variables

question <- question %>% relocate(c(`Horizontal or vertical scale_horizontal`,`Horizontal or vertical scale_vertical`), .before = `Horizontal or vertical scale`) %>%
            select(-`Horizontal or vertical scale`)



#### Overlap of scale labels and categ ####


question$`Overlap of scale labels and categories_present`    <- ifelse(!is.na(question$`Overlap of scale labels and categories`) & question$`Overlap of scale labels and categories`=="0",1,0)
question$`Overlap of scale labels and categories_notPresent` <- ifelse(!is.na(question$`Overlap of scale labels and categories`) & question$`Overlap of scale labels and categories`=="1",1,0)

### relocate variables

question <- question %>% relocate(c(`Overlap of scale labels and categories_present`,`Overlap of scale labels and categories_notPresent`), .before = `Overlap of scale labels and categories`) %>%
            select(-`Overlap of scale labels and categories`)


#### Numbers or letters before the answer categories #####


question$`Numbers or letters before the answer categories_numbers` <- ifelse(!is.na(question$`Numbers or letters before the answer categories`) & question$`Numbers or letters before the answer categories`=="1",1,0)
question$`Numbers or letters before the answer categories_letters` <- ifelse(!is.na(question$`Numbers or letters before the answer categories`) & question$`Numbers or letters before the answer categories`=="2",1,0)
question$`Numbers or letters before the answer categories_neither` <- ifelse(!is.na(question$`Numbers or letters before the answer categories`) & question$`Numbers or letters before the answer categories`=="0",1,0)


### relocate variables

question <- question %>% relocate(c(`Numbers or letters before the answer categories_numbers`,`Numbers or letters before the answer categories_letters`
                                    ,`Numbers or letters before the answer categories_neither`), .before = `Numbers or letters before the answer categories`) %>%
                        select(-`Numbers or letters before the answer categories`)


#### scale with only numbers or numbers in boxes ####


question$`Scale with only numbers or numbers in boxes_onlyNumbers`    <- ifelse(!is.na(question$`Scale with only numbers or numbers in boxes`) & question$`Scale with only numbers or numbers in boxes`=="0",1,0)
question$`Scale with only numbers or numbers in boxes_numbersInBoxes` <- ifelse(!is.na(question$`Scale with only numbers or numbers in boxes`) & question$`Scale with only numbers or numbers in boxes`=="1",1,0)


### relocate variables

question <- question %>% relocate(c(`Scale with only numbers or numbers in boxes_onlyNumbers`,`Scale with only numbers or numbers in boxes_numbersInBoxes`
                                    ), .before = `Scale with only numbers or numbers in boxes`) %>%
                          select(-`Scale with only numbers or numbers in boxes`)


#### start of the response sentence on the visual aid #####


question$`Start of the response sentence on the visual aid_yes` <- ifelse(!is.na(question$`Start of the response sentence on the visual aid`) & question$`Start of the response sentence on the visual aid`=="1",1,0)
question$`Start of the response sentence on the visual aid_no`  <- ifelse(!is.na(question$`Start of the response sentence on the visual aid`) & question$`Start of the response sentence on the visual aid`=="0",1,0)

#### relocate variables

question <- question %>% relocate(c(`Start of the response sentence on the visual aid_yes`,`Start of the response sentence on the visual aid_no`
                                    ), .before = `Start of the response sentence on the visual aid`) %>%
                          select(-`Start of the response sentence on the visual aid`)

#### request on the visual aid ####


question$`Request on the visual aid_yes` <- ifelse(!is.na(question$`Request on the visual aid`) & question$`Request on the visual aid`=="1",1,0)
question$`Request on the visual aid_no`  <- ifelse(!is.na(question$`Request on the visual aid`) & question$`Request on the visual aid`=="0",1,0)


### relocate variables

question <- question %>% relocate(c(`Request on the visual aid_yes`,`Request on the visual aid_no`), .before = `Request on the visual aid`) %>%
                          select(-`Request on the visual aid`)


#### picture provided ?

question$`Picture provided?_yes` <- ifelse(!is.na(question$`Picture provided?`) & question$`Picture provided?`=="1",1,0)
question$`Picture provided?_no`  <- ifelse(!is.na(question$`Picture provided?`) & question$`Picture provided?`=="0",1,0)


### relocate variables

question <- question %>% relocate(c(`Picture provided?_yes`,`Picture provided?_no`), .before = `Picture provided?`) %>%
                          select(-`Picture provided?`)



###################          turning the integers into factor variables




DF$Concept <- as.factor(DF$Concept)
DF$Concept..complex.concept_importOfAJudgement <- as.factor(DF$Concept..complex.concept_importOfAJudgement)
DF$Concept..complex.concept_CertaintyOfAJudgement <- as.factor(DF$Concept..complex.concept_CertaintyOfAJudgement)
DF$Concept..complex.concept_Other <- as.factor(DF$Concept..other.simple.concepts_TRUE)
DF$Social.Desirability <-as.factor(DF$Social.Desirability)
DF$Centrality <- as.factor(DF$Centrality)
DF$Reference.period <- as.factor(DF$Reference.period)
DF$Formulation.of.the.request.for.an.answer..basic.choice <- as.factor(DF$Formulation.of.the.request.for.an.answer..basic.choice)
DF$WH.word.used.in.the.request_used <- as.factor(DF$WH.word.used.in.the.request_used)
DF$WH.word.used.in.the.request_notused <- as.factor(DF$WH.word.used.in.the.request_notused)
DF$X.WH..word_who <- as.factor(DF$X.WH..word_who)
DF$X.WH..word_which <- as.factor(DF$X.WH..word_which)
DF$X.WH..word_what <- as.factor(DF$X.WH..word_what)
DF$X.WH..word_whentime <- as.factor(DF$X.WH..word_whentime)
DF$X.WH..word_whereplace <- as.factor(DF$X.WH..word_whereplace)
DF$X.WH..word_howProcedure <- as.factor(DF$X.WH..word_howProcedure)
DF$X.WH..word_howRelationship <- as.factor(DF$X.WH..word_howRelationship)
DF$X.WH..word_howOpinion <- as.factor(DF$X.WH..word_howOpinion)
DF$X.WH..word_howQuantity <- as.factor(DF$X.WH..word_howQuantity)
DF$X.WH..word_howExtremity <- as.factor(DF$X.WH..word_howExtremity)
DF$X.WH..word_how_Intensity <- as.factor(DF$X.WH..word_how_Intensity)
DF$X.WH..word_why <- as.factor(DF$X.WH..word_why)
DF$Request.for.an.answer.type_interrogative <- as.factor(DF$Request.for.an.answer.type_interrogative)
DF$Request.for.an.answer.type_imperative <- as.factor(DF$Request.for.an.answer.type_imperative)
DF$Request.for.an.answer.type_declarative <- as.factor(DF$Request.for.an.answer.type_declarative)
DF$Request.for.an.answer.type_NoneOfThese <- as.factor(DF$Request.for.an.answer.type_NoneOfThese)
DF$Use.of.gradation_nogradation <- as.factor(DF$Use.of.gradation_nogradation)
DF$Use.of.gradation_gradation <- as.factor(DF$Use.of.gradation_gradation)
DF$Balance.of.the.request_balanced <- as.factor(DF$Balance.of.the.request_balanced)
DF$Balance.of.the.request_unbalanced <- as.factor(DF$Balance.of.the.request_unbalanced)
DF$Presence.of.encouragement.to.answer_Encour <- as.factor(DF$Presence.of.encouragement.to.answer_Encour)
DF$Presence.of.encouragement.to.answer_noEncour <- as.factor(DF$Presence.of.encouragement.to.answer_noEncour)
DF$Emphasis.on.subjective.opinion.in.request_emphasis <- as.factor(DF$Emphasis.on.subjective.opinion.in.request_emphasis)
DF$Emphasis.on.subjective.opinion.in.request_noEmphasis <- as.factor(DF$Emphasis.on.subjective.opinion.in.request_noEmphasis)
DF$Use.of.stimulus.or.statement.in.the.request_notUsed <- as.factor(DF$Use.of.stimulus.or.statement.in.the.request_notUsed)
DF$Use.of.stimulus.or.statement.in.the.request_used <- as.factor(DF$Use.of.stimulus.or.statement.in.the.request_used)
DF$Absolute.or.comparative.judgment <- as.factor(DF$Absolute.or.comparative.judgment)
DF$Response.scale..basic.choice_morethan2cat <- as.factor(DF$Response.scale..basic.choice_morethan2cat)
DF$Response.scale..basic.choice_twoCatScale <- as.factor(DF$Response.scale..basic.choice_twoCatScale)
DF$Response.scale..basic.choice_openEnded <- as.factor(DF$Response.scale..basic.choice_openEnded)
DF$Response.scale..basic.choice_notAvailableMagnEst <- as.factor(DF$Response.scale..basic.choice_notAvailableMagnEst)
DF$Response.scale..basic.choice_NotavailableLineProd <- as.factor(DF$Response.scale..basic.choice_NotavailableLineProd)
DF$Response.scale..basic.choice_MoreStepsProcedure <- as.factor(DF$Response.scale..basic.choice_MoreStepsProcedure)
DF$Labels.of.categories_noLabels <- as.factor(DF$Labels.of.categories_noLabels)
DF$Labels.of.categories_partiallyLabeled <- as.factor(DF$Labels.of.categories_partiallyLabeled)
DF$Labels.of.categories_fully_Labeled <- as.factor(DF$Labels.of.categories_fully_Labeled)
DF$Labels.with.short.text.or.complete.sentences_completeSentences <- as.factor(DF$Labels.with.short.text.or.complete.sentences_completeSentences)
DF$Labels.with.short.text.or.complete.sentences_shortText <- as.factor(DF$Labels.with.short.text.or.complete.sentences_shortText)
DF$Order.of.the.labels_firstNegativeOrNonApplicable <- as.factor(DF$Order.of.the.labels_firstNegativeOrNonApplicable)
DF$Order.of.the.labels_firstPositive <- as.factor(DF$Order.of.the.labels_firstPositive)
DF$Correspondence.between.labels.and.numbers.of.the.scale_highCorrespondence <- as.factor(DF$Correspondence.between.labels.and.numbers.of.the.scale_highCorrespondence)
DF$Correspondence.between.labels.and.numbers.of.the.scale_mediumCorrespondence <- as.factor(DF$Correspondence.between.labels.and.numbers.of.the.scale_mediumCorrespondence)
DF$Correspondence.between.labels.and.numbers.of.the.scale_lowCorrespondence <- as.factor(DF$Correspondence.between.labels.and.numbers.of.the.scale_lowCorrespondence)
DF$Correspondence.between.labels.and.numbers.of.the.scale_notApplicable <- as.factor(DF$Correspondence.between.labels.and.numbers.of.the.scale_notApplicable)
DF$Theoretical.range.of.the.concept.bipolar.unipolar_bipolar <- as.factor(DF$Theoretical.range.of.the.concept.bipolar.unipolar_bipolar)
DF$Theoretical.range.of.the.concept.bipolar.unipolar_unipolar <- as.factor(DF$Theoretical.range.of.the.concept.bipolar.unipolar_unipolar)
DF$Range.of.the.used.scale.bipolar.unipolar_bipolar <- as.factor(DF$Range.of.the.used.scale.bipolar.unipolar_bipolar)
DF$Range.of.the.used.scale.bipolar.unipolar_unipolar <- as.factor(DF$Range.of.the.used.scale.bipolar.unipolar_unipolar)
DF$Symmetry.of.response.scale_symmetric <- as.factor(DF$Symmetry.of.response.scale_symmetric)
DF$Symmetry.of.response.scale_asymmetric <- as.factor(DF$Symmetry.of.response.scale_asymmetric)
DF$Neutral.category_present <- as.factor(DF$Neutral.category_present)
DF$Neutral.category_notPresent <- as.factor(DF$Neutral.category_notPresent)
DF$Don.t.know.option_notPresent <- as.factor(DF$Don.t.know.option_notPresent)
DF$Don.t.know.option_onlyRegistered <- as.factor(DF$Don.t.know.option_onlyRegistered)
DF$Don.t.know.option_present <- as.factor(DF$Don.t.know.option_present)
DF$Interviewer.instruction <- as.factor(DF$Interviewer.instruction)
DF$Respondent.instruction <- as.factor(DF$Respondent.instruction)
DF$Extra.information.or.definition <- as.factor(DF$Extra.information.or.definition)
DF$Knowledge.provided_noExtraInfo <- as.factor(DF$Knowledge.provided_noExtraInfo)
DF$Knowledge.provided_definitionsOnly <- as.factor(DF$Knowledge.provided_definitionsOnly)
DF$Knowledge.provided_otherExplanations <- as.factor(DF$Knowledge.provided_otherExplanations)
DF$Knowledge.provided_bothDefAndExpl <- as.factor(DF$Knowledge.provided_bothDefAndExpl)
DF$Introduction.available. <- as.factor(DF$Introduction.available.)
DF$Request.present.in.the.introduction_notPresent <- as.factor(DF$Request.present.in.the.introduction_notPresent)
DF$Request.present.in.the.introduction_present <-as.factor(DF$Request.present.in.the.introduction_present)
DF$Showcard.or.other.visual.aids.used <- as.factor(DF$Showcard.or.other.visual.aids.used)
DF$Horizontal.or.vertical.scale_horizontal <- as.factor(DF$Horizontal.or.vertical.scale_horizontal)
DF$Horizontal.or.vertical.scale_vertical <- as.factor(DF$Horizontal.or.vertical.scale_vertical)
DF$Overlap.of.scale.labels.and.categories_notPresent <- as.factor(DF$Overlap.of.scale.labels.and.categories_notPresent)
DF$Overlap.of.scale.labels.and.categories_present <- as.factor(DF$Overlap.of.scale.labels.and.categories_present)
DF$Numbers.or.letters.before.the.answer.categories_letters <- as.factor(DF$Numbers.or.letters.before.the.answer.categories_letters)
DF$Numbers.or.letters.before.the.answer.categories_neither <- as.factor(DF$Numbers.or.letters.before.the.answer.categories_neither)
DF$Numbers.or.letters.before.the.answer.categories_numbers <-as.factor(DF$Numbers.or.letters.before.the.answer.categories_numbers)
DF$Scale.with.only.numbers.or.numbers.in.boxes_onlyNumbers <-as.factor(DF$Scale.with.only.numbers.or.numbers.in.boxes_onlyNumbers)
DF$Scale.with.only.numbers.or.numbers.in.boxes_numbersInBoxes <-as.factor(DF$Scale.with.only.numbers.or.numbers.in.boxes_numbersInBoxes)
DF$Start.of.the.response.sentence.on.the.visual.aid_no <- as.factor(DF$Start.of.the.response.sentence.on.the.visual.aid_no)
DF$Start.of.the.response.sentence.on.the.visual.aid_yes <-as.factor(DF$Start.of.the.response.sentence.on.the.visual.aid_yes)
DF$Request.on.the.visual.aid_yes <-as.factor(DF$Request.on.the.visual.aid_yes)
DF$Request.on.the.visual.aid_no <- as.factor(DF$Request.on.the.visual.aid_no)
DF$Picture.provided._no <-as.factor(DF$Picture.provided._no)
DF$Picture.provided._yes <- as.factor(DF$Picture.provided._yes)
DF$Computer.assisted <- as.factor(DF$Computer.assisted)
DF$Interviewer <- as.factor(DF$Interviewer)
DF$Visual.or.oral.presentation <- as.factor(DF$Visual.or.oral.presentation)



















