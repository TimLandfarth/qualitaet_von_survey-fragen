# 0. Einlesen der daten und Verwendete Librarys----
## 0.1 Einlesen der daten----
df <- read.csv2("Aufbereitung/SQP3_dataset_100_perc_20220517.csv")
#df <- readxl::read_xlsx("Aufbereitung/SQP3_dfaset_100_perc_20220517.xlsx", .name_repair = "universal")

## 0.2 Librarys----
library(dplyr)
library(stringr)

#### 83 Variablen

# 1. Aufbereitung der daten----
## 1.0 Study----
names(df)[names(df) == "ï..Study"] <- "Study"

## 1.1 Item Admin----
table(df$ItemAdmin)

## 1.2 Item Name----
##### Was in der einzelnen Studie gemacht wurde.
table(df$ItemName)

## 1.3 Item Concept
##### Was in der einzelnen Studie gemacht wurde.
table(df$ItemConcept)

## 1.4 Country----
##### Land
table(df$Country)
df$Country <- relevel(factor(df$Country), ref = "Netherlands")

## 1.5 Language----
##### Sprache
table(df$Language)
df$Language <- relevel(factor(df$Language), ref = "Dutch")

## 1.6 Introduction Text----
##### Einleitungstext
head(df$Introduction.text[which(df$Introduction.text != ".")])

## 1.7 Request for answer text----
##### Frage
head(df$Request.for.answer.text)

## 1.8 Answer options text----
##### Antwortmoeglichkeiten
head(df$Answer.options.text)

## 1.9 Domain----
##### Was ist der Bereich, in dem die Studie angelegt wurde
##### Abkürzungen: 
##### na. pol.                          = national politics
##### Eur. pol.                         = european politics
##### living cond. and background var.  = living condition and background variables
##### other bel.                        = other believes
##### int. pol.                         = international politics
##### pers. rel.                        = personal relations
##### con. beh.                         = consumer behaviour
##### leis. act.                        = leisure activities

df$Domain <- relevel(factor(df$Domain, labels = c("na. pol.", "Eur. pol.", "health", "living cond. and \nbackground var.", "other bel.", "work", "int. pol.",
                             "family", "pers. rel.", "con. beh.", "leis. act.")), ref = "na. pol.")

## 1.10 Concept----
##### Konzept der Frage
df$Concept<- relevel(factor(df$Concept, labels = c("Eval. belief", "Feeling", "Importance of sth", "Exp. of future", "Complex con.", "All other con.")), ref = "Feeling")

## 1.11 Social desirability----
names(df)
df$Social.Desirability <- relevel(factor(df$Social.Desirability, labels = c("No", "A bit", "A lot")), ref = "No")

## 1.12 Centrality----
df$Centrality <- relevel(factor(df$Centrality, labels = c("No", "A bit", "Rather", "Central", "Very")), ref = "No")

## 1.13 Reference period----
df$Reference.period <- relevel(factor(df$Reference.period, labels = c("Future", "Present", "Past")), ref = "Past")

## 1.14 Formulation of the request for an answer----
df$Formulation.of.the.request.for.an.answer..basic.choice <- relevel(factor(df$Formulation.of.the.request.for.an.answer..basic.choice, label = c("No", "Indirect", "Direct")), ref = "No")

## 1.15 WH - Word used in the request----
df$WH.word.used.in.the.request <- factor(df$WH.word.used.in.the.request, labels = c("without", "used"))

## 1.16 Request for an answer type----
df$Request.for.an.answer.type <- factor(df$Request.for.an.answer.type, labels = c("None", "Inter.", "Imper.", "Declar."))

## 1.17 Use of gradation----
df$Use.of.gradation <- factor(df$Use.of.gradation, labels = c("No", "Yes"))

## 1.18 Balance of the request----
df$Balance.of.the.request <- factor(df$Balance.of.the.request, labels = c("Balanced", "Unbalanced"))

## 1.19 Presence of encouragement to answer----
df$Presence.of.encouragement.to.answer <- factor(df$Presence.of.encouragement.to.answer, labels = c("No", "Yes"))

## 1.20 Emphasis on subjective opinion in request----
df$Emphasis.on.subjective.opinion.in.request <- factor(df$Emphasis.on.subjective.opinion.in.request, label = c("No", "Yes"))

## 1.21 ! Information about the opinion of other people----
table(factor(df$Information.about.the.opinion.of.other.people, labels = c("No")))
#### Theoretisch gibt es zwei verschiedene Kategorien:
####                                                  - No information about opinions of others
####                                                  - Information about opinions of other present
#### wobei die zweite Kategorie (Information about opinions of other present) kein einziges mal auftaucht! -> Weglassen!!!!

## 1.22 Use of stimulus or statement in the request----
df$Use.of.stimulus.or.statement.in.the.request <- relevel(factor(df$Use.of.stimulus.or.statement.in.the.request, labels = c("No", "Yes")), ref = "No")

## 1.23 Absolute or comparative judgment----
df$Absolute.or.comparative.judgment <- relevel(factor(df$Absolute.or.comparative.judgment, labels = c("absolute", "comparative")), ref = "absolute")

## 1.24 Response scale basic choice----
df$Response.scale..basic.choice <- relevel(factor(df$Response.scale..basic.choice, labels = c("More than 2", "2", "open-ended", "Magnitude", "Line prod.", "More steps proc.")), ref = "More than 2")

## 1.25 number of categories----
table(df$Number.of.categories)

## 1.26 Theoretical range of concept----
df$Theoretical.range.of.the.concept.bipolar.unipolar <- factor(df$Theoretical.range.of.the.concept.bipolar.unipolar, labels = c("unipolar", "bipolar"))

## 1.27 Range of the used scale----
df$Range.of.the.used.scale.bipolar.unipolar <- factor(df$Range.of.the.used.scale.bipolar.unipolar, labels = c("Unipolar", "Bipolar"))

## 1.28 Symmetry of response scale----
df$Symmetry.of.response.scale <- factor(df$Symmetry.of.response.scale, labels = c("Asymmetric", "Symmetric"))

## 1.29 Neutral category----
df$Neutral.category <- factor(df$Neutral.category, labels = c("Present", "Not present"))

## 1.30 Number of fixed reference points----
table(df$Number.of.fixed.reference.points)

## 1.31 Dont know option----
df$Don.t.know.option <- relevel(factor(df$Don.t.know.option, labels = c("present", "only registered", "not present")), ref = "not present")

## 1.32 Interviewer instruction----
df$Interviewer.instruction <- relevel(factor(df$Interviewer.instruction, labels = c("Absent", "Present")), ref = "Absent")

## 1.33 Respondent instruction----
df$Respondent.instruction <- relevel(factor(df$Respondent.instruction, labels =c("Absent", "Present")), ref = "Absent")

## 1.34 Extra information or definition----
df$Extra.information.or.definition <- relevel(factor(df$Extra.information.or.definition, labels = c("Absent", "Present")), ref = "Absent")

## 1.35 Knowledge provided----
df$Knowledge.provided <- factor(df$Knowledge.provided, labels = c("No", "Definitions", "Other", "def. and other"))

## 1.36 Introduction available?----
df$Introduction.available. <- relevel(factor(df$Introduction.available., labels = c("No", "Yes")), ref = "No")

## 1.37 Request present in the introduction----
df$Request.present.in.the.introduction <- factor(df$Request.present.in.the.introduction, labels = c("not present", "present"))

## 1.38 Showcard or other visual aids used----
df$Showcard.or.other.visual.aids.used <-  relevel(factor(df$Showcard.or.other.visual.aids.used, labels = c("Not used", "Used")), ref = "Not used")

## 1.39 Horizontal or vertical scale----
df$Horizontal.or.vertical.scale <- factor(df$Horizontal.or.vertical.scale, labels = c("Vertical", "Horizontal"))

## 1.40 Overlap of scale labels and categories----
df$Overlap.of.scale.labels.and.categories <- factor(df$Overlap.of.scale.labels.and.categories, labels = c("Overlap present", "clearly connected"))

## 1.41 ! Numbers or letters before the answer categories----
df$Numbers.or.letters.before.the.answer.categories <- factor(df$Numbers.or.letters.before.the.answer.categories, labels = c("Neither", "Numbers"))
#### Theoretisch gibt es drei verschiedene Kategorien:
####                                                  - Neither
####                                                  - Numbers
####                                                  - Letters
#### wobei die dritte Kategorie (Letters) kein einziges mal auftaucht! 

## 1.42 Scale with only numbers or numbers in boxes----
df$Scale.with.only.numbers.or.numbers.in.boxes <- factor(df$Scale.with.only.numbers.or.numbers.in.boxes, labels = c("Only numbers", "Numbers in boxes"))

## 1.43 Start of the response sentence on the visual aid----
df$Start.of.the.response.sentence.on.the.visual.aid <- factor(df$Start.of.the.response.sentence.on.the.visual.aid, labels = c("No", "Yes"))

## 1.44 Request on the visual aid----
df$Request.on.the.visual.aid <- factor(df$Request.on.the.visual.aid, labels = c("No", "Yes"))

## 1.45 Picture provided?----
df$Picture.provided. <- factor(df$Picture.provided., label = c("No", "Yes"))

## 1.46 Computer assisted----
df$Computer.assisted <- relevel(factor(df$Computer.assisted, label = c("No", "Yes")), ref = "No")

## 1.47 Interviewer----
df$Interviewer <- relevel(factor(df$Interviewer, label = c("No", "Yes")), ref = "No")

## 1.48 Visual or oral presentation----
df$Visual.or.oral.presentation <- relevel(factor(df$Visual.or.oral.presentation, label = c("Oral", "Visual")), ref = "Oral")

## 1.11 Qualitaet----
df$quality <- df$reliability.r.2. * df$validity.v.2.

## 1.49 language----
df$Language <- factor(df$Language)

## 1.50 Labels of categories----
##### WICHTIG FRAGEN WAS FÜR FAKTOREN DAS HAT UND WIE DAS IM ROUTING DRIN IST#####-----
df$Labels.of.categories <- factor(df$Labels.of.categories)

## 1.51 Labels with short text or complete sentences----
df$Labels.with.short.text.or.complete.sentences <- factor(df$Labels.with.short.text.or.complete.sentences)

## 1.52 Order of the labels----
df$Order.of.the.labels <- factor(df$Order.of.the.labels)

## 1.53 Correspondence between labels and numbers of the scale----
df$Correspondence.between.labels.and.numbers.of.the.scale <- factor(df$Correspondence.between.labels.and.numbers.of.the.scale)

# 2. Entfernen von unnoetigen Beobachtungen (Qualitaet = NA)----
df <- df %>% filter(!is.na(quality))

# 3. Gifi-System----
## 3.1 Funktion zur Implementierung von GIFI----
GIFIrisieren <- function(df, column){
  namen <- levels(as.factor(df[[column]]))
  if(!is.factor(df[[column]])){
    warning(paste0(column, " ist kein Faktor!"))
  }
  namen_neu <- gsub(" ",".", namen)
  neue_kat_namen <- paste0(column, "_", namen_neu)
  for( i in 1:length(namen)){
    df[,neue_kat_namen[i]] <- ifelse(!is.na(df[,column]) & df[,column] == namen[i], paste0(namen_neu[i], "_j"), paste0(namen_neu[i], "_n"))
  }
  return(df)
}

## 3.2 Sammeln der Namen----
n <- c("WH.word.used.in.the.request", "Request.for.an.answer.type", "Use.of.gradation", "Balance.of.the.request", "Presence.of.encouragement.to.answer",
  "Emphasis.on.subjective.opinion.in.request", "Information.about.the.opinion.of.other.people", # Filter 1
  "Theoretical.range.of.the.concept.bipolar.unipolar", "Range.of.the.used.scale.bipolar.unipolar", "Symmetry.of.response.scale", 
  "Neutral.category", # Filter 2
  "Knowledge.provided", # Filter 3
  "Request.present.in.the.introduction", # Filter 4
  "Horizontal.or.vertical.scale", "Overlap.of.scale.labels.and.categories", "Numbers.or.letters.before.the.answer.categories",
  "Scale.with.only.numbers.or.numbers.in.boxes", "Start.of.the.response.sentence.on.the.visual.aid", "Request.on.the.visual.aid",
  "Picture.provided."# Filter 5
  )

## 3.3 Gifirisieren der Spalten----
for(j in 1:length(n)){
   df <- GIFIrisieren(df, n[j])
}

# 4. Hinzufügen von 0ern in numerischen Filterfragenspalten----
## 4.1 Filter 2: Number of categories----
df$Number.of.categories[which(is.na(df$Number.of.categories))]  <- 0

## 4.2 Filter 2: Number of fixed reference points----
df$Number.of.fixed.reference.points[which(is.na(df$Number.of.fixed.reference.points))] <- 0

## 4.3 Filter 4: Number of sentences in introduction----
df$Number.of.sentences.in.introduction[which(is.na(df$Number.of.sentences.in.introduction))] <- 0

## 4.4 Filter 4: Number of words in introduction----
df$Number.of.words.in.introduction[which(is.na(df$Number.of.words.in.introduction))] <- 0

## 4.5 Filter 4: Number of subordinate clauses in introduction----
df$Number.of.subordinated.clauses.in.introduction[which(is.na(df$Number.of.subordinated.clauses.in.introduction))] <- 0

# 5. Datenaufbereitung Frau Felderer----
# codes for the experiments in the ESS according to document "ESS1-7 Rounds experimental items"
# ESS 1
df$experiment <- ifelse(df$ItemAdmin %in% c("A1","A2", "A3", "A4", "A5", "H1",
                                              "H2", 
                                              "H3",
                                              "H19",
                                              "H20",
                                              "H21") & df$Study == "ESS Round 1", "ESS1 media_use", 0)

df$experiment <- ifelse(df$ItemAdmin %in% c("A8", "A9", "A10", "H10",
                                              "H11",# what about A6
                                              "H12",
                                              "H28",
                                              "H29",
                                              "H30") & df$Study == "ESS Round 1", "ESS1 social_trust", df$experiment)
df$experiment <- ifelse((df$ItemAdmin %in% c("B2",  "B3", "B4", "H4",
                                               "H5",
                                               "H6",
                                               "H22",
                                               "H23",
                                               "H24") ) & df$Study == "ESS Round 1", "ESS1 political_efficacy", df$experiment)
# ESS 1
df$experiment <- ifelse(df$ItemAdmin %in% c("B7", "B8", "B9", 
                                              "H13",
                                              "H14",
                                              "H15",
                                              "H31",
                                              "H32",
                                              "H33") & df$Study == "ESS Round 1", "ESS1 political_trust", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("H7",
                                              "H8",
                                              "H9",
                                              "H25",
                                              "H26",
                                              "H27" ,
                                              "B30" ,
                                              "B31" ,
                                              "B32") & df$Study == "ESS Round 1", "ESS1 political_satisfaction", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("B43", "B44", "B45", 
                                              "H16",
                                              "H17",
                                              "H18",
                                              "H34",
                                              "H35",
                                              "H36") & df$Study == "ESS Round 1", "ESS1 left-right_orientation", df$experiment)

# ESS 2 

df$experiment <- ifelse(df$ItemAdmin %in% c( "B4", "B5", "B7", 
                                               "IS25",
                                               "IS26", 
                                               "IS27",
                                               "IS38",
                                               "IS39",
                                               "IS40") & df$Study == "ESS Round 2", "ESS2 political_trust", df$experiment)
df$experiment <- ifelse(df$ItemAdmin %in% c("B25", "B26", "B27",
                                              "IS11" ,
                                              "IS12" ,
                                              "IS13" ,
                                              "IS35" ,
                                              "IS36",
                                              "IS37" ) & df$Study == "ESS Round 2", "ESS2 political_satisfaction", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("D25", "D26", "D27",
                                              "IS5" ,
                                              "IS6" ,
                                              "IS7" ,
                                              "IS28" ,
                                              "IS29",
                                              "IS30" ) & df$Study == "ESS Round 2", "ESS2 evaluation_of_doctors", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("G6", "G7", "G8",
                                              "IS8" ,
                                              "IS9" ,
                                              "IS10" ,
                                              "IS22" ,
                                              "IS23",
                                              "IS24" ) & df$Study == "ESS Round 2", "ESS2 gender_inequalities", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("G22", "G23", "G24",
                                              "IS2" ,
                                              "IS3" ,
                                              "IS4" ,
                                              "IS15" ,
                                              "IS16",
                                              "IS17" ) & df$Study == "ESS Round 2", "ESS2 housework", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("G64", "G66", "G70",
                                              "IS19" ,
                                              "IS20" ,
                                              "IS21" ,
                                              "IS32" ,
                                              "IS33",
                                              "IS34" ) & df$Study == "ESS Round 2", "ESS2 current_job", df$experiment)

#ESS 3
df$experiment <- ifelse(df$ItemAdmin %in% c("B35", "B36", "B37",
                                              "HS1" ,
                                              "HS2" ,
                                              "HS3" ,
                                              "HS13" ,
                                              "HS14",
                                              "HS15",
                                              "HS25",
                                              "HS26",
                                              "HS27") & df$Study == "ESS Round 3", "ESS3 immigration_perceptions", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("B38", "B39", "B40",
                                              "HS4" ,
                                              "HS5" ,
                                              "HS6" ,
                                              "HS16" ,
                                              "HS17",
                                              "HS18",
                                              "HS28",
                                              "HS29",
                                              "HS30") & df$Study == "ESS Round 3", "ESS3 evaluation_of_immigration", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("E26", "E27", "E28",
                                              "HS7" ,
                                              "HS8" ,
                                              "HS9" ,
                                              "HS19" ,
                                              "HS20",
                                              "HS21",
                                              "HS31",
                                              "HS32",
                                              "HS33") & df$Study == "ESS Round 3", "ESS3 eudaimonic_well-being", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("E40", "E43", "E45",
                                              "HS10" ,
                                              "HS11" ,
                                              "HS12" ,
                                              "HS22" ,
                                              "HS23",
                                              "HS24",
                                              "HS34",
                                              "HS35",
                                              "HS36") & df$Study == "ESS Round 3", "ESS3 life_satisfaction", df$experiment)

# ESS 4 
df$experiment <- ifelse(df$ItemAdmin %in% c("A1", "A3", "A5",
                                              "HS1" ,
                                              "HS2" ,
                                              "HS3" ,
                                              "HS13" ,
                                              "HS14",
                                              "HS15") & df$Study == "ESS Round 4", "ESS4 media_use", df$experiment)#

df$experiment <- ifelse(df$ItemAdmin %in% c("A8", "A9",
                                              "HS4" ,
                                              "HS5" ,
                                              "HS6" ,
                                              "HS25" ,
                                              "HS26",
                                              "HS27") & df$Study == "ESS Round 4", "ESS4 social_trust", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("B4", "B5", "B6",
                                              "HS16" ,
                                              "HS17" ,
                                              "HS18" ,
                                              "HS28" ,
                                              "HS29",
                                              "HS30") & df$Study == "ESS Round 4", "ESS4 political_trust", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("B23",
                                              "HS22" ,
                                              "HS23" ,
                                              "HS24" ,
                                              "HS34" ,
                                              "HS35",
                                              "HS36") & df$Study == "ESS Round 4", "ESS4 left-right_placement", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("B25", "B26", "B27",
                                              "HS7" ,
                                              "HS8" ,
                                              "HS9" ,
                                              "HS19" ,
                                              "HS20",
                                              "HS21") & df$Study == "ESS Round 4", "ESS4 political_satisfaction", df$experiment)

df$experiment <- ifelse(df$ItemAdmin %in% c("B30", "B31",
                                              "HS10" ,
                                              "HS11" ,
                                              "HS12" ,
                                              "HS31" ,
                                              "HS32",
                                              "HS33") & df$Study == "ESS Round 4", "ESS4 left-right_orientation", df$experiment)

## ESS 5 
df$experiment <- ifelse(df$ItemAdmin %in% c("D4", "D5","D6",
                                              "I10" ,
                                              "I11" ,
                                              "I12" ,
                                              "I19" ,
                                              "I20",
                                              "I21") & df$Study == "ESS Round 5", "ESS5 effectiveness_of_the_police", df$experiment)#

df$experiment <- ifelse(df$ItemAdmin %in% c("D12", "D13","D14",
                                              "I15" ,
                                              "I13" ,
                                              "I14" ,
                                              "I4" ,
                                              "I5",
                                              "I6") & df$Study == "ESS Round 5", "ESS5 satisfaction_with_the_police", df$experiment)#

df$experiment <- ifelse(df$ItemAdmin %in% c("D15", "D17","D16",
                                              "I7" ,
                                              "I8" ,
                                              "I9" ,
                                              "I16" ,
                                              "I17",
                                              "I18") & df$Study == "ESS Round 5", "ESS5 evaluation_of_the_police", df$experiment)

## ESS 6  
df$experiment <- ifelse(df$ItemName %in% c("imbgeco",
                                             "imueclt",
                                             "imwbcnt",
                                             "teste19", 
                                             "teste20", 
                                             "teste21", 
                                             "teste28", 
                                             "teste29", 
                                             "teste30" ) & df$Study == "ESS Round 6", "ESS6 evaluation_of_immigration", df$experiment)


df$experiment <- ifelse(df$ItemName %in% c("fltdpr",
                                             "slprl",
                                             "fltlnl",
                                             "teste4", 
                                             "teste5", 
                                             "teste6", 
                                             "teste13", 
                                             "teste14", 
                                             "teste15",
                                             "teste25", 
                                             "teste26", 
                                             "teste27", 
                                             "teste34", 
                                             "teste35", 
                                             "teste36") & df$Study == "ESS Round 6", "ESS6 feelings_past_week", df$experiment)


df$experiment <- ifelse(df$ItemName %in% c( "tmimdng",
                                              "tmabdng",
                                              "tmendng",
                                              "teste1", 
                                              "teste2", 
                                              "teste3", 
                                              "teste10", 
                                              "teste11", 
                                              "teste12",
                                              "teste22", 
                                              "teste23", 
                                              "teste24", 
                                              "teste31", 
                                              "teste32", 
                                              "teste33") & df$Study == "ESS Round 6", "ESS6 everyday_life_engagement", df$experiment)

df$experiment <- ifelse(df$ItemName %in% c("oppcrgvc","medcrgvc","meprinfc",
                                             "teste7", 
                                             "teste8", 
                                             "teste9", 
                                             "teste16", 
                                             "teste17", 
                                             "teste18") & df$Study == "ESS Round 6", "ESS6 evaluation_of_democracy", df$experiment)#
# ESS 7 
#
df$experiment <- ifelse(df$ItemName %in% c("psppsgv",
                                             "psppipl",
                                             "ptcpplt",
                                             "testf4",
                                             "testf5",
                                             "testf6",
                                             "testf13",
                                             "testf14",
                                             "testf15") & df$Study == "ESS Round 7", "ESS7 system__responsiveness", df$experiment)

df$experiment <- ifelse(df$ItemName %in% c("actrolg",
                                             "cptppol",
                                             "etapapl",
                                             "testf7",
                                             "testf8",
                                             "testf9",
                                             "testf16",
                                             "testf17",
                                             "testf18") & df$Study == "ESS Round 7", "ESS7 subjective_competence", df$experiment)#

df$experiment <- ifelse(df$ItemName %in% c("qfimlng",
                                             "qfimwht",
                                             "qfimcmt",
                                             "testf1",
                                             "testf2",
                                             "testf3",
                                             "testf10",
                                             "testf11",
                                             "testf12") & df$Study == "ESS Round 7", "ESS7 importance_to_immigration", df$experiment)

##



# split dat$Study before the : to receive variable with study name for non-ESS studies
df$study <- str_split_fixed(df$Study, ":", 2)[, 1]

# add experiment names of non-ESS studies to experiment variable
df$experiment <- ifelse(df$experiment == 0, str_split_fixed(df$Study, ":", 2)[, 2], df$experiment)


# 6. Implementieren der "Count" Daten----
## 6.1 ESS Daten----
dfESSc <- read.csv("Aufbereitung/sample_size.csv")
dfESSc$cntry <- as.character(factor(dfESSc$cntry, labels = c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic",
                                                             "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "United Kingdom",
                                                             "Greece", "Croatia", "Hungary", "Ireland", "Israel", "Iceland", "Italy",
                                                             "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", "Portugal",
                                                             "Romania", "Russian Federation", "Sweden", "Slovenia", "Slovakia", "Turkey", "Ukraine")))




df$ess_study <- ifelse(df$Study %in% c("ESS Round 1", "ESS Round 2", "ESS Round 3", "ESS Round 4", "ESS Round 5", "ESS Round 6", "ESS Round 7"), 1, 0)

# generate unique identifier variable to define country, language, ESS round and experiment to merge ESS sample sizes
df$merge <- str_c(df$Country, df$experiment, sep=" ")

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dfESSc$merge <- str_c(dfESSc$cntry, paste("ESS", dfESSc$rounds, sep = "") ,dfESSc$exp_name, sep = " ")


# ess sanple sizes
load(file="//svmafile01.gesis.intra/users/felderba/papers/ESS/data/ess_sample_size.Rdata")

#other experiment's sample sizes
ss_non_ess <- read.csv2("//svmafile01.gesis.intra/users/felderba/SQP/data/sample_sizes/sample_size_non_ess.csv", sep=";")

dat <- merge(dfESSc[, c(7, 8)], df, by.y="merge", by.x="merge", no.dups = TRUE) 
dat <- merge(ss_non_ess[, c(1, 7)], dat, by.y="?..Study", by.x="?..study_name", all.y = TRUE) 
dat$sample_size <- as.numeric(ifelse(dat$ess_study == 0, dat$sample_size.x, dat$sample_size.y))

dat <- dat[, c(5:6, 10:14, 16:98, 100)]



which((df$merge %in% dfESSc$merge) == FALSE)

df$merge[3384]
df$merge[3384] ==  dfESSc$merge[dfESSc$cntry == "Switzerland" & dfESSc$rounds == 4][8]


unique(dfESSc$cntry[dfESSc$rounds == 1])



t1 <- df[which(df$Study == "ESS Round 1" & df$Country == "Austria"),]
t1$experiment <- str_sub(t1$experiment, start = 6)
t2 <- dfESSc[which(dfESSc$rounds == 1 & dfESSc$cntry  == "Austria"),]

table(t1$experiment)
table(t2$exp_name)


# 7. Setzen der Referenzkategorien----
## 7.1 WH- word -> used ----
df$WH.word.used.in.the.request_used <- relevel(factor(df$WH.word.used.in.the.request_used), ref = "used_n")

## 7.2 Request for an answer type----
df$Request.for.an.answer.type_Declar. <- relevel(factor(df$Request.for.an.answer.type_Declar.), ref = "Declar._n")
df$Request.for.an.answer.type_Imper. <- relevel(factor(df$Request.for.an.answer.type_Imper.), ref = "Imper._n")
df$Request.for.an.answer.type_Inter. <- relevel(factor(df$Request.for.an.answer.type_Inter.), ref = "Inter._n")

## 7.3 Use of gradation----
df$Use.of.gradation_Yes <- relevel(factor(df$Use.of.gradation_Yes), ref = "Yes_n")
df$Use.of.gradation_No <- relevel(factor(df$Use.of.gradation_No), ref = "No_n")

## 7.4 Balance of the request----
df$Balance.of.the.request_Balanced <- relevel(factor(df$Balance.of.the.request_Balanced), ref = "Balanced_n")

## 7.5 Presence of encouragementto answer----
df$Presence.of.encouragement.to.answer_Yes <- relevel(factor(df$Presence.of.encouragement.to.answer_Yes), ref = "Yes_n")

## 7.6 Emphasis on subjective opinion----
df$Emphasis.on.subjective.opinion.in.request_Yes <- relevel(factor(df$Emphasis.on.subjective.opinion.in.request_Yes), "Yes_n")

## 7.7 Theoretical range of the concept----
df$Theoretical.range.of.the.concept.bipolar.unipolar_unipolar <- relevel(factor(df$Theoretical.range.of.the.concept.bipolar.unipolar_unipolar), "unipolar_n")

## 7.8 Range of the used scale----
df$Range.of.the.used.scale.bipolar.unipolar_Unipolar <- relevel(factor(df$Range.of.the.used.scale.bipolar.unipolar_Unipolar), "Unipolar_n")

## 7.9 Symmetrie of the response scale----
df$Symmetry.of.response.scale_Symmetric <- relevel(factor(df$Symmetry.of.response.scale_Symmetric), "Symmetric_n")

## 7.10 Neutral category----
df$Neutral.category_Present <- relevel(factor(df$Neutral.category_Present), ref = "Present_n")

## 7.11 Knowledge provided----
df$Knowledge.provided_def..and.other <- relevel(factor(df$Knowledge.provided_def..and.other), ref = "def..and.other_n")
df$Knowledge.provided_Definitions <- relevel(factor(df$Knowledge.provided_Definitions), ref = "Definitions_n")
df$Knowledge.provided_No <- relevel(factor(df$Knowledge.provided_No), ref = "No_n")
df$Knowledge.provided_Other <- relevel(factor(df$Knowledge.provided_Other), ref = "Other_n")

## 7.12 Request present in the introduction----
df$Request.present.in.the.introduction_present <- relevel(factor(df$Request.present.in.the.introduction_present), ref = "present_n")

## 7.13 horizontal or vertical scale----
df$Horizontal.or.vertical.scale_Horizontal <- relevel(factor(df$Horizontal.or.vertical.scale_Horizontal), ref = "Horizontal_n")
df$Horizontal.or.vertical.scale_Vertical <- relevel(factor(df$Horizontal.or.vertical.scale_Vertical), ref = "Vertical_n")

## 7.14 Overlap of scale labels and categories----
df$Overlap.of.scale.labels.and.categories_clearly.connected <- relevel(factor(df$Overlap.of.scale.labels.and.categories_clearly.connected), ref = "clearly.connected_n")

## 7.15 Number or letters before the answer category----
df$Numbers.or.letters.before.the.answer.categories_Numbers <- relevel(factor(df$Numbers.or.letters.before.the.answer.categories_Numbers), ref = "Numbers_n")

## 7.16 Scale with only numbers or numbers in boxes----
df$Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes <- relevel(factor(df$Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes), ref = "Numbers.in.boxes_n")
df$Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers <- relevel(factor(df$Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers), ref = "Only.numbers_n")

## 7.17 Start of the response sentence on the visual aid----
df$Start.of.the.response.sentence.on.the.visual.aid_Yes <- relevel(factor(df$Start.of.the.response.sentence.on.the.visual.aid_Yes), "Yes_n")

## 7.18 Request on the visual aid----
df$Request.on.the.visual.aid_Yes <- relevel(factor(df$Request.on.the.visual.aid_Yes), ref = "Yes_n")

## 7.19 Picture provided----
df$Picture.provided._Yes <- relevel(factor(df$Picture.provided._Yes), ref = "Yes_n")


# 8. Alle Variablen, welche im Modell verwendet werden sollten (-> von Schweisstal kopiert, Namen sind anders!)----
## 8.1 Von Schweisstal (d.h. Original)----
length(c("lang", "domain", "concept", "socdesir" ,"centrality" ,"ref_period", "form_basic", "used_WH_word",
  "questiontype", "gradation","balance","encourage","subjectiveop","opinionother",
  "stimulus","absolute","scale_basic","labels","fixrefpoints",
  "labels_gramm","labels_order","scale_corres","scale_trange","scale_urange",
  "symmetry","scale_neutral","Dont_know","instr_interv","instr_respon",
  "motivation","knowledge","intropresent","intr_request","usedshowcard",
  "showc_horiz","showc_over","showc_letters","showc_boxes","showc_start",
  "showc_quest","showc_pict","ncategories","nsents_intro","nsents_quest",
  "nwords_intro","numsub_intro","nwords_quest","nnouns_quest","nabst_quest",
  "nsyll_quest","nsub_quest","nsyll_ans","nnouns_ans","nabst_ans",
  "computer_assisted","interviewer","visual", "range_correspondence"))

## 8.2 Mit unseren Namen----
n1 <- c("Language" , "Domain" , "Concept" , "Social.Desirability" , "Centrality" , "Reference.period" ,
  "Formulation.of.the.request.for.an.answer..basic.choice" , "WH.word.used.in.the.request" , 
  "Request.for.an.answer.type" , "Use.of.gradfion" , "Balance.of.the.request" , 
  "Presence.of.encouragement.to.answer" , "Emphasis.on.subjective.opinion.in.request" , "Use.of.stimulus.or.statement.in.the.request",
  "Absolute.or.comparative.judgment" , "Response.scale..basic.choice" , "Number.of.categories" ,
  "Theoretical.range.of.the.concept.bipolar.unipolar" , "Range.of.the.used.scale.bipolar.unipolar" ,
  "Symmetry.of.response.scale" , "Neutral.category" , "Number.of.fixed.reference.points" ,
  "Don.t.know.option" , "Interviewer.instruction" , "Respondent.instruction" , "Extra.information.or.definition",
  "Knowledge.provided" , "Introduction.available." , "Request.present.in.the.introduction" ,
  "Number.of.sentences.in.introduction" , "Number.of.words.in.introduction", "Number.of.sentences.in.introduction" ,
  "Number.of.sentences.in.the.request" , "Number.of.words.in.request" , "Total.number.of.nouns.in.request.for.an.answer" ,
  "Total.number.of.abstract.nouns.in.request.for.an.answer" , "Total.number.of.syllables.in.request" ,
  "Number.of.subordinate.clauses.in.request" , "Number.of.syllables.in.answer.scale" ,
  "Total.number.of.nouns.in.answer.scale" , "Total.number.of.abstract.nouns.in.answer.scale" ,
  "Showcard.or.other.visual.aids.used" , "Horizontal.or.vertical.scale" , "Overlap.of.scale.labels.and.categories",
  "Numbers.or.letters.before.the.answer.categories" , "Scale.with.only.numbers.or.numbers.in.boxes" , 
  "Start.of.the.response.sentence.on.the.visual.aid" , "Request.on.the.visual.aid" ,
  "Picture.provided." , "Computer.assisted" , "Interviewer" , "Visual.or.oral.presentation" , "Position")

## 8.3 Mit unseren Namen UND Gifirisierung----
c("Language" , "Domain" , "Concept" , "Social.Desirability" , "Centrality" , "Reference.period" ,
  "Formulation.of.the.request.for.an.answer..basic.choice" , "WH.word.used.in.the.request_used", "WH.word.used.in.the.request_without", 
  "Request.for.an.answer.type_Declar.", "Request.for.an.answer.type_Imper.", "Request.for.an.answer.type_Inter.", "Request.for.an.answer.type_None",
  "Use.of.gradation_No", "Use.of.gradation_Yes" , "Balance.of.the.request_Balanced","Balance.of.the.request_Unbalanced" , 
  "Presence.of.encouragement.to.answer_No","Presence.of.encouragement.to.answer_Yes" , "Emphasis.on.subjective.opinion.in.request_No",
  "Emphasis.on.subjective.opinion.in.request_Yes", "Use.of.stimulus.or.statement.in.the.request",
  "Absolute.or.comparative.judgment" , "Response.scale..basic.choice" , "Number.of.categories" ,
  "Theoretical.range.of.the.concept.bipolar.unipolar_bipolar","Theoretical.range.of.the.concept.bipolar.unipolar_unipolar",
  "Range.of.the.used.scale.bipolar.unipolar_Bipolar", "Range.of.the.used.scale.bipolar.unipolar_Unipolar",
  "Symmetry.of.response.scale_Asymmetric", "Symmetry.of.response.scale_Symmetric",
  "Neutral.category_Not.present", "Neutral.category_Present" , "Number.of.fixed.reference.points" ,
  "Don.t.know.option" , "Interviewer.instruction" , "Respondent.instruction" , "Extra.information.or.definition",
  "Knowledge.provided_Definitions", "Knowledge.provided_Other", "Knowledge.provided_No","Knowledge.provided_def..and.other" ,
  "Introduction.available." , "Request.present.in.the.introduction_present","Request.present.in.the.introduction_not.present",
  "Number.of.sentences.in.introduction" , "Number.of.words.in.introduction", "Number.of.sentences.in.introduction" ,
  "Number.of.sentences.in.the.request" , "Number.of.words.in.request" , "Total.number.of.nouns.in.request.for.an.answer" ,
  "Total.number.of.abstract.nouns.in.request.for.an.answer" , "Total.number.of.syllables.in.request" ,
  "Number.of.subordinate.clauses.in.request" , "Number.of.syllables.in.answer.scale" ,
  "Total.number.of.nouns.in.answer.scale" , "Total.number.of.abstract.nouns.in.answer.scale" ,
  "Showcard.or.other.visual.aids.used" , "Horizontal.or.vertical.scale_Horizontal", "Horizontal.or.vertical.scale_Vertical",
  "Overlap.of.scale.labels.and.categories_clearly.connected", "Overlap.of.scale.labels.and.categories_Overlap.present",
  "Numbers.or.letters.before.the.answer.categories_Neither", "Numbers.or.letters.before.the.answer.categories_Numbers",
  "Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes", "Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers",
  "Start.of.the.response.sentence.on.the.visual.aid_No", "Start.of.the.response.sentence.on.the.visual.aid_Yes" , "Request.on.the.visual.aid_No",
  "Request.on.the.visual.aid_Yes", "Picture.provided._No", "Picture.provided._Yes",
  "Computer.assisted" , "Interviewer" , "Visual.or.oral.presentation" , "Position")

## 8.3 Mit unseren Namen, Gifirisierung und ohne "Singularitaets" Variablen----
model_names <- c("Domain" , "Concept" , "Social.Desirability" , "Centrality" , "Reference.period" ,
  "Formulation.of.the.request.for.an.answer..basic.choice" , "WH.word.used.in.the.request_used",  
  "Request.for.an.answer.type_Declar.", "Request.for.an.answer.type_Imper.", "Request.for.an.answer.type_Inter.",
  "Use.of.gradation_No", "Use.of.gradation_Yes" , "Balance.of.the.request_Balanced", 
  "Presence.of.encouragement.to.answer_Yes" , 
  "Emphasis.on.subjective.opinion.in.request_Yes", "Use.of.stimulus.or.statement.in.the.request",
  "Absolute.or.comparative.judgment" , "Response.scale..basic.choice" , "Number.of.categories" ,
  "Theoretical.range.of.the.concept.bipolar.unipolar_unipolar", "Range.of.the.used.scale.bipolar.unipolar_Unipolar", "Symmetry.of.response.scale_Symmetric",
  "Neutral.category_Present" , "Number.of.fixed.reference.points" ,
  "Don.t.know.option" , "Interviewer.instruction" , "Respondent.instruction" , "Extra.information.or.definition",
  "Knowledge.provided_Definitions", "Knowledge.provided_Other", "Knowledge.provided_No","Knowledge.provided_def..and.other" ,
  "Introduction.available." , "Request.present.in.the.introduction_present",
  "Number.of.sentences.in.introduction" , "Number.of.words.in.introduction",
  "Number.of.sentences.in.the.request" , "Number.of.words.in.request" , "Total.number.of.nouns.in.request.for.an.answer" ,
  "Total.number.of.abstract.nouns.in.request.for.an.answer" , "Total.number.of.syllables.in.request" ,
  "Number.of.subordinate.clauses.in.request" , "Number.of.syllables.in.answer.scale" ,
  "Total.number.of.nouns.in.answer.scale" , "Total.number.of.abstract.nouns.in.answer.scale" ,
  "Showcard.or.other.visual.aids.used" , "Horizontal.or.vertical.scale_Horizontal", "Horizontal.or.vertical.scale_Vertical",
  "Overlap.of.scale.labels.and.categories_clearly.connected", "Numbers.or.letters.before.the.answer.categories_Numbers",
  "Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes", "Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers",
  "Start.of.the.response.sentence.on.the.visual.aid_Yes" , "Request.on.the.visual.aid_Yes", "Picture.provided._Yes",
  "Computer.assisted" , "Interviewer" , "Visual.or.oral.presentation" , "Position")

# 9. Speichern----
save(df, model_names, file = "C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/data.Rdata")



