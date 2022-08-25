# 0. Einlesen der Daten und Verwendete Librarys----
## 0.1 Einlesen der Daten----
#df <- read.csv2("C:/Uni/13. Semester/Praktikum/Github mlcu_gesis/mlcu_gesis-main/mlcu_gesis-main/data/SQP3_dataset_2022 03 10.csv")
df <- readxl::read_xlsx("Aufbereitung/SQP3_dataset_100_perc_20220517.xlsx", .name_repair = "universal")

## 0.2 Librarys----
library(dplyr)

#### 83 Variablen

# 1. Aufbereitung der Daten----
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

## 1.5 Language----
##### Sprache
table(df$Language)

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

df$Domain <- factor(df$Domain, labels = c("na. pol.", "Eur. pol.", "health", "living cond. and \nbackground var.", "other bel.", "work", "int. pol.",
                             "family", "pers. rel.", "con. beh.", "leis. act."))

## 1.10 Concept----
##### Konzept der Frage
df$Concept<- factor(df$Concept, labels = c("Eval. belief", "Feeling", "Importance of sth", "Exp. of future", "Complex con.", "All other con."))

## 1.11 Social desirability----
names(df)
df$Social.Desirability <- factor(df$Social.Desirability, labels = c("No", "A bit", "A lot"))

## 1.12 Centrality----
df$Centrality <- factor(df$Centrality, labels = c("No", "A bit", "Rather", "Central", "Very"))

## 1.13 Reference period----
df$Reference.period <- factor(df$Reference.period, labels = c("Future", "Present", "Past"))

## 1.14 Formulation of the request for an answer----
df$Formulation.of.the.request.for.an.answer..basic.choice <- factor(df$Formulation.of.the.request.for.an.answer..basic.choice, label = c("No", "Indirect", "Direct"))

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
df$Use.of.stimulus.or.statement.in.the.request <- factor(df$Use.of.stimulus.or.statement.in.the.request, labels = c("No", "Yes"))

## 1.23 Absolute or comparative judgment----
df$Absolute.or.comparative.judgment <- factor(df$Absolute.or.comparative.judgment, labels = c("absolute", "comparative"))

## 1.24 Response scale basic choice----
df$Response.scale..basic.choice <- factor(df$Response.scale..basic.choice, labels = c("More than 2", "2", "open-ended", "Magnitude", "Line prod.", "More steps proc."))

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
df$Don.t.know.option <- factor(df$Don.t.know.option, labels = c("present", "only registered", "not present"))

## 1.32 Interviewer instruction----
df$Interviewer.instruction <- factor(df$Interviewer.instruction, labels = c("Absent", "Present"))

## 1.33 Respondent instruction----
df$Respondent.instruction <- factor(df$Respondent.instruction, labels =c("Absent", "Present"))

## 1.34 Extra information or definition----
df$Extra.information.or.definition <- factor(df$Extra.information.or.definition, labels = c("Absent", "Present"))

## 1.35 Knowledge provided----
df$Knowledge.provided <- factor(df$Knowledge.provided, labels = c("No", "Definitions", "Other", "def. and other"))

## 1.36 Introduction available?----
df$Introduction.available. <- factor(df$Introduction.available., labels = c("No", "Yes"))

## 1.37 Request present in the introduction----
df$Request.present.in.the.introduction <- factor(df$Request.present.in.the.introduction, labels = c("not present", "present"))

## 1.38 Showcard or other visual aids used----
df$Showcard.or.other.visual.aids.used <-  factor(df$Showcard.or.other.visual.aids.used, labels = c("Not used", "Used"))

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
df$Computer.assisted <- factor(df$Computer.assisted, label = c("No", "Yes"))

## 1.47 Interviewer----
df$Interviewer <- factor(df$Interviewer, label = c("No", "Yes"))

## 1.48 Visual or oral presentation----
df$Visual.or.oral.presentation <- factor(df$Visual.or.oral.presentation, label = c("Oral", "Visual"))

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
  namen <- gsub(" ",".", namen)
  neue_kat_namen <- paste0(column, "_", namen)
  for( i in 1:length(namen)){
    df[,neue_kat_namen[i]] <- ifelse(!is.na(df[,column]) & df[,column] == namen[i], paste0(namen[i], "_j"), paste0(namen[i], "_n"))
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

# 5. Speichern----
save(df, file = "C:/Uni/13. Semester/Praktikum/R/Aufbereitung/data.RData")


# 6. Alle Variablen, welche im Modell verwendet werden sollten (-> von Schweisstal kopiert, Namen sind anders!)----
## 6.1 Von Schweisstal (d.h. Original)----
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

## 6.2 Mit unseren Namen----
n1 <- c("Language" , "Domain" , "Concept" , "Social.Desirability" , "Centrality" , "Reference.period" ,
  "Formulation.of.the.request.for.an.answer..basic.choice" , "WH.word.used.in.the.request" , 
  "Request.for.an.answer.type" , "Use.of.gradation" , "Balance.of.the.request" , 
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

## 6.3 Mit unseren Namen UND Gifirisierung----
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






