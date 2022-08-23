# 0. Einlesen + librarys----
## 0.1 librarys----
library(dplyr)

## 0.2 Einlesen----
load("C:/Uni/13. Semester/Praktikum/R/Aufbereitung/data.RData")

# 1. Modelle----
## 1.1 Navies LIMO----
### 1.1.1 Umformulieren der NAs----
##### Erster Versuch: NAs in "-99" umwandeln.
##### Betrachtung bis zum ersten Splitpoint: Formulation.of.the.request.for.an.answer..basic.choice, falls "No request present" vs. die anderen beiden Kategorien
##### Danach: WH.word.used.in.the.request
test <- df
test$WH.word.used.in.the.request <- as.character(test$WH.word.used.in.the.request)
test$WH.word.used.in.the.request[is.na(test$WH.word.used.in.the.request)] <- "No request present"
test$WH.word.used.in.the.request <- factor(test$WH.word.used.in.the.request)
test$WH.word.used.in.the.request <- relevel(test$WH.word.used.in.the.request, ref = "No request present")
test$Request.for.an.answer.type <- as.character(test$Request.for.an.answer.type)
test$Request.for.an.answer.type[is.na(test$Request.for.an.answer.type)] <- "No request present"
test$Request.for.an.answer.type <- factor(test$Request.for.an.answer.type)
test$Request.for.an.answer.type <- relevel(test$Request.for.an.answer.type, ref = "No request present")
test$Use.of.gradation <- as.character(test$Use.of.gradation)
test$Use.of.gradation[is.na(test$Use.of.gradation)] <- "No request present"
test$Use.of.gradation <- factor(test$Use.of.gradation)
test$Use.of.gradation <- relevel(test$Use.of.gradation, ref = "No request present")

(model_99 <- lm(quality ~ Language + Domain + Concept + Social.Desirability + Centrality + Reference.period +
                    Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request + Request.for.an.answer.type + Use.of.gradation, data = test))
summary(model_99)

##### Funktioniert nicht: Die Spalten sind abhaengig, sodass einige Kategorien herausgeschmissen werden.



df$path1 <- ifelse(df$Formulation.of.the.request.for.an.answer..basic.choice != "No request present", 0, 1)
model_naive <- lm(quality ~ Language + Domain + Concept + Social.Desirability + Centrality + Reference.period +
                    Formulation.of.the.request.for.an.answer..basic.choice + path1:WH.word.used.in.the.request, data = df)

#Concept, Domain, Social.Desirability, Centrality, Reference.period, Formulation.of.the.request.for.an.answer..basic.choice,

#Wh.word.used.in.the.request, Request.for.an.answer.type, Use.of.gradation, Balance.of.the.request, Presence.of.encouragement,
#Emphasis.on.subjective.opinion.in.request, Information.about.the.opinion.of.other.people

#Use.of.stimulus.or.statement.in.the.request

model_naive <- lm(quality ~ Language + Domain + Concept + Social.Desirability + Centrality + Reference.period +
     Formulation.of.the.request.for.an.answer..basic.choice + WH.word.used.in.the.request + 
     Request.for.an.answer.type + Use.of.gradation + Balance.of.the.request + 
     Presence.of.encouragement.to.answer + Emphasis.on.subjective.opinion.in.request + Use.of.stimulus.or.statement.in.the.request+
     Absolute.or.comparative.judgment + Response.scale..basic.choice + Number.of.categories +
     Theoretical.range.of.the.concept.bipolar.unipolar + Range.of.the.used.scale.bipolar.unipolar +
     Symmetry.of.response.scale + Neutral.category + Number.of.fixed.reference.points +
     Don.t.know.option + Interviewer.instruction + Respondent.instruction + Extra.information.or.definition+
     Knowledge.provided + Introduction.available. + Request.present.in.the.introduction +
     Number.of.sentences.in.introduction + Number.of.words.in.introduction+ Number.of.sentences.in.introduction +
     Number.of.sentences.in.the.request + Number.of.words.in.request + Total.number.of.nouns.in.request.for.an.answer +
     Total.number.of.abstract.nouns.in.request.for.an.answer + Total.number.of.syllables.in.request +
     Number.of.subordinate.clauses.in.request + Number.of.syllables.in.answer.scale +
     Total.number.of.nouns.in.answer.scale + Total.number.of.abstract.nouns.in.answer.scale +
     Showcard.or.other.visual.aids.used + Horizontal.or.vertical.scale + Overlap.of.scale.labels.and.categories+
     Numbers.or.letters.before.the.answer.categories + Scale.with.only.numbers.or.numbers.in.boxes + 
     Start.of.the.response.sentence.on.the.visual.aid + Request.on.the.visual.aid +
     Picture.provided. + Computer.assisted + Interviewer + Visual.or.oral.presentation + Position,
   data = df)


## 1.2 Modell mit Strata----
### 1.2.1 Anzahl der Beobachtungen für die einzelnen Strata----

mod_count <- function(f1 = 0, f2 = 0, f2_1 = 0, f2_2 = 0, f3 = 0, f4 = 0, f5 = 0, f5_1 = 0){
  d <- NA
  f1c <- NA
  f1c <- ifelse(f1 == 0, "No request present", c("Indirect request","Direct request"))
  f2c <- NA
  f2c <- ifelse(f2 == 0, c("Two-category scales", "numerical open-ended answers"), c("More than 2 categories scales", "Marnitude estimation", "Line production", "More steps procedures"))
  f2_1c <- NA
  f2_1c <- ifelse(f2 == 1 & f2_1 == 0, "Theoretically unipolar", ifelse(f2 == 1 & f2_1 == 1, "Theoretically bipolar", NA))
  f2_2c <- NA
  f2_2c <- ifelse(f2 == 1 & f2_2 == 0, "Bipolar", ifelse(f2 == 1 & f2_2 == 1, "Unipolar", NA))
  f3c <- NA
  f3c <- ifelse(f3 == 0, "Absent", "Present")
  f4c <- NA
  f4c <- ifelse(f4 == 0, "Not available", "Abvailable")
  f5c <- NA
  f5c <- ifelse(f5 == 0, "Not used", "Used")
  f5_1c <- NA
  f5_1c <- ifelse(f5 == 1 & f5_1 == 0, "No", ifelse(f5 == 0 & f5_1 == 1, "Yes", NA))
  
d <- ifelse(f2 == 1 & f5 == 1,
         nrow(
           df %>% filter(
             Formulation.of.the.request.for.an.answer..basic.choice %in% f1c &
               Response.scale..basic.choice %in% f2c &
               Theoretical.range.of.the.concept.bipolar.unipolar %in%f2_1c &
               Range.of.the.used.scale.bipolar.unipolar %in% f2_2c &
               Extra.information.or.definition %in% f3c &
               Introduction.available. %in% f4c &
               Showcard.or.other.visual.aids.used %in% f5c &
             Start.of.the.response.sentence.on.the.visual.aid %in% f5_1c
           )
         ),
  ifelse(f2 == 1 & f5 == 0, 
         nrow(df %>% filter(
           Formulation.of.the.request.for.an.answer..basic.choice %in% f1c &
             Response.scale..basic.choice %in% f2c &
           Theoretical.range.of.the.concept.bipolar.unipolar %in% f2_1c &
           Range.of.the.used.scale.bipolar.unipolar %in% f2_2c &
           Extra.information.or.definition %in% f3c &
           Introduction.available. %in% f4c &
           Showcard.or.other.visual.aids.used %in% f5c
  ) 
  ),
  ifelse(f2 == 0 & f5 == 1,
         nrow(
           df %>% filter(
             Formulation.of.the.request.for.an.answer..basic.choice %in% f1c &
               Response.scale..basic.choice %in% f2c &
               Extra.information.or.definition %in% f3c &
               Introduction.available. %in% f4c &
               Showcard.or.other.visual.aids.used %in% f5c &
               Start.of.the.response.sentence.on.the.visual.aid %in% f5_1c
           )
         ), 
  nrow(
    df %>% filter(
      Formulation.of.the.request.for.an.answer..basic.choice %in% f1c &
        Response.scale..basic.choice %in% f2c &
        Extra.information.or.definition %in% f3c &
        Introduction.available. %in% f4c &
        Showcard.or.other.visual.aids.used %in% f5c
      
      
    ))
  )
  ))
  
  return(data.frame(n = d, f1 = f1, f2 = f2, f2_1 = f2_1, f2_2 = f2_2, f3 = f3, f4 = f4, f5 = f5, f5_1 = f5_1))
}

m <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)
m <- m[which(!(m[,2] == 0 & m[,3] == 1)),]
m <- m[which(!(m[,2] == 0 & m[,4] == 1)),]
m <- m[which(!(m[,7] == 0 & m[,8] == 1)),]

count <- mod_count(f1 = m[1,1], f2 =  m[1,2], f2_1 = m[1,3], f2_2 = m[1,4], f3 = m[1,5], f4 = m[1,6], f5 = m[1,7], f5_1 = m[1,8])
count <- rbind(count, mod_count(m[2,1], m[2,2], m[2,3], m[2,4], m[2,5], m[2,6], m[2,7], m[2,8]))

for(i in 2:nrow(m)){
  count <- rbind(count, mod_count(m[i,1], m[i,2], m[i,3], m[i,4], m[i,5], m[i,6], m[i,7], m[i,8]))
}







nrow(df %>% filter(
  Formulation.of.the.request.for.an.answer..basic.choice == "No request present" &
    Response.scale..basic.choice == "numerical open-ended answers" &
    Extra.information.or.definition == "Absent"
))

nrow(df[which(df$Formulation.of.the.request.for.an.answer..basic.choice == "No request present" &
           df$Response.scale..basic.choice == "numerical open-ended answers"&
           df$Extra.information.or.definition == "Absent"&
             df$Introduction.available. == "Not available"
           ),]
       
       )


nrow(
  df %>% filter(
    Formulation.of.the.request.for.an.answer..basic.choice %in% f1c &
      Response.scale..basic.choice %in% f2c &
      Extra.information.or.definition %in% f3c &
      Introduction.available. %in% f4c &
      Showcard.or.other.visual.aids.used %in% f5c
  ))


data.frame(
            df %>% filter(
              Formulation.of.the.request.for.an.answer..basic.choice == "No request present" &
                Theoretical.range.of.the.concept.bipolar.unipolar == "Theoretically unipolar" &
                Introduction.available. == "Not available" &
                Showcard.or.other.visual.aids.used == "Not used"
            ) %>% summarise(n = n()) %>% select(n)
          )

c("lang", "domain", "concept", "socdesir" ,"centrality" ,"ref_period", "form_basic", "used_WH_word",
  "questiontype", "gradation","balance","encourage","subjectiveop","opinionother",
  "stimulus","absolute","scale_basic","labels","fixrefpoints",
  "labels_gramm","labels_order","scale_corres","scale_trange","scale_urange",
  "symmetry","scale_neutral","Dont_know","instr_interv","instr_respon",
  "motivation","knowledge","intropresent","intr_request","usedshowcard",
  "showc_horiz","showc_over","showc_letters","showc_boxes","showc_start",
  "showc_quest","showc_pict","ncategories","nsents_intro","nsents_quest",
  "nwords_intro","numsub_intro","nwords_quest","nnouns_quest","nabst_quest",
  "nsyll_quest","nsub_quest","nsyll_ans","nnouns_ans","nabst_ans",
  "computer_assisted","interviewer","visual", "range_correspondence")



maxi <- data.frame(F1 = sample(1:5,5, replace = T),
                   F2 = sample(1:5, 5, replace = T),
                   F3 = sample(1:5, 5, replace = T))

for(i in 1 : 40){
  maxi[,i] <- sample(1:5, 5, replace = T)
}
