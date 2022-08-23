# 0. Einlesen, Librarys, Farben----
## 0.1 Librarys----
library(dplyr)
library(ggplot2)
library(tidyr)
library(ineq)

## 0.2 Einlesen----
load("C:/Uni/13. Semester/Praktikum/R/Aufbereitung/data.RData")


##### WICHTIG MITTEILEN!!!!
##### df$Information.about.the.opinion.of.other.people hat nur 0 oder NA -> rauslassen!
ggplot(gather(df_sub[,c(2:17)]), aes(value, fill = value))+
  geom_histogram(stat = "count", show.legend = F)+
  facet_wrap(~key, scales = "free_x")


## 0.3 Farben----

# 1. Deskription----
## 1.1 Stimmen die Pfade aus Generelle Informationen -> Plots Daten Routing ?
### 1.1.1 Formulation of the request for an answer: basic choice----
#### Erster Splitpoint in Graph bei "Formulation of the request for an answer: basic choice
#### -> Pfad in Grafik 4 und 5
table(df$Formulation.of.the.request.for.an.answer..basic.choice)

df_sub <- df %>% filter(Formulation.of.the.request.for.an.answer..basic.choice == "No request present" & !is.na(df$WH.word.used.in.the.request))


### 1.1.2 Response scale basic choice----
##### Zweiter Splitpoint in Graph bei "Response scale basic choice"
##### -> Pfad in Graphik sechs, sieben und acht

df_sub11 <- df %>% filter(Response.scale..basic.choice == "Two-category scales" & !is.na(Theoretical.range.of.the.concept.bipolar.unipolar))
df_sub12 <- df %>% filter(Response.scale..basic.choice ==  "numerical open-ended answers" & !is.na(Theoretical.range.of.the.concept.bipolar.unipolar))


table(df_sub11$Number.of.categories, useNA = "always")
table(df_sub12$Number.of.categories, useNA = "always")

table(df_sub11$Theoretical.range.of.the.concept.bipolar.unipolar, useNA = "always")
table(df_sub12$Theoretical.range.of.the.concept.bipolar.unipolar, useNA = "always")

table(df_sub11$Range.of.the.used.scale.bipolar.unipolar, useNA = "always")
table(df_sub12$Range.of.the.used.scale.bipolar.unipolar, useNA = "always")

table(df_sub11$Symmetry.of.response.scale, useNA = "always")
table(df_sub12$Symmetry.of.response.scale, useNA = "always")

table(df_sub11$Neutral.category, useNA = "always")
table(df_sub12$Neutral.category, useNA = "always")

table(df_sub11$Number.of.fixed.reference.points, useNA = "always")
table(df_sub12$Number.of.fixed.reference.points, useNA = "always")

table(df_sub11$Don.t.know.option, useNA = "always")
table(df_sub12$Don.t.know.option, useNA = "always")

### 1.1.4 Extra information or definition 4. Pfad----
##### Vierter Splitpoint in Graph bei "Extra information or definition"
##### -> Pfad in Graphik acht
df_sub_31 <- df %>% filter(Extra.information.or.definition == "Absent" & !is.na(Knowledge.provided))

table(df_sub_31$Knowledge.provided, useNA = "always")

### 1.1.5 Introduction available? 5. Pfad----
##### Fünfter Splitpoint in Graph bei "Introduction available?"
##### -> Pfad in Graphik acht / neun
df_sub_51 <- df %>% filter(Introduction.available. == "Not Available" & !is.na(Request.present.in.the.introduction))

table(df_sub_31$Request.present.in.the.introduction, useNA = "always")

table(df_sub_31$Number.of.words.in.introduction, useNA = "always")

table(df_sub_31$Number.of.subordinated.clauses.in.introduction, useNA = "always")

### 1.1.6 Showcard or other visual aids used 6. Pfad----
##### sechster Splitpoint in Graph bei "Showcard or other visual aids used"
##### -> Pfad in Graphik zehn
df_sub_61 <- df %>% filter(Showcard.or.other.visual.aids.used == "Not used" & !is.na(Horizontal.or.vertical.scale))


## 1.1 Outcome----
ggplot(df, aes(x = quality))+
  geom_histogram()

#pdf("C:/Uni/13. Semester/Praktikum/Generelle Informationen/Plots/Plots/Val_Rel.pdf", width = 8, height = 5)
ggplot(df, aes(x = reliability.r.2., y = validity.v.2.))+
  geom_point()+
  labs(title = "Validität und Reliabilität im gesamten Datensatz", x = expression(paste(Reliabilität^{2})), y = expression(paste(Validität^{2})))
#dev.off()

ggplot(df,aes(x = quality, y = rep(0, nrow(df))))+
  geom_point()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

df <- df[!is.na(df$quality),]
m <- fitdistrplus::fitdist(df$quality, "beta", method = "mme")
label <- paste0("alpha = ", round(m$estimate[1],2), "\nbeta = ", round(m$estimate[2],2))

ggplot(df) +
  geom_histogram(aes(quality, y = ..density..), color = "black", fill = "white", binwidth = .0125) +
  stat_function(fun = function(x) dbeta(x, m$estimate[1], m$estimate[2]), color = "red",
                size = 1) +
  geom_label(data = data.frame(x = .125, y = 1.5, label = paste0("alpha = ", round(m$estimate[1],2), "\nbeta = ", round(m$estimate[2],2))),
             aes(x = x, y = y, label = label), fill = "white")+
  xlab("Qualitaet")+
  geom_vline(xintercept = m$estimate[1] / (m$estimate[1] + m$estimate[2]), linetype="dotted", 
             color = "blue", size=1.5)

m1 <- fitdist(df$quality[which(df$quality != 1)], dist = "nbinom", fix.arg = list(size = nrow(df)), start = list(prob = mean(df$quality / nrow(df))), method = "mle")
m1 <- fitdist(df$quality, dist = "norm")
m2 <- fitdist(df$quality, dist = "chisq", start = list(df = 6073))

geom_histogram(aes(quality, y = ..density..), color = "black", fill = "white", binwidth = .0125) +
## 1.2 Outcome + Domain----
df$Domain <- factor(df$Domain)
ggplot(df, aes(x = quality, y = Domain, color = Domain))+
  geom_point()

MASS::fitdistr(career_filtered$average, dbeta,
               start = list(shape1 = 1, shape2 = 10))

## 1.3 Outcome + Language----
df$Language
ggplot(df, aes(x = quality, fill = Language))+
  geom_density(alpha = .75)+
  geom_rug()

ggplot(df, aes(x = validity.v.2., fill = Language))+
  geom_density(alpha = .75)+
  geom_rug()


df_sub <- df %>% dplyr::select(quality, Language, Domain, Concept, Social.Desirability, Centrality, Reference.period, Formulation.of.the.request.for.an.answer..basic.choice,
              WH.word.used.in.the.request, Request.for.an.answer.type, Use.of.gradation, Balance.of.the.request, Presence.of.encouragement.to.answer,
              Emphasis.on.subjective.opinion.in.request, Use.of.stimulus.or.statement.in.the.request, Absolute.or.comparative.judgment, Response.scale..basic.choice,
              Number.of.categories, Theoretical.range.of.the.concept.bipolar.unipolar, Range.of.the.used.scale.bipolar.unipolar, Symmetry.of.response.scale,
              Neutral.category, Number.of.fixed.reference.points, Don.t.know.option, Interviewer.instruction, Respondent.instruction, Extra.information.or.definition,
              Knowledge.provided, Introduction.available., Request.present.in.the.introduction, Number.of.sentences.in.introduction, Number.of.words.in.introduction,
              Number.of.sentences.in.introduction, Number.of.sentences.in.the.request, Number.of.words.in.request, Total.number.of.nouns.in.request.for.an.answer,
              Total.number.of.abstract.nouns.in.request.for.an.answer, Total.number.of.syllables.in.request, Number.of.subordinate.clauses.in.request,
              Number.of.syllables.in.answer.scale, Total.number.of.nouns.in.answer.scale, Total.number.of.abstract.nouns.in.answer.scale,
              Showcard.or.other.visual.aids.used, Horizontal.or.vertical.scale, Overlap.of.scale.labels.and.categories, Numbers.or.letters.before.the.answer.categories,
              Scale.with.only.numbers.or.numbers.in.boxes, Start.of.the.response.sentence.on.the.visual.aid, Request.on.the.visual.aid,
              Picture.provided., Computer.assisted, Interviewer, Visual.or.oral.presentation, Position)


vars <- data.frame( namen = names(df_sub), 
                    klasse = unlist(lapply(df_sub, class)))

unlist(ifelse(lapply(df_sub, is.factor), lapply(df_sub, function(x) length(levels(x))), NA))

vars$klasse[which(vars$klasse == "factor")]

 