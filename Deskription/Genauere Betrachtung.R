# 0. 
## 0.1 Einlesen der Daten----
load("Aufbereitung/data.RData")

## 0.2 Librarys ----
library(ggplot2)
library(dplyr)

## 0.3 zusammenfassen aller nicht ESS studien zu einer Studie----
df$Study[which( !(df$Study %in% c(paste("ESS Round", 1:7))))] <- "Other"

## 0.4 Funktionen ----
## 0.4.1 zur Betrachtung der stetigen Parameter----
stetig <- function(column){
  a <- enquo(column)
  n_studie_med <- df %>% group_by(Study, !!a) %>% summarise(n = n(), .groups = "drop") %>% group_by(Study) %>% summarise(n = n()) %>% summarise(m = median(n))
  n_studie_min <- df %>% group_by(Study, !!a) %>% summarise(n = n(), .groups = "drop") %>% group_by(Study) %>% summarise(n = n()) %>% summarise(m = min(n))
  n_Land_med <- df %>% group_by(Country, !!a) %>% summarise(n = n(), .groups = "drop") %>% group_by(Country) %>% summarise(n = n()) %>% summarise(m = median(n))
  n_Land_min <- df %>% group_by(Country, !!a) %>% summarise(n = n(), .groups = "drop") %>% group_by(Country) %>% summarise(n = n()) %>% summarise(m = min(n))
  return(cat(paste("Anzahl an Kategorien in Studien:", "\nMin: ", n_studie_min, "\nMed: ", n_studie_med, "\n \nAnzahl an Kategorien in Laendern:",
                   "\nMin: ", n_Land_min, "\nMed: ", n_Land_med)))

}

stetig_plot <- function(column){
  a <- enquo(column)
  p_studie <- ggplot(data = df, aes(x = !!a, y = quality, color = Study))+
    geom_point()+
    geom_smooth(formula = y ~ x, method = "gam")+
    facet_wrap(~Study)+
    labs(title = "Studien")
  
  p_Land <- ggplot(data = df, aes(x = !!a, y = quality, color = Country))+
    geom_point()+
    geom_smooth(formula = y ~ x, method = "gam")+
    facet_wrap(~Country)+
    labs(title = "Laender")
  return(list(p_studie, p_Land))
}
# 1. Exploratory analysis of cluster specific heterogeneity----
## 1.1 Stetige Merkmale----
### 1.1.1 Number of categories----
##### Kommentare: Siehe .txt File vom 06_09_2022.txt im Barbara Ordner
##### Median: Anzahl an Kategorien pro Studie
stetig(Number.of.categories)
stetig_plot(Number.of.categories)

### 1.1.2 Number.of.fixed.reference.points----
stetig(Number.of.fixed.reference.points)
stetig_plot(Number.of.fixed.reference.points)

### 1.1.3 Number.of.sentences.in.introduction----
stetig(Number.of.sentences.in.introduction)
stetig_plot(Number.of.sentences.in.introduction)

### 1.1.4 Number.of.words.in.introduction----
stetig(Number.of.words.in.introduction)
stetig_plot(Number.of.words.in.introduction)

### 1.1.6 Number.of.sentences.in.the.request----
stetig(Number.of.sentences.in.the.request)
stetig_plot(Number.of.sentences.in.the.request)

### 1.1.7 Number.of.words.in.request----
stetig(Number.of.words.in.request)
stetig_plot(Number.of.words.in.request)


### 1.1.8 Total.number.of.nouns.in.request.for.an.answer----
stetig(Total.number.of.nouns.in.request.for.an.answer)
stetig_plot(Total.number.of.nouns.in.request.for.an.answer)


### 1.1.9 Total.number.of.syllables.in.request----
stetig(Total.number.of.syllables.in.request)
stetig_plot(Total.number.of.syllables.in.request)

### 1.1.10 Number.of.subordinate.clauses.in.request----
stetig(Number.of.subordinate.clauses.in.request)
stetig_plot(Number.of.subordinate.clauses.in.request)

### 1.1.11 Number.of.syllables.in.answer.scale----
stetig(Number.of.syllables.in.answer.scale)
stetig_plot(Number.of.syllables.in.answer.scale)

### 1.1.12 Total.number.of.nouns.in.answer.scale----
stetig(Total.number.of.nouns.in.answer.scale)
stetig_plot(Total.number.of.nouns.in.answer.scale)

### 1.1.13 Total.number.of.abstract.nouns.in.answer.scale----
stetig(Total.number.of.abstract.nouns.in.answer.scale)
stetig_plot(Total.number.of.abstract.nouns.in.answer.scale)
