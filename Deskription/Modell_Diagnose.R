# 0. 
## 0.1 Einlesen der Daten----
load("Aufbereitung/data.RData")
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Modelle/modelle.RData")

## 0.2 Librarys ----
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(forcats)

## 0.3 zusammenfassen aller nicht ESS studien zu einer Studie----
df$Study[which( !(df$Study %in% c(paste("ESS Round", 1:7))))] <- "Other"

## 0.4 Funktionen ----
### 0.4.1 zur Betrachtung der stetigen Parameter----
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

### 0.4.2 Caterpillarplots fuer glmmTMB zum Vergleich zweier Modelle----
### Input:
#### mod: glmmTMB Modell 1
#### mod2: glmmTMB Modell 2
#### j: Welche Random Effecte?
##### j = 1: Country
##### j = 2: Experiment:Study
##### j = 3: Study

### Output:
#### caterpillarplot

### To Do:
#### Hinzufuegen der KIs, welche bisher aufgrund von NA nicht berechnet werden koennen.

caterp <- function(mod, mod2 = NULL, j){
  if(is.null(mod2)){
    t <- ranef(mod)$cond
    for(i in 1:length(t)){
      t[[i]]$names <- rownames(t[[i]])
      t[[i]]$mod <- 1
    }  
    v <- t[[j]] %>% arrange(`(Intercept)`) %>% mutate(names = factor(names, levels = names))
    v %>%
      ggplot(aes(x = names, y = `(Intercept)`, color = names))+
      geom_point(show.legend = F)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  if(!is.null(mod2)){
  t <- ranef(mod)$cond
  for(i in 1:length(t)){
    t[[i]]$names <- rownames(t[[i]])
    t[[i]]$mod <- 1
  }
  
  u <- ranef(mod2)$cond
  for(i in 1:length(u)){
    u[[i]]$names <- rownames(u[[i]])
    u[[i]]$mod <- 2
  }
  v <- rbind(t[[j]] %>% arrange(`(Intercept)`) %>% mutate(names = factor(names, levels = names))
             , u[[j]] %>% arrange(`(Intercept)`) %>% mutate(names = factor(names, levels = names))
  )
  
  v %>%
    ggplot(aes(x = names, y = `(Intercept)`, color = names))+
    geom_point(show.legend = F)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_wrap(~mod)
  }
  
  # Fuer spaeter:
  ## Covarianz der random effekte:
  # sqrt(TMB::sdreport(mod_glmmTMB_beta_full_non.reml$obj,getJointPrecision=TRUE)$diag.cov.random)
}


### 0.4.3 Caterpillarplots fuer mehrere glms----

function(df = df, all.vars = T){
  contr_name <- unique(df$Country)
  ifelse(all.vars == T, model_names = model_names, model_names = model_names_nondich)
  for(i in 1:length(unique(df$Country))){
    glmmTMB::glmmTMB(as.formula(paste("quality ~", paste(model_names, collapse = " + "))), data = df %>% filter(df$Country == contr_name[i]), family = glmmTMB::beta_family(link = "logit"))
  }
  glmmTMB::glmmTMB(as.formula(paste("quality ~", paste0(model_names, collapse = " + "))), data = df %>% filter(df$Country == contr_name[1]), family = glmmTMB::beta_family(link = "logit"))
}
test <- glmmTMB::glmmTMB(as.formula(paste("quality ~", paste0(model_names[1:30], collapse = " + "))), data = df %>% filter(df$Country == "Austria"), family = glmmTMB::beta_family(link = "logit"))




# 1. QQ und Residuen----
## 1.1 glmmTMB----
### 1.1.1 Volles Modell----
DHARMa::simulateResiduals(mod_glmmTMB_beta_full_non.reml, plot = T)

### 1.1.2 Ohne dichotome GIFI Variablen----
DHARMa::simulateResiduals(mod_glmmTMB_beta_sub_non.reml, plot = T)

# 2. 
?ranef()





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

# 2. Modellannahmen----

