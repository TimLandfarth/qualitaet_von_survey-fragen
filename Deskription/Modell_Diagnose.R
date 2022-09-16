# 0. 
## 0.1 Einlesen der Daten----
load("Aufbereitung/data.RData")

## 0.2 Librarys ----
library(ggplot2)
library(dplyr)
library(glmmTMB)

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

## 0.5 Berechnen der Modelle----
### 0.5.1 lmer----
lmer_model_nloptwrap <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
                   control = lmerControl(optCtrl = list(maxfun = 100000)))

lmer_model_nerlder <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
                        control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))

lmer_model_bobyqa <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
                           control = lmerControl(optCtrl = list(optimizer = "Nelder Mead", maxfun = 100000)))

lmer_model_nlminbwrap <- lmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df,
                          control = lmerControl(optCtrl = list(optimizer = "nlminbwrap", maxfun = 100000)))


### 0.5.2 glmer----
glmer_bin.model_nloptwrap <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
                                weights = sample_size, control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 100000)))


glmer_bin.model_nelder <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
                         weights = sample_size, control = glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000)))

glmer_bin.model_bobyqa <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
                                weights = sample_size, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

glmer_bin.model_nlminbwrap <- glmer(paste("quality ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1|Country) + (1|Study) + (1|experiment)"), data = df, family = binomial(link = "logit"),
                                weights = sample_size, control = glmerControl(optimizer = "nlminbwrap", optCtrl = list(maxfun = 100000)))

### 0.5.3 glmmTMB----
df$quality_adj <- NA
df$quality_adj <- ifelse(df$quality == 0, .0001, df$quality)
df$quality_adj[df$quality == 1] <- 0.999
glmmTMB_beta.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[-55], collapse = " + "), " + (1 | Country) + (1|Study/experiment)")), data = df, family = glmmTMB::beta_family(link = "logit"),
                         control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size)

glmmTMB_normal.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1 | Country) + (1 | Study/experiment)")), data = df, family = gaussian(),
                                       control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), collect = FALSE), weights = sample_size)

glmmTMB_beta.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1 | Language) + (1 | Study) + (1|experiment)")), data = df, family = glmmTMB::beta_family(link = "logit"),
                                       control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), profile = TRUE, collect = FALSE))

glmmTMB_normal.model <- glmmTMB::glmmTMB(as.formula(paste("quality_adj ~", paste(model_names[c(1:54, 56:59)], collapse = " + "), "+ (1 | Language) + (1 | Study) + (1|experiment)")), data = df, family = gaussian(),
                                         control = glmmTMB::glmmTMBControl(optCtrl = list(iter.max = 20000,eval.max = 20000), profile = TRUE, collect = FALSE))

DHARMa::simulateResiduals(glmmTMB_beta.model, plot = T)
DHARMa::simulateResiduals(glmmTMB_normal.model, plot = T)

### 0.5.2 GLMMadaptive----
GLMMadaptive::mixed_model(as.formula(paste("quality ~", paste(model_names, collapse = " + "))), random = ~ 1 | Country / Study / experiment, family = GLMMadaptive::beta.fam(), optimizer = "nlminb",
                          iter_qN_incr = 20, nAGQ = 21, data = df)


# Reproduzierbares Beispiel
set.seed(12345)
a <- sample(c(0,1), 1000, replace = T)
df1 <- data.frame(a = a,
                  b = ifelse(a == 1, sample(c(0,1)), NA))
df1$b_gifi_eins <- ifelse(!is.na(df1$b) & df1$b == 1, 1, 0)
df1$b_gifi_null <- ifelse(!is.na(df1$b) & df1$b == 0, 1, 0)
df1$out <- rbeta(1000, 1, 1)

m <- glmmTMB::glmmTMB(out ~ b_gifi_eins + b_gifi_null + a, data = df1, family = glmmTMB::beta_family(link = "logit"))
m1 <- glmmTMB::glmmTMB(out ~ b_gifi_null + a, data = df1, family = glmmTMB::beta_family(link = "logit")) 
m2 <- glmmTMB::glmmTMB(out ~ b_gifi_null + a, data = df1, family = gaussian())
# 1. Diagnostics nach Buch----
## 1.1 Caterpillar plot----










#random = ~1 | Study,family = beta.fam(),iter_EM = 0, optimizer = "nlminb",
#initial_values = list(betas = GLMMadaptive_question1$coefficients, D = matrix(c(0.5, 0, 0, 0.1), 2, 2)),iter_qN_incr = 20,nAGQ = 21, data = df



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

