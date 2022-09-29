# 0. Laden und Librarys
## 0.1 Einlesen der Daten----
load("Aufbereitung/data.RData")
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Modelle/modelle.RData")

## 0.2 Librarys ----
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(forcats)
library(DHARMa)
library(ggExtra)
library(stringr)
library(tidyr)
library(scales)
library(gridExtra)

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

stetig_plot <- function(column, lang = T, study_exp = F, study = F){
  a <- enquo(column)
  if(isTRUE(lang)){
    return(ggplot(data = df, aes(x = !!a, y = quality, color = Language))+
      geom_point(show.legend = F)+
      geom_smooth(formula = y ~ x, method = "gam", show.legend = F)+
      facet_wrap(~Language)+
      labs(title = "Sprache"))
  }
  if(isTRUE(study_exp)){
    return(ggplot(data = df %>% mutate(study_exp = stringr::str_c(df$Study, df$experiment, sep = " : ")), aes(x = !!a, y = quality, color = study_exp, group = study_exp))+
    geom_point(show.legend = F)+
    geom_smooth(formula = y ~ x, method = "gam", show.legend = F, se = F)+
    facet_wrap(~study_exp)+
    labs(title = "Studien"))
  }
  if(isTRUE(study)){
  return(ggplot(data = df, aes(x = !!a, y = quality, color = Language))+
    geom_point(show.legend = F)+
    geom_smooth(formula = y ~ x, method = "gam", show.legend = F)+
    facet_wrap(~Language)+
    labs(title = "Sprache"))
  }
}

diskret_plot <- function(column){
  a <- enquo(column)
  ggplot(data = df, aes(x = !!a, y = quality, color = Language))+
    geom_boxplot()+
    facet_wrap(~Language)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))+
    labs(title = "Sprache")
  
  
  
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

caterp <- function(mod, mod2 = NULL, j){
  if(is.null(mod2)){
    t <- ranef(mod)$cond
    s <- TMB::sdreport(mod[["obj"]], getJointPrecision = T)$diag.cov.random
    t[[1]]$sd <- s[1:length(t[[1]])]
    t[[2]]$sd <- s[1:length(t[[2]])]
    t[[3]]$sd <- s[1:length(t[[3]])]
    #return(t)
    t[[1]]$cmax <- t[[1]]$`(Intercept)` + 1.96 * t[[1]]$sd
    t[[2]]$cmax <- t[[2]]$`(Intercept)` + 1.96 * t[[2]]$sd
    t[[3]]$cmax <- t[[3]]$`(Intercept)` + 1.96 * t[[3]]$sd
    t[[1]]$cmin <- t[[1]]$`(Intercept)` - 1.96 * t[[1]]$sd
    t[[2]]$cmin <- t[[2]]$`(Intercept)` - 1.96 * t[[2]]$sd
    t[[3]]$cmin <- t[[3]]$`(Intercept)` - 1.96 * t[[3]]$sd
    
        for(i in 1:length(t)){
      t[[i]]$names <- rownames(t[[i]])
      t[[i]]$mod <- 1
    }
    #return(t)
    v <- t[[j]] %>% arrange(`(Intercept)`) %>% mutate(names = factor(names, levels = names))
    labelz <- c("Language", "Intercept", "Caterpillarplot for random effect")
    labelz[1] <- ifelse(j == 2, "Experiment : Study", ifelse(j == 3, "Study", labelz[1]))
    #return(v)
    return(v %>%
      ggplot(aes(x = names, y = `(Intercept)`, color = names, ymax = `(Intercept)` + 1.96 * sd, ymin =  `(Intercept)` - 1.96 * sd))+
      geom_hline(yintercept = 0, linetype = "dashed", alpha = .75)+
      geom_point(show.legend = F)+
      geom_errorbar(show.legend = F)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = ifelse(j == 2, 4, 8)), plot.title = element_text(hjust = .5))+
      scale_y_continuous(limits = c(-1.5, 1.5), n.breaks = 6)+
      labs(title = paste(labelz[3], labelz[1]), x = labelz[1], y = labelz[2]))
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.y = "Intercept")+
    facet_wrap(~mod)
  }
  
}

### 0.4.3 Residuen vs. Kovariablen----
##### Designmatrix muss selbst berechnet werden, da keine Funktion vorhanden

X_fix <- model.matrix(as.formula(paste("quality ~", paste(model_names[-55], collapse = " + "))), df)
X_fix <- X_fix[,which((colnames(X_fix) %in% names(fixef(mod_lme4_gauss_q))))]
y_Xbetahat <- as.vector(df$quality) - X_fix %*% as.vector(fixef(mod_lme4_gauss_q))
Xbeta <- test %*% as.vector(fixef(mod_lme4_gauss_q))

resid_marginal <- resid(mod_lme4_gauss_q, level = 0)
resididi <- resid(mod_glmmTMB_gauss_q, level = 0)

y_Xbetahat[1:10]
resid_marginal[1:10]
resididi[1:10]

df$res <- y_Xbetahat

res_cov_plot <- function(mod, columns, continuous = FALSE, with.quality = FALSE, lme = FALSE, val = "quality"){
  out <- df[[val]]
  if(isFALSE(lme)){
    X_fix <- model.matrix(as.formula(paste(as.character(val), "~", paste(model_names[-55], collapse = " + "))), df)
    X_fix <- X_fix[,which((colnames(X_fix) %in% names(fixef(mod)$cond)))]
    y_Xbetahat <- as.vector(out) - X_fix %*% as.vector(fixef(mod)$cond)
    limits <- c(-ceiling(max(abs(y_Xbetahat[,1]))), ceiling(max(abs(y_Xbetahat[,1]))))
  }
  if(isTRUE(lme)){
    X_fix <- model.matrix(as.formula(paste(val,"~", paste(model_names[-55], collapse = " + "))), df)
    X_fix <- X_fix[,which((colnames(X_fix) %in% names(fixef(mod))))]
    y_Xbetahat <- as.vector(out) - X_fix %*% as.vector(fixef(mod))
    limits <- c(-ceiling(max(abs(y_Xbetahat[,1]))), ceiling(max(abs(y_Xbetahat[,1]))))
  }
  a <- enquo(columns)
  #return(-ceiling(max(abs(df %>% pivot_longer(cols = !!a) %>% mutate(res = y_Xbetahat[,1]) %>% select(res)))))
  #return(y_Xbetahat)
  if(isTRUE(continuous)){
    return(
      df %>% mutate(res = y_Xbetahat[,1]) %>% pivot_longer(cols = !!a) %>% 
        ggplot(aes(x = value, y = res))+
        ylim(limits)+
        stat_bin2d(bins = 75) +
        scale_fill_gradientn(colours = c("#91bfdb", "#ffffbf", "#fc8d59"), limits = c(0,500),values = rescale(c(0, 50, 500)))+
        geom_smooth(method = "loess", se = F)+
        geom_hline(yintercept = 0, color = "red", alpha = .5)+
        labs(x = "Count", y = "Residuals", fill = "Count")+
        facet_wrap(~name)
    )
  }
  if(isFALSE(continuous) & isFALSE(with.quality)){
    return(
      df %>% mutate(res = y_Xbetahat[,1]) %>% pivot_longer(cols = !!a) %>%
        ggplot(aes(x = value, y = res))+
        ylim(limits)+
        geom_point(position = position_jitter(width = .25), alpha = .05)+
        geom_hline(yintercept = c(-.5, .5), alpha = .25, color = "blue")+
        geom_hline(yintercept = 0, alpha = .25, colour = "red")+
        labs(x = "Category", y = "Residuals")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))+
        facet_wrap(~name, scales = "free", nrow = 3)
    )
  }
  if(isFALSE(continuous) & isTRUE(with.quality)){
    return(
      df %>% mutate(res = y_Xbetahat[,1]) %>% pivot_longer(cols = !!a) %>%
        ggplot(aes(x = value, y = res, color = quality))+
        ylim(limits)+
        geom_point(position = position_jitter(width = .25), alpha = 1)+
        geom_hline(yintercept = c(-.5, .5), alpha = .5, color = "blue")+
        geom_hline(yintercept = 0, alpha = .5, colour = "red")+
        labs(x = "Category", y = "Residuals")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))+
        scale_color_gradientn(colours = c("#7570b3", "#1b9e77"))+
        facet_wrap(~name, scales = "free", nrow = 3))
  }
}




# 1. QQ----
## 1.1 Qualitaet----
## 1.1.1 lmer----
qqnorm(resid(mod_lme4_gauss_q))

## 1.1.2 glmmTMB----
#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/QQ plot.pdf", width = 8, height = 5) 
DHARMa::plotQQunif(mod_glmmTMB_gauss_q, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residual plot.pdf", width = 8, height = 5) 
DHARMa::plotQQunif(mod_glmmTMB_beta_q, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)
#dev.off()

## 1.2 Reliabilitaet----
### 1.2.1 lmer----
qqnorm(resid(mod_lme4_gauss_r))

### 1.2.2 glmmTMB----
DHARMa::plotQQunif(mod_glmmTMB_gauss_r, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)

DHARMa::plotQQunif(mod_glmmTMB_beta_r, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)

## 1.3 Validitaet----
### 1.3.1 lmer----
qqnorm(resid(mod_lme4_gauss_v))

### 1.2.2 glmmTMB----
DHARMa::plotQQunif(mod_glmmTMB_gauss_v, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)

DHARMa::plotQQunif(mod_glmmTMB_beta_v, plot = T, testUniformity = F, testOutliers = F, testDispersion = F)

# 2. Residuen plots----
## 2.1 Qualitaet----
#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen stetig 1.pdf", width = 10, height = 6.25) 
res_cov_plot(mod_glmmTMB_gauss_q, Position, continuous = T)
res_cov_plot(mod_lme4_gauss_q, Position, continuous = T, lme = T)
res_cov_plot(mod_glmmTMB_beta_q, Position, continuous = T)
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen stetig 2.pdf", width = 10, height = 6.25) 
res_cov_plot(mod_glmmTMB_gauss_q,
  c(
    Number.of.syllables.in.answer.scale,
    Number.of.words.in.request,
    Number.of.words.in.introduction,
    Total.number.of.syllables.in.request
  ),
  continuous = T
)
res_cov_plot(mod_lme4_gauss_q,
             c(
               Number.of.syllables.in.answer.scale,
               Number.of.words.in.request,
               Number.of.words.in.introduction,
               Total.number.of.syllables.in.request
             ),
             continuous = T, lme = T
)
res_cov_plot(mod_glmmTMB_beta_q,
             c(
               Number.of.syllables.in.answer.scale,
               Number.of.words.in.request,
               Number.of.words.in.introduction,
               Total.number.of.syllables.in.request
             ),
             continuous = T
             )
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen stetig 3.pdf", width = 10, height = 6.25) 
p1 <- res_cov_plot(
  mod_glmmTMB_gauss_q,
  c(
    Number.of.categories,
    Number.of.fixed.reference.points,
    Number.of.sentences.in.introduction,
    Number.of.sentences.in.the.request,
    Total.number.of.nouns.in.request.for.an.answer,
    Number.of.subordinate.clauses.in.request,
    Total.number.of.nouns.in.answer.scale,
    Total.number.of.abstract.nouns.in.answer.scale
  )
  ,
  continuous = T
)

p2 <- res_cov_plot(
  mod_lme4_gauss_q,
  c(
    Number.of.categories,
    Number.of.fixed.reference.points,
    Number.of.sentences.in.introduction,
    Number.of.sentences.in.the.request,
    Total.number.of.nouns.in.request.for.an.answer,
    Number.of.subordinate.clauses.in.request,
    Total.number.of.nouns.in.answer.scale,
    Total.number.of.abstract.nouns.in.answer.scale
  )
  ,
  continuous = T, lme = TRUE
)

p3 <- res_cov_plot(
  mod_glmmTMB_beta_q,
  c(
    Number.of.categories,
    Number.of.fixed.reference.points,
    Number.of.sentences.in.introduction,
    Number.of.sentences.in.the.request,
    Total.number.of.nouns.in.request.for.an.answer,
    Number.of.subordinate.clauses.in.request,
    Total.number.of.nouns.in.answer.scale,
    Total.number.of.abstract.nouns.in.answer.scale
  )
  ,
  continuous = T
)
grid.arrange(p1,p2,p3,ncol = 3)
#dev.off()

#### 1.1.2.2 Diskrete Effekte----
#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 1.pdf", width = 10, height = 6.25) 
res_cov_plot(mod_glmmTMB_beta_q,
  c(
    Domain,
    Concept,
    Social.Desirability,
    Centrality,
    Reference.period,
    Formulation.of.the.request.for.an.answer..basic.choice
  )
)

res_cov_plot(mod_lme4_gauss_q,
  c(
    Domain,
    Concept,
    Social.Desirability,
    Centrality,
    Reference.period,
    Formulation.of.the.request.for.an.answer..basic.choice
  ), lme = TRUE
)

res_cov_plot(mod_glmmTMB_beta_q,
                   c(
                     Domain,
                     Concept,
                     Social.Desirability,
                     Centrality,
                     Reference.period,
                     Formulation.of.the.request.for.an.answer..basic.choice
                   )
)
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 2.pdf", width = 10, height = 6.25) 
res_cov_plot(mod_glmmTMB_gauss_q,
  c(
    Use.of.gradation_No,
    Use.of.gradation_Yes,
    Request.for.an.answer.type_Declar.,
    Request.for.an.answer.type_Imper.,
    Request.for.an.answer.type_Inter.,
    WH.word.used.in.the.request_used
  )
)
res_cov_plot(mod_lme4_gauss_q,
             c(
               Use.of.gradation_No,
               Use.of.gradation_Yes,
               Request.for.an.answer.type_Declar.,
               Request.for.an.answer.type_Imper.,
               Request.for.an.answer.type_Inter.,
               WH.word.used.in.the.request_used
             ), lme = TRUE
)
res_cov_plot(mod_glmmTMB_beta_q,
             c(
               Use.of.gradation_No,
               Use.of.gradation_Yes,
               Request.for.an.answer.type_Declar.,
               Request.for.an.answer.type_Imper.,
               Request.for.an.answer.type_Inter.,
               WH.word.used.in.the.request_used
             )
)

#dev.off()

pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 4.pdf", width = 10, height = 6.25) 
res_cov_plot(
  c(
    Balance.of.the.request_Balanced,
    Presence.of.encouragement.to.answer_Yes,
    Emphasis.on.subjective.opinion.in.request_Yes,
    Use.of.stimulus.or.statement.in.the.request,
    Absolute.or.comparative.judgment,
    Theoretical.range.of.the.concept.bipolar.unipolar_unipolar
  )
)
dev.off()

pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 5.pdf", width = 10, height = 6.25) 
res_cov_plot(
  c(
    Response.scale..basic.choice,
    Range.of.the.used.scale.bipolar.unipolar_Unipolar,
    Symmetry.of.response.scale_Symmetric,
    Neutral.category_Present,
    Don.t.know.option,
    Interviewer.instruction
  )
)
dev.off()

pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 6.pdf", width = 10, height = 6.25) 
res_cov_plot(
  c(
    Respondent.instruction,
    Extra.information.or.definition,
    Knowledge.provided_def..and.other,
    Knowledge.provided_Definitions,
    Knowledge.provided_No,
    Knowledge.provided_Other
  )
)
dev.off()

pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 7.pdf", width = 10, height = 6.25) 
res_cov_plot(
  c(
    Introduction.available.,
    Request.present.in.the.introduction_present,
    Showcard.or.other.visual.aids.used,
    Horizontal.or.vertical.scale_Horizontal,
    Horizontal.or.vertical.scale_Vertical,
    Overlap.of.scale.labels.and.categories_clearly.connected
  )
)
dev.off()

pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 8.pdf", width = 10, height = 6.25) 
res_cov_plot(
  c(
    Numbers.or.letters.before.the.answer.categories_Numbers,
    Scale.with.only.numbers.or.numbers.in.boxes_Numbers.in.boxes,
    Scale.with.only.numbers.or.numbers.in.boxes_Only.numbers,
    Start.of.the.response.sentence.on.the.visual.aid_Yes,
    Request.on.the.visual.aid_Yes,
    Computer.assisted
  )
)
dev.off()

pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuen diskret 9.pdf", width = 10, height = 6.25) 
res_cov_plot(
  c(
    Interviewer,
    Visual.or.oral.presentation
  )
)
dev.off()


names(fixef(mod_glmmTMB_beta_full_non.reml_lang)$cond)


#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuals continuous covariates 1.pdf", width = 10, height = 6.25) 
res_cov_1
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Residuals continuous covariates 2.pdf", width = 10, height = 6.25) 
res_cov_2
#dev.off()

as.vector(fixef(mod_glmmTMB_beta_full_non.reml_lang)$cond)
predict(mod_glmmTMB_beta_full_non.reml_lang, level = 0)


# 2. Caterpillarplot----
#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Caterpillarplots RndIntercept Language.pdf", width = 10, height = 6.25) 
caterp(mod_glmmTMB_beta_q, j = 1)
caterp(mod_glmmTMB_gauss_q, j = 1)
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Caterpillarplots RndIntercept Study.Experiment.pdf", width = 10, height = 6.25) 
caterp(mod_glmmTMB_beta_q, j = 2)
#dev.off()

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Diagnostik/Caterpillarplots RndIntercept Study.pdf", width = 10, height = 6.25) 
caterp(mod_glmmTMB_beta_q, j = 3)
#dev.off()



# 3. Residuen----
residualz <- data.frame(res = residuals(mod_glmmTMB_beta_full_non.reml_lang),
                        Study = df$Study,
                        experiment = df$experiment,
                        Language = df$Language)

p <- ggplot(residualz, aes(x = Study, y = res, color = Study))+
  geom_point(show.legend = F)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(c(-30, 30))+
  geom_hline(yintercept = 0, linetype = "dashed")
  ggMarginal(p, type = "density", groupColour = T, groupFill = T, alpha = .5)



# 1. Exploratory analysis of cluster specific heterogeneity----
## 1.1 Stetige Merkmale----
### 1.1.1 Number of categories----
##### Kommentare: Siehe .txt File vom 06_09_2022.txt im Barbara Ordner
##### Median: Anzahl an Kategorien pro Studie
stetig(Number.of.categories)
stetig_plot(Number.of.categories, lang = F, study_exp = T, study = F)

### 1.1.2 Number.of.fixed.reference.points----
stetig(Number.of.fixed.reference.points)
stetig_plot(Number.of.fixed.reference.points, lang = F, study_exp = T, study = F)

### 1.1.3 Number.of.sentences.in.introduction----
stetig(Number.of.sentences.in.introduction)
stetig_plot(Number.of.sentences.in.introduction, lang = F, study_exp = T, study = F)

### 1.1.4 Number.of.words.in.introduction----
stetig(Number.of.words.in.introduction)
stetig_plot(Number.of.words.in.introduction, lang = F, study_exp = T, study = F)

### 1.1.6 Number.of.sentences.in.the.request----
stetig(Number.of.sentences.in.the.request)
stetig_plot(Number.of.sentences.in.the.request, lang = F, study_exp = T, study = F)

### 1.1.7 Number.of.words.in.request----
stetig(Number.of.words.in.request)
stetig_plot(Number.of.words.in.request, lang = F, study_exp = T, study = F)


### 1.1.8 Total.number.of.nouns.in.request.for.an.answer----
stetig(Total.number.of.nouns.in.request.for.an.answer)
stetig_plot(Total.number.of.nouns.in.request.for.an.answer, lang = F, study_exp = T, study = F)


### 1.1.9 Total.number.of.syllables.in.request----
stetig(Total.number.of.syllables.in.request)
stetig_plot(Total.number.of.syllables.in.request, lang = F, study_exp = T, study = F)

### 1.1.10 Number.of.subordinate.clauses.in.request----
stetig(Number.of.subordinate.clauses.in.request)
stetig_plot(Number.of.subordinate.clauses.in.request, lang = F, study_exp = T, study = F)

### 1.1.11 Number.of.syllables.in.answer.scale----
stetig(Number.of.syllables.in.answer.scale)
stetig_plot(Number.of.syllables.in.answer.scale, lang = F, study_exp = T, study = F)

### 1.1.12 Total.number.of.nouns.in.answer.scale----
stetig(Total.number.of.nouns.in.answer.scale)
stetig_plot(Total.number.of.nouns.in.answer.scale, lang = F, study_exp = T, study = F)

### 1.1.13 Total.number.of.abstract.nouns.in.answer.scale----
stetig(Total.number.of.abstract.nouns.in.answer.scale)
stetig_plot(Total.number.of.abstract.nouns.in.answer.scale, lang = F, study_exp = T, study = F)

# 2. Modellannahmen----

