load("Aufbereitung/data.RData")
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Modelle/modelle.RData")

library(gridExtra)
library(dplyr)
library(gtable)
library(grid)
library(kableExtra)
library(broom)

# 1. Grafiken----
## 1.1 Filtervariablen----
### 1.1.1 Tabelle zur Visualisierung von Missing Data pattern----

set.seed(123456)
cols <- which(names(df) %in% c("Formulation.of.the.request.for.an.answer..basic.choice", "WH.word.used.in.the.request", "Request.for.an.answer.type",
                               "Use.of.stimulus.or.statement.in.the.request"))
df_sub_yes <- df %>% filter(Formulation.of.the.request.for.an.answer..basic.choice != "No") %>% sample_n(5)
df_sub_no <- df %>% filter(Formulation.of.the.request.for.an.answer..basic.choice == "No") %>% sample_n(3)
df_sub <- bind_rows(df_sub_yes, df_sub_no)
df_sub <- df_sub[,cols]
df_sub$WH.word.used.in.the.request <- factor(df_sub$WH.word.used.in.the.request, labels = c("No", "Yes"))

colnames(df_sub) <- c("Formulation of the request for an anwer", "WH word used in the request", "Request for an anwer type",
                      "Use of stimulus or statement in the request")

g <- tableGrob(df_sub)
#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/filter1.pdf", width = 12, height = 3)
grid.draw(g)
dev.off()

#png("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/filter1.png", width = 900, height = 200)
grid.draw(g)
#dev.off()

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#018571", col = "#018571", lwd=5)
g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#018571", col = "#018571", lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#018571", col = "#018571", lwd=5)

#pdf("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/filter2.pdf", width = 15.5, height = 3)
#png("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/filter2.png", width = 900, height = 200)
grid.draw(g)
dev.off()


g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 5, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 6, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)

g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 7, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 8, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 9, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)

#png("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/filter3.png", width = 900, height = 200)
grid.draw(g)
dev.off()


rm(g, df_sub)

df_sub <- bind_rows(df_sub_yes, df_sub_no)
cols <- which(names(df_sub) %in% c("Formulation.of.the.request.for.an.answer..basic.choice", "WH.word.used.in.the.request_used", "WH.word.used.in.the.request_without", "Request.for.an.answer.type", "Use.of.stimulus.or.statement.in.the.request"))
df_sub <- df_sub[,cols]
df_sub <- df_sub[,c(1,5,4,2,3)]
colnames(df_sub) <- c("Formulation of the request for an anwer", "WH word used in the request", "WH word not used in the request","Request for an answer type" , "Use of stimulus or statement in the request")
df_sub$`WH word used in the request` <- factor(df_sub$`WH word used in the request`, labels = c("No", "Yes"))
df_sub$`WH word not used in the request` <- factor(df_sub$`WH word not used in the request`, labels = c("Yes", "No"))
g <- tableGrob(df_sub)

g$grobs[find_cell( g, 2, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 3, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 4, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 5, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 6, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#80cdc1", col = "#80cdc1", lwd=5)
g$grobs[find_cell( g, 7, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#018571", col = "#018571", lwd=5)
g$grobs[find_cell( g, 8, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#018571", col = "#018571", lwd=5)
g$grobs[find_cell( g, 9, 2, "core-bg")][[1]][["gp"]] <- gpar(fill="#018571", col = "#018571", lwd=5)


g$grobs[find_cell( g, 2, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 3, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 4, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 5, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 6, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 2, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 3, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 4, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 5, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 6, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 2, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 3, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 4, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 5, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)
g$grobs[find_cell( g, 6, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#dfc27d", col = "#dfc27d", lwd=5)

g$grobs[find_cell( g, 7, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 8, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 9, 3, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 7, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 8, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 9, 4, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 7, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 8, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)
g$grobs[find_cell( g, 9, 5, "core-bg")][[1]][["gp"]] <- gpar(fill="#a6611a", col = "#a6611a", lwd=5)


png("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/filter4.png", width = 1050, height = 200)
grid.draw(g)
dev.off()

m <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)
m <- m[which(!(m[,2] == 0 & m[,3] == 1)),]
m <- m[which(!(m[,2] == 0 & m[,4] == 1)),]
m <- m[which(!(m[,7] == 0 & m[,8] == 1)),]
m <- as.matrix(m)

dat <- expand.grid(y=seq(nrow(m)), x=seq(ncol(m)))

## add in the values from the matrix. 
dat <- data.frame(dat, value=as.vector(m))

## Create a column with the appropriate colors based on the value.
dat$color <- ifelse(dat$value == 0, "green", "yellow")

ggplot(data=dat, aes(x=factor(x), y=y, fill = factor(value))) +
  geom_raster(color = "grey25") + 
  labs(fill = "Filter\nverwendet", x = "Filter", y = "Modellnummer") + 
  scale_x_discrete(breaks = 1:8, labels = c("Filter \n1", "Filter \n2", "Filter \n2.1", "Filter \n2.1.1", "Filter \n3", "Filter \n4", "Filter \n5", "Filter \n5.1")) + 
  ylim(c(0,120))+ scale_fill_manual(labels = c("nein", "ja"), values = c("#a6611a", "#dfc27d"))

png("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Plots/Praesentation/Filtervariablen/Strata.png", width = 1300, height = 250)
ggplot(data=dat, aes(x=factor(x), y=y, fill = factor(value))) +
  geom_raster(color = "grey25") + 
  labs(fill = "Filter\nverwendet", x = "Filter", y = "Modellnummer") + 
  scale_x_discrete(breaks = 1:8, labels = c("Filter \n1", "Filter \n2", "Filter \n2.1", "Filter \n2.1.1", "Filter \n3", "Filter \n4", "Filter \n5", "Filter \n5.1")) + 
  ylim(c(0,120))+ scale_fill_manual(labels = c("nein", "ja"), values = c("#a6611a", "#dfc27d"))
dev.off()


# 1.1.2 Kovariablen----
mod_glmmTMB_beta_q %>% 
  gtsummary::tbl_regression(tidy_fun = broom.mixed::tidy) %>%
  gtsummary::modify_caption("Table 1: Summary Statistics of Financial Well-Being  
                            Score by Gender and Education") %>%
  as_gt() %>%
  gt::gtsave(filename = "table2.html")

ggstatsplot::ggcoefstats(x = mod_glmmTMB_beta_q,  caption.summary = F)#+
#  ggplot2::scale_y_discrete(labels = vapply(labels.1, str_wrap, character(1), width = 10)










# Webseiten
t1 <- test[2,]
t1_1 <- t1
t1$Introduction.available.
t1_1$Introduction.available. <- "Yes"


p_hat <- exp(predict(mod_glmmTMB_beta_q, t1))/(1+exp(predict(mod_glmmTMB_beta_q, t1)))
p_tilde <- exp(predict(mod_glmmTMB_beta_q, t1_1))/(1+exp(predict(mod_glmmTMB_beta_q, t1_1)))
beta <- fix$cond["Introduction.available.Yes"]
ebeta <- exp(fix$cond["Introduction.available.Yes"])

q <- (p_hat/(1-p_hat))*ebeta
q/(1+q)
### LOGIT LINK
#https://stats.stackexchange.com/questions/442789/interpretation-of-the-beta-regression-coefficients-with-logit-link-used-to-analy
#https://stats.stackexchange.com/questions/120472/how-to-interpret-coefficients-of-a-beta-regression-model-with-logit-link
#https://stats.stackexchange.com/questions/297659/interpretation-of-betareg-coef

### CROSSED vs. NESTED
# https://yury-zablotski.netlify.app/post/mixed-effects-models-2/
# https://stats.stackexchange.com/questions/484419/crossed-or-nested-random-effects-in-a-repeated-measures-and-a-between-subject-de
# https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified/228814#228814
#https://www.sciencedirect.com/science/article/pii/S0749596X07001398 
