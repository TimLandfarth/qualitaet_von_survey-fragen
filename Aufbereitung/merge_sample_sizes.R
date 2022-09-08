# merge sample sizes
# use only items with MTMM result
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/data.RData")
dat <- df
dat$ess_study <- ifelse(dat$Study %in% c("ESS Round 1", "ESS Round 2", "ESS Round 3", "ESS Round 4", "ESS Round 5", "ESS Round 6", "ESS Round 7"), 1, 0)

# generate unique identifier variable to define country, language, ESS round and experiment to merge ESS sample sizes
dat$merge <- str_c( dat$Country, dat$experiment, sep=" ")

# ess sanple sizes
load(file="C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/ess_sample_size.Rdata")
ss_merge[,8:11] <- str_split_fixed(ss_merge$merge, " ", 4)
ss_merge$V8 <- as.character(factor(ss_merge$V8, labels = c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic",
                                                             "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "United Kingdom",
                                                             "Greece", "Croatia", "Hungary", "Ireland", "Israel", "Iceland", "Italy",
                                                             "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", "Portugal",
                                                             "Romania", "Russian Federation", "Sweden", "Slovenia", "Slovakia", "Turkey", "Ukraine")))
ss_merge$merge <- str_c(ss_merge$V8, ss_merge$V10, ss_merge$V11, sep = " ")


#other experiment's sample sizes
ss_non_ess <- read.csv2("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/sample_size_non_ess.csv", sep=";")

bind_rows()


dat <- merge(dat, ss_merge[, c(1, 6)],  by.y="merge", by.x="merge") 
dat <- left_join(dat, ss_merge[, c(1,6)], by = c("merge" = "merge"))

dat <- merge(ss_non_ess[, c(1, 7)], dat, by.y="Study", by.x="?..study_name", all.y = TRUE) 
dat$sample_size <- as.numeric(ifelse(dat$ess_study == 0, dat$sample_size.x, dat$sample_size.y))

dat <- dat[, c(5:6, 10:14, 16:98, 100)]


# Originalversion----
# merge sample sizes
# use only items with MTMM result
#dat <- subset(dat, is.na(dat$reliability.r.2.) == F) # 7278
#dat$ess_study <- ifelse(dat$?..Study %in% c("ESS Round 1", "ESS Round 2", "ESS Round 3", "ESS Round 4", "ESS Round 5", "ESS Round 6", "ESS Round 7"), 1, 0)
#
## generate unique identifier variable to define country, language, ESS round and experiment to merge ESS sample sizes
#dat$merge <- str_c( dat$cntry, dat$lang, dat$experiment, sep=" ")
#
## ess sanple sizes
#load(file="//svmafile01.gesis.intra/users/felderba/papers/ESS/data/ess_sample_size.Rdata")
#
##other experiment's sample sizes
#ss_non_ess <- read.csv2("//svmafile01.gesis.intra/users/felderba/SQP/data/sample_sizes/sample_size_non_ess.csv", sep=";")
#
#dat <- merge(ss_merge[, c(1, 6)], dat, by.y="merge", by.x="merge", all.y = TRUE) 
#dat <- merge(ss_non_ess[, c(1, 7)], dat, by.y="?..Study", by.x="?..study_name", all.y = TRUE) 
#dat$sample_size <- as.numeric(ifelse(dat$ess_study == 0, dat$sample_size.x, dat$sample_size.y))
#
#dat <- dat[, c(5:6, 10:14, 16:98, 100)]



## BEVOR ICH JETZT NOCH VERRÜCKT WERDE MIT DEM SCHEIß
# Laden vom Datensatz
load("C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/data.RData")
dat <- df
dat <- cbind(dat, experiment_1 = str_split_fixed(dat$experiment, " ",2)[,2])

# Laden von Variablen, welche gemerged werden sollen, ESS
load(file="C:/Uni/13. Semester/Praktikum/github qualitaet_von_survey-fragen/qualitaet_von_survey-fragen/Aufbereitung/ess_sample_size.Rdata")

# Fixen der Namen der Länder
ss_merge[,8:11] <- str_split_fixed(ss_merge$merge, " ", 4)
ss_merge$V8 <- as.character(factor(ss_merge$V8, labels = c("Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic",
                                                           "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "United Kingdom",
                                                           "Greece", "Croatia", "Hungary", "Ireland", "Israel", "Iceland", "Italy",
                                                           "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", "Portugal",
                                                           "Romania", "Russian Federation", "Sweden", "Slovenia", "Slovakia", "Turkey", "Ukraine")))

ss_merge$V10 <- ifelse(ss_merge$V10 == "ESS1","ESS Round 1",
                       ifelse(ss_merge$V10 == "ESS2","ESS Round 2",
                              ifelse(ss_merge$V10 == "ESS3","ESS Round 3",
                                     ifelse(ss_merge$V10 == "ESS4","ESS Round 4",
                                            ifelse(ss_merge$V10 == "ESS5","ESS Round 5",
                                                   ifelse(ss_merge$V10 == "ESS6", "ESS Round 6", "ESS Round 7")
                                                   )
                                            )
                                     )
                              )
                       )

# Laden von Variablen, welche gemerged werden sollen, NICHT ESS
dat_1 <-  merge(dat %>% filter(study %in% c("ESS Round 1",
                                             "ESS Round 2",
                                             "ESS Round 3",
                                             "ESS Round 4",
                                             "ESS Round 5",
                                             "ESS Round 6",
                                             "ESS Round 7")), ss_merge, by.x = c("Study", "experiment_1", "Country"), by.y = c("V10",  "exp_name", "V8"))

dat_2 <- dat %>% filter(!(Study %in% c(
  "ESS Round 1",
  "ESS Round 2",
  "ESS Round 3",
  "ESS Round 4",
  "ESS Round 5",
  "ESS Round 6",
  "ESS Round 7"
))
) %>% merge(ss_non_ess, by.x = c("Study"), by.y = "ï..study_name")


dat <- bind_rows(dat_1,dat_2)




##### TESTEN
dat$Study[dat$experiment_1 == unique(ss_merge$exp_name)[4]]
ss_merge$V10[ss_merge$exp_name == unique(dat$experiment_1)[4]]
which(duplicated(ss_merge[,c(3,8,10)]))


