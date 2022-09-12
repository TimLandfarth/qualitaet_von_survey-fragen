# merge sample sizes
# use only items with MTMM result
dat <- subset(dat, is.na(dat$reliability.r.2.) == F) # 7278
dat$ess_study <- ifelse(dat$?..Study %in% c("ESS Round 1", "ESS Round 2", "ESS Round 3", "ESS Round 4", "ESS Round 5", "ESS Round 6", "ESS Round 7"), 1, 0)

# generate unique identifier variable to define country, language, ESS round and experiment to merge ESS sample sizes
dat$merge <- str_c( dat$cntry, dat$lang, dat$experiment, sep=" ")

# ess sanple sizes
load(file="//svmafile01.gesis.intra/users/felderba/papers/ESS/data/ess_sample_size.Rdata")

#other experiment's sample sizes
ss_non_ess <- read.csv2("//svmafile01.gesis.intra/users/felderba/SQP/data/sample_sizes/sample_size_non_ess.csv", sep=";")

dat <- merge(ss_merge[, c(1, 6)], dat, by.y="merge", by.x="merge", all.y = TRUE) 
dat <- merge(ss_non_ess[, c(1, 7)], dat, by.y="?..Study", by.x="?..study_name", all.y = TRUE) 
dat$sample_size <- as.numeric(ifelse(dat$ess_study == 0, dat$sample_size.x, dat$sample_size.y))

dat <- dat[, c(5:6, 10:14, 16:98, 100)]
