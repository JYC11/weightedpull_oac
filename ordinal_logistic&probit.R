setwd("~/Desktop/projects/weightedpull_oac")

library("readxl")
library("MASS")
insane_ordinal <- read_excel("insane_ordinal_final.xlsx")
insane_ordinal_tt <- read_excel("insane_ordinal_final_tt.xlsx")
sane_ordinal <- read_excel("sane_ordinal_final.xlsx")
sane_ordinal_tt <- read_excel("sane_ordinal_final_tt.xlsx")

insane_ordinal$oac_ability_rank = factor(insane_ordinal$oac_ability_rank,levels=c("zero","beginner","intermediate","advanced"),ordered=TRUE)
insane_ordinal_tt$oac_ability_rank = factor(insane_ordinal_tt$oac_ability_rank,levels=c("zero","beginner","intermediate","advanced"),ordered=TRUE)
sane_ordinal$oac_ability_rank = factor(sane_ordinal$oac_ability_rank,levels=c("zero","beginner","intermediate","advanced"),ordered=TRUE)
sane_ordinal_tt$oac_ability_rank = factor(sane_ordinal_tt$oac_ability_rank,levels=c("zero","beginner","intermediate","advanced"),ordered=TRUE)

modelprobit1 <- polr(formula=oac_ability_rank ~ .,data=insane_ordinal,Hess=TRUE,method="probit")
mp1s <- summary(modelprobit1)
capture.output(mp1s, file = "insane_ordinal_probit_summary.txt")

modelprobit2 <- polr(formula=oac_ability_rank ~ .,data=insane_ordinal_tt,Hess=TRUE,method="probit")
mp2s <- summary(modelprobit2)
capture.output(mp2s, file = "insane_ordinal_tt_probit_summary.txt")

modelprobit3 <- polr(formula=oac_ability_rank ~ .,data=sane_ordinal,Hess=TRUE,method="probit")
mp3s <- summary(modelprobit3)
capture.output(mp3s, file = "sane_ordinal_probit_summary.txt")

modelprobit4 <- polr(formula=oac_ability_rank ~ .,data=sane_ordinal_tt,Hess=TRUE,method="probit")
mp4s <- summary(modelprobit4)
capture.output(mp4s, file = "sane_ordinal_tt_probit_summary.txt")


modellogit1 <- polr(oac_ability_rank ~ .,data=insane_ordinal,Hess=TRUE)
ml1s <- summary(modellogit1)
capture.output(ml1s, file = "insane_ordinal_logit_summary.txt")

modellogit2 <- polr(oac_ability_rank ~ .,data=insane_ordinal_tt,Hess=TRUE)
ml2s <- summary(modellogit2)
capture.output(ml2s, file = "insane_ordinal_tt_logit_summary.txt")

modellogit3 <- polr(oac_ability_rank ~ .,data=sane_ordinal,Hess=TRUE)
ml3s <- summary(modellogit3)
capture.output(ml3s, file = "sane_ordinal_logit_summary.txt")

modellogit4 <- polr(oac_ability_rank ~ .,data=sane_ordinal_tt,Hess=TRUE)
ml4s <- summary(modellogit4)
capture.output(ml4s, file = "sane_ordinal_tt_logit_summary.txt")
