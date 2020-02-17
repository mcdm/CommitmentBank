# Analysis file for the figures and models reported in 
# de Marneffe et al (2019, Sinn und Bedeutung proceedings)

# written by Marie-Catherine de Marneffe, modified by Judith Tonhauser after publication

# this file assumes that the following three directories occur at the same level:
# data (raw data and output data frames created by this analysis script)
# graphs (figures created by this analysis script)
# rscripts (the location of this file as well as helpers.R)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.R")

# load packages
library(tidyverse)
library(dplyr)
library(xtable)
library(ordinal)
library(rcompanion)

# b/w background in figures
theme_set(theme_bw())

#load the data
d = read.csv("../data/CommitmentBank-All.csv", header=T, comment.char="")
nrow(d) #11545
length(unique(d$uID)) #1200 unique discourses

# Table 1 ----
# aggregate data
agrD = d %>%
select(uID, Verb, Embedding, Target, Prompt, Answer) %>%
group_by(uID, Verb, Embedding, Target, Prompt) %>%
summarize(Mean = mean(Answer))
agrD = as.data.frame(agrD)
nrow(agrD)

# verb by embedding
xtabs(~ Verb + Embedding, agrD)

#drop the non-epistemic modals
dataM = droplevels(subset(d, ModalType != "AB" & ModalType != "CI" & ModalType != "DE"))
length(unique(dataM$uID)) #982 unique discourses
table(dataM$ModalType) # only empty annotations & EP
nrow(dataM) #9599

# Figure 2 ----
names(dataM)

#aggregate data
aData = dataM %>%
  select(uID, Verb, Embedding, Target, Prompt, Answer) %>%
  group_by(uID, Verb, Embedding, Target, Prompt) %>%
  summarize(Mean = mean(Answer), low.ci = ci.low(Answer), high.ci = ci.high(Answer), SD = sd(Answer), C = n())
aData = as.data.frame(aData)
nrow(aData) #982 discourses


# paste predicate and number of discourses per predicate
verbNum = as.data.frame(xtabs(~ Verb, aData))
verbNum #number of discourses per verb
dataM = merge(dataM, verbNum, by="Verb")
head(dataM)
dataM$VerbNum = paste(dataM$Verb, paste0("(",dataM$Freq,")"), sep="  ")

mean_proj = aggregate(Answer~VerbNum, data=dataM, FUN="mean")
mean_proj$YMin = mean_proj$Answer - aggregate(Answer~VerbNum, data=dataM, FUN="ci.low")$Answer
mean_proj$YMax = mean_proj$Answer + aggregate(Answer~VerbNum, data=dataM, FUN="ci.high")$Answer
mean_proj
names(mean_proj)
head(mean_proj)
nrow(mean_proj) #45 verbs

dataM$VerbNum <-factor(dataM$VerbNum, levels=mean_proj[order(mean_proj$Answer), "VerbNum"])
head(dataM)
table(dataM$VerbNum)

cols = data.frame(V=levels(dataM$VerbNum))
cols$VeridicalityGroup = as.factor(
  ifelse(cols$V %in% c("learn  (6)","forget  (13)", "notice  (32)", "understand  (7)", "recognize  (1)", "bother  (1)","remember  (5)","realize  (29)","know  (122)","find  (17)","see  (12)"), "F", "NF")) 
#         ifelse(cols$V %in% c("tell  (18)", "guess  (16)", "bet  (1)", "suspect  (18)", "take  (1)","decide  (11)","hear  (9)","fear  (3)","say  (67)","hypothesize  (1)",
#                              "figure  (1)","assume  (5)","imagine  (15)","mean  (50)","insist  (3)","demand  (2)","hope  (8)","feel  (29)",
#                              "believe  (46)","think  (378)","suggest  (17)","pretend  (4)","expect  (4)","seem  (2)","suppose  (5)","occur  (1)"),"NF","VNF")))
cols$Colors =  ifelse(cols$VeridicalityGroup == "F", "#D55E00", "#999999")
#                      ifelse(cols$VeridicalityGroup == "NF", "brown","black"))
head(cols$Colors)

#means with confidence intervals -- used for SuB poster
mean_proj <- mutate(mean_proj,VerbNum = reorder(VerbNum, Answer, mean)) # put mean_proj in sorted order

ggplot(mean_proj, aes(x=VerbNum, y=Answer)) + 
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(text = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.4, color=cols$Colors)) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  ylab("Mean certainty rating") +
  xlab("Predicate (with number of discourses)")
ggsave(f="../graphs/Figure2.pdf",height=3,width=6)

# Figure 3 ----

# "know"
know <- droplevels(subset(dataM,dataM$Verb == "know"))

head(know)
length(unique(know$uID)) # 122 discourses

number_of_ratings = table(know$uID)
number_of_ratings <- as.data.frame(number_of_ratings)
mean(number_of_ratings$Freq) #9.3

mean_projK = aggregate(Answer~uID, data=know, FUN="mean")
mean_projK$YMin = mean_projK$Answer - aggregate(Answer~uID, data=know, FUN="ci.low")$Answer
mean_projK$YMax = mean_projK$Answer + aggregate(Answer~uID, data=know, FUN="ci.high")$Answer

mean_projK <- mutate(mean_projK,uID = reorder(uID, Answer, mean))

ggplot(mean_projK, aes(x=uID,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  #facet_wrap(~VerbNum,scales="free_x", ncol=4) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  ylab("Mean certainty rating") +
  xlab("Discourses with \"know\" (122)")
ggsave(f="../graphs/Figure3a.pdf",height=2.5,width=6)


# "believe"
believe <- droplevels(subset(dataM,dataM$Verb == "believe"))

head(believe)
length(unique(believe$uID)) #  46 discourses
mean_projB = aggregate(Answer~uID, data=believe, FUN="mean")
mean_projB$YMin = mean_projB$Answer - aggregate(Answer~uID, data=believe, FUN="ci.low")$Answer
mean_projB$YMax = mean_projB$Answer + aggregate(Answer~uID, data=believe, FUN="ci.high")$Answer

mean_projB <- mutate(mean_projB,uID = reorder(uID, Answer, mean))

ggplot(mean_projB, aes(x=uID,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  ylab("Mean certainty rating") +
  xlab("Discourses with \"believe\" (46)")
ggsave(f="../graphs/Figure3b.pdf",height=2.5,width=6)


# "tell"
tell <- droplevels(subset(dataM,dataM$Verb == "tell"))

head(tell)
length(unique(tell$uID)) # 18 discourses

mean_projT = aggregate(Answer~uID, data=tell, FUN="mean")
mean_projT$YMin = mean_projT$Answer - aggregate(Answer~uID, data=tell, FUN="ci.low")$Answer
mean_projT$YMax = mean_projT$Answer + aggregate(Answer~uID, data=tell, FUN="ci.high")$Answer

mean_projT <- mutate(mean_projT,uID = reorder(uID, Answer, mean))

ggplot(mean_projT, aes(x=uID,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  #facet_wrap(~VerbNum,scales="free_x", ncol=4) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  ylab("Mean certainty rating") +
  xlab("Discourses with \"tell\" (18)")
ggsave(f="../graphs/Figure3c.pdf",height=2.5,width=6)

# Figure 4 ----

factive = droplevels(subset(dataM,dataM$factive == "yes"))

# person for factive predicates
mean_Fperson = aggregate(Answer~Verb+MatSubjPer, data=factive, FUN="mean")
mean_Fperson$YMin = mean_Fperson$Answer - aggregate(Answer~Verb+MatSubjPer, data=factive, FUN="ci.low")$Answer
mean_Fperson$YMax = mean_Fperson$Answer + aggregate(Answer~Verb+MatSubjPer, data=factive, FUN="ci.high")$Answer

# add number of discourses
mean_Fperson = merge(mean_Fperson, verbNum, by="Verb")
mean_Fperson$VerbNum = paste(mean_Fperson$Verb, paste0("(",mean_Fperson$Freq,")"), sep="  ")

ggplot(mean_Fperson, aes(x=MatSubjPer,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  facet_wrap(~VerbNum) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  ylab("Mean certainty rating") +
  xlab("Person of predicate subject")
ggsave(f="../graphs/Figure4.pdf",height=4,width=6)

# Figure 5 ----

# tense x subject for factive
mean_Ftenseperson = aggregate(Answer~MatTense+MatSubjPer, data=factive, FUN="mean")
mean_Ftenseperson$YMin = mean_Ftenseperson$Answer - aggregate(Answer~MatTense+MatSubjPer, data=factive, FUN="ci.low")$Answer
mean_Ftenseperson$YMax = mean_Ftenseperson$Answer + aggregate(Answer~MatTense+MatSubjPer, data=factive, FUN="ci.high")$Answer

ggplot(mean_Ftenseperson, aes(x=MatSubjPer,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  facet_wrap(~MatTense) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  ylab("Mean certainty rating") +
  xlab("Factive predicates")
ggsave(f="../graphs/Figure5a.pdf",height=3,width=6)

# tense x subject for all predicates
tmp = droplevels(subset(dataM,dataM$MatTense != ""))

mean_tenseperson = aggregate(Answer~MatTense+MatSubjPer, data = tmp, FUN="mean")
mean_tenseperson$YMin = mean_tenseperson$Answer - aggregate(Answer~MatTense+MatSubjPer, data = tmp, FUN="ci.low")$Answer
mean_tenseperson$YMax = mean_tenseperson$Answer + aggregate(Answer~MatTense+MatSubjPer, data = tmp, FUN="ci.high")$Answer

ggplot(mean_tenseperson, aes(x=MatSubjPer,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  facet_wrap(~MatTense) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  ylab("Mean certainty rating") +
  xlab("All predicates")
ggsave(f="../graphs/Figure5b.pdf",height=3,width=6)

# Figure 6 ----

# embedding x genre for all predicates
mean_embeddinggenre = aggregate(Answer~genre+Embedding, data=dataM, FUN="mean")
mean_embeddinggenre$YMin = mean_embeddinggenre$Answer - aggregate(Answer~genre+Embedding, data=dataM, FUN="ci.low")$Answer
mean_embeddinggenre$YMax = mean_embeddinggenre$Answer + aggregate(Answer~genre+Embedding, data=dataM, FUN="ci.high")$Answer

ggplot(mean_embeddinggenre, aes(x=Embedding,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  facet_wrap(~genre) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4)) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  ylab("Mean certainty rating") +
  xlab("Embedding")
ggsave(f="../graphs/Figure6.pdf",height=3,width=6)

# Figure 7 ----

# know: embedding x genre
know <- droplevels(subset(dataM,dataM$Verb == "know"))
nrow(know) #1134
length(unique(know$uID)) #122

aknow = know %>%
  select(uID, Verb, Embedding, genre, Target, Prompt, Answer) %>%
  group_by(uID, Verb, Embedding, genre, Target, Prompt) %>%
  summarize(Mean = mean(Answer), low.ci = ci.low(Answer), high.ci = ci.high(Answer), SD = sd(Answer), C = n())
aknow = as.data.frame(aknow)

xtabs(~ genre + Embedding, data = aknow)
# Embedding
# genre  conditional modal negation question
# BNC           12     5       28       15
# SWBD           6     0       47        6
# WSJ            0     0        3        0

know$embNum = as.character(know$Embedding)
know[know$Embedding == "conditional" & know$genre == "BNC",]$embNum = "conditional (12)"
know[know$Embedding == "modal" & know$genre == "BNC",]$embNum = "modal (5)"
know[know$Embedding == "negation" & know$genre == "BNC",]$embNum = "negation (28)"
know[know$Embedding == "question" & know$genre == "BNC",]$embNum = "question (15)"
know[know$Embedding == "conditional" & know$genre == "SWBD",]$embNum = "conditional (6)"
know[know$Embedding == "negation" & know$genre == "SWBD",]$embNum = "negation (47)"
know[know$Embedding == "question" & know$genre == "SWBD",]$embNum = "question (6)"
know[know$Embedding == "negation" & know$genre == "WSJ",]$embNum = "negation (3)"

mean_kembeddinggenre = aggregate(Answer~genre+embNum, data=know, FUN="mean")
mean_kembeddinggenre$YMin = mean_kembeddinggenre$Answer - aggregate(Answer~genre+embNum, data=know, FUN="ci.low")$Answer
mean_kembeddinggenre$YMax = mean_kembeddinggenre$Answer + aggregate(Answer~genre+embNum, data=know, FUN="ci.high")$Answer

ggplot(mean_kembeddinggenre, aes(x=embNum,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  facet_wrap(~genre,scales="free") +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4)) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  ylab("Mean certainty rating") +
  xlab("Embedding")
ggsave(f="../graphs/Figure7a.pdf",height=3,width=6)

know.conv2 <- droplevels(subset(know,know$MatSubjPer == "third"))
length(unique(know.conv2$uID)) #59 discourses

aknow3 = know.conv2 %>%
  select(uID, Verb, Embedding, genre, Target, Prompt, Answer) %>%
  group_by(uID, Verb, Embedding, genre, Target, Prompt) %>%
  summarize(Mean = mean(Answer), low.ci = ci.low(Answer), high.ci = ci.high(Answer), SD = sd(Answer), C = n())
aknow = as.data.frame(aknow3)

xtabs(~ genre + Embedding, data = aknow3)
# Embedding
# genre  conditional modal negation question
# BNC           10     5       22        8
# SWBD           4     0        6        1
# WSJ            0     0        3        0

aknow3$embNum = as.character(aknow3$Embedding)
aknow3[aknow3$Embedding == "conditional" & aknow3$genre == "BNC",]$embNum = "conditional (10)"
aknow3[aknow3$Embedding == "modal" & aknow3$genre == "BNC",]$embNum = "modal (5)"
aknow3[aknow3$Embedding == "negation" & aknow3$genre == "BNC",]$embNum = "negation (22)"
aknow3[aknow3$Embedding == "question" & aknow3$genre == "BNC",]$embNum = "question (8)"
aknow3[aknow3$Embedding == "conditional" & aknow3$genre == "SWBD",]$embNum = "conditional (4)"
aknow3[aknow3$Embedding == "negation" & aknow3$genre == "SWBD",]$embNum = "negation (6)"
aknow3[aknow3$Embedding == "question" & aknow3$genre == "SWBD",]$embNum = "question (1)"
aknow3[aknow3$Embedding == "negation" & aknow3$genre == "WSJ",]$embNum = "negation (3)"

mean_k3embeddinggenre = aggregate(Answer~genre+embNum, data=know.conv2, FUN="mean")
mean_k3embeddinggenre$YMin = mean_k3embeddinggenre$Answer - aggregate(Answer~genre+embNum, data=know.conv2, FUN="ci.low")$Answer
mean_k3embeddinggenre$YMax = mean_k3embeddinggenre$Answer + aggregate(Answer~genre+embNum, data=know.conv2, FUN="ci.high")$Answer

ggplot(mean_k3embeddinggenre, aes(x=embNum,y=Answer)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  facet_wrap(~genre,scales="free") +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.4)) +
  theme(panel.background = element_blank(), plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="grey90", size=0.5)) +
  scale_y_continuous(limits = c(-3,3)) +
  ylab("Mean certainty rating") +
  xlab("Embedding")
ggsave(f="../graphs/Figure7b.pdf",height=3,width=6)


## Models

dataM$Answer <- as.factor(dataM$Answer)

# Models: predicting projection from factivity (section 3.1.1) ----
m.factive <- clmm(Answer ~ factive + (1|WorkerID), data = dataM)
summary(m.factive) # factive predicates higher ratings than non-factive

# Models: Table 2: Nagelkerke R^2 ----

# null model
m.null = clmm(Answer ~ 1 + (1|WorkerID), data = dataM)

# factivity
nagelkerke(fit = m.factive, null = m.null) #.126

# genre (WSJ, BNC, SWBD)
table(dataM$genre)
m.genre <- clmm(Answer ~ genre + (1|WorkerID), data = dataM)
summary(m.genre) #SWBD and WSJ lower ratings than BNC
nagelkerke(fit = m.genre, null = m.null) #6.2

# predicate lemma
m.verb <- clmm(Answer ~ Verb + (1|WorkerID), data = dataM)
summary(m.verb)
nagelkerke(fit = m.verb, null = m.null) #22.8
nagelkerke(fit = m.verb, null = m.factive) #11.6

# Embedding
table(dataM$Embedding)
m.embedding <- clmm(Answer ~ Embedding + (1|WorkerID), data = dataM)
summary(m.embedding) # modal higher than conditional, negation lower than conditional, no diff to question
nagelkerke(fit = m.embedding, null = m.null) #9.1

# predicate tense
#at time of publication, there were a few items without tense annotation, these got fixed after publication -- hence some slight discrepancies with what is reported in the paper
table(dataM$MatTense)
m.matTense <- clmm(Answer ~ MatTense + (1|WorkerID), data = droplevels(subset(dataM,dataM$MatTense != "")))
summary(m.matTense) #past and present lower than future
nagelkerke(fit = m.matTense, null = m.null) #4.1 (different than what was reported in the paper)

# person of predicate subject (where 1/3sg and 1/3pl are different)
dataM$PersonSub = paste(dataM$MatSubjPer,dataM$MatSubjNum)
dataM$PersonSub = as.factor(dataM$PersonSub)
m.matPersonSubj <- clmm(Answer ~ PersonSub + (1|WorkerID), data = dataM)
nagelkerke(fit = m.matPersonSubj, null = m.null) #12.4 (this was reported for "person of predicate subject")

# not reported in the paper
# person of predicate subject (as 1, 2 or 3 person)
table(dataM$MatSubjPer)
m.matSubjPer <- clmm(Answer ~ MatSubjPer + (1|WorkerID), data = dataM)
summary(m.matSubjPer) #second and third higher than first
nagelkerke(fit = m.matSubjPer, null = m.null) #11.7

# not reported in the paper
# matrix subject number ("unknown" are "you" items, all BNC and WSJ codes whether "you" is generic or not: ImpMatSubj)
table(data$MatSubjNum)
m.matSubjNum <- clmm(Answer ~ MatSubjNum + (1|WorkerID), data = data)
summary(m.matSubjNum) # singular lower than plural, unknown higher than plural
nagelkerke(fit = m.matSubjNum, null = m.null) #4

# Models: Table 3 ----
m.FactiveR <- clmm(Answer ~ factive + (1|WorkerID) + (1|Verb), data = dataM)
summary(m.FactiveR)

ranef(m.FactiveR)$Verb

# Models (section 3.2): predicting certainty ratings from tense and subject person, and their interaction ----

# Paper: "This is true for models fitted to the factive predicates and models 
# fitted to all predicates. (All models included random by-annotator intercepts.)"

# factive predicates
m.TensePersonIF <- clmm(as.factor(Answer) ~ MatTense * MatSubjPer + (1|WorkerID), data = factive)
m.TensePersonAF <- clmm(as.factor(Answer) ~ MatTense + MatSubjPer + (1|WorkerID), data = factive)
anova(m.TensePersonIF, m.TensePersonAF)
# model with interaction is better


# all predicates
dataM$MatSubjPer <- relevel(dataM$MatSubjPer, ref = "first")
dataM$MatTense <- relevel(dataM$MatTense, ref = "present")

m.TensePersonI <- clmm(Answer ~ MatTense * MatSubjPer + (1|WorkerID), data = droplevels(subset(dataM,dataM$MatTense != "")))
m.TensePersonA <- clmm(Answer ~ MatTense + MatSubjPer + (1|WorkerID), data = droplevels(subset(dataM,dataM$MatTense != "")))
anova(m.TensePersonI, m.TensePersonA)
# model with interaction is better

# Models (section 3.4): plausibility of the CC given the context ----

# mean no target rating
table(dataM$mean.noTarget)
nrow(dataM[!is.na(dataM$mean.noTarget),])

tmp = droplevels(subset(dataM,!is.na(dataM$mean.noTarget)))
nrow(tmp)
names(tmp)

m.nullS = clmm(Answer ~ 1 + (1|WorkerID), data = tmp)
m.mean.noT = clmm(Answer ~ mean.noTarget + (1|WorkerID), data = tmp)
summary(m.mean.noT) # the higher the mean no target rating, the higher the projection rating
nagelkerke(fit = m.mean.noT, null = m.nullS) #0.036

# Models (section 3.5): models with all predictors ----

#"We fitted an ordinal mixed effects model that predicts the certainty ratings from fixed effects of 
# embedding and genre, and their interaction, as well as tense and person, and their interaction, 
# and the predicate lemma. We again included random by-annotator intercepts."

# full data, with interaction between gender and embedding
m.null <- clmm (Answer ~ 1 + (1|WorkerID), data = dataM)
m.Verb.i <- clmm (Answer ~ Verb + genre + Embedding + genre:Embedding + MatSubjPer + MatTense + (1|WorkerID), data = dataM)
m.Factive.i <- clmm (Answer ~ factive + genre + Embedding + genre:Embedding + MatSubjPer + MatTense + (1|WorkerID), data = dataM)
nagelkerke(fit = m.Verb.i, null = m.null) #34.4
nagelkerke(fit = m.Factive.i, null = m.null) #28.9

drop1(m.Verb.i,test="Chi")
# all factors matter (interaction for genre and Embedding, no info on individual factors)

# Single term deletions
# 
# Model:
#   Answer ~ Verb + genre + Embedding + genre:Embedding + MatSubjPer + 
#   MatTense + (1 | WorkerID)
# Df   AIC
# <none>             32592
# Verb            44 34542
# MatSubjPer       2 32721
# MatTense         2 32605
# genre:Embedding  6 32746
# LRT
# <none>                 
#   Verb            2038.34
# MatSubjPer       133.32
# MatTense          17.06
# genre:Embedding  165.49
# Pr(>Chi)    
# <none>                       
#   Verb            < 2.2e-16 ***
#   MatSubjPer      < 2.2e-16 ***
#   MatTense        0.0001972 ***
#   genre:Embedding < 2.2e-16 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01
# '*' 0.05 '.' 0.1 ' ' 1

# full data, without interaction between gender and embedding
m.Verb.s <- clmm (Answer ~ Verb + genre + Embedding + MatSubjPer + MatTense + (1|WorkerID), data = dataM)
m.Factive.s <- clmm (Answer ~ factive + genre + Embedding + MatSubjPer + MatTense + (1|WorkerID), data = dataM)
nagelkerke(fit = m.Verb.s, null = m.null) #0.333
nagelkerke(fit = m.Factive.s, null = m.null) #0.275

drop1(m.Verb.s,test="Chi")
# all factors matter

# Single term deletions
# 
# Model:
#   Answer ~ Verb + genre + Embedding + MatSubjPer + MatTense + (1 | 
#                                                                  WorkerID)
# Df   AIC     LRT  Pr(>Chi)    
# <none>        32746                      
# Verb       44 34695 2037.29 < 2.2e-16 ***
#   genre       2 32759   17.68 0.0001447 ***
#   Embedding   3 32952  212.47 < 2.2e-16 ***
#   MatSubjPer  2 33046  304.78 < 2.2e-16 ***
#   MatTense    2 32759   17.07 0.0001965 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#"We also fitted a variant of this model which includes the plausibility means of the CCs as a fixed effect, 
# for the 558 BNC and WSJ discourses for which we have such annotations. 

# plausibility-annotated data, with interaction between genre and embedding
m.nullS <- clmm(Answer ~ 1 + (1|WorkerID), data = droplevels(subset(dataM,!is.na(dataM$mean.noTarget))))
m.s.AllV.i <- clmm (Answer ~ Verb + genre + Embedding + genre:Embedding + MatSubjPer + MatTense + mean.noTarget + (1|WorkerID), data = droplevels(subset(dataM,!is.na(dataM$mean.noTarget))))
m.s.AllF.i <- clmm (Answer ~ factive + genre + Embedding + genre:Embedding + MatSubjPer + MatTense + mean.noTarget + (1|WorkerID), data = droplevels(subset(dataM,!is.na(dataM$mean.noTarget))))
nagelkerke(fit = m.s.AllV.i, null = m.nullS) #30.9
nagelkerke(fit = m.s.AllF.i, null = m.nullS) #23.7

drop1(m.s.AllV.i,test="Chi")
# all factors significant except MatTense and the interaction between genre and embedding 
# (different than what was reported in the paper)

# Single term deletions
# 
# Model:
#   Answer ~ Verb + genre + Embedding + genre:Embedding + MatSubjPer + 
#   MatTense + mean.noTarget + (1 | WorkerID)
# Df   AIC
# <none>             19081
# Verb            35 20437
# MatSubjPer       2 19103
# MatTense         2 19082
# mean.noTarget    1 19162
# genre:Embedding  3 19080
# LRT
# <none>                 
#   Verb            1426.81
# MatSubjPer        26.86
# MatTense           5.38
# mean.noTarget     83.93
# genre:Embedding    5.14
# Pr(>Chi)    
# <none>                       
#   Verb            < 2.2e-16 ***
#   MatSubjPer      1.473e-06 ***
#   MatTense          0.06802 .  
# mean.noTarget   < 2.2e-16 ***
#   genre:Embedding   0.16183    
# ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01
# '*' 0.05 '.' 0.1 ' ' 1

# plausibility-annotated data, without interaction between genre and embedding
m.s.AllV.s <- clmm (Answer ~ Verb + genre + Embedding + MatSubjPer + MatTense + mean.noTarget + (1|WorkerID), data = droplevels(subset(dataM,!is.na(dataM$mean.noTarget))))
m.s.AllF.s <- clmm (Answer ~ factive + genre + Embedding + MatSubjPer + MatTense + mean.noTarget + (1|WorkerID), data = droplevels(subset(dataM,!is.na(dataM$mean.noTarget))))
nagelkerke(fit = m.s.AllV.s, null = m.nullS) #30.9
nagelkerke(fit = m.s.AllF.s, null = m.nullS) #23.4

drop1(m.s.AllV.s,test="Chi")
# all factors matter, except genre and matrix tense (the latter is marginal)
# (different than what was reported in the paper)

# Single term deletions
# 
# Model:
#   Answer ~ Verb + genre + Embedding + MatSubjPer + MatTense + mean.noTarget + 
#   (1 | WorkerID)
# Df   AIC     LRT  Pr(>Chi)    
# <none>           19080                      
# Verb          35 20470 1459.75 < 2.2e-16 ***
#   genre          1 19079    1.63   0.20128    
# Embedding      3 19172   98.27 < 2.2e-16 ***
#   MatSubjPer     2 19102   26.32 1.926e-06 ***
#   MatTense       2 19081    5.01   0.08147 .  
# mean.noTarget  1 19163   84.94 < 2.2e-16 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
