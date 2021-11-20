########################################################################################
########################################################################################
# CAMEO ConfliBERT
#
# Inter-coder reliability
# 
# Javier Osorio
# 9/28/2021
########################################################################################
########################################################################################


################################################################
# CONTENT
# 
################################################################


################################################################
################################################################
# SETUP

# Clean the environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, readxl, glue, dplyr, tidyr, stringr, irr, ggplot2, stringdist)



################################################################
################################################################
# GET DATA ROUND 1
{

################################################
# Get Round 1
data.r1 <- read_csv(here("training_round_1",
                           "summary.csv"))

# Explore data
names(data.r1)

# Extract sentence number and make it first column
data.r1 <- data.r1 %>%
  mutate(num=as.numeric(str_extract_all(data.r1$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)
  
  
################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r1 <- data.r1 %>%
  select('1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r1) <- c('rater1','rater2','rater3','rater4','rater5')


##################
# Task 2: PentaClass

# Action 1
penta.a1.r1 <- data.r1 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r1) <- c('rater1','rater3','rater4','rater5')

# Action 2
penta.a2.r1 <- data.r1 %>% 
  select("Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r1) <- c('rater3','rater4','rater5')


# Action 3
penta.a3.r1 <- data.r1 %>% 
  select("Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r1) <- c('rater3','rater4','rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r1 <- data.r1 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r1) <- c('rater1','rater3','rater4','rater5')

# Action 2
cameo.a2.r1 <- data.r1 %>%
  select('Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r1) <- c('rater3','rater4','rater5')

# Action 3
cameo.a3.r1 <- data.r1 %>%
  select('Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r1) <- c('rater3','rater4','rater5')




################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Task 1: Relevant
k.rel.r1 <- as.numeric(kappam.fleiss(relevant.r1)[5])
k.rel.r1



##################
# Task 2: PentaClass

#kappa2(penta.a1.r1[,c(1,4)], "unweighted")

# PentaClass by actor
k.penta.a1.r1 <- as.numeric(kappam.fleiss(penta.a1.r1, exact=TRUE)[5])
k.penta.a2.r1 <- as.numeric(kappam.fleiss(penta.a2.r1, exact=TRUE)[5])
k.penta.a3.r1 <- as.numeric(kappam.fleiss(penta.a3.r1, exact=TRUE)[5])

# Average 
k.penta.r1 <- mean(c(k.penta.a1.r1,k.penta.a2.r1,k.penta.a3.r1))
k.penta.r1




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r1 <- as.numeric(kappam.fleiss(cameo.a1.r1, exact=TRUE)[5])
k.cameo.a2.r1 <- as.numeric(kappam.fleiss(cameo.a2.r1, exact=TRUE)[5])
k.cameo.a3.r1 <- as.numeric(kappam.fleiss(cameo.a3.r1, exact=TRUE)[5])

# Average 
k.cameo.r1 <- mean(c(k.cameo.a1.r1,k.cameo.a2.r1,k.cameo.a3.r1))
k.cameo.r1












################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r1 <- as.data.frame(c(k.cameo.r1,k.penta.r1,k.rel.r1))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r1 <- cbind(df.r1, tasks)
colnames(df.r1) <- c("value","task")


# Plot
plot.r1 <- ggplot(df.r1, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r1$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 1 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r1

ggsave(here("graphs","fkappa_round1.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r1 <- as.data.frame(c(k.cameo.a3.r1,k.penta.a3.r1,
                             k.cameo.a2.r1,k.penta.a2.r1,
                             k.cameo.a1.r1,k.penta.a1.r1,
                             k.rel.r1))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r1 <- cbind(df.dis.r1, tasks)
colnames(df.dis.r1) <- c("value","task")

# Plot
plot.dis.r1 <- ggplot(df.dis.r1, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r1$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 1 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r1

ggsave(here("graphs","fkappa_dis_round1.pdf"), width = 8, height = 4)

}




###############################################################
################################################################
# GET DATA ROUND 2
{
################################################
# Get Round 2
data.r2 <- read_csv(here("training_round_2",
                         "summary.csv"))

# Explore data
names(data.r2)

# Extract sentence number and make it first column
data.r2 <- data.r2 %>%
  mutate(num=as.numeric(str_extract_all(data.r2$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r2 <- data.r2 %>%
  select('1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r2) <- c('rater1','rater2','rater3','rater4','rater5')

# export relevant
relevant.r2.export <- data.r2 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r2.export) <- c('story','sentences','rater1','rater2','rater3','rater4','rater5')
write_csv(relevant.r2.export,here("training_round_2","relevant_r2.csv"))


##################
# Task 2: PentaClass

# Action 1
penta.a1.r2 <- data.r2 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r2) <- c('rater1','rater3','rater4','rater5')

# Action 2
penta.a2.r2 <- data.r2 %>% 
  select("Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r2) <- c('rater3','rater4','rater5')


# Action 3
penta.a3.r2 <- data.r2 %>% 
  select("Action3-PentaClass_3", "Action3-PentaClass_4","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r2) <- c('rater3','rater4','rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r2 <- data.r2 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r2) <- c('rater1','rater3','rater4','rater5')

# Action 2
cameo.a2.r2 <- data.r2 %>%
  select('Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r2) <- c('rater3','rater4','rater5')

# Action 3
cameo.a3.r2 <- data.r2 %>%
  select('Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r2) <- c('rater3','rater4','rater5')





################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Task 1: Relevant
k.rel.r2 <- as.numeric(kappam.fleiss(relevant.r2)[5])
k.rel.r2



##################
# Task 2: PentaClass

#kappa2(penta.a1.r2[,c(1,4)], "unweighted")

# PentaClass by actor
k.penta.a1.r2 <- as.numeric(kappam.fleiss(penta.a1.r2, exact=TRUE)[5])
k.penta.a2.r2 <- as.numeric(kappam.fleiss(penta.a2.r2, exact=TRUE)[5])
k.penta.a3.r2 <- as.numeric(kappam.fleiss(penta.a3.r2, exact=TRUE)[5])

# Average 
k.penta.r2 <- mean(c(k.penta.a1.r2,k.penta.a2.r2,k.penta.a3.r2))
k.penta.r2




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r2 <- as.numeric(kappam.fleiss(cameo.a1.r2, exact=TRUE)[5])
k.cameo.a2.r2 <- as.numeric(kappam.fleiss(cameo.a2.r2, exact=TRUE)[5])
k.cameo.a3.r2 <- as.numeric(kappam.fleiss(cameo.a3.r2, exact=TRUE)[5])

# Average 
k.cameo.r2 <- mean(c(k.cameo.a1.r2,k.cameo.a2.r2,k.cameo.a3.r2))
k.cameo.r2







################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r2 <- as.data.frame(c(k.cameo.r2,k.penta.r2,k.rel.r2))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r2 <- cbind(df.r2, tasks)
colnames(df.r2) <- c("value","task")


# Plot
plot.r2 <- ggplot(df.r2, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r2$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 2 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r2

ggsave(here("graphs","fkappa_round2.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r2 <- as.data.frame(c(k.cameo.a3.r2,k.penta.a3.r2,
                             k.cameo.a2.r2,k.penta.a2.r2,
                             k.cameo.a1.r2,k.penta.a1.r2,
                             k.rel.r2))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r2 <- cbind(df.dis.r2, tasks)
colnames(df.dis.r2) <- c("value","task")

# Plot
plot.dis.r2 <- ggplot(df.dis.r2, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r2$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 2 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r2

ggsave(here("graphs","fkappa_dis_round2.pdf"), width = 8, height = 4)

}




################################################################
################################################################
# GET DATA ROUND 3
{

################################################
# Get Round 3
data.r3 <- read_csv(here("training_round_3",
                         "summary_v2.csv"))

# Explore data
names(data.r3)

# Extract sentence number and make it first column
data.r3 <- data.r3 %>%
  mutate(num=as.numeric(str_extract_all(data.r3$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r3 <- data.r3 %>%
  select('1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r3) <- c('rater1','rater2','rater3','rater4','rater5')

# export relevant
relevant.r3.export <- data.r3 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r3.export) <- c('story','sentences','rater1','rater2','rater3','rater4','rater5')
write_csv(relevant.r3.export,here("training_round_3","relevant_r3.csv"))


##################
# Task 2: PentaClass

# Action 1
penta.a1.r3 <- data.r3 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r3) <- c('rater1','rater3','rater4','rater5')

# Action 2
penta.a2.r3 <- data.r3 %>% 
  select("Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r3) <- c('rater3','rater4','rater5')


# Action 3
penta.a3.r3 <- data.r3 %>% 
  select("Action3-PentaClass_3", "Action3-PentaClass_4","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r3) <- c('rater3','rater4','rater5')


# Action 4
penta.a4.r3 <- data.r3 %>% 
  select("Action4-PentaClass_4","Action4-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a4.r3) <- c('rater4','rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r3 <- data.r3 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r3) <- c('rater1','rater3','rater4','rater5')

# Action 2
cameo.a2.r3 <- data.r3 %>%
  select('Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r3) <- c('rater3','rater4','rater5')

# Action 3
cameo.a3.r3 <- data.r3 %>%
  select('Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r3) <- c('rater3','rater4','rater5')


# Action 4
cameo.a4.r3 <- data.r3 %>%
  select('Action4_CAMEO_4', 'Action4_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a4.r3) <- c('rater4','rater5')





################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Task 1: Relevant
k.rel.r3 <- as.numeric(kappam.fleiss(relevant.r3)[5])
k.rel.r3



##################
# Task 2: PentaClass

#kappa2(penta.a1.r3[,c(1,4)], "unweighted")

# PentaClass by actor
k.penta.a1.r3 <- as.numeric(kappam.fleiss(penta.a1.r3, exact=TRUE)[5])
k.penta.a2.r3 <- as.numeric(kappam.fleiss(penta.a2.r3, exact=TRUE)[5])
k.penta.a3.r3 <- as.numeric(kappam.fleiss(penta.a3.r3, exact=TRUE)[5])

# Average 
k.penta.r3 <- mean(c(k.penta.a1.r3,k.penta.a2.r3,k.penta.a3.r3))
k.penta.r3




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r3 <- as.numeric(kappam.fleiss(cameo.a1.r3, exact=TRUE)[5])
k.cameo.a2.r3 <- as.numeric(kappam.fleiss(cameo.a2.r3, exact=TRUE)[5])
k.cameo.a3.r3 <- as.numeric(kappam.fleiss(cameo.a3.r3, exact=TRUE)[5])

# Average 
k.cameo.r3 <- mean(c(k.cameo.a1.r3,k.cameo.a2.r3,k.cameo.a3.r3))
k.cameo.r3











################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r3 <- as.data.frame(c(k.cameo.r3,k.penta.r3,k.rel.r3))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r3 <- cbind(df.r3, tasks)
colnames(df.r3) <- c("value","task")


# Plot
plot.r3 <- ggplot(df.r3, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r3$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 3 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r3

ggsave(here("graphs","fkappa_round3.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r3 <- as.data.frame(c(k.cameo.a3.r3,k.penta.a3.r3,
                             k.cameo.a2.r3,k.penta.a2.r3,
                             k.cameo.a1.r3,k.penta.a1.r3,
                             k.rel.r3))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r3 <- cbind(df.dis.r3, tasks)
colnames(df.dis.r3) <- c("value","task")

# Plot
plot.dis.r3 <- ggplot(df.dis.r3, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r3$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 3 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r3

ggsave(here("graphs","fkappa_dis_round3.pdf"), width = 8, height = 4)


}




################################################################
################################################################
# GET DATA ROUND 4
{
################################################
# Get Round 4
data.r4 <- read_csv(here("training_round_4",
                         #"summary.csv"))
                         "summary_v2.csv"))

# Explore data
names(data.r4)

# Extract sentence number and make it first column
data.r4 <- data.r4 %>%
  mutate(num=as.numeric(str_extract_all(data.r4$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r4 <- data.r4 %>%
  select('1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r4) <- c('rater1','rater2','rater3','rater4','rater5')

# export relevant
relevant.r4.export <- data.r4 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r4.export) <- c('story','sentences','rater1','rater2','rater3','rater4','rater5')
write_csv(relevant.r4.export,here("training_round_4","relevant_r4.csv"))


##################
# Task 2: PentaClass

# Action 1
penta.a1.r4 <- data.r4 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_2','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r4) <- c('rater1','rater2','rater3','rater4','rater5')

# Action 2
penta.a2.r4 <- data.r4 %>% 
  select("Action2-PentaClass_1","Action2-PentaClass_2","Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r4) <- c('rater1','rater2','rater3','rater4','rater5')


# Action 3
penta.a3.r4 <- data.r4 %>% 
  select("Action3-PentaClass_1","Action3-PentaClass_3", "Action3-PentaClass_4","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r4) <- c('rater1','rater3','rater4','rater5')


# Action 4
penta.a4.r4 <- data.r4 %>% 
  select("Action4-PentaClass_4","Action4-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a4.r4) <- c('rater4','rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r4 <- data.r4 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_2','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r4) <- c('rater1','rater2','rater3','rater4','rater5')

# Action 2
cameo.a2.r4 <- data.r4 %>%
  select('Action2_CAMEO_1','Action2_CAMEO_2','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r4) <- c('rater1','rater2','rater3','rater4','rater5')

# Action 3
cameo.a3.r4 <- data.r4 %>%
  select('Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r4) <- c('rater1','rater3','rater4','rater5')


# Action 4
cameo.a4.r4 <- data.r4 %>%
  select('Action4_CAMEO_4', 'Action4_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a4.r4) <- c('rater4','rater5')






################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Task 1: Relevant
k.rel.r4 <- as.numeric(kappam.fleiss(relevant.r4)[5])
k.rel.r4



##################
# Task 2: PentaClass

#kappa2(penta.a1.r4[,c(1,4)], "unweighted")

# PentaClass by actor
k.penta.a1.r4 <- as.numeric(kappam.fleiss(penta.a1.r4, exact=TRUE)[5])
k.penta.a2.r4 <- as.numeric(kappam.fleiss(penta.a2.r4, exact=TRUE)[5])
k.penta.a3.r4 <- as.numeric(kappam.fleiss(penta.a3.r4, exact=TRUE)[5])
k.penta.a4.r4 <- as.numeric(kappam.fleiss(penta.a4.r4, exact=TRUE)[5])

# Average 
k.penta.r4 <- mean(c(k.penta.a1.r4,k.penta.a2.r4,k.penta.a3.r4,k.penta.a4.r4))
k.penta.r4




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r4 <- as.numeric(kappam.fleiss(cameo.a1.r4, exact=TRUE)[5])
k.cameo.a2.r4 <- as.numeric(kappam.fleiss(cameo.a2.r4, exact=TRUE)[5])
k.cameo.a3.r4 <- as.numeric(kappam.fleiss(cameo.a3.r4, exact=TRUE)[5])
k.cameo.a4.r4 <- as.numeric(kappam.fleiss(cameo.a4.r4, exact=TRUE)[5])

# Average 
k.cameo.r4 <- mean(c(k.cameo.a1.r4,k.cameo.a2.r4,k.cameo.a3.r4,k.cameo.a4.r4))
k.cameo.r4











################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r4 <- as.data.frame(c(k.cameo.r4,k.penta.r4,k.rel.r4))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r4 <- cbind(df.r4, tasks)
colnames(df.r4) <- c("value","task")


# Plot
plot.r4 <- ggplot(df.r4, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r4$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 4 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r4

ggsave(here("graphs","fkappa_round4.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r4 <- as.data.frame(c(k.cameo.a3.r4,k.penta.a3.r4,
                             k.cameo.a2.r4,k.penta.a2.r4,
                             k.cameo.a1.r4,k.penta.a1.r4,
                             k.rel.r4))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r4 <- cbind(df.dis.r4, tasks)
colnames(df.dis.r4) <- c("value","task")

# Plot
plot.dis.r4 <- ggplot(df.dis.r4, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r4$task)) + 
  ylim(0,0.6) +
  ggtitle("Intercoder reliability - Round 4 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r4

ggsave(here("graphs","fkappa_dis_round4.pdf"), width = 8, height = 4)








################################################################
################################################################
# PLOT RELEVANCE FLEISS KAPPA BY SELECTED CODERS

# Check different 
k.rel.r4.12 <- as.numeric(kappam.fleiss(relevant.r4[c(1,2)])[5])
k.rel.r4.12 
k.rel.r4.345 <- as.numeric(kappam.fleiss(relevant.r4[c(3,4,5)])[5])
k.rel.r4.345
k.rel.r4.34 <- as.numeric(kappam.fleiss(relevant.r4[c(3,4)])[5])
k.rel.r4.34
k.rel.r4.35 <- as.numeric(kappam.fleiss(relevant.r4[c(3,5)])[5])
k.rel.r4.35
k.rel.r4.45 <- as.numeric(kappam.fleiss(relevant.r4[c(4,5)])[5])
k.rel.r4.45
k.rel.r4.135 <- as.numeric(kappam.fleiss(relevant.r4[c(1,3,5)])[5])
k.rel.r4.135
k.rel.r4.1345 <- as.numeric(kappam.fleiss(relevant.r4[c(1,3,4,5)])[5])
k.rel.r4.1345


# Generate data frame of aggregate results
df.dis.r4.coders <- as.data.frame(c(k.rel.r4.12,
                                    k.rel.r4.345,
                                    k.rel.r4.34,
                                    k.rel.r4.35,
                                    k.rel.r4.45,
                                    k.rel.r4.135,
                                    k.rel.r4.1345))
tasks <- c("Jennifer\n Karina",
           "Marcus\n Praj\n Geoffrey",
           "Marcus\n Praj",
           "Marcus\n Geoffrey",
           "Praj\n Geoffrey",
           "Jennifer\n Marcus\n Geoffrey",
           "Jennifer\n Marcus\n Praj\n Geoffrey")

df.dis.r4.coders <- cbind(df.dis.r4.coders, tasks)
colnames(df.dis.r4.coders) <- c("value","task")



# Plot
plot.dis.r4.coders <- ggplot(df.dis.r4.coders, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r4.coders$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 4 (by coders)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r4.coders

ggsave(here("graphs","fkappa_dis_round4_coders.pdf"), width = 8, height = 4)




################################################################
################################################################
# PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS

# Check different 
k.rel.r4.12 <- as.numeric(kappam.fleiss(relevant.r4[c(1,2)])[5])
k.rel.r4.13 <- as.numeric(kappam.fleiss(relevant.r4[c(1,3)])[5])
k.rel.r4.14 <- as.numeric(kappam.fleiss(relevant.r4[c(1,4)])[5])
k.rel.r4.15 <- as.numeric(kappam.fleiss(relevant.r4[c(1,5)])[5])

k.rel.r4.21 <- as.numeric(kappam.fleiss(relevant.r4[c(2,1)])[5])
k.rel.r4.23 <- as.numeric(kappam.fleiss(relevant.r4[c(2,3)])[5])
k.rel.r4.24 <- as.numeric(kappam.fleiss(relevant.r4[c(2,4)])[5])
k.rel.r4.25 <- as.numeric(kappam.fleiss(relevant.r4[c(2,5)])[5])

k.rel.r4.31 <- as.numeric(kappam.fleiss(relevant.r4[c(3,1)])[5])
k.rel.r4.32 <- as.numeric(kappam.fleiss(relevant.r4[c(3,2)])[5])
k.rel.r4.34 <- as.numeric(kappam.fleiss(relevant.r4[c(3,4)])[5])
k.rel.r4.35 <- as.numeric(kappam.fleiss(relevant.r4[c(3,5)])[5])

k.rel.r4.41 <- as.numeric(kappam.fleiss(relevant.r4[c(4,1)])[5])
k.rel.r4.42 <- as.numeric(kappam.fleiss(relevant.r4[c(4,2)])[5])
k.rel.r4.43 <- as.numeric(kappam.fleiss(relevant.r4[c(4,3)])[5])
k.rel.r4.45 <- as.numeric(kappam.fleiss(relevant.r4[c(4,5)])[5])

k.rel.r4.51 <- as.numeric(kappam.fleiss(relevant.r4[c(5,1)])[5])
k.rel.r4.52 <- as.numeric(kappam.fleiss(relevant.r4[c(5,2)])[5])
k.rel.r4.53 <- as.numeric(kappam.fleiss(relevant.r4[c(5,3)])[5])
k.rel.r4.54 <- as.numeric(kappam.fleiss(relevant.r4[c(5,4)])[5])

coder1.all.r4 <-c(1,k.rel.r4.12,k.rel.r4.13,k.rel.r4.14,k.rel.r4.15)
coder2.all.r4 <-c(k.rel.r4.21,1,k.rel.r4.23,k.rel.r4.24,k.rel.r4.25)
coder3.all.r4 <-c(k.rel.r4.31,k.rel.r4.32,1,k.rel.r4.34,k.rel.r4.35)
coder4.all.r4 <-c(k.rel.r4.41,k.rel.r4.42,k.rel.r4.43,1,k.rel.r4.45)
coder5.all.r4 <-c(k.rel.r4.51,k.rel.r4.52,k.rel.r4.53,k.rel.r4.51,1)

coders.all.r4 <- data.frame(coder1.all.r4,
                            coder2.all.r4,
                            coder3.all.r4,
                            coder4.all.r4,
                            coder5.all.r4)

names(coders.all.r4) <- c("Jennifer","Karina","Marcus","Praj","Geoffrey")
row.names(coders.all.r4) <- c("Jennifer","Karina","Marcus","Praj","Geoffrey")



coders.all.r4 <- as.matrix(coders.all.r4)



# Heatmap with ggplot

library(ggplot2)
library(reshape2)


coders.all.r4.m  <- melt(coders.all.r4)
colnames(coders.all.r4.m) <- c("x", "y", "value")

ggplot(coders.all.r4.m, aes(x = x, y = y, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
  ggtitle("Pairwise inter-coder agreement (round 4)") + 
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Fleiss\n Kappa"))


ggsave(here("graphs","fkappa_pairwise_r4.pdf"), width = 5, height = 4)



}




################################################################
################################################################
# GET DATA ROUND 5
{

################################################
# Get Round 5
data.r5 <- read_csv(here("training_round_5",
                         #"summary.csv"))
                         "summary.csv"))

# Explore data
names(data.r5)

# Extract sentence number and make it first column
data.r5 <- data.r5 %>%
  mutate(num=as.numeric(str_extract_all(data.r5$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r5 <- data.r5 %>%
  select('1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r5) <- c('rater1','rater2','rater3','rater4','rater5')

# export relevant
relevant.r5.export <- data.r5 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_2','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r5.export) <- c('story','sentences','rater1','rater2','rater3','rater4','rater5')
write_csv(relevant.r5.export,here("training_round_5","relevant_r5.csv"))


##################
# Task 2: PentaClass

# Action 1
penta.a1.r5 <- data.r5 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_2','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r5) <- c('rater1','rater2','rater3','rater4','rater5')

# Action 2
penta.a2.r5 <- data.r5 %>% 
  select("Action2-PentaClass_1","Action2-PentaClass_2","Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r5) <- c('rater1','rater2','rater3','rater4','rater5')


# Action 3
penta.a3.r5 <- data.r5 %>% 
  select("Action3-PentaClass_1","Action3-PentaClass_2", "Action3-PentaClass_3","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r5) <- c('rater1','rater2','rater3','rater5')


# Action 4
penta.a4.r5 <- data.r5 %>% 
  select("Action4-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a4.r5) <- c('rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r5 <- data.r5 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_2','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r5) <- c('rater1','rater2','rater3','rater4','rater5')

# Action 2
cameo.a2.r5 <- data.r5 %>%
  select('Action2_CAMEO_1','Action2_CAMEO_2','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r5) <- c('rater1','rater2','rater3','rater4','rater5')

# Action 3
cameo.a3.r5 <- data.r5 %>%
  select('Action3_CAMEO_1','Action3_CAMEO_2','Action3_CAMEO_3', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r5) <- c('rater1','rater2','rater3','rater5')

# Action 4
cameo.a4.r5 <- data.r5 %>%
  select('Action4_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a4.r5) <- c('rater5')





################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Eliminate coder 4 (Praj) in this round

relevant.r5 <- relevant.r5[,c(1,2,3,5)]
penta.a1.r5 <- penta.a1.r5[,c(1,2,3,5)]
penta.a2.r5 <- penta.a2.r5[,c(1,2,3,5)]
cameo.a1.r5 <- cameo.a1.r5[,c(1,2,3,5)]
cameo.a2.r5 <- cameo.a2.r5[,c(1,2,3,5)]




##################
# Task 1: Relevant
k.rel.r5 <- as.numeric(kappam.fleiss(relevant.r5)[5])
k.rel.r5



##################
# Task 2: PentaClass

#kappa2(penta.a1.r5[,c(1,4)], "unweighted")


# PentaClass by actor
k.penta.a1.r5 <- as.numeric(kappam.fleiss(penta.a1.r5, exact=TRUE)[5])
k.penta.a2.r5 <- as.numeric(kappam.fleiss(penta.a2.r5, exact=TRUE)[5])
k.penta.a3.r5 <- as.numeric(kappam.fleiss(penta.a3.r5, exact=TRUE)[5])
#k.penta.a4.r5 <- as.numeric(kappam.fleiss(penta.a4.r5, exact=TRUE)[5])

# Average 
k.penta.r5 <- mean(c(k.penta.a1.r5,k.penta.a2.r5,k.penta.a3.r5))
k.penta.r5




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r5 <- as.numeric(kappam.fleiss(cameo.a1.r5, exact=TRUE)[5])
k.cameo.a2.r5 <- as.numeric(kappam.fleiss(cameo.a2.r5, exact=TRUE)[5])
k.cameo.a3.r5 <- as.numeric(kappam.fleiss(cameo.a3.r5, exact=TRUE)[5])
#k.cameo.a4.r5 <- as.numeric(kappam.fleiss(cameo.a4.r5, exact=TRUE)[5])

# Average 
k.cameo.r5 <- mean(c(k.cameo.a1.r5,k.cameo.a2.r5,k.cameo.a3.r5))
k.cameo.r5














################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r5 <- as.data.frame(c(k.cameo.r5,k.penta.r5,k.rel.r5))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r5 <- cbind(df.r5, tasks)
colnames(df.r5) <- c("value","task")


# Plot
plot.r5 <- ggplot(df.r5, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r5$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 5 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r5

ggsave(here("graphs","fkappa_round5.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r5 <- as.data.frame(c(k.cameo.a3.r5,k.penta.a3.r5,
                             k.cameo.a2.r5,k.penta.a2.r5,
                             k.cameo.a1.r5,k.penta.a1.r5,
                             k.rel.r5))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r5 <- cbind(df.dis.r5, tasks)
colnames(df.dis.r5) <- c("value","task")

# Plot
plot.dis.r5 <- ggplot(df.dis.r5, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r5$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 5 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r5

ggsave(here("graphs","fkappa_dis_round5.pdf"), width = 8, height = 4)








# ################################################################
# ################################################################
# # PLOT RELEVANCE FLEISS KAPPA BY SELECTED CODERS
# 
# # Check different 
# k.rel.r5.12 <- as.numeric(kappam.fleiss(relevant.r5[c(1,2)])[5])
# k.rel.r5.12 
# k.rel.r5.345 <- as.numeric(kappam.fleiss(relevant.r5[c(3,4,5)])[5])
# k.rel.r5.345
# k.rel.r5.34 <- as.numeric(kappam.fleiss(relevant.r5[c(3,4)])[5])
# k.rel.r5.34
# k.rel.r5.35 <- as.numeric(kappam.fleiss(relevant.r5[c(3,5)])[5])
# k.rel.r5.35
# k.rel.r5.45 <- as.numeric(kappam.fleiss(relevant.r5[c(4,5)])[5])
# k.rel.r5.45
# k.rel.r5.135 <- as.numeric(kappam.fleiss(relevant.r5[c(1,3,5)])[5])
# k.rel.r5.135
# k.rel.r5.1345 <- as.numeric(kappam.fleiss(relevant.r5[c(1,3,4,5)])[5])
# k.rel.r5.1345
# 
# 
# # Generate data frame of aggregate results
# df.dis.r5.coders <- as.data.frame(c(k.rel.r5.12,
#                                     k.rel.r5.345,
#                                     k.rel.r5.34,
#                                     k.rel.r5.35,
#                                     k.rel.r5.45,
#                                     k.rel.r5.135,
#                                     k.rel.r5.1345))
# tasks <- c("Jennifer\n Karina",
#            "Marcus\n Praj\n Geoffrey",
#            "Marcus\n Praj",
#            "Marcus\n Geoffrey",
#            "Praj\n Geoffrey",
#            "Jennifer\n Marcus\n Geoffrey",
#            "Jennifer\n Marcus\n Praj\n Geoffrey")
# 
# df.dis.r5.coders <- cbind(df.dis.r5.coders, tasks)
# colnames(df.dis.r5.coders) <- c("value","task")
# 
# 
# 
# # Plot
# plot.dis.r5.coders <- ggplot(df.dis.r5.coders, aes(task,value, fill=task)) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
#                              "firebrick3", "dodgerblue3", "darkorange3",
#                              "forestgreen")) + 
#   scale_x_discrete(limits = rev(df.dis.r5.coders$task)) + 
#   ylim(0,0.9) +
#   ggtitle("Intercoder reliability - Round 4 (by coders)") + ylab("Fleiss Kappa") +
#   theme(legend.position = "none")  
# 
# plot.dis.r5.coders
# 
# ggsave(here("graphs","fkappa_dis_round4_coders.pdf"), width = 8, height = 4)




################################################################
################################################################
# PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS

# Check different 
k.rel.r5.12 <- as.numeric(kappam.fleiss(relevant.r5[c(1,2)])[5])
k.rel.r5.13 <- as.numeric(kappam.fleiss(relevant.r5[c(1,3)])[5])
k.rel.r5.14 <- as.numeric(kappam.fleiss(relevant.r5[c(1,4)])[5])
#k.rel.r5.15 <- as.numeric(kappam.fleiss(relevant.r5[c(1,5)])[5])

k.rel.r5.21 <- as.numeric(kappam.fleiss(relevant.r5[c(2,1)])[5])
k.rel.r5.23 <- as.numeric(kappam.fleiss(relevant.r5[c(2,3)])[5])
k.rel.r5.24 <- as.numeric(kappam.fleiss(relevant.r5[c(2,4)])[5])
#k.rel.r5.25 <- as.numeric(kappam.fleiss(relevant.r5[c(2,5)])[5])

k.rel.r5.31 <- as.numeric(kappam.fleiss(relevant.r5[c(3,1)])[5])
k.rel.r5.32 <- as.numeric(kappam.fleiss(relevant.r5[c(3,2)])[5])
k.rel.r5.34 <- as.numeric(kappam.fleiss(relevant.r5[c(3,4)])[5])
#k.rel.r5.35 <- as.numeric(kappam.fleiss(relevant.r5[c(3,5)])[5])

k.rel.r5.41 <- as.numeric(kappam.fleiss(relevant.r5[c(4,1)])[5])
k.rel.r5.42 <- as.numeric(kappam.fleiss(relevant.r5[c(4,2)])[5])
k.rel.r5.43 <- as.numeric(kappam.fleiss(relevant.r5[c(4,3)])[5])
#k.rel.r5.45 <- as.numeric(kappam.fleiss(relevant.r5[c(4,5)])[5])

#k.rel.r5.51 <- as.numeric(kappam.fleiss(relevant.r5[c(5,1)])[5])
#k.rel.r5.52 <- as.numeric(kappam.fleiss(relevant.r5[c(5,2)])[5])
#k.rel.r5.53 <- as.numeric(kappam.fleiss(relevant.r5[c(5,3)])[5])
#k.rel.r5.54 <- as.numeric(kappam.fleiss(relevant.r5[c(5,4)])[5])

coder1.all.r5 <-c(1,k.rel.r5.12,k.rel.r5.13,k.rel.r5.14)
coder2.all.r5 <-c(k.rel.r5.21,1,k.rel.r5.23,k.rel.r5.24)
coder3.all.r5 <-c(k.rel.r5.31,k.rel.r5.32,1,k.rel.r5.34)
coder4.all.r5 <-c(k.rel.r5.41,k.rel.r5.42,k.rel.r5.43,1)
#coder5.all.r5 <-c(k.rel.r5.51,k.rel.r5.52,k.rel.r5.53,k.rel.r5.51,1)

coders.all.r5 <- data.frame(coder1.all.r5,
                            coder2.all.r5,
                            coder3.all.r5,
                            coder4.all.r5
                            #,
                            #coder5.all.r5
)

names(coders.all.r5) <- c("Jennifer","Karina","Marcus","Geoffrey")
row.names(coders.all.r5) <- c("Jennifer","Karina","Marcus","Geoffrey")



coders.all.r5 <- as.matrix(coders.all.r5)



# Heatmap with ggplot

library(ggplot2)
library(reshape2)


coders.all.r5.m  <- melt(coders.all.r5)
colnames(coders.all.r5.m) <- c("x", "y", "value")

ggplot(coders.all.r5.m, aes(x = x, y = y, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
  ggtitle("Pairwise inter-coder agreement (round 5)") + 
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Fleiss\n Kappa"))


ggsave(here("graphs","fkappa_pairwise_r5.pdf"), width = 5, height = 4)



}




################################################################
################################################################
# GET DATA ROUND 6
{

################################################
# Get Round 6
data.r6 <- read_csv(here("training_round_6",
                         "summary.csv"))

# Explore data
names(data.r6)

# Extract sentence number and make it first column
data.r6 <- data.r6 %>%
  mutate(num=as.numeric(str_extract_all(data.r6$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r6 <- data.r6 %>%
  select('1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r6) <- c('rater1','rater3','rater4','rater5')

# export relevant
relevant.r6.export <- data.r6 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r6.export) <- c('story','sentences','rater1','rater3','rater4','rater5')
write_csv(relevant.r6.export,here("training_round_6","relevant_r6.csv"))


##################
# Task 2: PentaClass

# Action 1
penta.a1.r6 <- data.r6 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r6) <- c('rater1','rater3','rater4','rater5')

# Action 2
penta.a2.r6 <- data.r6 %>% 
  select("Action2-PentaClass_1","Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r6) <- c('rater1','rater3','rater4','rater5')


# Action 3
penta.a3.r6 <- data.r6 %>% 
  select("Action3-PentaClass_1", "Action3-PentaClass_3","Action3-PentaClass_4","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r6) <- c('rater1','rater3','rater4','rater5')


# Action 4
penta.a4.r6 <- data.r6 %>% 
  select("Action4-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a4.r6) <- c('rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r6 <- data.r6 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r6) <- c('rater1','rater3','rater4','rater5')

# Action 2
cameo.a2.r6 <- data.r6 %>%
  select('Action2_CAMEO_1','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r6) <- c('rater1','rater3','rater4','rater5')

# Action 3
cameo.a3.r6 <- data.r6 %>%
  select('Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r6) <- c('rater1','rater3','rater4','rater5')

# Action 4
cameo.a4.r6 <- data.r6 %>%
  select('Action4_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a4.r6) <- c('rater5')





################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Eliminate coder 2 (Karina) permanently

relevant.r6 <- relevant.r6[,c(1,2,3,4)]
penta.a1.r6 <- penta.a1.r6[,c(1,2,3,4)]
penta.a2.r6 <- penta.a2.r6[,c(1,2,3,4)]
cameo.a1.r6 <- cameo.a1.r6[,c(1,2,3,4)]
cameo.a2.r6 <- cameo.a2.r6[,c(1,2,3,4)]




##################
# Task 1: Relevant
k.rel.r6 <- as.numeric(kappam.fleiss(relevant.r6)[5])
k.rel.r6



##################
# Task 2: PentaClass

#kappa2(penta.a1.r6[,c(1,4)], "unweighted")


# PentaClass by actor
k.penta.a1.r6 <- as.numeric(kappam.fleiss(penta.a1.r6, exact=TRUE)[5])
k.penta.a2.r6 <- as.numeric(kappam.fleiss(penta.a2.r6, exact=TRUE)[5])
k.penta.a3.r6 <- as.numeric(kappam.fleiss(penta.a3.r6, exact=TRUE)[5])
#k.penta.a4.r6 <- as.numeric(kappam.fleiss(penta.a4.r6, exact=TRUE)[5])

# Average 
k.penta.r6 <- mean(c(k.penta.a1.r6,k.penta.a2.r6,k.penta.a3.r6))
k.penta.r6




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r6 <- as.numeric(kappam.fleiss(cameo.a1.r6, exact=TRUE)[5])
k.cameo.a2.r6 <- as.numeric(kappam.fleiss(cameo.a2.r6, exact=TRUE)[5])
k.cameo.a3.r6 <- as.numeric(kappam.fleiss(cameo.a3.r6, exact=TRUE)[5])
#k.cameo.a4.r6 <- as.numeric(kappam.fleiss(cameo.a4.r6, exact=TRUE)[5])

# Average 
k.cameo.r6 <- mean(c(k.cameo.a1.r6,k.cameo.a2.r6,k.cameo.a3.r6))
k.cameo.r6














################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r6 <- as.data.frame(c(k.cameo.r6,k.penta.r6,k.rel.r6))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r6 <- cbind(df.r6, tasks)
colnames(df.r6) <- c("value","task")


# Plot
plot.r6 <- ggplot(df.r6, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r6$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 6 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r6

ggsave(here("graphs","fkappa_round6.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r6 <- as.data.frame(c(k.cameo.a3.r6,k.penta.a3.r6,
                             k.cameo.a2.r6,k.penta.a2.r6,
                             k.cameo.a1.r6,k.penta.a1.r6,
                             k.rel.r6))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r6 <- cbind(df.dis.r6, tasks)
colnames(df.dis.r6) <- c("value","task")

# Plot
plot.dis.r6 <- ggplot(df.dis.r6, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r6$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 6 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r6

ggsave(here("graphs","fkappa_dis_round6.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS

# Check different 
k.rel.r6.12 <- as.numeric(kappam.fleiss(relevant.r6[c(1,2)])[5])
k.rel.r6.13 <- as.numeric(kappam.fleiss(relevant.r6[c(1,3)])[5])
k.rel.r6.14 <- as.numeric(kappam.fleiss(relevant.r6[c(1,4)])[5])
#k.rel.r6.15 <- as.numeric(kappam.fleiss(relevant.r6[c(1,5)])[5])

k.rel.r6.21 <- as.numeric(kappam.fleiss(relevant.r6[c(2,1)])[5])
k.rel.r6.23 <- as.numeric(kappam.fleiss(relevant.r6[c(2,3)])[5])
k.rel.r6.24 <- as.numeric(kappam.fleiss(relevant.r6[c(2,4)])[5])
#k.rel.r6.25 <- as.numeric(kappam.fleiss(relevant.r6[c(2,5)])[5])

k.rel.r6.31 <- as.numeric(kappam.fleiss(relevant.r6[c(3,1)])[5])
k.rel.r6.32 <- as.numeric(kappam.fleiss(relevant.r6[c(3,2)])[5])
k.rel.r6.34 <- as.numeric(kappam.fleiss(relevant.r6[c(3,4)])[5])
#k.rel.r6.35 <- as.numeric(kappam.fleiss(relevant.r6[c(3,5)])[5])

k.rel.r6.41 <- as.numeric(kappam.fleiss(relevant.r6[c(4,1)])[5])
k.rel.r6.42 <- as.numeric(kappam.fleiss(relevant.r6[c(4,2)])[5])
k.rel.r6.43 <- as.numeric(kappam.fleiss(relevant.r6[c(4,3)])[5])
#k.rel.r6.45 <- as.numeric(kappam.fleiss(relevant.r6[c(4,5)])[5])

#k.rel.r6.51 <- as.numeric(kappam.fleiss(relevant.r6[c(5,1)])[5])
#k.rel.r6.52 <- as.numeric(kappam.fleiss(relevant.r6[c(5,2)])[5])
#k.rel.r6.53 <- as.numeric(kappam.fleiss(relevant.r6[c(5,3)])[5])
#k.rel.r6.54 <- as.numeric(kappam.fleiss(relevant.r6[c(5,4)])[5])

coder1.all.r6 <-c(1,k.rel.r6.12,k.rel.r6.13,k.rel.r6.14)
coder2.all.r6 <-c(k.rel.r6.21,1,k.rel.r6.23,k.rel.r6.24)
coder3.all.r6 <-c(k.rel.r6.31,k.rel.r6.32,1,k.rel.r6.34)
coder4.all.r6 <-c(k.rel.r6.41,k.rel.r6.42,k.rel.r6.43,1)
#coder5.all.r6 <-c(k.rel.r6.51,k.rel.r6.52,k.rel.r6.53,k.rel.r6.51,1)

coders.all.r6 <- data.frame(coder1.all.r6,
                            coder2.all.r6,
                            coder3.all.r6,
                            coder4.all.r6
                            #,
                            #coder5.all.r6
)

names(coders.all.r6) <- c("Jennifer","Marcus","Praj","Geoffrey")
row.names(coders.all.r6) <- c("Jennifer","Marcus","Praj","Geoffrey")



coders.all.r6 <- as.matrix(coders.all.r6)



# Heatmap with ggplot

library(ggplot2)
library(reshape2)


coders.all.r6.m  <- melt(coders.all.r6)
colnames(coders.all.r6.m) <- c("x", "y", "value")

ggplot(coders.all.r6.m, aes(x = x, y = y, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
  ggtitle("Pairwise inter-coder agreement (round 6)") + 
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Fleiss\n Kappa"))


ggsave(here("graphs","fkappa_pairwise_r6.pdf"), width = 5, height = 4)


}




################################################################
################################################################
# GET DATA ROUND 7
{
################################################
# Get Round 7
data.r7 <- read_csv(here("training_round_7",
                         "summary.csv"))

# Explore data
names(data.r7)

# Extract sentence number and make it first column
data.r7 <- data.r7 %>%
  mutate(num=as.numeric(str_extract_all(data.r7$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r7 <- data.r7 %>%
  select('1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r7) <- c('rater1','rater3','rater4','rater5')

# export relevant
relevant.r7.export <- data.r7 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r7.export) <- c('story','sentences','rater1','rater3','rater4','rater5')
write_csv(relevant.r7.export,here("training_round_7","relevant_r7.csv"))






##################
# Task 2: PentaClass

# Action 1
penta.a1.r7 <- data.r7 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r7) <- c('rater1','rater3','rater4','rater5')

# Action 2
penta.a2.r7 <- data.r7 %>% 
  select("Action2-PentaClass_1","Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r7) <- c('rater1','rater3','rater4','rater5')


# Action 3
penta.a3.r7 <- data.r7 %>% 
  select("Action3-PentaClass_1", "Action3-PentaClass_3","Action3-PentaClass_4","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r7) <- c('rater1','rater3','rater4','rater5')


# Action 4
penta.a4.r7 <- data.r7 %>% 
  select("Action4-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a4.r7) <- c('rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r7 <- data.r7 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r7) <- c('rater1','rater3','rater4','rater5')

# Action 2
cameo.a2.r7 <- data.r7 %>%
  select('Action2_CAMEO_1','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r7) <- c('rater1','rater3','rater4','rater5')

# Action 3
cameo.a3.r7 <- data.r7 %>%
  select('Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r7) <- c('rater1','rater3','rater4','rater5')

# Action 4
cameo.a4.r7 <- data.r7 %>%
  select('Action4_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a4.r7) <- c('rater5')





################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Eliminate coder 2 (Karina) permanently

relevant.r7 <- relevant.r7[,c(1,2,3,4)]
penta.a1.r7 <- penta.a1.r7[,c(1,2,3,4)]
penta.a2.r7 <- penta.a2.r7[,c(1,2,3,4)]
cameo.a1.r7 <- cameo.a1.r7[,c(1,2,3,4)]
cameo.a2.r7 <- cameo.a2.r7[,c(1,2,3,4)]




##################
# Task 1: Relevant
k.rel.r7 <- as.numeric(kappam.fleiss(relevant.r7)[5])
k.rel.r7



##################
# Task 2: PentaClass

#kappa2(penta.a1.r7[,c(1,4)], "unweighted")


# PentaClass by actor
k.penta.a1.r7 <- as.numeric(kappam.fleiss(penta.a1.r7, exact=TRUE)[5])
k.penta.a2.r7 <- as.numeric(kappam.fleiss(penta.a2.r7, exact=TRUE)[5])
k.penta.a3.r7 <- as.numeric(kappam.fleiss(penta.a3.r7, exact=TRUE)[5])
#k.penta.a4.r7 <- as.numeric(kappam.fleiss(penta.a4.r7, exact=TRUE)[5])

# Average 
k.penta.r7 <- mean(c(k.penta.a1.r7,k.penta.a2.r7,k.penta.a3.r7))
k.penta.r7




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r7 <- as.numeric(kappam.fleiss(cameo.a1.r7, exact=TRUE)[5])
k.cameo.a2.r7 <- as.numeric(kappam.fleiss(cameo.a2.r7, exact=TRUE)[5])
k.cameo.a3.r7 <- as.numeric(kappam.fleiss(cameo.a3.r7, exact=TRUE)[5])
#k.cameo.a4.r7 <- as.numeric(kappam.fleiss(cameo.a4.r7, exact=TRUE)[5])

# Average 
k.cameo.r7 <- mean(c(k.cameo.a1.r7,k.cameo.a2.r7,k.cameo.a3.r7))
k.cameo.r7














################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r7 <- as.data.frame(c(k.cameo.r7,k.penta.r7,k.rel.r7))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r7 <- cbind(df.r7, tasks)
colnames(df.r7) <- c("value","task")


# Plot
plot.r7 <- ggplot(df.r7, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r7$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 7 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r7

ggsave(here("graphs","fkappa_round7.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r7 <- as.data.frame(c(k.cameo.a3.r7,k.penta.a3.r7,
                             k.cameo.a2.r7,k.penta.a2.r7,
                             k.cameo.a1.r7,k.penta.a1.r7,
                             k.rel.r7))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r7 <- cbind(df.dis.r7, tasks)
colnames(df.dis.r7) <- c("value","task")

# Plot
plot.dis.r7 <- ggplot(df.dis.r7, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r7$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 7 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r7

ggsave(here("graphs","fkappa_dis_round7.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS

# Check different 
k.rel.r7.12 <- as.numeric(kappam.fleiss(relevant.r7[c(1,2)])[5])
k.rel.r7.13 <- as.numeric(kappam.fleiss(relevant.r7[c(1,3)])[5])
k.rel.r7.14 <- as.numeric(kappam.fleiss(relevant.r7[c(1,4)])[5])
#k.rel.r7.15 <- as.numeric(kappam.fleiss(relevant.r7[c(1,5)])[5])

k.rel.r7.21 <- as.numeric(kappam.fleiss(relevant.r7[c(2,1)])[5])
k.rel.r7.23 <- as.numeric(kappam.fleiss(relevant.r7[c(2,3)])[5])
k.rel.r7.24 <- as.numeric(kappam.fleiss(relevant.r7[c(2,4)])[5])
#k.rel.r7.25 <- as.numeric(kappam.fleiss(relevant.r7[c(2,5)])[5])

k.rel.r7.31 <- as.numeric(kappam.fleiss(relevant.r7[c(3,1)])[5])
k.rel.r7.32 <- as.numeric(kappam.fleiss(relevant.r7[c(3,2)])[5])
k.rel.r7.34 <- as.numeric(kappam.fleiss(relevant.r7[c(3,4)])[5])
#k.rel.r7.35 <- as.numeric(kappam.fleiss(relevant.r7[c(3,5)])[5])

k.rel.r7.41 <- as.numeric(kappam.fleiss(relevant.r7[c(4,1)])[5])
k.rel.r7.42 <- as.numeric(kappam.fleiss(relevant.r7[c(4,2)])[5])
k.rel.r7.43 <- as.numeric(kappam.fleiss(relevant.r7[c(4,3)])[5])
#k.rel.r7.45 <- as.numeric(kappam.fleiss(relevant.r7[c(4,5)])[5])

#k.rel.r7.51 <- as.numeric(kappam.fleiss(relevant.r7[c(5,1)])[5])
#k.rel.r7.52 <- as.numeric(kappam.fleiss(relevant.r7[c(5,2)])[5])
#k.rel.r7.53 <- as.numeric(kappam.fleiss(relevant.r7[c(5,3)])[5])
#k.rel.r7.54 <- as.numeric(kappam.fleiss(relevant.r7[c(5,4)])[5])

coder1.all.r7 <-c(1,k.rel.r7.12,k.rel.r7.13,k.rel.r7.14)
coder2.all.r7 <-c(k.rel.r7.21,1,k.rel.r7.23,k.rel.r7.24)
coder3.all.r7 <-c(k.rel.r7.31,k.rel.r7.32,1,k.rel.r7.34)
coder4.all.r7 <-c(k.rel.r7.41,k.rel.r7.42,k.rel.r7.43,1)
#coder5.all.r7 <-c(k.rel.r7.51,k.rel.r7.52,k.rel.r7.53,k.rel.r7.51,1)

coders.all.r7 <- data.frame(coder1.all.r7,
                            coder2.all.r7,
                            coder3.all.r7,
                            coder4.all.r7
                            #,
                            #coder5.all.r7
)

names(coders.all.r7) <- c("Jennifer","Marcus","Praj","Geoffrey")
row.names(coders.all.r7) <- c("Jennifer","Marcus","Praj","Geoffrey")



coders.all.r7 <- as.matrix(coders.all.r7)



# Heatmap with ggplot

library(ggplot2)
library(reshape2)


coders.all.r7.m  <- melt(coders.all.r7)
colnames(coders.all.r7.m) <- c("x", "y", "value")

ggplot(coders.all.r7.m, aes(x = x, y = y, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
  ggtitle("Pairwise inter-coder agreement (round 7)") + 
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Fleiss\n Kappa"))


ggsave(here("graphs","fkappa_pairwise_r7.pdf"), width = 5, height = 4)

}




################################################################
################################################################
# GET DATA ROUND 8
{

################################################
# Get Round 8
data.r8 <- read_csv(here("training_round_8",
                         "summary.csv"))

# Explore data
names(data.r8)

# Extract sentence number and make it first column
data.r8 <- data.r8 %>%
  mutate(num=as.numeric(str_extract_all(data.r8$plain_text,"(?<=No.).+(?=sentences)"))) %>%
  relocate(num)


################################################
# Generate data by annotation task

##################
# Task 1: Relevant
relevant.r8 <- data.r8 %>%
  select('1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r8) <- c('rater1','rater3','rater4','rater5')

# export relevant
relevant.r8.export <- data.r8 %>%
  select('num','sentences','1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
colnames(relevant.r8.export) <- c('story','sentences','rater1','rater3','rater4','rater5')
write_csv(relevant.r8.export,here("training_round_8","relevant_r8.csv"))






##################
# Task 2: PentaClass

# Action 1
penta.a1.r8 <- data.r8 %>% 
  select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
  replace(is.na(.), "")
colnames(penta.a1.r8) <- c('rater1','rater3','rater4','rater5')

# Action 2
penta.a2.r8 <- data.r8 %>% 
  select("Action2-PentaClass_1","Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a2.r8) <- c('rater1','rater3','rater4','rater5')


# Action 3
penta.a3.r8 <- data.r8 %>% 
  select("Action3-PentaClass_1", "Action3-PentaClass_3","Action3-PentaClass_4","Action3-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a3.r8) <- c('rater1','rater3','rater4','rater5')


# Action 4
penta.a4.r8 <- data.r8 %>% 
  select("Action4-PentaClass_5") %>%
  replace(is.na(.), "")
colnames(penta.a4.r8) <- c('rater5')



##################
# Task 3: CAMEO

# Action 1
cameo.a1.r8 <- data.r8 %>%
  select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a1.r8) <- c('rater1','rater3','rater4','rater5')

# Action 2
cameo.a2.r8 <- data.r8 %>%
  select('Action2_CAMEO_1','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a2.r8) <- c('rater1','rater3','rater4','rater5')

# Action 3
cameo.a3.r8 <- data.r8 %>%
  select('Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a3.r8) <- c('rater1','rater3','rater4','rater5')

# Action 4
cameo.a4.r8 <- data.r8 %>%
  select('Action4_CAMEO_5') %>%
  replace(is.na(.), "")
colnames(cameo.a4.r8) <- c('rater5')



################################################################
################################################################
# EXPORT ALL ANNOTATIONS

# Explort annotations and sentences
penta.a1.r8.sent <- data.r8 %>% select('num','sentences','Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>% replace(is.na(.), "")
penta.a2.r8.sent <- data.r8 %>% select('num','sentences','Action2-PentaClass_1','Action2-PentaClass_3','Action2-PentaClass_4','Action2-PentaClass_5') %>% replace(is.na(.), "")
penta.a3.r8.sent <- data.r8 %>% select('num','sentences','Action3-PentaClass_1','Action3-PentaClass_3','Action3-PentaClass_4','Action3-PentaClass_5') %>% replace(is.na(.), "")
penta.a4.r8.sent <- data.r8 %>% select('num','sentences','Action4-PentaClass_5') %>% replace(is.na(.), "")
cameo.a1.r8.sent <- data.r8 %>% select('num','sentences','Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>% replace(is.na(.), "")
cameo.a2.r8.sent <- data.r8 %>% select('num','sentences','Action2_CAMEO_1','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>% replace(is.na(.), "")
cameo.a3.r8.sent <- data.r8 %>% select('num','sentences','Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>% replace(is.na(.), "")
cameo.a4.r8.sent <- data.r8 %>% select('num','sentences','Action4_CAMEO_5') %>% replace(is.na(.), "")

# # save three dataframes to a excel spreadsheet 
# openxlsx::write.xlsx(list("relevant" = relevant.r8.export, 
#                           "penta1" = penta.a1.r8.sent,
#                           "penta2" = penta.a2.r8.sent,
#                           "penta3" = penta.a3.r8.sent,
#                           "penta4" = penta.a4.r8.sent,
#                           "cameo1" = cameo.a1.r8.sent,
#                           "cameo2" = cameo.a2.r8.sent,
#                           "cameo3" = cameo.a3.r8.sent,
#                           "cameo4" = cameo.a4.r8.sent), 
#                      file = here("training_round_8",
#                                  glue("annotations.xlsx")))




################################################################
################################################################
# INTERCODER RELIABILITY FLEISS KAPPA

##################
# Eliminate coder 2 (Karina) permanently

relevant.r8 <- relevant.r8[,c(1,2,3,4)]
penta.a1.r8 <- penta.a1.r8[,c(1,2,3,4)]
penta.a2.r8 <- penta.a2.r8[,c(1,2,3,4)]
cameo.a1.r8 <- cameo.a1.r8[,c(1,2,3,4)]
cameo.a2.r8 <- cameo.a2.r8[,c(1,2,3,4)]




##################
# Task 1: Relevant
k.rel.r8 <- as.numeric(kappam.fleiss(relevant.r8)[5])
k.rel.r8



##################
# Task 2: PentaClass

#kappa2(penta.a1.r8[,c(1,4)], "unweighted")


# PentaClass by actor
k.penta.a1.r8 <- as.numeric(kappam.fleiss(penta.a1.r8, exact=TRUE)[5])
k.penta.a2.r8 <- as.numeric(kappam.fleiss(penta.a2.r8, exact=TRUE)[5])
k.penta.a3.r8 <- as.numeric(kappam.fleiss(penta.a3.r8, exact=TRUE)[5])
#k.penta.a4.r8 <- as.numeric(kappam.fleiss(penta.a4.r8, exact=TRUE)[5])

# Average 
k.penta.r8 <- mean(c(k.penta.a1.r8,k.penta.a2.r8,k.penta.a3.r8))
k.penta.r8




##################
# Task 3: CAMEO

# CAMEO by actor
k.cameo.a1.r8 <- as.numeric(kappam.fleiss(cameo.a1.r8, exact=TRUE)[5])
k.cameo.a2.r8 <- as.numeric(kappam.fleiss(cameo.a2.r8, exact=TRUE)[5])
k.cameo.a3.r8 <- as.numeric(kappam.fleiss(cameo.a3.r8, exact=TRUE)[5])
#k.cameo.a4.r8 <- as.numeric(kappam.fleiss(cameo.a4.r8, exact=TRUE)[5])

# Average 
k.cameo.r8 <- mean(c(k.cameo.a1.r8,k.cameo.a2.r8,k.cameo.a3.r8))
k.cameo.r8




# Summary of results
k.rel.r8
k.penta.r8
k.cameo.r8






################################################################
################################################################
# PLOT AGGREGATE FLEISS KAPPA

# Generate data frame of aggregate results
df.r8 <- as.data.frame(c(k.cameo.r8,k.penta.r8,k.rel.r8))
tasks <- c("CAMEO","PentaClass","Relevant")
df.r8 <- cbind(df.r8, tasks)
colnames(df.r8) <- c("value","task")


# Plot
plot.r8 <- ggplot(df.r8, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
  scale_x_discrete(limits = rev(df.r8$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 8 (aggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")

plot.r8

ggsave(here("graphs","fkappa_round8.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT DISAGGREGATED FLEISS KAPPA

# Generate data frame of aggregate results
df.dis.r8 <- as.data.frame(c(k.cameo.a3.r8,k.penta.a3.r8,
                             k.cameo.a2.r8,k.penta.a2.r8,
                             k.cameo.a1.r8,k.penta.a1.r8,
                             k.rel.r8))
tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
           "CAMEO\n Action 2","PentaClass\n Action 2",
           "CAMEO\n Action 1","PentaClass\n Action 1",
           "Relevant")
df.dis.r8 <- cbind(df.dis.r8, tasks)
colnames(df.dis.r8) <- c("value","task")

# Plot
plot.dis.r8 <- ggplot(df.dis.r8, aes(task,value, fill=task)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                             "firebrick3", "dodgerblue3", "darkorange3",
                             "forestgreen")) + 
  scale_x_discrete(limits = rev(df.dis.r8$task)) + 
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Round 8 (disaggregated)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.dis.r8

ggsave(here("graphs","fkappa_dis_round8.pdf"), width = 8, height = 4)









################################################################
################################################################
# PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS

# Check different 
k.rel.r8.12 <- as.numeric(kappam.fleiss(relevant.r8[c(1,2)])[5])
k.rel.r8.13 <- as.numeric(kappam.fleiss(relevant.r8[c(1,3)])[5])
k.rel.r8.14 <- as.numeric(kappam.fleiss(relevant.r8[c(1,4)])[5])
#k.rel.r8.15 <- as.numeric(kappam.fleiss(relevant.r8[c(1,5)])[5])

k.rel.r8.21 <- as.numeric(kappam.fleiss(relevant.r8[c(2,1)])[5])
k.rel.r8.23 <- as.numeric(kappam.fleiss(relevant.r8[c(2,3)])[5])
k.rel.r8.24 <- as.numeric(kappam.fleiss(relevant.r8[c(2,4)])[5])
#k.rel.r8.25 <- as.numeric(kappam.fleiss(relevant.r8[c(2,5)])[5])

k.rel.r8.31 <- as.numeric(kappam.fleiss(relevant.r8[c(3,1)])[5])
k.rel.r8.32 <- as.numeric(kappam.fleiss(relevant.r8[c(3,2)])[5])
k.rel.r8.34 <- as.numeric(kappam.fleiss(relevant.r8[c(3,4)])[5])
#k.rel.r8.35 <- as.numeric(kappam.fleiss(relevant.r8[c(3,5)])[5])

k.rel.r8.41 <- as.numeric(kappam.fleiss(relevant.r8[c(4,1)])[5])
k.rel.r8.42 <- as.numeric(kappam.fleiss(relevant.r8[c(4,2)])[5])
k.rel.r8.43 <- as.numeric(kappam.fleiss(relevant.r8[c(4,3)])[5])
#k.rel.r8.45 <- as.numeric(kappam.fleiss(relevant.r8[c(4,5)])[5])

#k.rel.r8.51 <- as.numeric(kappam.fleiss(relevant.r8[c(5,1)])[5])
#k.rel.r8.52 <- as.numeric(kappam.fleiss(relevant.r8[c(5,2)])[5])
#k.rel.r8.53 <- as.numeric(kappam.fleiss(relevant.r8[c(5,3)])[5])
#k.rel.r8.54 <- as.numeric(kappam.fleiss(relevant.r8[c(5,4)])[5])

coder1.all.r8 <-c(1,k.rel.r8.12,k.rel.r8.13,k.rel.r8.14)
coder2.all.r8 <-c(k.rel.r8.21,1,k.rel.r8.23,k.rel.r8.24)
coder3.all.r8 <-c(k.rel.r8.31,k.rel.r8.32,1,k.rel.r8.34)
coder4.all.r8 <-c(k.rel.r8.41,k.rel.r8.42,k.rel.r8.43,1)
#coder5.all.r8 <-c(k.rel.r8.51,k.rel.r8.52,k.rel.r8.53,k.rel.r8.51,1)

coders.all.r8 <- data.frame(coder1.all.r8,
                            coder2.all.r8,
                            coder3.all.r8,
                            coder4.all.r8
                            #,
                            #coder5.all.r8
)

names(coders.all.r8) <- c("Jennifer","Marcus","Praj","Geoffrey")
row.names(coders.all.r8) <- c("Jennifer","Marcus","Praj","Geoffrey")



coders.all.r8 <- as.matrix(coders.all.r8)



# Heatmap with ggplot

library(ggplot2)
library(reshape2)


coders.all.r8.m  <- melt(coders.all.r8)
colnames(coders.all.r8.m) <- c("x", "y", "value")

ggplot(coders.all.r8.m, aes(x = x, y = y, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
  ggtitle("Pairwise inter-coder agreement (round 8)") + 
  xlab("") + ylab("") +
  guides(fill=guide_legend(title="Fleiss\n Kappa"))


ggsave(here("graphs","fkappa_pairwise_r8.pdf"), width = 5, height = 4)

}




################################################################
################################################################
# GET DATA ROUND 9
{
  
  ################################################
  # Get Round 9
  data.r9 <- read_csv(here("training_round_9",
                           "summary.csv"))
  
  # Explore data
  names(data.r9)
  
  # Extract sentence number and make it first column
  data.r9 <- data.r9 %>%
    mutate(num=as.numeric(str_extract_all(data.r9$plain_text,"(?<=No.).+(?=sentences)"))) %>%
    relocate(num)
  
  
  ################################################
  # Generate data by annotation task
  
  ##################
  # Task 1: Relevant
  relevant.r9 <- data.r9 %>%
    select('1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
  colnames(relevant.r9) <- c('rater1','rater3','rater4','rater5')
  
  # export relevant
  relevant.r9.export <- data.r9 %>%
    select('num','sentences','1_Relevant_1','1_Relevant_3','1_Relevant_4','1_Relevant_5')
  colnames(relevant.r9.export) <- c('story','sentences','rater1','rater3','rater4','rater5')
  write_csv(relevant.r9.export,here("training_round_9","relevant_r9.csv"))
  
  
  
  
  
  
  ##################
  # Task 2: PentaClass
  
  # Action 1
  penta.a1.r9 <- data.r9 %>% 
    select('Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>%
    replace(is.na(.), "")
  colnames(penta.a1.r9) <- c('rater1','rater3','rater4','rater5')
  
  # Action 2
  penta.a2.r9 <- data.r9 %>% 
    select("Action2-PentaClass_1","Action2-PentaClass_3", "Action2-PentaClass_4","Action2-PentaClass_5") %>%
    replace(is.na(.), "")
  colnames(penta.a2.r9) <- c('rater1','rater3','rater4','rater5')
  
  
  # Action 3
  penta.a3.r9 <- data.r9 %>% 
    select("Action3-PentaClass_1", "Action3-PentaClass_3","Action3-PentaClass_4","Action3-PentaClass_5") %>%
    replace(is.na(.), "")
  colnames(penta.a3.r9) <- c('rater1','rater3','rater4','rater5')
  
  
  # Action 4
  penta.a4.r9 <- data.r9 %>% 
    select("Action4-PentaClass_5") %>%
    replace(is.na(.), "")
  colnames(penta.a4.r9) <- c('rater5')
  
  
  
  ##################
  # Task 3: CAMEO
  
  # Action 1
  cameo.a1.r9 <- data.r9 %>%
    select('Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>%
    replace(is.na(.), "")
  colnames(cameo.a1.r9) <- c('rater1','rater3','rater4','rater5')
  
  # Action 2
  cameo.a2.r9 <- data.r9 %>%
    select('Action2_CAMEO_1','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>%
    replace(is.na(.), "")
  colnames(cameo.a2.r9) <- c('rater1','rater3','rater4','rater5')
  
  # Action 3
  cameo.a3.r9 <- data.r9 %>%
    select('Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>%
    replace(is.na(.), "")
  colnames(cameo.a3.r9) <- c('rater1','rater3','rater4','rater5')
  
  # Action 4
  cameo.a4.r9 <- data.r9 %>%
    select('Action4_CAMEO_5') %>%
    replace(is.na(.), "")
  colnames(cameo.a4.r9) <- c('rater5')
  
  
  
  ################################################################
  ################################################################
  # EXPORT ALL ANNOTATIONS
  
  # Explort annotations and sentences
  penta.a1.r9.sent <- data.r9 %>% select('num','sentences','Action1-PentaClass_1','Action1-PentaClass_3','Action1-PentaClass_4','Action1-PentaClass_5') %>% replace(is.na(.), "")
  penta.a2.r9.sent <- data.r9 %>% select('num','sentences','Action2-PentaClass_1','Action2-PentaClass_3','Action2-PentaClass_4','Action2-PentaClass_5') %>% replace(is.na(.), "")
  penta.a3.r9.sent <- data.r9 %>% select('num','sentences','Action3-PentaClass_1','Action3-PentaClass_3','Action3-PentaClass_4','Action3-PentaClass_5') %>% replace(is.na(.), "")
  penta.a4.r9.sent <- data.r9 %>% select('num','sentences','Action4-PentaClass_5') %>% replace(is.na(.), "")
  cameo.a1.r9.sent <- data.r9 %>% select('num','sentences','Action1_CAMEO_1','Action1_CAMEO_3','Action1_CAMEO_4', 'Action1_CAMEO_5') %>% replace(is.na(.), "")
  cameo.a2.r9.sent <- data.r9 %>% select('num','sentences','Action2_CAMEO_1','Action2_CAMEO_3','Action2_CAMEO_4', 'Action2_CAMEO_5') %>% replace(is.na(.), "")
  cameo.a3.r9.sent <- data.r9 %>% select('num','sentences','Action3_CAMEO_1','Action3_CAMEO_3','Action3_CAMEO_4', 'Action3_CAMEO_5') %>% replace(is.na(.), "")
  cameo.a4.r9.sent <- data.r9 %>% select('num','sentences','Action4_CAMEO_5') %>% replace(is.na(.), "")
  
  # # save three dataframes to a excel spreadsheet 
  # openxlsx::write.xlsx(list("relevant" = relevant.r9.export, 
  #                           "penta1" = penta.a1.r9.sent,
  #                           "penta2" = penta.a2.r9.sent,
  #                           "penta3" = penta.a3.r9.sent,
  #                           "penta4" = penta.a4.r9.sent,
  #                           "cameo1" = cameo.a1.r9.sent,
  #                           "cameo2" = cameo.a2.r9.sent,
  #                           "cameo3" = cameo.a3.r9.sent,
  #                           "cameo4" = cameo.a4.r9.sent), 
  #                      file = here("training_round_9",
  #                                  glue("annotations.xlsx")))
  
  
  
  
  ################################################################
  ################################################################
  # INTERCODER RELIABILITY FLEISS KAPPA
  
  ##################
  # Eliminate coder 2 (Karina) permanently
  
  relevant.r9 <- relevant.r9[,c(1,2,3,4)]
  penta.a1.r9 <- penta.a1.r9[,c(1,2,3,4)]
  penta.a2.r9 <- penta.a2.r9[,c(1,2,3,4)]
  cameo.a1.r9 <- cameo.a1.r9[,c(1,2,3,4)]
  cameo.a2.r9 <- cameo.a2.r9[,c(1,2,3,4)]
  
  
  
  
  ##################
  # Task 1: Relevant
  k.rel.r9 <- as.numeric(kappam.fleiss(relevant.r9)[5])
  k.rel.r9
  
  
  
  ##################
  # Task 2: PentaClass
  
  #kappa2(penta.a1.r9[,c(1,4)], "unweighted")
  
  
  # PentaClass by actor
  k.penta.a1.r9 <- as.numeric(kappam.fleiss(penta.a1.r9, exact=TRUE)[5])
  k.penta.a2.r9 <- as.numeric(kappam.fleiss(penta.a2.r9, exact=TRUE)[5])
  k.penta.a3.r9 <- as.numeric(kappam.fleiss(penta.a3.r9, exact=TRUE)[5])
  #k.penta.a4.r9 <- as.numeric(kappam.fleiss(penta.a4.r9, exact=TRUE)[5])
  
  # Average 
  k.penta.r9 <- mean(c(k.penta.a1.r9,k.penta.a2.r9,k.penta.a3.r9))
  k.penta.r9
  
  
  
  
  ##################
  # Task 3: CAMEO
  
  # CAMEO by actor
  k.cameo.a1.r9 <- as.numeric(kappam.fleiss(cameo.a1.r9, exact=TRUE)[5])
  k.cameo.a2.r9 <- as.numeric(kappam.fleiss(cameo.a2.r9, exact=TRUE)[5])
  k.cameo.a3.r9 <- as.numeric(kappam.fleiss(cameo.a3.r9, exact=TRUE)[5])
  #k.cameo.a4.r9 <- as.numeric(kappam.fleiss(cameo.a4.r9, exact=TRUE)[5])
  
  # Average 
  k.cameo.r9 <- mean(c(k.cameo.a1.r9,k.cameo.a2.r9,k.cameo.a3.r9))
  k.cameo.r9
  
  
  
  
  # Summary of results
  k.rel.r9
  k.penta.r9
  k.cameo.r9
  
  
  
  
  
  
  ################################################################
  ################################################################
  # PLOT AGGREGATE FLEISS KAPPA
  
  # Generate data frame of aggregate results
  df.r9 <- as.data.frame(c(k.cameo.r9,k.penta.r9,k.rel.r9))
  tasks <- c("CAMEO","PentaClass","Relevant")
  df.r9 <- cbind(df.r9, tasks)
  colnames(df.r9) <- c("value","task")
  
  
  # Plot
  plot.r9 <- ggplot(df.r9, aes(task,value, fill=task)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
    scale_x_discrete(limits = rev(df.r9$task)) + 
    ylim(0,0.9) +
    ggtitle("Intercoder reliability - Round 9 (aggregated)") + ylab("Fleiss Kappa") +
    theme(legend.position = "none")
  
  plot.r9
  
  ggsave(here("graphs","fkappa_round9.pdf"), width = 8, height = 4)
  
  
  
  
  
  
  
  
  
  ################################################################
  ################################################################
  # PLOT DISAGGREGATED FLEISS KAPPA
  
  # Generate data frame of aggregate results
  df.dis.r9 <- as.data.frame(c(k.cameo.a3.r9,k.penta.a3.r9,
                               k.cameo.a2.r9,k.penta.a2.r9,
                               k.cameo.a1.r9,k.penta.a1.r9,
                               k.rel.r9))
  tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
             "CAMEO\n Action 2","PentaClass\n Action 2",
             "CAMEO\n Action 1","PentaClass\n Action 1",
             "Relevant")
  df.dis.r9 <- cbind(df.dis.r9, tasks)
  colnames(df.dis.r9) <- c("value","task")
  
  # Plot
  plot.dis.r9 <- ggplot(df.dis.r9, aes(task,value, fill=task)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                               "firebrick3", "dodgerblue3", "darkorange3",
                               "forestgreen")) + 
    scale_x_discrete(limits = rev(df.dis.r9$task)) + 
    ylim(0,0.9) +
    ggtitle("Intercoder reliability - Round 9 (disaggregated)") + ylab("Fleiss Kappa") +
    theme(legend.position = "none")  
  
  plot.dis.r9
  
  ggsave(here("graphs","fkappa_dis_round9.pdf"), width = 8, height = 4)
  
  
  
  
  
  
  
  
  
  ################################################################
  ################################################################
  # PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS
  
  # Check different 
  k.rel.r9.12 <- as.numeric(kappam.fleiss(relevant.r9[c(1,2)])[5])
  k.rel.r9.13 <- as.numeric(kappam.fleiss(relevant.r9[c(1,3)])[5])
  k.rel.r9.14 <- as.numeric(kappam.fleiss(relevant.r9[c(1,4)])[5])
  #k.rel.r9.15 <- as.numeric(kappam.fleiss(relevant.r9[c(1,5)])[5])
  
  k.rel.r9.21 <- as.numeric(kappam.fleiss(relevant.r9[c(2,1)])[5])
  k.rel.r9.23 <- as.numeric(kappam.fleiss(relevant.r9[c(2,3)])[5])
  k.rel.r9.24 <- as.numeric(kappam.fleiss(relevant.r9[c(2,4)])[5])
  #k.rel.r9.25 <- as.numeric(kappam.fleiss(relevant.r9[c(2,5)])[5])
  
  k.rel.r9.31 <- as.numeric(kappam.fleiss(relevant.r9[c(3,1)])[5])
  k.rel.r9.32 <- as.numeric(kappam.fleiss(relevant.r9[c(3,2)])[5])
  k.rel.r9.34 <- as.numeric(kappam.fleiss(relevant.r9[c(3,4)])[5])
  #k.rel.r9.35 <- as.numeric(kappam.fleiss(relevant.r9[c(3,5)])[5])
  
  k.rel.r9.41 <- as.numeric(kappam.fleiss(relevant.r9[c(4,1)])[5])
  k.rel.r9.42 <- as.numeric(kappam.fleiss(relevant.r9[c(4,2)])[5])
  k.rel.r9.43 <- as.numeric(kappam.fleiss(relevant.r9[c(4,3)])[5])
  #k.rel.r9.45 <- as.numeric(kappam.fleiss(relevant.r9[c(4,5)])[5])
  
  #k.rel.r9.51 <- as.numeric(kappam.fleiss(relevant.r9[c(5,1)])[5])
  #k.rel.r9.52 <- as.numeric(kappam.fleiss(relevant.r9[c(5,2)])[5])
  #k.rel.r9.53 <- as.numeric(kappam.fleiss(relevant.r9[c(5,3)])[5])
  #k.rel.r9.54 <- as.numeric(kappam.fleiss(relevant.r9[c(5,4)])[5])
  
  coder1.all.r9 <-c(1,k.rel.r9.12,k.rel.r9.13,k.rel.r9.14)
  coder2.all.r9 <-c(k.rel.r9.21,1,k.rel.r9.23,k.rel.r9.24)
  coder3.all.r9 <-c(k.rel.r9.31,k.rel.r9.32,1,k.rel.r9.34)
  coder4.all.r9 <-c(k.rel.r9.41,k.rel.r9.42,k.rel.r9.43,1)
  #coder5.all.r9 <-c(k.rel.r9.51,k.rel.r9.52,k.rel.r9.53,k.rel.r9.51,1)
  
  coders.all.r9 <- data.frame(coder1.all.r9,
                              coder2.all.r9,
                              coder3.all.r9,
                              coder4.all.r9
                              #,
                              #coder5.all.r9
  )
  
  names(coders.all.r9) <- c("Jennifer","Marcus","Praj","Geoffrey")
  row.names(coders.all.r9) <- c("Jennifer","Marcus","Praj","Geoffrey")
  
  
  
  coders.all.r9 <- as.matrix(coders.all.r9)
  
  
  
  # Heatmap with ggplot
  
  library(ggplot2)
  library(reshape2)
  
  
  coders.all.r9.m  <- melt(coders.all.r9)
  colnames(coders.all.r9.m) <- c("x", "y", "value")
  
  ggplot(coders.all.r9.m, aes(x = x, y = y, fill = value)) +
    geom_tile() + 
    scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
    ggtitle("Pairwise inter-coder agreement (round 9)") + 
    xlab("") + ylab("") +
    guides(fill=guide_legend(title="Fleiss\n Kappa"))
  
  
  ggsave(here("graphs","fkappa_pairwise_r9.pdf"), width = 5, height = 4)
  
  
  
}




################################################################
################################################################
# GET DATA ROUND 10
{
  
  ################################################
  # Get Round 10
  data.r10 <- read_csv(here("training_round_10",
                            "summary.csv"))
  
  # Explore data
  names(data.r10)
  
  # Extract sentence number and make it first column
  data.r10 <- data.r10 %>%
    mutate(num=as.numeric(str_extract_all(data.r10$plain_text,"(?<=No.).+(?=sentences)"))) %>%
    relocate(num)
  
  
  ################################################
  # Generate data by annotation task
  
  ##################
  # Task 1: Relevant
  relevant.r10 <- data.r10 %>%
    select('1_Relevant_1','1_Relevant_2','1_Relevant_4')
  colnames(relevant.r10) <- c('rater1','rater2','rater4')
  
  # export relevant
  relevant.r10.export <- data.r10 %>%
    select('num','sentences','1_Relevant_1','1_Relevant_2','1_Relevant_4')
  colnames(relevant.r10.export) <- c('story','sentences','rater1','rater2','rater4')
  write_csv(relevant.r10.export,here("training_round_10","relevant_r10.csv"))
  
  
  
  
  
  
  ##################
  # Task 2: PentaClass
  
  # Action 1
  penta.a1.r10 <- data.r10 %>% 
    select('Action1-PentaClass_1','Action1-PentaClass_2','Action1-PentaClass_4') %>%
    replace(is.na(.), "")
  colnames(penta.a1.r10) <- c('rater1','rater2','rater4')
  
  # Action 2
  penta.a2.r10 <- data.r10 %>% 
    select("Action2-PentaClass_1","Action2-PentaClass_2", "Action2-PentaClass_4") %>%
    replace(is.na(.), "")
  colnames(penta.a2.r10) <- c('rater1','rater2','rater4')
  
  # Action 3
  penta.a3.r10 <- data.r10 %>% 
    select("Action3-PentaClass_1", "Action3-PentaClass_2","Action3-PentaClass_4") %>%
    replace(is.na(.), "")
  colnames(penta.a3.r10) <- c('rater1','rater2','rater4')
  
  
  
  
  
  ##################
  # Task 3: CAMEO
  
  # Action 1
  cameo.a1.r10 <- data.r10 %>%
    select('Action1_CAMEO_1','Action1_CAMEO_2', 'Action1_CAMEO_4') %>%
    replace(is.na(.), "")
  colnames(cameo.a1.r10) <- c('rater1','rater2','rater4')
  
  # Action 2
  cameo.a2.r10 <- data.r10 %>%
    select('Action2_CAMEO_1','Action2_CAMEO_2', 'Action2_CAMEO_4') %>%
    replace(is.na(.), "")
  colnames(cameo.a2.r10) <- c('rater1','rater2','rater4')
  
  # Action 3
  cameo.a3.r10 <- data.r10 %>%
    select('Action3_CAMEO_1','Action3_CAMEO_2', 'Action3_CAMEO_4') %>%
    replace(is.na(.), "")
  colnames(cameo.a3.r10) <- c('rater1','rater2','rater4')
  
  
  
  
  
  ################################################################
  ################################################################
  # EXPORT ALL ANNOTATIONS
  
  # Explort annotations and sentences
  penta.a1.r10.sent <- data.r10 %>% select('num','sentences','Action1-PentaClass_1','Action1-PentaClass_2','Action1-PentaClass_4') %>% replace(is.na(.), "")
  penta.a2.r10.sent <- data.r10 %>% select('num','sentences','Action2-PentaClass_1','Action2-PentaClass_2','Action2-PentaClass_4') %>% replace(is.na(.), "")
  penta.a3.r10.sent <- data.r10 %>% select('num','sentences','Action3-PentaClass_1','Action3-PentaClass_2',,'Action3-PentaClass_4') %>% replace(is.na(.), "")
  cameo.a1.r10.sent <- data.r10 %>% select('num','sentences','Action1_CAMEO_1','Action1_CAMEO_2', 'Action1_CAMEO_4') %>% replace(is.na(.), "")
  cameo.a2.r10.sent <- data.r10 %>% select('num','sentences','Action2_CAMEO_1','Action2_CAMEO_2', 'Action2_CAMEO_4') %>% replace(is.na(.), "")
  cameo.a3.r10.sent <- data.r10 %>% select('num','sentences','Action3_CAMEO_1','Action3_CAMEO_2', 'Action3_CAMEO_4') %>% replace(is.na(.), "")
  
  # save three dataframes to a excel spreadsheet 
  openxlsx::write.xlsx(list("relevant" = relevant.r10.export, 
                            "penta1" = penta.a1.r10.sent,
                            "penta2" = penta.a2.r10.sent,
                            "penta3" = penta.a3.r10.sent,
                            "cameo1" = cameo.a1.r10.sent,
                            "cameo2" = cameo.a2.r10.sent,
                            "cameo3" = cameo.a3.r10.sent), 
                       file = here("training_round_10",
                                   glue("annotations.xlsx")))
  
  
  
  
  ################################################################
  ################################################################
  # INTERCODER RELIABILITY FLEISS KAPPA
  
  ##################
  # Eliminate coder 2 (Karina) permanently
  
  relevant.r10 <- relevant.r10[,c(1,2,3)]
  penta.a1.r10 <- penta.a1.r10[,c(1,2,3)]
  penta.a2.r10 <- penta.a2.r10[,c(1,2,3)]
  cameo.a1.r10 <- cameo.a1.r10[,c(1,2,3)]
  cameo.a2.r10 <- cameo.a2.r10[,c(1,2,3)]
  
  
  
  
  ##################
  # Task 1: Relevant
  k.rel.r10 <- as.numeric(kappam.fleiss(relevant.r10)[5])
  k.rel.r10
  
  
  
  ##################
  # Task 2: PentaClass
  
  #kappa2(penta.a1.r10[,c(1,4)], "unweighted")
  
  
  # PentaClass by actor
  k.penta.a1.r10 <- as.numeric(kappam.fleiss(penta.a1.r10, exact=TRUE)[5])
  k.penta.a2.r10 <- as.numeric(kappam.fleiss(penta.a2.r10, exact=TRUE)[5])
  k.penta.a3.r10 <- as.numeric(kappam.fleiss(penta.a3.r10, exact=TRUE)[5])
  #k.penta.a4.r10 <- as.numeric(kappam.fleiss(penta.a4.r10, exact=TRUE)[5])
  
  # Average 
  k.penta.r10 <- mean(c(k.penta.a1.r10,k.penta.a2.r10,k.penta.a3.r10))
  k.penta.r10
  
  
  
  
  ##################
  # Task 3: CAMEO
  
  # CAMEO by actor
  k.cameo.a1.r10 <- as.numeric(kappam.fleiss(cameo.a1.r10, exact=TRUE)[5])
  k.cameo.a2.r10 <- as.numeric(kappam.fleiss(cameo.a2.r10, exact=TRUE)[5])
  k.cameo.a3.r10 <- as.numeric(kappam.fleiss(cameo.a3.r10, exact=TRUE)[5])
  #k.cameo.a4.r10 <- as.numeric(kappam.fleiss(cameo.a4.r10, exact=TRUE)[5])
  
  # Average 
  k.cameo.r10 <- mean(c(k.cameo.a1.r10,k.cameo.a2.r10,k.cameo.a3.r10))
  k.cameo.r10
  
  
  
  
  # Summary of results
  k.rel.r10
  k.penta.r10
  k.cameo.r10
  
  
  
  
  
  
  ################################################################
  ################################################################
  # PLOT AGGREGATE FLEISS KAPPA
  
  # Generate data frame of aggregate results
  df.r10 <- as.data.frame(c(k.cameo.r10,k.penta.r10,k.rel.r10))
  tasks <- c("CAMEO","PentaClass","Relevant")
  df.r10 <- cbind(df.r10, tasks)
  colnames(df.r10) <- c("value","task")
  
  
  # Plot
  plot.r10 <- ggplot(df.r10, aes(task,value, fill=task)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("firebrick1", "firebrick3","forestgreen")) +
    scale_x_discrete(limits = rev(df.r10$task)) + 
    ylim(0,0.9) +
    ggtitle("Intercoder reliability - Round 10 (aggregated)") + ylab("Fleiss Kappa") +
    theme(legend.position = "none")
  
  plot.r10
  
  ggsave(here("graphs","fkappa_round10.pdf"), width = 8, height = 4)
  
  
  
  
  
  
  
  
  
  ################################################################
  ################################################################
  # PLOT DISAGGREGATED FLEISS KAPPA
  
  # Generate data frame of aggregate results
  df.dis.r10 <- as.data.frame(c(k.cameo.a3.r10,k.penta.a3.r10,
                                k.cameo.a2.r10,k.penta.a2.r10,
                                k.cameo.a1.r10,k.penta.a1.r10,
                                k.rel.r10))
  tasks <- c("CAMEO\n Action 3","PentaClass\n Action 3",
             "CAMEO\n Action 2","PentaClass\n Action 2",
             "CAMEO\n Action 1","PentaClass\n Action 1",
             "Relevant")
  df.dis.r10 <- cbind(df.dis.r10, tasks)
  colnames(df.dis.r10) <- c("value","task")
  
  # Plot
  plot.dis.r10 <- ggplot(df.dis.r10, aes(task,value, fill=task)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values=c("firebrick1", "dodgerblue", "darkorange",
                               "firebrick3", "dodgerblue3", "darkorange3",
                               "forestgreen")) + 
    scale_x_discrete(limits = rev(df.dis.r10$task)) + 
    ylim(0,0.9) +
    ggtitle("Intercoder reliability - Round 10 (disaggregated)") + ylab("Fleiss Kappa") +
    theme(legend.position = "none")  
  
  plot.dis.r10
  
  ggsave(here("graphs","fkappa_dis_round10.pdf"), width = 8, height = 4)
  
  
  
  
  
  
  
  
  
  ################################################################
  ################################################################
  # PLOT RELEVANCE FLEISS KAPPA BY PAIRS OF CODERS
  
  # Check different 
  k.rel.r10.12 <- as.numeric(kappam.fleiss(relevant.r10[c(1,2)])[5])
  k.rel.r10.13 <- as.numeric(kappam.fleiss(relevant.r10[c(1,3)])[5])
  #k.rel.r10.14 <- as.numeric(kappam.fleiss(relevant.r10[c(1,4)])[5])
  
  k.rel.r10.21 <- as.numeric(kappam.fleiss(relevant.r10[c(2,1)])[5])
  k.rel.r10.23 <- as.numeric(kappam.fleiss(relevant.r10[c(2,3)])[5])
  #k.rel.r10.24 <- as.numeric(kappam.fleiss(relevant.r10[c(2,4)])[5])
  
  k.rel.r10.31 <- as.numeric(kappam.fleiss(relevant.r10[c(3,1)])[5])
  k.rel.r10.32 <- as.numeric(kappam.fleiss(relevant.r10[c(3,2)])[5])
  #k.rel.r10.34 <- as.numeric(kappam.fleiss(relevant.r10[c(3,4)])[5])
  
  #k.rel.r10.41 <- as.numeric(kappam.fleiss(relevant.r10[c(4,1)])[5])
  #k.rel.r10.42 <- as.numeric(kappam.fleiss(relevant.r10[c(4,2)])[5])
  #k.rel.r10.43 <- as.numeric(kappam.fleiss(relevant.r10[c(4,3)])[5])
  
  
  
  coder1.all.r10 <-c(1,k.rel.r10.12,k.rel.r10.13)
  coder2.all.r10 <-c(k.rel.r10.21,1,k.rel.r10.23)
  coder3.all.r10 <-c(k.rel.r10.31,k.rel.r10.32,1)
  #coder4.all.r10 <-c(k.rel.r10.41,k.rel.r10.42,k.rel.r10.43,1)
  
  
  coders.all.r10 <- data.frame(coder1.all.r10,
                               coder2.all.r10,
                               coder3.all.r10
                               #,
                               #coder4.all.r10
  )
  
  names(coders.all.r10) <- c("Jennifer","Marcus","Geoffrey")
  row.names(coders.all.r10) <- c("Jennifer","Marcus","Geoffrey")
  
  
  
  coders.all.r10 <- as.matrix(coders.all.r10)
  
  
  
  # Heatmap with ggplot
  
  library(ggplot2)
  library(reshape2)
  
  
  coders.all.r10.m  <- melt(coders.all.r10)
  colnames(coders.all.r10.m) <- c("x", "y", "value")
  
  ggplot(coders.all.r10.m, aes(x = x, y = y, fill = value)) +
    geom_tile() + 
    scale_fill_gradient(low = "cyan1", high = "navyblue", na.value = NA, limits=c(0,1),breaks=c(0,.2,.4,.6,.8,1)) + 
    ggtitle("Pairwise inter-coder agreement (round 9)") + 
    xlab("") + ylab("") +
    guides(fill=guide_legend(title="Fleiss\n Kappa"))
  
  
  ggsave(here("graphs","fkappa_pairwise_r10.pdf"), width = 5, height = 4)
  
  
  
}




################################################################
################################################################
# PLOT AGGREGATE PERFORMANCE BY BY ROUNDS

# Create data frame
df.rounds <- data.frame(
  round = c("round 1","round 2","round 3","round 4","round 5","round 6", "round 7", "round 8", "round 9", "round 10"),
  relevant = c(k.rel.r1, k.rel.r2, k.rel.r3, k.rel.r4,k.rel.r5,k.rel.r6,k.rel.r7,k.rel.r8,k.rel.r9,k.rel.r10),
  penta = c(k.penta.r1, k.penta.r2, k.penta.r3, k.penta.r4, k.penta.r5, k.penta.r6,k.penta.r7,k.penta.r8,k.penta.r9,k.penta.r10),
  cameo = c(k.cameo.r1, k.cameo.r2, k.cameo.r3, k.cameo.r4, k.cameo.r5, k.cameo.r6,k.cameo.r7,k.cameo.r8,k.cameo.r9,k.cameo.r10),
  sample = c("Sample 1","Sample 1","Sample 1","Sample 1","Sample 1","Sample 1","Sample 1","Sample 2","Sample 2","Sample 2"))

# Set order of rounds
df.rounds$round <- factor(df.rounds$round, levels = c("round 1","round 2","round 3","round 4","round 5","round 6", "round 7", "round 8", "round 9", "round 10")) 
df.rounds$sample <- factor(df.rounds$sample)


# Plot rounds
plot.df.rounds <- ggplot(df.rounds, aes(round,relevant, fill=round)) +
  geom_bar(stat = "identity") +
  ylim(0,0.9) +
  ggtitle("Intercoder reliability - Relevance (by rounds)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none") 

plot.df.rounds


df.rounds.2 <- df.rounds %>% gather(key = "key", value="value", cameo:relevant )

# # Plot rounds
# plot.df.rounds.2 <- ggplot(df.rounds.2, aes(key,value, fill=round)) +
#   geom_bar(stat = "identity", width = 2.2,
#            position = position_dodge(width = 1.7)) +
#   ylim(0,0.9) +
#   scale_x_discrete(limits = rev(df.rounds.2$key)) + 
#   ggtitle("Intercoder reliability - by rounds") + ylab("Fleiss Kappa")  
# plot.df.rounds.2


# RGB colors https://www.rapidtables.com/web/color/RGB_Color.html

# Plot rounds
plot.df.rounds.2 <- ggplot(df.rounds.2, aes(round,value, fill=round)) +
  geom_bar(stat = "identity", width = 0.5,
           position = position_dodge(width = 2.7)) +
  ylim(0,1) +
  ggtitle("Intercoder reliability - by rounds") + ylab("Fleiss Kappa")  + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(~factor(key, levels=c("relevant","penta","cameo"))) + 
  scale_fill_manual(values=c("#FF9999","#F59095","#FF6666","#FF3333","#FF0000","#CC0000", "#990000",
                             "#6AA7E5","#5890F0","#3560C3")) 


plot.df.rounds.2

ggsave(here("graphs","fkappa_dis_rounds_1_10.pdf"), width = 8, height = 4)

ggsave(here("graphs","fkappa_dis_rounds_1_10_small.pdf"), width = 4, height = 2)


# End of script
