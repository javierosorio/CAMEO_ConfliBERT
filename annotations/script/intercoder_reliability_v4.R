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
# CONTET
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


##################
# Task 4: Source, Action, Target

# Extract entities
  {
  ###########
  # Entity 1
  
  # Select entities_1
  e1 <- data.r1 %>% select(num, entities_1) 
  
  # Delete brackets
  e1 <- e1 %>% mutate(gsub("\\[|\\]", "", entities_1))
  
  # Extract the content from parentheses
  e1 <- str_extract_all(e1$entities_1,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e1 <-plyr::ldply(e1, rbind)
  
  
  ###########
  # Entity 2
  
  # Select entities_2
  e2 <- data.r1 %>% select(entities_2) 
  
  # Delete brackets
  e2 <- e2 %>% mutate(gsub("\\[|\\]", "", entities_2))
  
  # Extract the content from parentheses
  e2 <- str_extract_all(e2$entities_2,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e2 <-plyr::ldply(e2, rbind)
  
  
  ###########
  # Entity 3
  
  # Select entities_3
  e3 <- data.r1 %>% select(entities_3) 
  
  # Delete brackets
  e3 <- e3 %>% mutate(gsub("\\[|\\]", "", entities_3))
  
  # Extract the content from parentheses
  e3 <- str_extract_all(e3$entities_3,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e3 <-plyr::ldply(e3, rbind)
  
  
  ###########
  # Entity 4
  
  # Select entities_4
  e4 <- data.r1 %>% select(entities_4) 
  
  # Delete brackets
  e4 <- e4 %>% mutate(gsub("\\[|\\]", "", entities_4))
  
  # Extract the content from parentheses
  e4 <- str_extract_all(e4$entities_4,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e4 <-plyr::ldply(e4, rbind)
  
  
  ###########
  # Entity 5
  
  # Select entities_5
  e5 <- data.r1 %>% select(entities_5) 
  
  # Delete brackets
  e5 <- e5 %>% mutate(gsub("\\[|\\]", "", entities_5))
  
  # Extract the content from parentheses
  e5 <- str_extract_all(e5$entities_5,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e5 <-plyr::ldply(e5, rbind)
  
  }
  # End extract entities



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




##################
# Entity similarity using the 'stringdist' package

# Set combinations of entities e1:e5
# e1e2,e1e3,e1e4,e1e5,
# e2e3,e2e4,e2e5,
# e3e4,e3e5
# e4e5

# Begin calculating similarity
  {
  ############
  # sim.e1e2
  sim.e1e2 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e2)){
    
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e2[,j])
    
      # Add new column
      new <- rep(i, nrow(sim.e1e2))                               # Create new column
      sim.e1e2[ , ncol(sim.e1e2) + 1] <- sim.score                # Append new column
      colnames(sim.e1e2)[ncol(sim.e1e2)] <- paste0("e1.", i,"e2.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e3
  sim.e1e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e3)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e3[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e3))                               # Create new column
      sim.e1e3[ , ncol(sim.e1e3) + 1] <- sim.score                # Append new column
      colnames(sim.e1e3)[ncol(sim.e1e3)] <- paste0("e1.", i,"e3.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e4
  sim.e1e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e4))                               # Create new column
      sim.e1e4[ , ncol(sim.e1e4) + 1] <- sim.score                # Append new column
      colnames(sim.e1e4)[ncol(sim.e1e4)] <- paste0("e1.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e5
  sim.e1e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e5))                               # Create new column
      sim.e1e5[ , ncol(sim.e1e5) + 1] <- sim.score                # Append new column
      colnames(sim.e1e5)[ncol(sim.e1e5)] <- paste0("e1.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e2e3
  sim.e2e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e3)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e3[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e3))                               # Create new column
      sim.e2e3[ , ncol(sim.e2e3) + 1] <- sim.score                # Append new column
      colnames(sim.e2e3)[ncol(sim.e2e3)] <- paste0("e2.", i,"e3.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e2e4
  sim.e2e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e4))                               # Create new column
      sim.e2e4[ , ncol(sim.e2e4) + 1] <- sim.score                # Append new column
      colnames(sim.e2e4)[ncol(sim.e2e4)] <- paste0("e2.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e2e5
  sim.e2e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e5))                               # Create new column
      sim.e2e5[ , ncol(sim.e2e5) + 1] <- sim.score                # Append new column
      colnames(sim.e2e5)[ncol(sim.e2e5)] <- paste0("e2.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e3e4
  sim.e3e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e3)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e3[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e3e4))                               # Create new column
      sim.e3e4[ , ncol(sim.e3e4) + 1] <- sim.score                # Append new column
      colnames(sim.e3e4)[ncol(sim.e3e4)] <- paste0("e3.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e3e5
  sim.e3e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e3)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e3[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e3e5))                               # Create new column
      sim.e3e5[ , ncol(sim.e3e5) + 1] <- sim.score                # Append new column
      colnames(sim.e3e5)[ncol(sim.e3e5)] <- paste0("e3.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e4e5
  sim.e4e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e4)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e4[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e4e5))                               # Create new column
      sim.e4e5[ , ncol(sim.e4e5) + 1] <- sim.score                # Append new column
      colnames(sim.e4e5)[ncol(sim.e4e5)] <- paste0("e4.", i,"e5.",j)  # Rename column name
    }
  }
  
  }
  # End calculating similarity



##################
# Round up similarity by 75%

# Begin rounding up
  {
  sim.e1e2 <-  sim.e1e2 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e3 <-  sim.e1e3 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e4 <-  sim.e1e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e5 <-  sim.e1e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e3 <-  sim.e2e3 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e4 <-  sim.e2e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e5 <-  sim.e2e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e3e4 <-  sim.e3e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e3e5 <-  sim.e3e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e4e5 <-  sim.e4e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  }
  # End rounding up
  


##################
# Database indicating number of matches

# Merge all similar entity databases
sim.full <- cbind(sim.e1e2, sim.e1e3)
sim.full <- cbind(sim.full, sim.e1e4)
sim.full <- cbind(sim.full, sim.e1e5)
sim.full <- cbind(sim.full, sim.e2e3)
sim.full <- cbind(sim.full, sim.e2e4)
sim.full <- cbind(sim.full, sim.e2e5)
sim.full <- cbind(sim.full, sim.e3e4)
sim.full <- cbind(sim.full, sim.e3e5)
sim.full <- cbind(sim.full, sim.e4e5)

# Assign column names
colnames(sim.full) <- c('sim.e1e2', 'sim.e1e3', 'sim.e1e4', 'sim.e1e5', 'sim.e2e3', 'sim.e2e4' ,'sim.e2e5' ,'sim.e3e4' ,'sim.e3e5' ,'sim.e4e5' )


##################
# how to compare them???









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






################################################################
################################################################
# GET DATA ROUND 2


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


##################
# Task 4: Source, Action, Target

# Extract entities
{
  ###########
  # Entity 1
  
  # Select entities_1
  e1 <- data.r2 %>% select(num, entities_1) 
  
  # Delete brackets
  e1 <- e1 %>% mutate(gsub("\\[|\\]", "", entities_1))
  
  # Extract the content from parentheses
  e1 <- str_extract_all(e1$entities_1,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e1 <-plyr::ldply(e1, rbind)
  
  
  ###########
  # Entity 2
  
  # Select entities_2
  e2 <- data.r2 %>% select(entities_2) 
  
  # Delete brackets
  e2 <- e2 %>% mutate(gsub("\\[|\\]", "", entities_2))
  
  # Extract the content from parentheses
  e2 <- str_extract_all(e2$entities_2,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e2 <-plyr::ldply(e2, rbind)
  
  
  ###########
  # Entity 3
  
  # Select entities_3
  e3 <- data.r2 %>% select(entities_3) 
  
  # Delete brackets
  e3 <- e3 %>% mutate(gsub("\\[|\\]", "", entities_3))
  
  # Extract the content from parentheses
  e3 <- str_extract_all(e3$entities_3,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e3 <-plyr::ldply(e3, rbind)
  
  
  ###########
  # Entity 4
  
  # Select entities_4
  e4 <- data.r2 %>% select(entities_4) 
  
  # Delete brackets
  e4 <- e4 %>% mutate(gsub("\\[|\\]", "", entities_4))
  
  # Extract the content from parentheses
  e4 <- str_extract_all(e4$entities_4,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e4 <-plyr::ldply(e4, rbind)
  
  
  ###########
  # Entity 5
  
  # Select entities_5
  e5 <- data.r2 %>% select(entities_5) 
  
  # Delete brackets
  e5 <- e5 %>% mutate(gsub("\\[|\\]", "", entities_5))
  
  # Extract the content from parentheses
  e5 <- str_extract_all(e5$entities_5,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e5 <-plyr::ldply(e5, rbind)
  
}
# End extract entities



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




##################
# Entity similarity using the 'stringdist' package

# Set combinations of entities e1:e5
# e1e2,e1e3,e1e4,e1e5,
# e2e3,e2e4,e2e5,
# e3e4,e3e5
# e4e5

# Begin calculating similarity
{
  ############
  # sim.e1e2
  sim.e1e2 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e2)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e2[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e2))                               # Create new column
      sim.e1e2[ , ncol(sim.e1e2) + 1] <- sim.score                # Append new column
      colnames(sim.e1e2)[ncol(sim.e1e2)] <- paste0("e1.", i,"e2.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e3
  sim.e1e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e3)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e3[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e3))                               # Create new column
      sim.e1e3[ , ncol(sim.e1e3) + 1] <- sim.score                # Append new column
      colnames(sim.e1e3)[ncol(sim.e1e3)] <- paste0("e1.", i,"e3.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e4
  sim.e1e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e4))                               # Create new column
      sim.e1e4[ , ncol(sim.e1e4) + 1] <- sim.score                # Append new column
      colnames(sim.e1e4)[ncol(sim.e1e4)] <- paste0("e1.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e5
  sim.e1e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e5))                               # Create new column
      sim.e1e5[ , ncol(sim.e1e5) + 1] <- sim.score                # Append new column
      colnames(sim.e1e5)[ncol(sim.e1e5)] <- paste0("e1.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e2e3
  sim.e2e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e3)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e3[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e3))                               # Create new column
      sim.e2e3[ , ncol(sim.e2e3) + 1] <- sim.score                # Append new column
      colnames(sim.e2e3)[ncol(sim.e2e3)] <- paste0("e2.", i,"e3.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e2e4
  sim.e2e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e4))                               # Create new column
      sim.e2e4[ , ncol(sim.e2e4) + 1] <- sim.score                # Append new column
      colnames(sim.e2e4)[ncol(sim.e2e4)] <- paste0("e2.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e2e5
  sim.e2e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e5))                               # Create new column
      sim.e2e5[ , ncol(sim.e2e5) + 1] <- sim.score                # Append new column
      colnames(sim.e2e5)[ncol(sim.e2e5)] <- paste0("e2.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e3e4
  sim.e3e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e3)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e3[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e3e4))                               # Create new column
      sim.e3e4[ , ncol(sim.e3e4) + 1] <- sim.score                # Append new column
      colnames(sim.e3e4)[ncol(sim.e3e4)] <- paste0("e3.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e3e5
  sim.e3e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e3)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e3[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e3e5))                               # Create new column
      sim.e3e5[ , ncol(sim.e3e5) + 1] <- sim.score                # Append new column
      colnames(sim.e3e5)[ncol(sim.e3e5)] <- paste0("e3.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e4e5
  sim.e4e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e4)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e4[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e4e5))                               # Create new column
      sim.e4e5[ , ncol(sim.e4e5) + 1] <- sim.score                # Append new column
      colnames(sim.e4e5)[ncol(sim.e4e5)] <- paste0("e4.", i,"e5.",j)  # Rename column name
    }
  }
  
}
# End calculating similarity



##################
# Round up similarity by 75%

# Begin rounding up
{
  sim.e1e2 <-  sim.e1e2 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e3 <-  sim.e1e3 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e4 <-  sim.e1e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e5 <-  sim.e1e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e3 <-  sim.e2e3 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e4 <-  sim.e2e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e5 <-  sim.e2e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e3e4 <-  sim.e3e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e3e5 <-  sim.e3e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e4e5 <-  sim.e4e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
}
# End rounding up



##################
# Database indicating number of matches

# Merge all similar entity databases
sim.full <- cbind(sim.e1e2, sim.e1e3)
sim.full <- cbind(sim.full, sim.e1e4)
sim.full <- cbind(sim.full, sim.e1e5)
sim.full <- cbind(sim.full, sim.e2e3)
sim.full <- cbind(sim.full, sim.e2e4)
sim.full <- cbind(sim.full, sim.e2e5)
sim.full <- cbind(sim.full, sim.e3e4)
sim.full <- cbind(sim.full, sim.e3e5)
sim.full <- cbind(sim.full, sim.e4e5)

# Assign column names
colnames(sim.full) <- c('sim.e1e2', 'sim.e1e3', 'sim.e1e4', 'sim.e1e5', 'sim.e2e3', 'sim.e2e4' ,'sim.e2e5' ,'sim.e3e4' ,'sim.e3e5' ,'sim.e4e5' )


##################
# how to compare them???









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





################################################################
################################################################
# GET DATA ROUND 3


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




##################
# Task 4: Source, Action, Target

# Extract entities
{
  ###########
  # Entity 1
  
  # Select entities_1
  e1 <- data.r3 %>% select(num, entities_1) 
  
  # Delete brackets
  e1 <- e1 %>% mutate(gsub("\\[|\\]", "", entities_1))
  
  # Extract the content from parentheses
  e1 <- str_extract_all(e1$entities_1,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e1 <-plyr::ldply(e1, rbind)
  
  
  ###########
  # Entity 2
  
  # Select entities_2
  e2 <- data.r3 %>% select(entities_2) 
  
  # Delete brackets
  e2 <- e2 %>% mutate(gsub("\\[|\\]", "", entities_2))
  
  # Extract the content from parentheses
  e2 <- str_extract_all(e2$entities_2,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e2 <-plyr::ldply(e2, rbind)
  
  
  ###########
  # Entity 3
  
  # Select entities_3
  e3 <- data.r3 %>% select(entities_3) 
  
  # Delete brackets
  e3 <- e3 %>% mutate(gsub("\\[|\\]", "", entities_3))
  
  # Extract the content from parentheses
  e3 <- str_extract_all(e3$entities_3,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e3 <-plyr::ldply(e3, rbind)
  
  
  ###########
  # Entity 4
  
  # Select entities_4
  e4 <- data.r3 %>% select(entities_4) 
  
  # Delete brackets
  e4 <- e4 %>% mutate(gsub("\\[|\\]", "", entities_4))
  
  # Extract the content from parentheses
  e4 <- str_extract_all(e4$entities_4,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e4 <-plyr::ldply(e4, rbind)
  
  
  ###########
  # Entity 5
  
  # Select entities_5
  e5 <- data.r3 %>% select(entities_5) 
  
  # Delete brackets
  e5 <- e5 %>% mutate(gsub("\\[|\\]", "", entities_5))
  
  # Extract the content from parentheses
  e5 <- str_extract_all(e5$entities_5,  "(?<=\\().+?(?=\\))")
  
  # Convert list to data frame with uneven length
  e5 <-plyr::ldply(e5, rbind)
  
}
# End extract entities



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




##################
# Entity similarity using the 'stringdist' package

# Set combinations of entities e1:e5
# e1e2,e1e3,e1e4,e1e5,
# e2e3,e2e4,e2e5,
# e3e4,e3e5
# e4e5

# Begin calculating similarity
{
  ############
  # sim.e1e2
  sim.e1e2 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e2)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e2[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e2))                               # Create new column
      sim.e1e2[ , ncol(sim.e1e2) + 1] <- sim.score                # Append new column
      colnames(sim.e1e2)[ncol(sim.e1e2)] <- paste0("e1.", i,"e2.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e3
  sim.e1e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e3)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e3[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e3))                               # Create new column
      sim.e1e3[ , ncol(sim.e1e3) + 1] <- sim.score                # Append new column
      colnames(sim.e1e3)[ncol(sim.e1e3)] <- paste0("e1.", i,"e3.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e4
  sim.e1e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e4))                               # Create new column
      sim.e1e4[ , ncol(sim.e1e4) + 1] <- sim.score                # Append new column
      colnames(sim.e1e4)[ncol(sim.e1e4)] <- paste0("e1.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e1e5
  sim.e1e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e1[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e1e5))                               # Create new column
      sim.e1e5[ , ncol(sim.e1e5) + 1] <- sim.score                # Append new column
      colnames(sim.e1e5)[ncol(sim.e1e5)] <- paste0("e1.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e2e3
  sim.e2e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e3)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e3[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e3))                               # Create new column
      sim.e2e3[ , ncol(sim.e2e3) + 1] <- sim.score                # Append new column
      colnames(sim.e2e3)[ncol(sim.e2e3)] <- paste0("e2.", i,"e3.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e2e4
  sim.e2e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e4))                               # Create new column
      sim.e2e4[ , ncol(sim.e2e4) + 1] <- sim.score                # Append new column
      colnames(sim.e2e4)[ncol(sim.e2e4)] <- paste0("e2.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e2e5
  sim.e2e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e2)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e2[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e2e5))                               # Create new column
      sim.e2e5[ , ncol(sim.e2e5) + 1] <- sim.score                # Append new column
      colnames(sim.e2e5)[ncol(sim.e2e5)] <- paste0("e2.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e3e4
  sim.e3e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e3)){
    for(j in 1:length(e4)){
      
      # Calculate similarity
      sim.score <- stringsim(e3[,i], e4[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e3e4))                               # Create new column
      sim.e3e4[ , ncol(sim.e3e4) + 1] <- sim.score                # Append new column
      colnames(sim.e3e4)[ncol(sim.e3e4)] <- paste0("e3.", i,"e4.",j)  # Rename column name
    }
  }
  
  ############
  # sim.e3e5
  sim.e3e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e3)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e3[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e3e5))                               # Create new column
      sim.e3e5[ , ncol(sim.e3e5) + 1] <- sim.score                # Append new column
      colnames(sim.e3e5)[ncol(sim.e3e5)] <- paste0("e3.", i,"e5.",j)  # Rename column name
    }
  }
  
  
  ############
  # sim.e4e5
  sim.e4e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
  
  for(i in 1:length(e4)){
    for(j in 1:length(e5)){
      
      # Calculate similarity
      sim.score <- stringsim(e4[,i], e5[,j])
      
      # Add new column
      new <- rep(i, nrow(sim.e4e5))                               # Create new column
      sim.e4e5[ , ncol(sim.e4e5) + 1] <- sim.score                # Append new column
      colnames(sim.e4e5)[ncol(sim.e4e5)] <- paste0("e4.", i,"e5.",j)  # Rename column name
    }
  }
  
}
# End calculating similarity



##################
# Round up similarity by 75%

# Begin rounding up
{
  sim.e1e2 <-  sim.e1e2 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e3 <-  sim.e1e3 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e4 <-  sim.e1e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e1e5 <-  sim.e1e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e3 <-  sim.e2e3 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e4 <-  sim.e2e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e2e5 <-  sim.e2e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e3e4 <-  sim.e3e4 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e3e5 <-  sim.e3e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
  sim.e4e5 <-  sim.e4e5 %>% 
    mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
    rowSums(., na.rm = TRUE) %>% 
    as.data.frame()
  
}
# End rounding up



##################
# Database indicating number of matches

# Merge all similar entity databases
sim.full <- cbind(sim.e1e2, sim.e1e3)
sim.full <- cbind(sim.full, sim.e1e4)
sim.full <- cbind(sim.full, sim.e1e5)
sim.full <- cbind(sim.full, sim.e2e3)
sim.full <- cbind(sim.full, sim.e2e4)
sim.full <- cbind(sim.full, sim.e2e5)
sim.full <- cbind(sim.full, sim.e3e4)
sim.full <- cbind(sim.full, sim.e3e5)
sim.full <- cbind(sim.full, sim.e4e5)

# Assign column names
colnames(sim.full) <- c('sim.e1e2', 'sim.e1e3', 'sim.e1e4', 'sim.e1e5', 'sim.e2e3', 'sim.e2e4' ,'sim.e2e5' ,'sim.e3e4' ,'sim.e3e5' ,'sim.e4e5' )


##################
# how to compare them???









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







################################################################
################################################################
# GET DATA ROUND 4


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




##################
# Task 4: Source, Action, Target

# # Extract entities
# {
#   ###########
#   # Entity 1
#   
#   # Select entities_1
#   e1 <- data.r4 %>% select(num, entities_1) 
#   
#   # Delete brackets
#   e1 <- e1 %>% mutate(gsub("\\[|\\]", "", entities_1))
#   
#   # Extract the content from parentheses
#   e1 <- str_extract_all(e1$entities_1,  "(?<=\\().+?(?=\\))")
#   
#   # Convert list to data frame with uneven length
#   e1 <-plyr::ldply(e1, rbind)
#   
#   
#   ###########
#   # Entity 2
#   
#   # Select entities_2
#   e2 <- data.r4 %>% select(entities_2) 
#   
#   # Delete brackets
#   e2 <- e2 %>% mutate(gsub("\\[|\\]", "", entities_2))
#   
#   # Extract the content from parentheses
#   e2 <- str_extract_all(e2$entities_2,  "(?<=\\().+?(?=\\))")
#   
#   # Convert list to data frame with uneven length
#   e2 <-plyr::ldply(e2, rbind)
#   
#   
#   ###########
#   # Entity 3
#   
#   # Select entities_3
#   e3 <- data.r4 %>% select(entities_3) 
#   
#   # Delete brackets
#   e3 <- e3 %>% mutate(gsub("\\[|\\]", "", entities_3))
#   
#   # Extract the content from parentheses
#   e3 <- str_extract_all(e3$entities_3,  "(?<=\\().+?(?=\\))")
#   
#   # Convert list to data frame with uneven length
#   e3 <-plyr::ldply(e3, rbind)
#   
#   
#   ###########
#   # Entity 4
#   
#   # Select entities_4
#   e4 <- data.r4 %>% select(entities_4) 
#   
#   # Delete brackets
#   e4 <- e4 %>% mutate(gsub("\\[|\\]", "", entities_4))
#   
#   # Extract the content from parentheses
#   e4 <- str_extract_all(e4$entities_4,  "(?<=\\().+?(?=\\))")
#   
#   # Convert list to data frame with uneven length
#   e4 <-plyr::ldply(e4, rbind)
#   
#   
#   ###########
#   # Entity 5
#   
#   # Select entities_5
#   e5 <- data.r4 %>% select(entities_5) 
#   
#   # Delete brackets
#   e5 <- e5 %>% mutate(gsub("\\[|\\]", "", entities_5))
#   
#   # Extract the content from parentheses
#   e5 <- str_extract_all(e5$entities_5,  "(?<=\\().+?(?=\\))")
#   
#   # Convert list to data frame with uneven length
#   e5 <-plyr::ldply(e5, rbind)
#   
# }
# # End extract entities
# 


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




##################
# Entity similarity using the 'stringdist' package

# Set combinations of entities e1:e5
# e1e2,e1e3,e1e4,e1e5,
# e2e3,e2e4,e2e5,
# e3e4,e3e5
# e4e5

# # Begin calculating similarity
# {
#   ############
#   # sim.e1e2
#   sim.e1e2 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e1)){
#     for(j in 1:length(e2)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e1[,i], e2[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e1e2))                               # Create new column
#       sim.e1e2[ , ncol(sim.e1e2) + 1] <- sim.score                # Append new column
#       colnames(sim.e1e2)[ncol(sim.e1e2)] <- paste0("e1.", i,"e2.",j)  # Rename column name
#     }
#   }
#   
#   ############
#   # sim.e1e3
#   sim.e1e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e1)){
#     for(j in 1:length(e3)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e1[,i], e3[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e1e3))                               # Create new column
#       sim.e1e3[ , ncol(sim.e1e3) + 1] <- sim.score                # Append new column
#       colnames(sim.e1e3)[ncol(sim.e1e3)] <- paste0("e1.", i,"e3.",j)  # Rename column name
#     }
#   }
#   
#   ############
#   # sim.e1e4
#   sim.e1e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e1)){
#     for(j in 1:length(e4)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e1[,i], e4[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e1e4))                               # Create new column
#       sim.e1e4[ , ncol(sim.e1e4) + 1] <- sim.score                # Append new column
#       colnames(sim.e1e4)[ncol(sim.e1e4)] <- paste0("e1.", i,"e4.",j)  # Rename column name
#     }
#   }
#   
#   ############
#   # sim.e1e5
#   sim.e1e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e1)){
#     for(j in 1:length(e5)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e1[,i], e5[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e1e5))                               # Create new column
#       sim.e1e5[ , ncol(sim.e1e5) + 1] <- sim.score                # Append new column
#       colnames(sim.e1e5)[ncol(sim.e1e5)] <- paste0("e1.", i,"e5.",j)  # Rename column name
#     }
#   }
#   
#   
#   ############
#   # sim.e2e3
#   sim.e2e3 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e2)){
#     for(j in 1:length(e3)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e2[,i], e3[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e2e3))                               # Create new column
#       sim.e2e3[ , ncol(sim.e2e3) + 1] <- sim.score                # Append new column
#       colnames(sim.e2e3)[ncol(sim.e2e3)] <- paste0("e2.", i,"e3.",j)  # Rename column name
#     }
#   }
#   
#   ############
#   # sim.e2e4
#   sim.e2e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e2)){
#     for(j in 1:length(e4)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e2[,i], e4[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e2e4))                               # Create new column
#       sim.e2e4[ , ncol(sim.e2e4) + 1] <- sim.score                # Append new column
#       colnames(sim.e2e4)[ncol(sim.e2e4)] <- paste0("e2.", i,"e4.",j)  # Rename column name
#     }
#   }
#   
#   ############
#   # sim.e2e5
#   sim.e2e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e2)){
#     for(j in 1:length(e5)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e2[,i], e5[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e2e5))                               # Create new column
#       sim.e2e5[ , ncol(sim.e2e5) + 1] <- sim.score                # Append new column
#       colnames(sim.e2e5)[ncol(sim.e2e5)] <- paste0("e2.", i,"e5.",j)  # Rename column name
#     }
#   }
#   
#   
#   ############
#   # sim.e3e4
#   sim.e3e4 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e3)){
#     for(j in 1:length(e4)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e3[,i], e4[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e3e4))                               # Create new column
#       sim.e3e4[ , ncol(sim.e3e4) + 1] <- sim.score                # Append new column
#       colnames(sim.e3e4)[ncol(sim.e3e4)] <- paste0("e3.", i,"e4.",j)  # Rename column name
#     }
#   }
#   
#   ############
#   # sim.e3e5
#   sim.e3e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e3)){
#     for(j in 1:length(e5)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e3[,i], e5[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e3e5))                               # Create new column
#       sim.e3e5[ , ncol(sim.e3e5) + 1] <- sim.score                # Append new column
#       colnames(sim.e3e5)[ncol(sim.e3e5)] <- paste0("e3.", i,"e5.",j)  # Rename column name
#     }
#   }
#   
#   
#   ############
#   # sim.e4e5
#   sim.e4e5 <- data.frame(matrix("", ncol = 0, nrow = 100))  
#   
#   for(i in 1:length(e4)){
#     for(j in 1:length(e5)){
#       
#       # Calculate similarity
#       sim.score <- stringsim(e4[,i], e5[,j])
#       
#       # Add new column
#       new <- rep(i, nrow(sim.e4e5))                               # Create new column
#       sim.e4e5[ , ncol(sim.e4e5) + 1] <- sim.score                # Append new column
#       colnames(sim.e4e5)[ncol(sim.e4e5)] <- paste0("e4.", i,"e5.",j)  # Rename column name
#     }
#   }
#   
# }
# # End calculating similarity



##################
# Round up similarity by 75%

# # Begin rounding up
# {
#   sim.e1e2 <-  sim.e1e2 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e1e3 <-  sim.e1e3 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e1e4 <-  sim.e1e4 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e1e5 <-  sim.e1e5 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e2e3 <-  sim.e2e3 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e2e4 <-  sim.e2e4 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e2e5 <-  sim.e2e5 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e3e4 <-  sim.e3e4 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e3e5 <-  sim.e3e5 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
#   sim.e4e5 <-  sim.e4e5 %>% 
#     mutate_all(funs(case_when( . >=0.75 ~ 1, . < 0.75 ~ NA_real_, TRUE ~ .))) %>% 
#     rowSums(., na.rm = TRUE) %>% 
#     as.data.frame()
#   
# }
# # End rounding up



##################
# Database indicating number of matches

# # Merge all similar entity databases
# sim.full <- cbind(sim.e1e2, sim.e1e3)
# sim.full <- cbind(sim.full, sim.e1e4)
# sim.full <- cbind(sim.full, sim.e1e5)
# sim.full <- cbind(sim.full, sim.e2e3)
# sim.full <- cbind(sim.full, sim.e2e4)
# sim.full <- cbind(sim.full, sim.e2e5)
# sim.full <- cbind(sim.full, sim.e3e4)
# sim.full <- cbind(sim.full, sim.e3e5)
# sim.full <- cbind(sim.full, sim.e4e5)
# 
# # Assign column names
# colnames(sim.full) <- c('sim.e1e2', 'sim.e1e3', 'sim.e1e4', 'sim.e1e5', 'sim.e2e3', 'sim.e2e4' ,'sim.e2e5' ,'sim.e3e4' ,'sim.e3e5' ,'sim.e4e5' )


##################
# how to compare them???









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
  ylim(0,0.8) +
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


# Basic image
image(1:nrow(coders.all.r4),1:ncol(coders.all.r4), coders.all.r4, axes=F, ylab="", xlab="")
text(1:ncol(coders.all.r4), labels = rownames(coders.all.r4), xpd = TRUE)
axis(1, 1:nrow(coders.all.r4), colnames(coders.all.r4), las=1)
axis(2, 1:nrow(coders.all.r4), colnames(coders.all.r4), las=1)


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





################################################################
################################################################
# PLOT AGGREGATE PERFORMANCE BY BY ROUNDS


df.rounds <- data.frame(
  round = c("round 1","round 2","round 3","round 4"),
  relevant = c(k.rel.r1, k.rel.r2, k.rel.r3, k.rel.r4),
  penta = c(k.penta.r1, k.penta.r2, k.penta.r3, k.penta.r4),
  cameo = c(k.cameo.r1, k.cameo.r2, k.cameo.r3, k.cameo.r4))


# Plot rounds
plot.df.rounds <- ggplot(df.rounds, aes(round,relevant, fill=round)) +
  geom_bar(stat = "identity") +
  ylim(0,0.8) +
  ggtitle("Intercoder reliability - Relevance (by rounds)") + ylab("Fleiss Kappa") +
  theme(legend.position = "none")  

plot.df.rounds


df.rounds.2 <- df.rounds %>% gather(key = "key", value="value", cameo:relevant )

# Plot rounds
plot.df.rounds.2 <- ggplot(df.rounds.2, aes(key,value, fill=round)) +
  geom_bar(stat = "identity", width = 1.5,
           position = position_dodge(width = 2.5)) +
  ylim(0,0.8) +
  scale_x_discrete(limits = rev(df.rounds.2$key)) + 
  ggtitle("Intercoder reliability - by rounds") + ylab("Fleiss Kappa") 

plot.df.rounds.2

ggsave(here("graphs","fkappa_dis_rounds_1_4.pdf"), width = 8, height = 4)




# End of script