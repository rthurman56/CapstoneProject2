#read data
studies <- read.csv('/Users/ClairePeterson5/Desktop/YEAR THREE/Spring 2019/STAT 190/Project 2/studies.csv')
brief_summaries <- read.csv('/Users/ClairePeterson5/Desktop/YEAR THREE/Spring 2019/STAT 190/Project 2/brief_summaries.csv')
designs <- read.csv('/Users/ClairePeterson5/Desktop/YEAR THREE/Spring 2019/STAT 190/Project 2/designs.csv')
interventions <- read.csv('/Users/ClairePeterson5/Desktop/YEAR THREE/Spring 2019/STAT 190/Project 2/interventions.csv')

# summary of intervention type
summary(interventions$x)

# summary of trial status
summary(studies$overall_status)


#merging studies and brief summaries
study_summary <- merge(studies, brief_summaries, by = 'nct_id', all.x = T, all.y = T)
#merging study_summary with designs
study_summary_designs <- merge(study_summary, designs, by ='nct_id', all.x = T, all.y = T)
#merging study_summary_designs with interventions
study_summary_designs_interventions <- merge(study_summary_designs, interventions, by = 'nct_id', all.x = T, all.y = T)

# Removing X.x and X.y as these were just row identifiers for the respective datasets prior to the merge
# renaming the dataset to current_data 
current_data <- subset(study_summary_designs_interventions, select = -c(10,14))

# renaming the column named 'x' to reflect its decription (represents intervention type)
colnames(current_data)[colnames(current_data)=="x"] <- "intervention_type"

# histogram of enrollment -- extremely right skewed
hist(study_summary_designs_interventions$enrollment, breaks = )

# creating column indicating whether or not the trial was terminated
# terminated trial = 1
# completed trial = 0
current_data$status_bin <- 1 #setting it always equal to 1
current_data$status_bin[current_data$overall_status == 'Completed'] <- 0 #when trial was completed, set to 0


m1 <- lm(status_bin ~ phase + enrollment + has_dmc +  allocation + primary_purpose + intervention_model + intervention_type, 
         data = current_data)
summary(m1)


temp <- current_data[which(current_data$enrollment >=1000 & current_data$enrollment < Inf ),]
hist(temp$enrollment)
# 1st: 0-21,     26,899
# 2nd: 22-43,    27,459
# 3rd: 44-80,    26,049
# 4th: 81-199,   26,686
# 5th: 200-999,  26,651
# 6th: 1000-end,  7,947

# create levels of enrollment
current_data$enrollment_level <- cut(current_data$enrollment, c(-1,21,43,80,199,999,67128927))

