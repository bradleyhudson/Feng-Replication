# Replication analysis Feng et al.
# 16th September 2020

# Assumes individual csv-files for each participant in folder 'os_feng'

# Currently stuff is in: setwd("/Users/margrietgroen/OneDrive - Lancaster University/mg_work/mg_work_1920/mg_work_1920_teaching/mg_work_1920_teaching_ug/1920_PSYC304/1920_data")

library(lme4)
library(languageR)
library(psych)
library(multcomp)
library(LMERConvenienceFunctions)
library(tidyverse)
library(readbulk)
library(DescTools)

feng <- read_opensesame(directory = "os_feng") #Read in all log-files for reading comprehension task

feng_clean <- feng %>% #Select relevant columns, renaming some in the process
  select(participant = subject_nr,
         logfile,
         opensesame_version,
         datetime,
         booklet_assign,
         condition,
         conditionN = Condition_Num,
         passage,
         line,
         line_text,
         probe,
         comprehension_q,
         correct_response,
         response,
         correct,
         response_time)

feng_rt <- feng_clean %>%
  filter(probe == 'y' & response == 'space') # response_time = RT

check_p1 <- feng_rt %>%
  filter(participant == '1') # select participant

length(check_p1$participant)

check_p2 <- feng_rt %>%
  filter(participant == '2')

length(check_p2$participant)

check_p3 <- feng_rt %>%
  filter(participant == '3')

length(check_p3$participant)
  
check_p4 <- feng_rt %>%
  filter(participant == '4')

length(check_p4$participant)

check_p5 <- feng_rt %>%
  filter(participant == '5')

length(check_p5$participant)

check_p6 <- feng_rt %>%
  filter(participant == '6')

length(check_p6$participant)

check_p7 <- feng_rt %>%
  filter(participant == '7')

length(check_p7$participant)

check_p8 <- feng_rt %>%
  filter(participant == '8')

length(check_p8$participant)

check_p9 <- feng_rt %>%
  filter(participant == '9')

length(check_p9$participant)

check_p10 <- feng_rt %>%
  filter(participant == '10')

length(check_p10$participant)

check_p11 <- feng_rt %>%
  filter(participant == '11')

length(check_p11$participant)

check_p12 <- feng_rt %>%
  filter(participant == '12')

length(check_p12$participant)

check_p13 <- feng_rt %>%
  filter(participant == '13')

length(check_p13$participant)

check_p14 <- feng_rt %>%
  filter(participant == '14')

length(check_p14$participant)

check_p15 <- feng_rt %>%
  filter(participant == '15')

length(check_p15$participant)

check_p16 <- feng_rt %>%
  filter(participant == '16')

length(check_p16$participant)

check_p17 <- feng_rt %>%
  filter(participant == '17')

length(check_p17$participant)

check_p18 <- feng_rt %>%
  filter(participant == '18')

length(check_p18$participant)

check_p19 <- feng_rt %>%
  filter(participant == '19')

length(check_p19$participant)

check_p20 <- feng_rt %>%
  filter(participant == '20')

length(check_p20$participant)

check_p21 <- feng_rt %>%
  filter(participant == '21')

length(check_p21$participant)

check_p22 <- feng_rt %>%
  filter(participant == '22')

length(check_p22$participant)

check_p23 <- feng_rt %>%
  filter(participant == '23')

length(check_p23$participant)

check_p24 <- feng_rt %>%
  filter(participant == '24')

length(check_p24$participant)

check_p25 <- feng_rt %>%
  filter(participant == '25')

length(check_p25$participant)

check_p26 <- feng_rt %>%
  filter(participant == '26')

length(check_p26$participant)

check_p27 <- feng_rt %>%
  filter(participant == '27')

length(check_p27$participant)

check_p28 <- feng_rt %>%
  filter(participant == '28')

length(check_p28$participant)

check_p29 <- feng_rt %>%
  filter(participant == '29')

length(check_p29$participant)

check_p30 <- feng_rt %>%
  filter(participant == '30')

length(check_p30$participant)

check_p31 <- feng_rt %>%
  filter(participant == '31')

length(check_p31$participant)

check_p32 <- feng_rt %>%
  filter(participant == '32')

length(check_p32$participant)

check_p33 <- feng_rt %>%
  filter(participant == '33')

length(check_p33$participant)

check_p34 <- feng_rt %>%
  filter(participant == '34')

length(check_p34$participant)

check_p35 <- feng_rt %>%
  filter(participant == '35')

length(check_p35$participant)

check_p36 <- feng_rt %>%
  filter(participant == '36')

length(check_p36$participant)

check_p37 <- feng_rt %>%
  filter(participant == '37')

length(check_p37$participant)

check_p38 <- feng_rt %>%
  filter(participant == '38')

length(check_p38$participant)

check_p39 <- feng_rt %>%
  filter(participant == '39')

length(check_p39$participant)

check_p40 <- feng_rt %>%
  filter(participant == '40')

length(check_p40$participant)

feng_mw <- feng_clean %>%
  filter(probe == "y", response != "space") %>%
  select(-comprehension_q, -correct_response, -correct, -response_time) %>%
  mutate(probe_N = dplyr::recode(probe,
                                 "y" = 1,
                                 "n" = 0),
         MW = dplyr::recode(response,
                                    "y" = 1,
                                    "n" = 0)
  )

feng_comp <- feng_clean %>%
  filter(comprehension_q == 'y') %>% # to keep rows with responses to comprehension questions
  filter(passage == 1 & line != "5") # to keep rows with probe-specific comprehension questions - passage 1 
  filter(passage == 2 & line != "3")
  filter(passage == 2 & line != "4")
  filter(passage == 3 & line != "3")
  filter(passage == 3 & line != "4")
  filter(passage == 5 & line != "3")
  filter(passage == 6 & line != "3") # Num_probs = 5 and number of Qs = 4. Numbers match for other passages.
  filter(passage == 7 & line != "1")
  filter(passage == 7 & line != "4")
  filter(passage == 8 & line != "") # Need to work out whether it's 1 or 4 to be removed.
  

#group <-  rep(c("Native Signers","Controls"), each = 8400)
#fragment_ID <-  rep(c("Overlapping"), times = 16800)
#fragment_UNR <- rep(c("Unrelated"), times = 16800)
vec_Sen_Num <- rep(c(1:7,1:2,1:2,1:4,1:3,1:5,1:2,1:3), times = 92)
vec_Num_Probs <- rep(c(7,7,7,7,7,7,7,2,2,2,2,4,4,4,4,3,3,3,5,5,5,5,5,2,2,3,3,3), times = 92) # 5,5,5,5,5?



#feng_clean <- feng %>%                   #Put all relevant columns in one datafile to end up with df that is same as Feng et al.
  select(Subject = subject_nr,          #
         Passages = passage,
         Condition = condition,
         Condition_Num,
         Booklet = booklet_assign,
         Sen_Num = vec_Sen_Num, #create sequence
         Num_Probs, # create sequence
         RT = response_time,
         MW,
         Com_Score,
         Passages_Sen_Num)


data_file = 'Feng Original Data.txt'
data <- read.csv(data_file,header=TRUE,sep='\t') #to go that path "" go to properties to get actual path of particular file
str(data)

#defining variables as factors (independent variables)#change every "data" to "data" if doesn't work
data$Subject <- factor(data$Subject)
data$Passages <- factor(data$Passages)
data$Booklet <- factor(data$Booklet)
data$Com_Score_IDM <- factor(data$Com_Score_IDM)
data$RTNoOut_IDM <- factor(data$RTNoOut_IDM)


data<-subset(data, Passages_Sen_Num != "6_5" & Passages_Sen_Num != "6_3")

mean(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Hard")[, "RTNoOut"])))

mean(as.numeric(as.character(subset(data,sum(MW)/Num_Probs & Condition=="Hard")[, "MW"])))



mean(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Easy")[, "RTNoOut"])))

sd(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Hard")[, "RTNoOut"])))

sd(as.numeric(as.character(subset(data, RTNoOut!="." & Condition=="Easy")[, "RTNoOut"])))


#tests for mind wandering

m0 = lmer(MW ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
anova(m0, m1)  #Compare RANDOM only model to model with FIXED effects

#tests for response time
m1 = lmer(RTNoOut_Zub ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num), data)
pamer.fnc(m1)

#This is an ANOVA to test significance of linear MER models
#It needs the library(LMERConvenienceFunctions)
pamer.fnc(m1)


#does MW predict response times
m1 = lmer(RTNoOut_Zub ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), data)
pamer.fnc(m1)


#does MW interact with condition to predict RT
interaction.plot(data$MW, data$Condition , data$RT)
m1 = lmer(RTNoOut_Zub ~ as.factor(MW)*Condition + (1 | Subject) + (1 | Passages_Sen_Num), data)
pamer.fnc(m1)

m1 = lmer(Com_Score ~ as.factor(MW)*Condition + (1 | Subject) + (1 | Passages_Sen_Num), data)

##Predicting comp score as a function of condition
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
m1 = lmer(Com_Score ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
anova(m0, m1)

#Predicting comp score as a function of MW
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
m1 = lmer(Com_Score ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', data)
anova(m0, m1)


error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
stop("vectors must be same length")
arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)


interaction.plot(data$MW, data$Condition , data$Com_Score)
interaction.plot(data$MW, data$Condition , data$Com_Score, xlab = "Mind Wandering Frequency", ylab = "Mean Comprehension", main = "Mind Wandering and Comprehension by Text Difficulty", legend = TRUE, trace.label = "Text Diff.", xpd = FALSE, xaxt = par("xaxt"))

#Easy condition
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Easy"))
m1 = lmer(Com_Score ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Easy"))
anova(m0, m1)

#Hard condition
m0 = lmer(Com_Score ~ (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Hard"))
m1 = lmer(Com_Score ~ as.factor(MW) + (1 | Subject) + (1 | Passages_Sen_Num), family = 'binomial', subset(data, Condition == "Hard"))
anova(m0, m1)
anova(m1, m0)

#this is old stuff - not needed

#data_lolo <- subset(data, Com_Score_IDM == "1" & RTNoOut_IDM == "1")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_lolo)
#interaction.plot(data_lolo$MW, data_lolo$Condition , data_lolo$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lolo, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lolo, Condition == "Hard"))

#data_lohi <- subset(data, Com_Score_IDM == "1" & RTNoOut_IDM == "2")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_lohi)
#interaction.plot(data_lohi $MW, data_lohi$Condition , data_lohi$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lohi, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_lohi, Condition == "Hard"))


#data_hilo <- subset(data, Com_Score_IDM == "2" & RTNoOut_IDM == "1")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_hilo)
#interaction.plot(data_hilo $MW, data_hilo $Condition , data_hilo$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hilo, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hilo, Condition == "Hard"))


#data_hihi <- subset(data, Com_Score_IDM == "2" & RTNoOut_IDM == "2")
#m1 = lmer(MW ~ Condition + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', data_hihi)
#interaction.plot(data_hihi$MW, data_hihi$Condition , data_hihi$Com_Score)
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hihi, Condition == "Easy"))
#m1 = lmer(Com_Score ~ MW + (1 | Subject) + (1 | Passages_Sen_Num) + (1|Sen_Num), family = 'binomial', subset(data_hihi, Condition == "Hard"))


#data_comp_lo <- subset(data, Com_Score_IDM == "1")
#interaction.plot(data_comp_lo$MW, data_comp_lo$Condition , data_comp_lo$Com_Score)

#data_comp_hi <- subset(data, Com_Score_IDM == "2")
#interaction.plot(data_comp_hi$MW, data_comp_hi$Condition , data_comp_hi$Com_Score)


#p = pvals.fnc(m1, nsim = 1000)$fixed #make sure before you run this replace "m" to "m1, m2, m3" etc (depends on which function) if successfully, a R graphic should pop up

 #run this after every linear modeling, and put "p" in console for output
#estimate is estimated means from parameter, lower and upper bound, significance (no idea what MCMmean is...)

data$MW <- factor(data$MW)
