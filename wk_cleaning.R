setwd('/Users/williamkye/Box Sync/nyc data science academy/machine learning')
train <- read_csv("train.csv")
test <- read_csv("test.csv")
library(dplyr)
table(train$MSSubClass)
#split it into 1 story, 1.5 stories, 2 stories, split, projects
train = train %>% mutate(MSSubClass = ifelse(MSSubClass %in% c(20), '1 story (new styles)', 
                                             ifelse(MSSubClass %in% c(30,40), '1 story older',
                                                    ifelse(MSSubClass %in% c(45,50), '1.5 stories',
                                                           ifelse(MSSubClass %in% c(60, 75), '2 story newer',
                                                                  ifelse(MSSubClass %in% c(70), '2 story older',
                                                                         ifelse(MSSubClass %in% c(80,85), 'split',
                                                                                ifelse(MSSubClass %in% c(90), 'duplex', 'projects'))))))))
#split zoning into medium-high, low, floating, and miscelanneoius
train = train %>% mutate(MSZoning = ifelse(MSZoning %in% c('RH', 'RM'), 'medium-high density',
                                           ifelse(MSZoning == 'RL', 'low density',
                                                  ifelse(MSZoning == 'FV', 'floating', 'misc'))))
#lot frontage

#lot area - logged
train = train %>% mutate(LotArea = log(LotArea))
summary(train$LotArea)

#street - dropped
train$Street=NULL

#street - dropped
train$Alley = NULL

#lot shape- as is

#land contour- as is

#utilities
train$Utilities= NULL

#lot config - as is

#land slope - as is

#neighborhood - as is

#condition 1 - near positive fature, near railrod, near street, normal

train = train %>% mutate(Condition1 = ifelse(Condition1 %in% c('Artery', 'Feedr'), 'near street',
                                             ifelse(Condition1 %in% c('RRAe', 'RRAn', 'RRNe', 'RRNn'),'near railroad',
                                                    ifelse(Condition1 %in% c('PosA', 'PosN'), 'near positive feature', 'Normal'
                                                           ))))

#condition 2 - drop
train$Condition2 = NULL

#building type- as is

#housestyle
train = train %>% mutate(HouseStyle = ifelse(HouseStyle %in% c('1.5Fin', '1.5Unf'), '1.5 stories',
                                             ifelse(HouseStyle %in% c('1Story'), '1 story',
                                                    ifelse(HouseStyle %in% c('2.5Fin', '2.5Unf', '2Story'), '2 stories', 'foyer'))))

#overalqual as it

#overalcond as is

#year remolded
train = train %>% mutate(YearRemodAdd = ifelse(YearRemodAdd == YearBuilt, 1, 0))


#year build- diff logged

train = train %>% mutate(YearBuilt = log((YrSold - YearBuilt)+1))
