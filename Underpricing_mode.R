library(tidyverse)

#Loading csv file
# This is absolute URL to my local file.
# You can download file in PC and change this URL to see results.
# This is drive link to downlad csv file.

#url= 'https://drive.google.com/file/d/1b5VzvT7Rx4odgN6i3xvrSsB7B9bOSliK/view?usp=sharing'


data <- read_csv('C:/Users/hp/Downloads/Lead Managers info.csv')

head(data)
view(data)

#Total NUmber of IPOs
nrow(data)

# Renaming the column name for ease of convenience
data = data %>% dplyr::rename('Issue_Size' = 'Issue Size
(in Rs Crore)',  'Initial_Returns' = '% Change on Listing Day')
data =  data %>% dplyr::rename('Market_Cap' = 'market cap')
view(data)

# Scaling the data by taking the logarithm of size and market cap....

data$Issue_Size <- log(data$Issue_Size + 1)
view(data)

#Applying linear Regression :
Model_LM <- lm(Initial_Returns~Lead_Manager_Returns,data=data)
Model_age <-lm(Initial_Returns~Age,data=data)
Model_Issue_size <- lm(Initial_Returns~Issue_Size,data=data)
Model <- lm(Initial_Returns ~ Lead_Manager_Returns + Issue_Size + Age, data = data)


summary(Model_LM)
summary(Model_age)
summary(Model_Issue_size)
summary(Model)
