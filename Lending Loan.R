library(dplyr)     
library(prettyR)   
library(ggplot2)   
library(magrittr)  
library(lubridate) 
library(padr)      
library(funModeling) 
library(lattice)
library(vcd)
library(vcdExtra)
library(ggthemes)  
library(gridExtra) 
library(forcats)

setwd("C:/Users/DELL/Desktop/Lending Club Analysis")
Loandata <- read.csv("Lending_Club_Loan_Data_2015.csv")

dim(Loandata)
names(Loandata)
summary(Loandata)
str(Loandata)     


#Transform Characters to Factor variables
Loandata$term <- as.factor(Loandata$term)
Loandata$grade <- as.factor(Loandata$grade)
Loandata$sub_grade <- as.factor(Loandata$sub_grade)
Loandata$emp_length <- as.factor(Loandata$emp_length)
Loandata$home_ownership <- as.factor(Loandata$home_ownership)
Loandata$verification_status <- as.factor(Loandata$verification_status)
Loandata$loan_status <- as.factor(Loandata$loan_status)
Loandata$pymnt_plan <- as.factor(Loandata$pymnt_plan)
Loandata$purpose <- as.factor(Loandata$purpose)
Loandata$addr_state <- as.factor(Loandata$addr_state)
Loandata$initial_list_status <- as.factor(Loandata$initial_list_status)
Loandata$application_type <- as.factor(Loandata$application_type)

#Creating levels for Loan amount
Loandata$loan_amnt_level <- cut(Loandata$loan_amnt,3,labels = c("low","medium","high"))
table(Loandata$loan_amnt_level,Loandata$purpose)


#Relationship between loan amount requested and term
mosaic(  ~ term + loan_amnt_level, 
         gp = shading_max, 
         split_vertical = T, 
         data = Loandata
)


#Relationship between Loan Amount requested and borrowers annual income
spineplot(loan_amnt_level ~ annual_inc, breaks = quantile(Loandata$annual_inc,na.rm = TRUE), data = Loandata)


structable(~ grade + verification_status + home_ownership, data = Loandata)

#loan amount as per grade
ggplot(data=Loandata, aes(x=loan_amnt,fill=grade))+ geom_histogram(col="black",bins=50) 

#Loan status per state
ggplot(data = Loandata, aes(x=addr_state, fill=loan_status)) +
  labs(x="State", y="Total Loan issued") +
  geom_bar() +
  coord_flip()

#Distribution of Loan amount by status
box_status <- ggplot(Loandata, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))  


#state-wise loan distribution
ggplot(Loandata,aes(x=Loandata$addr_state))+
  geom_bar(width = 0.5, color="white", fill="blue")+
  theme_bw()+
  labs(list(
    title = "Loan amount Vs State",
    x = "State",y = "Loan Amount")) 


#types of loans taken in California
CA <- Loandata%>%filter(Loandata$addr_state=="CA")

ggplot(CA, aes(x=CA$purpose))+
  geom_bar(width=0.5,color="blue",fill="pink")+
  geom_text(stat='count',aes(label=..count..))+
  labs(list(
    title = "Loan amount Vs Loan type",
    x = "Loan Type",y = "Loan Amount")) 

#distribution of housing loan against employee experience
house <- Loandata%>%filter(Loandata$purpose=="house")

house_loan <- house%>%group_by(emp_length,home_ownership)%>%mutate(c=n(),s=sum(house$loan_amnt))
ggplot(house_loan,aes(x=house_loan$emp_length,y=house_loan$c,shape=house_loan$home_ownership))+
  geom_point()+
  labs(list(
    title = "Housing Loan Vs Self Owned Property",
    x = "Employment Length",y = "Housing Loan Amount",shape="Home Ownership Type"))

# loan verification status & loan amount
ggplot(data=Loandata, aes(x=Loandata$verification_status,y=Loandata$loan_amnt))+    
  geom_boxplot()+
  labs(list(
    title="Loan Verification Status Vs Loan Amount",
    x="Verification Status",y="Loan Amount"))

#relation between loan amount & loan duration
ggplot(data=Loandata, aes(x=Loandata$term,y=Loandata$loan_amnt))+   # Open window
  geom_boxplot(notch=TRUE)+
  geom_boxplot(aes(fill = term))+
  theme(axis.text.x = element_blank())+
  labs(list(
    title = "Loan Amount Vs Loan Term",
    x = "Loan Term",y = "Loan Amount"))  


#relationship between loan amount Vs purpose Vs term Vs loan application type
ggplot(data=Loandata,aes(x=purpose,y=loan_amnt))+
    geom_boxplot()+
  facet_grid( term~ application_type)+
  geom_boxplot(aes(fill = purpose))+
  theme(axis.text.x = element_blank())+
  labs(list(
    title="Loan Amount Vs Loan Purpose Vs Loan Term Vs Loan Application Type",
    x="Loan Purpose",y="Loan Amount", fill="Loan Purpose"))



#Text Mining
library(tidytext)
library(tm)
library(RColorBrewer)
library(wordcloud)

#Employee title for defaulters
Loandata$emp_title <- as.character(Loandata$emp_title)
Loandata2 <- subset(Loandata, Loandata$Status == "Default")
data <- data_frame(line = 269382, text = Loandata2$emp_title)
missing_line <- data_frame(line = 269382, sentiment1 = rep(0,269382))


# Remove stop_words
data("stop_words")
tidydata <- data %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

#visualizeted words ranking
tidydata %>% filter(n > 200) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Wordcloud Visualization
tidydata %>% with(wordcloud(word, n, max.words = 50, random.order=FALSE, 
                            rot.per=0.30, 
                            use.r.layout=FALSE, 
                            colors=brewer.pal(8, "Dark2")[factor(Loandata$Status)]))