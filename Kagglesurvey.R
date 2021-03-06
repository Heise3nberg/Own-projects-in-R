setwd("C:/Users/Georgi/Desktop/Data science/R-working")

rm(list=ls())

#Load libraries
requiredPackages = c("tidyverse",	"dplyr",	"stringr", "tidyr", "ggplot2")

for(p in requiredPackages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
#Load the data
responses = read.csv("kagglesurvey.csv")

#Print first 10 rows
print(responses[1:10,])

# Print the first respondent's tools and languages
r1 = responses %>%
    filter(Respondent == 1) %>%
    select(WorkToolsSelect,LanguageRecommendationSelect)

print(r1) 

print(responses[which(responses$Respondent == 1),2:3])

# Add a new column, and unnest the new column
tools <- responses  %>% 
    mutate(work_tools = str_split(WorkToolsSelect,","))  %>% 
    unnest(work_tools)
    
    # View the first 6 rows of tools
head(tools)
    
# Group the data by work_tools, summarise the counts, and arrange in descending order
tool_count <- tools %>%
    group_by(work_tools) %>%
    summarize(work_tools = n()) %>%
    arrange(desc(count))

# Print the first 6 results    
print(head(tool_count))

# Create a bar chart of the work_tools column, most counts on the far right
windows()
ggplot(tool_count,aes(x=count,y=work_tools)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))
    

# Create a new column called language preference
debate_tools <- responses  %>% 
    mutate(language_preference = ifelse(str_detect(WorkToolsSelect,"Python")==T & 
                                        str_detect(WorkToolsSelect,"R")==T,"both",
                                            ifelse(str_detect(WorkToolsSelect,"Python")==F & str_detect(WorkToolsSelect,"R")==F,"neither",
                                                ifelse(str_detect(WorkToolsSelect,"Python")==T & str_detect(WorkToolsSelect,"R")==F,"Python",
                                                     ifelse(str_detect(WorkToolsSelect,"Python")==F & str_detect(WorkToolsSelect,"R")==T,"R",NA)))))
        

#Same thing with case_when
debate_tools2 <- responses  %>% 
    mutate(language_preference = case_when(
        str_detect(WorkToolsSelect, "R") & ! str_detect(WorkToolsSelect, "Python") ~ "R",
        str_detect(WorkToolsSelect, "Python") & ! str_detect(WorkToolsSelect, "R") ~ "Python",
        str_detect(WorkToolsSelect, "R") & str_detect(WorkToolsSelect, "Python")   ~ "both",
        TRUE ~ "neither"
    ))


#Check language_preference contents
table(debate_tools$language_preference)

table(debate_tools2$language_preference)

# Print the first 6 rows
print(head(debate_tools))

#R vs Python users
# Group by language preference, calculate number of responses, and remove "neither"
debate_plot = debate_tools %>%
    group_by(language_preference) %>%
    summarize(count=n()) %>%
    filter(language_preference != "neither")

windows()
ggplot(debate_plot,aes(x=language_preference,y=count,fill = language_preference))+
    geom_bar(stat = "identity")
    
#Languaage recoomendations by group:
# Group by, summarise, arrange, mutate, and filter
recommendations = debate_tools %>%
    group_by(language_preference,LanguageRecommendationSelect) %>%
    summarize(count=n()) %>%
    arrange(language_preference,desc(count)) %>%
    mutate(row = row_number()) %>%
    filter(row <= 4)

# Create a faceted bar plot
windows()
ggplot(recommendations, aes(x = LanguageRecommendationSelect, y=count))+
    geom_bar(stat = "identity") +
    facet_wrap(~language_preference)
    







