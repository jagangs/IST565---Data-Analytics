install.packages("ggplot2")
library("ggplot2")

library("dplyr")

library("stringr")


#Load data from .csv
datateller <-  read.csv("C:/Users/jagan/Documents/Masters in Data Science/IST 565 Data Mining/Week 2 Data Transformation/data-storyteller.csv")
                                 
View(datateller)

#Renaming the columns

ndt <- c("School","Section","V.Ahe","Mid","Beh","M.Beh","V.Beh","Comp")

colnames(datateller) <- ndt

#Convert the section attribute as string

datateller$Section = factor(datateller$Section)
datateller$School = as.character(datateller$School)

mean(datateller$Completed)

tsa <- datateller[(datateller$School == "A"),]

#Aggregate the total students per each section

Total <- rowSums(datateller[,c("V.Ahe","Mid","Beh","M.Beh","V.Beh","Comp")])

datateller <- data.frame(datateller,Total)

select(datateller,School:Section)

mean(datateller$Total)

SS <- paste(datateller$School,datateller$Section,sep = "")
tsa <- data.frame(str_c(tsa,collapse = NULL))
datateller <- data.frame(datateller,SS)

datateller <- datateller[,-(tsa)]


plot(datateller$School,datateller$Total)
plot(datateller$SS,datateller$Comp)

# Calculating the average students behind and plot it against total students in a section
AvgSVB <- (datateller$V.Beh/datateller$Total)*100
datateller <- data.frame(datateller,AvgSVB)

plot(datateller$Total,datateller$AvgSVB.1)

# Calculating average students completed and plot against total students in a section

AvgSC <- (datateller$Comp/datateller$Total)*100
datateller <- data.frame(datateller,AvgSC)

plot(datateller$Total,datateller$AvgSC)

hist(datateller$Total)

#Middling and behind visualization 

TotalMB <- data.frame(datateller$Mid + datateller$Beh)
AvgMB <- (TotalMB/datateller$Total)*100
datateller <- data.frame(datateller,AvgSC)

plot(datateller$Total,datateller$AvgSC)

