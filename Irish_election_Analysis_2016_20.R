#Importing the libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

#Reading the xlsx and csv files
df_20 = read_excel("GalwayWest-2020.xlsx")
df_16 = read.csv("2016-04-28_general-election-count-details-galway-west-csv_en.csv",
                 stringsAsFactors = FALSE, encoding = "UTF-8")

#Pre-processing the election 2020 data
#First row as header
colnames(df_20) <- df_20[1,]
df_20 <- df_20[-c(1,17), ] #delete first row
df_20[1] <- NULL# delete party column
names(df_20)[1] <- "Party"

#Transforming the wide data into long with the help of tidyr
df_20 <- df_20 %>% gather(key = Counts, value = Votes,"Count 1":"Count 13")
df_20$Votes <- as.numeric(df_20$Votes)
df_20 <- df_20 %>% mutate(Votes = if_else(is.na(Votes),0,Votes))

#Preprocessing the 2016 election Data
#Adding the new column Candidate and deleting the existing extra columns
df_16$Candidate <- paste(df_16$Candidate.First.Name," ",df_16$Candidate.surname)


#Filling the empty fields in the result column with the fill function
df_16$Result[df_16$Result==""] <- NA #empty to NA
df_16 <- fill(df_16,Result, .direction = c("updown")) #fill to empty fields

#Rectifying the Partyname with extra spaces and the correct names
party_name <- c("Social Democratic Party", "Ind", "Fine  Gael", "Fianna Fail",
                "Sinn Fein")
party_name_actual <- c("Social Democrats","Independent","Fine Gael","Fianna Fáil",
                       "Sinn Féin")

df_16_pr <- plyr::mapvalues(as.vector(df_16$Party), from = party_name, 
                            to = party_name_actual)
df_16$Party <- df_16_pr

#Rename the Party values as expected 
cand_name = c("Catherine Martina Ann   Connolly","Niall   O' Tuathail","Éamon   O'Cuív","Sean   Kyne",
              "Hildegarde   Naughton","Noel   Grealish","Mike   Cubbard")
cand_name_actual = c("Catherine Connolly","Niall Ó Tuathail","Éamon Ó Cuív","Seán Kyne",
                     "Hildegarde Naughton","Noel Grealish","Mike Cubbard")

df_16_cd <- plyr::mapvalues(as.vector(df_16$Candidate), from = cand_name,
                            to = cand_name_actual)
df_16$Candidate <- df_16_cd

###########################Section 1##############################
#Filter only party and their respective votes
df_20_first <- df_20 %>% filter(Counts=="Count 1") %>%group_by(Party) %>% 
  dplyr::summarise(Votes=sum(Votes))

df_16_first <- df_16 %>% filter(Count.Number ==1) %>% group_by(Party) %>% 
  dplyr::summarise(Votes=sum(Votes))

#Performing inner Join on both the datasets
in_join <- inner_join(df_20_first,df_16_first, by = "Party")
first <- gather(in_join,Year,Votes,"Votes.x":"Votes.y")
first$Year <- sapply(first$Year,function(x) {ifelse(x=="Votes.x", "2020" ,"2016")})

#Plot bar graph Votes per party in the election years 2016 and 2020 
ggplot(first,aes(reorder(x = Party,Votes) , y= Votes, fill=Year)) +
  xlab("Party Name") + ylab("Votes") + 
  ggtitle("Votes per party in the year 2016 and 2020")+theme_bw() +
  scale_y_continuous(breaks = seq(0,15000,3000)) +
  geom_bar(stat = "identity",position = "dodge",colour = "Black")+ 
  geom_text(aes(label=Votes),size=2.5, vjust=-0.30,position=position_dodge(width=0.8))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.85),axis.text=element_text(size = 8)) +
  scale_fill_manual(values = c("#86DCF5", "#4AA3BD"))
ggsave("Votes per party in 2016-2020.png")

##############################Section 2###############################
in_join %>% mutate(Votes_diff = Votes.x - Votes.y) %>%
  ggplot(aes(x= reorder(Party,Votes_diff), y = Votes_diff, fill=Party)) +
  scale_fill_manual(values = c("#0F9230","#3392FF","#7AB624","#BCA00B","#B62912","#02523F","#550C5C")) +
  xlab("Party Name")+
  geom_bar(stat = "identity",position = "dodge",colour = "Black") + 
  geom_text(aes(label=Votes_diff), size=2.5,vjust=-0.30,position=position_dodge(width=0.8))+
  ggtitle("Change in vote party from 2016-2020") +
  scale_y_continuous(breaks = seq(-5000,2500,1000)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Change in party from 2016-2020.png")

#Importing the CSV file National Average
National_Avg <- read.csv("National_Average.csv",header = T,stringsAsFactors = FALSE)

#Filtering the values based on the first pref vote and group by part
df_20_three <- df_20 %>% filter(Counts== "Count 1") %>% group_by(Party) %>% 
  summarise(Votes=(sum(Votes)))
df_20_three <- mutate(df_20_three,FPv_Percent = (Votes/sum(df_20_three$Votes))*100)

#Performin inner join on both the dataframe
in_join_three <- inner_join(National_Avg,df_20_three,by="Party")
in_join_three$Votes <- NULL
Nat_Avg <- gather(in_join_three,Category,FPv_Percent,"FPv.":"FPv_Percent")
Nat_Avg$Category <- sapply(Nat_Avg$Category,function(x) 
{ifelse(x=="FPv.", "National Avg" ,"Galway West")})

#Comparing the National Average and Galway West vote share in the year 2020 
ggplot(Nat_Avg,aes(x=Party, y=FPv_Percent,group=Category,shape = Category)) +
  geom_point(aes(linetype = Category, colour=Category)) + geom_point(aes(color=Category),size=2) + 
  scale_shape_manual(values = c(8, 17)) +
  ggtitle("Compare vote share of 2020 b/w National Average & Galway West") +
  ylab("First Prefernce Vote in percentage") + 
  scale_colour_manual(values = c("#F34E4E", "#1D399F")) +
  scale_y_continuous(breaks = seq(0,25,5)) +
  theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave('Compare Vote share.png')
############################Section 4##################################

#Filter the votes based on first pref and group by Candidate and calculate the differenve between the votes
df_20_four <- df_20 %>% filter(Counts=="Count 1") %>% group_by(Candidate) %>% summarise(Votes = sum(Votes))   
df_16_four <- df_16 %>% filter(Count.Number==1) %>% group_by(Candidate) %>% summarise(Votes = sum(Votes))

#Perform inner join based on the above dataframes
in_join_four <- inner_join(df_20_four,df_16_four, by = "Candidate")
in_join_four <- in_join_four %>% mutate(Votes_diff = Votes.x - Votes.y)
