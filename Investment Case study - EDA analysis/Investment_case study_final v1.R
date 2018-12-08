############################################################################
############      Investment Case Study Project        #####################
############################################################################
# Group members 
# D Mruthyunjaya Kumar (Facilitator)
# Dharmanandana Reddy Pothula
# Ashwin Suresh
# Manohar Shanmugasundaram

########## Load the required libraries #################

library(tidyr)
library(dplyr)
library(stringr)
library(readr)    

####### Read the companies and rounds2 files ino dataframes 

# Read companies.csv into company dataframe
companies<-read.delim("companies.txt",sep="\t",stringsAsFactors = FALSE)

# Read rounds2.csv into rounds2 dataframe
rounds2<-read.csv("rounds2.csv",stringsAsFactors = FALSE)

# Read rounds2.csv into mapping dataframe
mapping<-read.csv("mapping.csv",stringsAsFactors = FALSE)

#############################################################################
#       Data Cleaning and Manupulation                #######################      
#############################################################################

# convert mapping data frame from wide to long format to get category list and main_sector
mapping<-gather(mapping,main_Sector,my_val,2:10)

# remove unwanted records(0) created as part of the gather function
mapping<-mapping[!(mapping$my_val == 0),]

# remove unwanted last column(my_val) from dataframe
mapping<-mapping[,-3]

# category names in mapping category_list are cleaned - replacing 0 with na
mapping$category_list<-gsub("0", "na", mapping$category_list)

# The above command replaced the value 'Enterprise 2.0' to 'Enterprise 2.na', hence reverting it back.
mapping$category_list<-gsub("2.na", "2.0", mapping$category_list)

# replacing null with OTH in companies country code
companies$country_code<-sub("^$","OTH",companies$country_code)

###########################################################################
# Checkpoint 1: Data Cleaning 1              ##############################
###########################################################################

# Table 1.1 - Understand the Data Set
# 1. How many unique companies are present in rounds2?
# Move the company link names to upper case, so that all the values will be in the same case.
# Unique to filter the unique records and store count value in unq_rounds2_cnt variable
unq_rounds2_cnt<-length(unique(toupper(rounds2$company_permalink)))

# Total unique records in rounds2 by company_permalink - 66368
unq_rounds2_cnt

# 2. How many unique companies are present in the companies file?
# Move the company link names to upper case, so that all the values will be in the same case.
# Unique to filter the unique records and store count value in unq_companies_cnt variable
unq_companies_cnt<-length(unique(toupper(companies$permalink)))

# Total unique records in companies by permalink - 66368
unq_companies_cnt

# 3. In the companies data frame, which column can be used as the 
# unique key for each company? Write the name of the column.
# Answer: permalink (This field holds unique key for each company)

# 4. Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
# Answer: N

# 5. Merge the two data frames so that all variables (columns) in the companies frame 
# are added to the rounds2 data frame. Name the merged frame master_frame. How many observations 
# are present in master_frame ?

# Changing rounds2 column name company_permalink to permalink for merging
names(rounds2)[1]<-paste("permalink")

# Converting rounds2 and companies permalink to same case for merging
rounds2$permalink<-tolower(rounds2$permalink)

companies$permalink<-tolower(companies$permalink)

# Merging companies and rounds2 data frames by permalink 
master_frame<-merge(companies,rounds2,by="permalink")

# Total records in Companies file - 66368
# Total records in Rounds2 file - 114949
# Total records in master_frame - 114949
unq_master_cnt<-length(unique(tolower(master_frame$permalink)))

# Total unique records in master_frame by permalink - 66368
unq_master_cnt

###############################################################################
# Checkpoint 2: Funding Type Analysis        ##################################
###############################################################################

# Calculate the average investment amount for four funding types 
# (venture, angel, seed, and private equity)

Master_Fund_Avg_Summary <- aggregate(raised_amount_usd~funding_round_type, data=master_frame, mean)

Master_Fund_Avg_Summary

# Table 2.1 - Average Values of Investments for Each of these Funding Types
# 1. Average funding amount of venture type - 11748949.1
# 2. Average funding amount of angel type - 958694.5
# 3. Average funding amount of seed type - 719818
# 4. Average funding amount of private equity type - 73308593

# Considering that Spark Funds wants to invest between 5 to 15 million USD per 
# investment round, which investment type is the most suitable for them?
FundPreferSpark<-subset(Master_Fund_Avg_Summary,Master_Fund_Avg_Summary$raised_amount_usd>=5000000 
                        & Master_Fund_Avg_Summary$raised_amount_usd<=15000000)

# Answer - Venture is the most suitable investment for Spark Funds, 
# since venture's average funding is within 5 and 15 million. 
FundPreferSpark


#############################################################################
# Checkpoint 3: Country Analysis                    #########################
#############################################################################

# 1. Spark Funds wants to see the top nine countries which have received the highest total funding
# (across ALL sectors for the chosen investment type)
country_venture_fund <- subset(master_frame, funding_round_type == "venture", 
                              select = c(country_code,raised_amount_usd, na.action = NULL))

country_venture_fund_Aggr <- aggregate(raised_amount_usd~country_code, data=country_venture_fund, sum)

# 2. For the chosen investment type, make a data frame named top9 with the top nine countries 
# (based on the total investment amount each country has received)
top9 <- head(arrange (country_venture_fund_Aggr,desc(raised_amount_usd)),9)

top9

# Table 3.1 - Analysing the Top 3 English-Speaking Countries
# 1. Top English-speaking country - USA
# 2. Second English speaking country - GBR
# 3. Third English speaking country - IND

##############################################################################
# Checkpoint 4: Sector Analysis 1                   ##########################
##############################################################################

# Extract the primary sector of each category list from the category_list column in master frame
mfcategorylist_separate<-separate(master_frame,category_list,into=c("primary_sector","category_list"),sep="\\|")
#ignore warning messages
# each primary sector mapped to its main sector using mapping data frame
mf_mastersector<-merge(mfcategorylist_separate,mapping,by.x="primary_sector",by.y="category_list")


###############################################################################
# Checkpoint 5: Sector Analysis 2                     #########################
###############################################################################

# Create three separate data frames D1, D2 and D3 for each of the three countries containing the
# observations of funding type FT falling within the 5-15 million USD range. 
# The three data frames should contain:
# 1. All the columns of the master_frame along with the primary sector and the main sector
# 3. The total amount invested in each main sector in a separate column For suitable funding type "venture"

############ Top English Country 1 - USA Analysis #################################

# D1(USA) data frame for funding b/w 5 and 15 millions for venture with eight sectors
D1<-filter(mf_mastersector,funding_round_type=="venture" & !(main_Sector=="Blanks") &
                                    country_code=="USA" & between(raised_amount_usd,5000000,15000000))

#Table 5.1 : Sector-wise Investment Analysis for USA

#The total number of investments main sector wise in a separate column For suitable funding type "venture"
D1TotInvestCnt <- aggregate(permalink~main_Sector, data=D1, length)

names(D1TotInvestCnt)[2]<-paste("Total_invest_count")

D1TotInvestAmt <- aggregate(raised_amount_usd~main_Sector, data=D1, sum)

#The total amount invested in each main sector in a separate column For suitable funding type "venture"
names(D1TotInvestAmt)[2]<-"Total_invest_amount"

D1InvestCntAmt <- merge (D1TotInvestCnt,D1TotInvestAmt,by="main_Sector", sort = TRUE)

#1. Total number of investments (count) includes all sectors - 12012
D1_Invest_TotCnt<-length(na.omit(D1$raised_amount_usd))

D1_Invest_TotCnt

#2. Total amount of investment (USD) includes all sectors - 107318294664 
D1_Invest_TotAmt<-sum(na.omit(D1$raised_amount_usd))

D1_Invest_TotAmt

#3. Top Sector name (no. of investment-wise) - Others
#4. Second Sector name (no. of investment-wise) - Social..Finance..Analytics..Advertising
#5. Third Sector name (no. of investment-wise) - Cleantech...Semiconductors

#Highest number of investments in Top3 sectors 
D1Top3InvestSector<-head(arrange(D1InvestCntAmt,desc(Total_invest_count)),3)

#6. Number of investments in top sector (3) - Others - 2950
#7. Number of investments in second sector (4) - Social..Finance..Analytics..Advertising - 2714
#8. Number of investments in third sector (5) - Cleantech...Semiconductors - 2300

#Extract records matching sector 'others'
D1Sector1<-filter(D1,main_Sector=="Others")

#summarize companies wise investments for the top sector
D1CmpSector1<-group_by(D1Sector1,name)

D1CmpSector1Summary<-summarise(D1CmpSector1,company_investment=sum(na.omit(raised_amount_usd)))

#Extract top company who receives highest investment for the top sector
D1topcompanySect1<-head(arrange(D1CmpSector1Summary,desc(company_investment)),1)

#9. For point 3 (top sector count-wise), which company received the highest investment? - Virtustream

#Extract records matching sector 'Social..Finance..Analytics..Advertising'
D1Sector2<-filter(D1,main_Sector=="Social..Finance..Analytics..Advertising")

#summarize companies wise investments for the second sector
D1CmpSector2<-group_by(D1Sector2,name)

D1CmpSector2Summary<-summarise(D1CmpSector2,company_investment=sum(na.omit(raised_amount_usd)))

#Extract top company who receives highest investment for the second sector
D1topcompanySect2<-head(arrange(D1CmpSector2Summary,desc(company_investment)),1)

#10. For point 4 (second best sector count-wise), which company received the highest investment? -- SST Inc. (Formerly ShotSpotter)


############ Top Second English Country 2 - GBR Analysis #################################

# D2(GBR) data frame for funding b/w 5 and 15 millions for venture with eight sectors
D2<-filter(mf_mastersector,funding_round_type=="venture" & !(main_Sector=="Blanks") &
             country_code=="GBR" & between(raised_amount_usd,5000000,15000000))

#Table 5.1 : Sector-wise Investment Analysis for D2 (Great Britain)

#The total number of investments main sector wise in a separate column For suitable funding type "venture"
D2TotInvestCnt <- aggregate(permalink~main_Sector, data=D2, length)

names(D2TotInvestCnt)[2]<-paste("Total_invest_count")

D2TotInvestAmt <- aggregate(raised_amount_usd~main_Sector, data=D2, sum)

#The total amount invested in each main sector in a separate column For suitable funding type "venture"

names(D2TotInvestAmt)[2]<-"Total_invest_amount"

D2InvestCntAmt <- merge (D2TotInvestCnt,D2TotInvestAmt,by="main_Sector", sort = TRUE)

#1. Total number of investments (count) includes all sectors - 619
D2_Invest_TotCnt<-length(na.omit(D2$raised_amount_usd))

D2_Invest_TotCnt

#2. Total amount of investment (USD) includes all sectors - 5365228300 
D2_Invest_TotAmt<-sum(na.omit(D2$raised_amount_usd))

D2_Invest_TotAmt

#3. Top Sector name (no. of investment-wise) - Others
#4. Second Sector name (no. of investment-wise) - Social..Finance..Analytics..Advertising
#5. Third Sector name (no. of investment-wise) - Cleantech...Semiconductors

# Highest number of investments in Top3 sectors 
D2Top3InvestSector<-head(arrange(D2InvestCntAmt,desc(Total_invest_count)),3)

#6. Number of investments in top sector (3) - Others - 147
#7. Number of investments in second sector (4) - Social..Finance..Analytics..Advertising - 133
#8. Number of investments in third sector (5) - Cleantech...Semiconductors - 128

#Extract records matching sector 'others'
D2Sector1<-filter(D2,main_Sector=="Others")

#summarize companies wise investments for the top sector
D2CmpSector1<-group_by(D2Sector1,name)

D2CmpSector1Summary<-summarise(D2CmpSector1,company_investment=sum(na.omit(raised_amount_usd)))

#Extract top company who receives highest investment for the top sector
D2topcompanySect1<-head(arrange(D2CmpSector1Summary,desc(company_investment)),1)

#9. For point 3 (top sector count-wise), which company received the highest investment? - Electric Cloud

#Extract records matching sector 'Social..Finance..Analytics..Advertising'
D2Sector2<-filter(D2,main_Sector=="Social..Finance..Analytics..Advertising")

#summarize companies wise investments for the second sector
D2CmpSector2<-group_by(D2Sector2,name)

D2CmpSector2Summary<-summarise(D2CmpSector2,company_investment=sum(na.omit(raised_amount_usd)))

#Extract top company who receives highest investment for the second sector
D2topcompanySect2<-head(arrange(D2CmpSector2Summary,desc(company_investment)),1)

#10. For point 4 (second best sector count-wise), which company received the highest investment? -- Celltick Technologies

############ Top Third English Country 3 - IND Analysis #################################

# D3(IND) data frame for funding b/w 5 and 15 millions for venture with eight sectors
D3<-filter(mf_mastersector,funding_round_type=="venture" & !(main_Sector=="Blanks") &
             country_code=="IND" & between(raised_amount_usd,5000000,15000000))

#Table 5.1 : Sector-wise Investment Analysis for D3 (India)

#The total number of investments main sector wise in a separate column For suitable funding type "venture"
D3TotInvestCnt <- aggregate(permalink~main_Sector, data=D3, length)

names(D3TotInvestCnt)[2]<-paste("Total_invest_count")

D3TotInvestAmt <- aggregate(raised_amount_usd~main_Sector, data=D3, sum)

#The total amount invested in each main sector in a separate column For suitable funding type "venture"
names(D3TotInvestAmt)[2]<-"Total_invest_amount"

D3InvestCntAmt <- merge (D3TotInvestCnt,D3TotInvestAmt,by="main_Sector", sort = TRUE)

#1. Total number of investments (count) includes all sectors - 328
D3_Invest_TotCnt<-length(na.omit(D3$raised_amount_usd))

D3_Invest_TotCnt

#2. Total amount of investment (USD) includes all sectors - 2949543602 
D3_Invest_TotAmt<-sum(na.omit(D3$raised_amount_usd))

D3_Invest_TotAmt

#3. Top Sector name (no. of investment-wise) - Others
#4. Second Sector name (no. of investment-wise) - Social..Finance..Analytics..Advertising 
#5. Third Sector name (no. of investment-wise) - News..Search.and.Messaging

#Highest number of investments in Top3 sectors 
D3Top3InvestSector<-head(arrange(D3InvestCntAmt,desc(Total_invest_count)),3)

#6. Number of investments in top sector (3) - Others - 110
#7. Number of investments in second sector (4) - Social..Finance..Analytics..Advertising  - 60
#8. Number of investments in third sector (5) - News..Search.and.Messaging - 52

#Extract records matching sector 'others'
D3Sector1<-filter(D3,main_Sector=="Others")

#summarize companies wise investments for the top sector
D3CmpSector1<-group_by(D3Sector1,name)

D3CmpSector1Summary<-summarise(D3CmpSector1,company_investment=sum(na.omit(raised_amount_usd)))

#Extract top company who receives highest investment for the top sector
D3topcompanySect1<-head(arrange(D3CmpSector1Summary,desc(company_investment)),1)

#9. For point 3 (top sector count-wise), which company received the highest investment? - FirstCry.com

#Extract records matching sector 'Social..Finance..Analytics..Advertising'
D3Sector2<-filter(D3,main_Sector=="Social..Finance..Analytics..Advertising")

#summarize companies wise investments for the second sector
D3CmpSector2<-group_by(D3Sector2,name)

D3CmpSector2Summary<-summarise(D3CmpSector2,company_investment=sum(na.omit(raised_amount_usd)))

#Extract top company who receives highest investment for the second sector
D3topcompanySect2<-head(arrange(D3CmpSector2Summary,desc(company_investment)),1)

#10. For point 4 (second best sector count-wise), which company received the highest investment? -- Manthan Systems

################################# END #################################