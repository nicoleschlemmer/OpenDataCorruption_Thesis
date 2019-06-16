###########################################################
#
#    Name = Nicole Schlemmer
#    Student Number = 410665
#    Title = 
#
###########################################################


## Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(NbClust)

############################################################################################################################
##
## Data Preparation
##
############################################################################################################################

## Load Data 
## Corruption Perception Index

dt.cpi <- read_excel("Data/2018_CPI_FullDataSet.xlsx", 
                     sheet = "CPI Timeseries 2012 - 2018", 
                     range = "A3:Z183") %>% as.data.table()
colnames(dt.cpi) <- gsub(" ","_",colnames(dt.cpi))

## Open Data Barometer
# 2017
dt.odb.2017 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2017-Rankings") %>% as.data.table()
colnames(dt.odb.2017) <- gsub(" ","_", colnames(dt.odb.2017)) %>% gsub("-","_", .) %>% 
  paste0("_2017")

# 2016
dt.odb.2016 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2016-Absolute-Recalculated") %>% as.data.table()
colnames(dt.odb.2016) <- gsub(" ","_", colnames(dt.odb.2016)) %>% gsub("-","_", .) %>% 
  paste0("_2016")
names(dt.odb.2016)[names(dt.odb.2016) == "ODB_Score_(0_100)_2016"] <- "ODB_Score_2016"
colnames(dt.odb.2016)

# 2015
dt.odb.2015 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2015-Absolute-Recalculated") %>% as.data.table()
colnames(dt.odb.2015) <- gsub(" ","_", colnames(dt.odb.2015)) %>% gsub("-","_", .) %>% 
  paste0("_2015")
names(dt.odb.2015)[names(dt.odb.2015) == "ODB_Score_(0_100)_2015"] <- "ODB_Score_2015"

colnames(dt.odb.2015)
head(dt.odb.2015)
# dt.odb.2016 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
#                           sheet = "ODB-2016-Rankings") %>% as.data.table()
# colnames(dt.odb.2016) <- gsub(" ","_", colnames(dt.odb.2016)) %>% gsub("-","_", .) %>% 
#   paste0("_2016")
# 
# dt.odb.2015 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
#                           sheet = "ODB-2015-Rankings") %>% as.data.table()
# colnames(dt.odb.2015) <- gsub(" ","_", colnames(dt.odb.2015)) %>% gsub("-","_", .) %>% 
#   paste0("_2015")
# 
# dt.odb.2014 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
#                           sheet = "ODB-2014-Rankings") %>% as.data.table()
# colnames(dt.odb.2014) <- gsub(" ","_", colnames(dt.odb.2014)) %>% gsub("-","_", .) %>% 
#   paste0("_2014")
# 
# dt.odb.2013 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
#                           sheet = "ODB-2013-Rankings") %>% as.data.table()
# colnames(dt.odb.2013) <- gsub(" ","_", colnames(dt.odb.2013)) %>% gsub("-","_", .) %>% 
#   paste0("_2013")

#dt.kaopen <- read_excel("Data/kaopen_2016.xls") %>% as.data.table()

## Clean Data
# toupper(dt.odb.2017$Country) %>% sort()
 dt.odb.2016[toupper(Country_2016) == "KOREA"]$Country_2016 <- "South Korea"
 dt.odb.2016 <- dt.odb.2016[toupper(Country_2016) %in% toupper(dt.odb.2017$Country_2017)]
 toupper(dt.odb.2016$Country) %>% sort()
 
 dt.odb.2015[toupper(Country_2015) == "KOREA (REPUBLIC OF)"]$Country_2015 <- "South Korea"
 dt.odb.2015[toupper(Country_2015) == "RUSSIAN FEDERATION"]$Country_2015 <- "Russia"
 dt.odb.2015 <- dt.odb.2015[toupper(Country_2015) %in% toupper(dt.odb.2017$Country_2017)] # Missing PANAMA and GUATAMALA
 toupper(dt.odb.2015$Country) %>% sort()
 
# # dt.odb.2014[toupper(Country_2014) %in% toupper(dt.odb.2017$Country_2017)]$Country_2014 %>% sort() %>% toupper()
# toupper(dt.odb.2014$Country) %>% sort()
# toupper(dt.odb.2013$Country) %>% sort()
# 
# # Join all ODB Scores into 1 table
# data.table(Country = dt.odb.2017$Country_2017,
#            ODB_Score_2017 = dt.odb.2017$ODB_Score_2017)
# data.table(Country = dt.odb.2016$Country_2016,
#            ODB_Score_2016 = dt.odb.2016$ODB_Score_2016)
# data.table(Country = dt.odb.2015$Country_2015,
#            ODB_Score_2015 = dt.odb.2015$ODB_Score_2015)

# Some countries are named differently and when you try to join two tables on "country" its not going to join properly if the country names are not exact. 
dt.cpi[Country == "Korea, South"]$Country <- "South Korea"
dt.cpi.odb.17 <- left_join(data.table(Country = dt.odb.2017$Country_2017,
                                   ODB_Score_2017 = dt.odb.2017$ODB_Score_2017), 
                        dt.cpi, by = "Country")


dt.cpi.odb.16.17 <- left_join(data.table(Country = dt.odb.2016$Country_2016,
                                   ODB_Score_2016 = dt.odb.2016$ODB_Score_2016), 
                        dt.cpi.odb.17, by = "Country")

dt.cpi.odb.16.17

dt.cpi.odb.15.16.17 <- left_join(data.table(Country = dt.odb.2015$Country_2015,
                                         ODB_Score_2015 = dt.odb.2015$ODB_Score_2015), 
                              dt.cpi.odb.16.17, by = "Country")

colnames(dt.cpi.odb.15.16.17)

dt.odb.2015
# List 30 countries in a vector so you can compare countries in whatever table you want maw maw
thirty.countries <- c("Argentina",	"Canada",	"Colombia",	"Germany",	"Indonesia",	
                      "Mexico",	"Paraguay",	"Saudi Arabia",	"South Korea",	"United Kingdom",	
                      "Australia",	"Chile",	"Costa Rica",	"Guatemala",	"Italy",	
                      "New Zealand",	"Philippines",	"Sierra Leone",	"Turkey",	"United States of America",	
                      "Brazil",	"China",	"France",	"India",	"Japan",	
                      "Panama",	"Russia",	"South Africa",	"Ukraine",	"Uruguay")

# Create a corruption table for 2018, with ODB_2017 coutnries (aka 30 countries listed above)
# Note: the table below called dt.cpi.2018 includes CPI scores also down to year 2012. The only thing we did was filter it to 30 countries
dt.cpi.2018 <- copy(dt.cpi[Country %in% thirty.countries])
dt.cpi.2018 %>% head()

############################################################################################################################
##
## Descriptive Statistics (http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html)
##
############################################################################################################################

summary(dt.cpi.2018)
summary(dt.cpi.odb.15.16.17)
# Histograms of CPI scores each year
#par(mfrow=c(1,7)) # change output plot to show 1 row and 6 columns
#hist(dt.cpi.2018$CPI_Score_2012)
#hist(dt.cpi.2018$CPI_Score_2013)
#hist(dt.cpi.2018$CPI_score_2014)
#hist(dt.cpi.2018$CPI_score_2015)
#hist(dt.cpi.2018$CPI_score_2016)
#hist(dt.cpi.2018$CPI_score_2017)
#hist(dt.cpi.2018$CPI_score_2018)
par(mfrow=c(1,1)) # change back to display only one graph at a time

# Bar chart of CPI 2018 per country
ggplot(dt.cpi.2018) +
  geom_bar(stat="identity", fill="tomato3", alpha=1, 
           aes(x = reorder(Country, -CPI_score_2018), y = CPI_score_2018)) + 
  labs(title="CPI by Country (2018)", 
       subtitle="maw", 
       caption="source: your mom") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


############################################################################################################################
##
## Correlation Analysis
##
############################################################################################################################

## Correlation Analysis (Open Data vs. Corruption)
ggplot(dt.cpi.odb, aes(x = CPI_score_2017, y = ODB_Score_2017)) + 
  geom_jitter(width = .5, size=1)

# linearMod <- lm(CPI_score_2017 ~ ODB_Score_2017, data = dt.cpi.odb)   
# summary(linearMod)


####### 2017
dt.cpi.odb <- dt.cpi.odb %>% distinct(Country, .keep_all = TRUE)
dt.cpi.odb

cor(dt.cpi.odb$CPI_score_2017, dt.cpi.odb$ODB_Score_2017, method = "pearson")
# --> what is the significance level? The value is 0.6566546 which means there is 
# a positive relationship between open data and corruption

####### 2016
cor(dt.cpi.odb.16.17$CPI_score_2016, dt.cpi.odb.16.17$ODB_Score_2016,  method = "pearson")
# --> outcome 0.700035

####### 2015
cor(dt.cpi.odb.15.16.17$CPI_score_2015, dt.cpi.odb.15.16.17$ODB_Score_2015,  method = "pearson")

dt.cpi.odb.15.16.17

############################################################################################################################
##
## Hierarchical Cluster Analysis 2017 (https://www.r-bloggers.com/hierarchical-clustering-in-r-2/)
##
############################################################################################################################

# Create cluster
dd <- dist(dt.cpi.odb.17[c("CPI_score_2017", "ODB_Score_2017")])
cluster.cpi.odb.2017 <- hclust(dd)

# View cluster (see how many branch you want to cut)
plot(cluster.cpi.odb.2017,
     main = "Hierarchical Cluster 2017",
     labels = dt.cpi.odb.17$Country,
     xlab = "Country",
     sub = " ")


data(dt.cpi.odb.17);
NbClust(data = cluster.cpi.odb.2017,diss="NULL",distance="euclidean",
        min.nc=2,max.nc=8,method="ward",index="pseudot2",
        alphaBeale=0.1);

# Cut the tree
cluster.cut.cpi.odb.2017 <- cutree(cluster.cpi.odb.2017, 4)
rect.hclust(fit, k=3, border="red")
table(cluster.cut.cpi.odb.2017, 
      dt.cpi.odb.17$Country)


dt.cpi.odb.17 <- left_join(dt.cpi.odb.17,
                        data.table(Country = dt.cpi.odb.17$Country, 
                                   Cluster17 = cluster.cut.cpi.odb.2017),
                       by = "Country") %>% as.data.table()

#dt.cpi.odb.17 <- within(dt.cpi.odb.17, rm("Cluster.x", "Cluster.y"))

head(dt.cpi.odb.17)

# If below yields an error, re-run the cutree() function, and run all the lines after until this one
ggplot(dt.cpi.odb.17, aes(CPI_score_2017, ODB_Score_2017)) + 
         geom_point(col = dt.cpi.odb.17$Cluster) +
  geom_text(label=dt.cpi.odb.17$Country, hjust=-0.2, check_overlap = T, size=3) +
  xlim(20, 100)+ylim(20, 100)


############################################################################################################################
##
## Hierarchical Cluster Analysis 2016 (https://www.r-bloggers.com/hierarchical-clustering-in-r-2/)
##
############################################################################################################################

## TEST

# Ward Hierarchical Clustering
data <- dt.cpi.odb.17 # distance matrix
res<-NbClust(data, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6, 
             method = "ward.D2", index = "kl") 
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")

example <- NbClust(data, diss=”NULL”, distance=”euclidean”, min.nc=2, max.nc=15, method=”ward.D”, index=”all”, alphaBeale=0.1)

## TEST END


# Create cluster
dd <- dist(dt.cpi.odb.16.17[c("CPI_score_2016", "ODB_Score_2016")])
cluster.cpi.odb.2016 <- hclust(dd)

# View cluster (see how many branch you want to cut)
plot(cluster.cpi.odb.2016,
     main = "Hierarchical Cluster 2016",
     labels = dt.cpi.odb.16.17$Country,
     xlab = "Country",
     sub = "")
rect.hclust(fit, k=4, border="red")

# Cut the tree
cluster.cut.cpi.odb.2016 <- cutree(cluster.cpi.odb.2016)
table(cluster.cut.cpi.odb.2016, 
      dt.cpi.odb.16.17$Country)


# add column to dataframe with the cluster
dt.cpi.odb <- left_join(dt.cpi.odb,
                        data.table(Country = dt.cpi.odb.16.17$Country, 
                                   Cluster16 = cluster.cut.cpi.odb.2016),
                        by = "Country") %>% as.data.table()

dt.cpi.odb <- within(dt.cpi.odb, rm("Cluster.x.x.x", "Cluster.y.y.y", "Cluster"))
dt.cpi.odb

# If below yields an error, re-run the cutree() function, and run all the lines after until this one
ggplot(dt.cpi.odb.16.17, aes(CPI_score_2016, ODB_Score_2016)) + 
  geom_point(col = dt.cpi.odb.16.17$Cluster) +
  geom_text(label=dt.cpi.odb.16.17$Country, hjust=-0.2, check_overlap = T, size=3) +
  xlim(20, 100)+ylim(20, 100)

