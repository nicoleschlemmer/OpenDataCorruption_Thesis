############################################################################################################################
#
#    Name = Nicole Schlemmer
#    Student Number = 410065
#    Thesis Title = Open Government or Corrupt Illusion? 
#                   Exploring the Association between Open Data and Corruption Perception Indexes
#
############################################################################################################################

## Load Libraries
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(NbClust)
library(gridExtra)
library(corrplot)
library(magrittr) 
library(caret)
library(ISLR)
library(randomForest)
library(factoextra)

############################################################################################################################
##
## Data Preparation
##
############################################################################################################################

## Load Data 
# List 30 countries of the sample in a vector so you can compare countries in all tables
thirty.countries <- c("Argentina",	"Canada",	"Colombia",	"Germany",	"Indonesia",	
                      "Mexico",	"Paraguay",	"Saudi Arabia",	"South Korea",	"United Kingdom",	
                      "Australia",	"Chile",	"Costa Rica",	"Guatemala",	"Italy",	
                      "New Zealand",	"Philippines",	"Sierra Leone",	"Turkey",	"United States of America",	
                      "Brazil",	"China",	"France",	"India",	"Japan",	
                      "Panama",	"Russia",	"South Africa",	"Ukraine",	"Uruguay")

frontrunners <- c("Canada", "Germany", "France", "United States of America", "United Kingdom", "Australia")
mid_rangers <- c("South Korea", "Chile",	"Italy",	"Japan", "Uruguay")
indecisives <- c("Saudi Arabia", "Sierra Leone", "Costa Rica")
closed_ups <- c("Mexico", "Colombia", "Philippines", "Indonesia", "India", "Brazil",
                "China", "Argentina", "South Africa", "Russia", "Turkey", "Ukraine")

############ Corruption Perception Index #################

dt.cpi <- read_excel("Data/2018_CPI_FullDataSet.xlsx", 
                     sheet = "CPI Timeseries 2012 - 2018", 
                     range = "A3:Z183") %>% as.data.table()
colnames(dt.cpi) <- gsub(" ","_",colnames(dt.cpi))
dt.cpi[Country == "Korea, South"]$Country <- "South Korea"
dt.cpi <- dt.cpi[Country %in% thirty.countries]
dt.cpi <- within(dt.cpi, rm("ISO3", "Sources_2018", "Standard_error_2018", "Sources_2017", 
                            "Standard_error_2017", "Sources_2016", "Standard_error_2016", 
                            "Sources_2015", "Standard_error_2015", "Sources_2014", 
                            "Standard_error_2014", "Sources_2013", "Standard_error_2013", 
                            "Sources_2012", "Standard_error_2012", "Rank_2017", "Rank_2018",
                            "Region"))

################### Open Data Barometer ###################

## 2017 # all countries
dt.odb.2017 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2017-Rankings") %>% as.data.table()
dt.odb.2017[toupper(Country) == "KOREA"]$Country <- "South Korea"
colnames(dt.odb.2017) <- gsub(" ","_", colnames(dt.odb.2017)) %>% gsub("-","_", .) %>% 
  paste0("_2017")

## 2016 # all countris
dt.odb.2016 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2016-Absolute-Recalculated") %>% as.data.table()
colnames(dt.odb.2016) <- gsub(" ","_", colnames(dt.odb.2016)) %>% gsub("-","_", .) %>% 
  paste0("_2016")
names(dt.odb.2016)[names(dt.odb.2016) == "ODB_Score_(0_100)_2016"] <- "ODB_Score_2016"

dt.odb.2016 <- dt.odb.2016[toupper(Country_2016) %in% toupper(dt.cpi$Country)]
toupper(dt.odb.2016$Country) %>% sort()

## 2015 # 28 countries - missing Panama and Guatemala
dt.odb.2015 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2015-Absolute-Recalculated") %>% as.data.table()
colnames(dt.odb.2015) <- gsub(" ","_", colnames(dt.odb.2015)) %>% gsub("-","_", .) %>% 
  paste0("_2015")
names(dt.odb.2015)[names(dt.odb.2015) == "ODB_Score_(0_100)_2015"] <- "ODB_Score_2015"

dt.odb.2015[toupper(Country_2015) == "KOREA (REPUBLIC OF)"]$Country_2015 <- "South Korea"
dt.odb.2015[toupper(Country_2015) == "RUSSIAN FEDERATION"]$Country_2015 <- "Russia"
dt.odb.2015 <- dt.odb.2015[toupper(Country_2015) %in% toupper(dt.cpi$Country)] 
toupper(dt.odb.2015$Country) %>% sort()

## 2014 # 27 countries - missing Guatemala, Panama and Paraguay 
dt.odb.2014 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2014-Absolute-Recalculated") %>% as.data.table()
colnames(dt.odb.2014) <- gsub(" ","_", colnames(dt.odb.2014)) %>% gsub("-","_", .) %>% 
  paste0("_2014")
names(dt.odb.2014)[names(dt.odb.2014) == "ODB_Score_(0_100)_2014"] <- "ODB_Score_2014"

dt.odb.2014 <- dt.odb.2014[toupper(Country_2014) %in% toupper(dt.cpi$Country)]
toupper(dt.odb.2014$Country) %>% sort()

## 2013 # missing Guatemala, Panama, Paraguay, Sierra Leone and Ukraine
dt.odb.2013 <- read_excel("Data/Open Data Barometer - Historical Data (All five Editions) - Public.xlsx", 
                          sheet = "ODB-2013-Absolute-Recalculated") %>% as.data.table()
colnames(dt.odb.2013) <- gsub(" ","_", colnames(dt.odb.2013)) %>% gsub("-","_", .) %>% 
  paste0("_2013")
names(dt.odb.2013)[names(dt.odb.2013) == "ODB_Score_(0_100)_2013"] <- "ODB_Score_2013"

dt.odb.2013 <- dt.odb.2013[toupper(Country_2013) %in% toupper(dt.cpi$Country)]
toupper(dt.odb.2013$Country) %>% sort() 

## Join tables together
dt.cpi.odb <- data.table(Country = thirty.countries) %>%
  left_join(.,data.table(Country = dt.odb.2013$Country_2013,
                         ODB_Score_2013 = dt.odb.2013$ODB_Score_2013),
            by = "Country") %>%
  left_join(.,data.table(Country = dt.odb.2014$Country_2014,
                         ODB_Score_2014 = dt.odb.2014$ODB_Score_2014),
            by = "Country") %>%
  left_join(.,data.table(Country = dt.odb.2015$Country_2015,
                         ODB_Score_2015 = dt.odb.2015$ODB_Score_2015),
            by = "Country") %>%
  left_join(.,data.table(Country = dt.odb.2016$Country_2016,
                         ODB_Score_2016 = dt.odb.2016$ODB_Score_2016),
            by = "Country") %>%
  left_join(.,data.table(Country = dt.odb.2017$Country_2017,
                         ODB_Score_2017 = dt.odb.2017$ODB_Score_2017),
            by = "Country") %>%
  left_join(.,dt.cpi,
            by = "Country") %>% as.data.table()

################### Explaining Variables #####################
dt.gender.govt <- read_excel("Data/Data - Indicators.xls", 
                             sheet = "Gender_Government") %>% as.data.table()
dt.edu <- read_excel("Data/Data - Indicators.xls", 
                     sheet = "Education_Index") %>% as.data.table()
dt.rd <- read_excel("Data/Data - Indicators.xls", 
                    sheet = "R&D") %>% as.data.table()
dt.life.exp <- read_excel("Data/Data - Indicators.xls", 
                          sheet = "Life_Expectancy") %>% as.data.table()
dt.unemp <- read_excel("Data/Data - Indicators.xls", 
                       sheet = "Unemployment") %>% as.data.table()
dt.power.dist <- read_excel("Data/Data - Indicators.xls", 
                            sheet = "Power Distance") %>% as.data.table()
dt.gdp <- read_excel("Data/Data - Indicators.xls", 
                     sheet = "GDP", range = "A1:G31") %>% as.data.table()

dt.ind <- data.table(Country = thirty.countries) %>%
  left_join(.,dt.gender.govt,by="Country") %>%
  left_join(.,dt.edu,by="Country") %>%
  left_join(.,dt.rd,by="Country") %>%
  left_join(.,dt.life.exp,by="Country") %>%
  left_join(.,dt.unemp,by="Country") %>%
  left_join(.,dt.power.dist,by="Country") %>%
  left_join(.,dt.gdp,by="Country") %>%
  as.data.table()

############################################################################################################################
##
## Descriptive Statistics
##
############################################################################################################################

## Summary Statistics
summary(dt.cpi.odb)
summary(dt.ind)

## Bar chart of CPI 2018 per country
dt.cpi$Country[dt.cpi$Country=="United States of America",] <- "USA"
dt.cpi[, Country := as.character(Country)][Country == "United States of America", Country := "USA"]
ggplot(dt.cpi) +
  geom_bar(stat="identity", fill="tomato3", alpha=1, 
           aes(x = reorder(Country, -CPI_score_2018), y = CPI_score_2018)) + 
  labs(title="CPI by Country (2018)") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs (x="Country", y="CPI Score") +
  ggpubr::rotate_x_text()

# Bar chart of ODBI 2017 per country
dt.cpi.odb[, Country := as.character(Country)][Country == "United States of America", Country := "USA"]
ggplot(dt.cpi.odb) +
  geom_bar(stat="identity", fill="tomato3", alpha=1, 
           aes(x = reorder(Country, -ODB_Score_2017), y = ODB_Score_2017)) + 
  labs(title="ODBI by Country (2017)") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs (x="Country", y="ODBI Score") +
  ggpubr::rotate_x_text()

############################################################################################################################
##
## Shapiro Wilk Normality Test
##
############################################################################################################################

## Normality teest using Shapiro-Wilk normality test
shapiro.test(dt.cpi.odb$CPI_Score_2013)
# 0.005047
shapiro.test(dt.cpi.odb$CPI_score_2014)
# 0.004386
shapiro.test(dt.cpi.odb$CPI_score_2015)
# 0.001971
shapiro.test(dt.cpi.odb$CPI_score_2016)
# 0.002732
shapiro.test(dt.cpi.odb$CPI_score_2017)
# 0.002832
shapiro.test(dt.cpi.odb$CPI_score_2018)
# 0.004589
shapiro.test(dt.cpi.odb$ODB_Score_2013)
# 0.3513
shapiro.test(dt.cpi.odb$ODB_Score_2014)
# 0.6411
shapiro.test(dt.cpi.odb$ODB_Score_2015)
# 0.2843
shapiro.test(dt.cpi.odb$ODB_Score_2016)
# 0.3390
shapiro.test(dt.cpi.odb$ODB_Score_2017)
# 0.0692

############################################################################################################################
##
## Correlation Analysis
##
############################################################################################################################

## Run Correlation Analysis (Open Data vs. Corruption) for each year's combination

## Corruption 2018
# Open data 2017, Corruption 2018
cor(dt.cpi.odb$CPI_score_2018, dt.cpi.odb$ODB_Score_2017, method = "spearman")

# Open data 2016, Corruption 2018
cor(dt.cpi.odb$CPI_score_2018, dt.cpi.odb$ODB_Score_2016, method = "spearman")

# Open data 2015, Corruption 2018
cor(dt.cpi.odb$CPI_score_2018, dt.cpi.odb$ODB_Score_2015, method = "spearman")

# Open data 2014, Corruption 2018
cor(dt.cpi.odb$CPI_score_2018, dt.cpi.odb$ODB_Score_2014, method = "spearman")

# Open data 2013, Corruption 2018
cor(dt.cpi.odb$CPI_score_2018, dt.cpi.odb$ODB_Score_2013, method = "spearman")

## Corruption 2017
# Both 2017
cor(dt.cpi.odb$CPI_score_2017, dt.cpi.odb$ODB_Score_2017, method = "spearman")

# Open data 2016, Corruption 2017
cor(dt.cpi.odb$CPI_score_2017, dt.cpi.odb$ODB_Score_2016, method = "spearman")

# Open data 2015, Corruption 2017
cor(dt.cpi.odb$CPI_score_2017, dt.cpi.odb$ODB_Score_2015, method = "spearman")

# Open data 2014, Corruption 2017
cor(dt.cpi.odb$CPI_score_2017, dt.cpi.odb$ODB_Score_2014, method = "spearman")

# Open data 2013, Corruption 2017
cor(dt.cpi.odb$CPI_score_2017, dt.cpi.odb$ODB_Score_2013, method = "spearman")

## Corruption 2016
# Both 2016
cor(dt.cpi.odb$CPI_score_2016, dt.cpi.odb$ODB_Score_2016,  method = "spearman")

# Open data 2015, Corruption 2016
cor(dt.cpi.odb$CPI_score_2016, dt.cpi.odb$ODB_Score_2015,  method = "spearman")

# Open data 2014, Corruption 2016
cor(dt.cpi.odb$CPI_score_2016, dt.cpi.odb$ODB_Score_2014,  method = "spearman")

# Open data 2013, Corruption 2016
cor(dt.cpi.odb$CPI_score_2016, dt.cpi.odb$ODB_Score_2013,  method = "spearman")

## Corruption 2015
# Both 2015
cor(dt.cpi.odb$CPI_score_2015, dt.cpi.odb$ODB_Score_2015,  method = "spearman")

# Open data 2014, Corruption 2015
cor(dt.cpi.odb$CPI_score_2015, dt.cpi.odb$ODB_Score_2014,  method = "spearman")

# Open data 2013, Corruption 2015
cor(dt.cpi.odb$CPI_score_2015, dt.cpi.odb$ODB_Score_2013,  method = "spearman")

## Corruption 2014
# Both 2014
cor(dt.cpi.odb$CPI_score_2014, dt.cpi.odb$ODB_Score_2014,  method = "spearman")

# Open data 2013, Corruption 2014
cor(dt.cpi.odb$CPI_score_2014, dt.cpi.odb$ODB_Score_2013,  method = "spearman")

## Corruption 2014
# Both 2013
cor(dt.cpi.odb$CPI_Score_2013, dt.cpi.odb$ODB_Score_2013,  method = "pearson")

#### Put it all together to a Correlation matrix - Spearman correlation
png("Output/correlation_matrix.png", width = 1000, height = 1000, res = 200)
corrplot(cor(within(dt.cpi.odb, rm("Country"), method="spearman"), use = "pairwise.complete.obs")[6:1,6:11, drop=FALSE], 
         cl.pos='n', 
         method="color",
         addCoef.col = "black", 
         tl.col = "black",
         cl.lim = c(0.63,1), is.corr = FALSE,
         type = "lower", 
         title = "Correlation Plot", 
         mar=c(0,0,2,0))
dev.off()

dt.corr <- cor(within(dt.cpi.odb, rm("Country")), use = "pairwise.complete.obs") 
corr.col.names <- dt.corr %>% row.names()
dt.corr <- dt.corr %>% as.data.table()
dt.corr$col_name <- corr.col.names
dt.corr <- within(dt.corr, rm("ODB_Score_2013","ODB_Score_2014","ODB_Score_2015","ODB_Score_2016","ODB_Score_2017"))
dt.corr <- dt.corr[col_name %in% c("ODB_Score_2013","ODB_Score_2014","ODB_Score_2015","ODB_Score_2016","ODB_Score_2017")]

############################################################################################################################
##
## Hierarchical Cluster Analysis
##
############################################################################################################################

# Create cluster and download all pictures of combinations to your computer
cpi_names <- c("CPI_score_2018", "CPI_score_2017", "CPI_score_2016", "CPI_score_2015","CPI_score_2014","CPI_Score_2013","CPI_Score_2012")
odb_names <- c("ODB_Score_2013", "ODB_Score_2014", "ODB_Score_2015","ODB_Score_2016", "ODB_Score_2017")
dt.country.clust <- data.table(Country = thirty.countries)
for (i in cpi_names) {
  for (j in odb_names) {
    temp.col.name <- paste0("CPI_", substr(i, nchar(i)-1, nchar(i)),
                            "-ODB_", substr(j, nchar(j)-1, nchar(j)))
    print(temp.col.name)
    dt.temp <- data.table(Country = dt.cpi.odb$Country,
                          cpi = dt.cpi.odb[[i]],
                          odb = dt.cpi.odb[[j]])
    dd <- dist(dt.temp[,c("cpi", "odb")])
    cluster.cpi.odb.loop <- hclust(dd)
    
    ## View cluster (see how many branch you want to cut)
    plot(cluster.cpi.odb.loop,
         main = "Hierarchical Cluster 2017",
         labels = dt.cpi.odb$Country,
         xlab = "Country",
         sub = " ")
    
    data <- copy(dt.cpi.odb) # distance matrix
    head(data)
    data$Country <- NULL
    res<-NbClust(data, diss=NULL, distance = "euclidean", min.nc=2, max.nc=8,
                 method = "ward.D", index = "kl")
    fit <- hclust(dd, method="ward.D")
    res
    
    # Cut the tree
    cluster.cut.cpi.odb.loop <- cutree(cluster.cpi.odb.loop, 4)
    rect.hclust(fit, k=4, border="red")
    table(cluster.cut.cpi.odb.loop,
          dt.cpi.odb$Country)
    
    dt.cluster.temp <- data.table(Country = dt.cpi.odb$Country,
                                  temp.col.name = cluster.cut.cpi.odb.loop)
    colnames(dt.cluster.temp) <- c("Country",temp.col.name)
    dt.country.clust <- left_join(dt.country.clust,
                                  dt.cluster.temp,
                                  by = "Country") %>% as.data.table()
    
    # Generate Plot
    dt.cpi.odb.clust <- left_join(dt.temp,
                                  dt.cluster.temp,
                                  by = "Country") %>% as.data.table()
    
    plot.temp <- ggplot(dt.cpi.odb.clust, aes(cpi, odb)) + 
      geom_point(col = dt.cpi.odb.clust[[temp.col.name]]) +
      geom_text(label=dt.cpi.odb.clust$Country, hjust=-0.2, check_overlap = T, size=3) +
      xlim(10, 100)+ylim(10, 100) + 
      labs(title=paste0("Clusters - CPI (",substr(i, nchar(i)-3, nchar(i)),
                        ") vs. ODB (",substr(j, nchar(j)-3, nchar(j)),")"),
           x = i, y = j)
    png(paste0("Output/clusters-",temp.col.name,".png"), width = 2000, height = 1000, res = 200)
    plot(plot.temp)
    dev.off()
  }
}

############################################################################################################################
##
## Random Forest Variable Importance
##
############################################################################################################################

## Conduct the random forest analysis with the explaining variables and the CPI scores for each cluster and each year

## Closed-ups cluster (Cluster 1)
rf18.clust1 <- randomForest(CPI_score_2018 ~ 
                              gender_govt_2018 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2018 + 
                              gdp_2017 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_18-ODB_14` == 1],
                            importance=F,
                            type="classification",
                            na.action = na.omit)

rf17.clust1 <- randomForest(CPI_score_2017 ~ gender_govt_2017 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2017 + 
                              gdp_2017 + 
                              power_distance, 
                            data=dt.cpi.odb.ind[`CPI_17-ODB_14` == 1], 
                            importance=F,
                            na.action = na.omit)

rf16.clust1 <- randomForest(CPI_score_2016 ~ gender_govt_2016 + 
                              edu_2016 + 
                              # rd_2016 + 
                              life_exp_2016 +
                              unemp_2016 +
                              gdp_2016 + 
                              power_distance, 
                            data=dt.cpi.odb.ind[`CPI_16-ODB_14` == 1], 
                            importance=F,
                            na.action = na.omit)

rf15.clust1 <- randomForest(CPI_score_2015 ~ 
                              gender_govt_2015 + 
                              edu_2015 + 
                              # rd_2015 + 
                              life_exp_2015 + 
                              unemp_2015 + 
                              gdp_2015 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_15-ODB_14` == 1], 
                            importance=F,
                            na.action = na.omit)
rf14.clust1 <- randomForest(CPI_score_2014 ~ 
                              gender_govt_2014 + 
                              edu_2014 + 
                              # rd_2014 + 
                              life_exp_2014 + 
                              unemp_2014 + 
                              gdp_2014 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_14-ODB_14` == 1], 
                            importance=F,
                            na.action = na.omit)
rf13.clust1 <- randomForest(CPI_Score_2013 ~ 
                              gender_govt_2013 + 
                              edu_2013 + 
                              # rd_2013 + 
                              life_exp_2013 + 
                              unemp_2013 + 
                              gdp_2013 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_13-ODB_13` == 1], 
                            importance=F,
                            na.action = na.omit)

## Frontrunners cluster (Cluster 2)
rf18.clust2 <- randomForest(CPI_score_2018 ~ 
                              gender_govt_2018 +
                              edu_2017 +
                              life_exp_2017 +
                              unemp_2018 +
                              gdp_2017 +
                              power_distance, 
                            data=dt.cpi.odb.ind[`CPI_18-ODB_14` == 2],
                            importance=F, 
                            na.action = na.omit)

rf17.clust2 <- randomForest(CPI_score_2017 ~ gender_govt_2017 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2017 + 
                              gdp_2017 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_17-ODB_14` == 2],
                            importance=F, 
                            na.action = na.omit)

rf16.clust2 <- randomForest(CPI_score_2016 ~ 
                              gender_govt_2016 + 
                              edu_2016 + 
                              life_exp_2016 + 
                              unemp_2016 + 
                              gdp_2016 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_16-ODB_14` == 2], 
                            importance=F,
                            na.action = na.omit)

rf15.clust2 <- randomForest(CPI_score_2015 ~ 
                              gender_govt_2015 + 
                              edu_2015 + 
                              life_exp_2015 + 
                              unemp_2015 + 
                              gdp_2015 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_15-ODB_14` == 2], 
                            importance=F,
                            na.action = na.omit)

rf14.clust2 <- randomForest(CPI_score_2014 ~ 
                              gender_govt_2014 + 
                              edu_2014 + 
                              life_exp_2014 + 
                              unemp_2014 + 
                              gdp_2014 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_14-ODB_14` == 2], 
                            importance=F,
                            na.action = na.omit)

rf13.clust2 <- randomForest(CPI_Score_2013 ~ 
                              gender_govt_2013 + 
                              edu_2013 + 
                              life_exp_2013 + 
                              unemp_2013 + 
                              gdp_2013 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_13-ODB_13` == 2], 
                            importance=F,
                            na.action = na.omit)

## Indecisives Cluster (Cluster 3)
rf18.clust3 <- randomForest(CPI_score_2018 ~ gender_govt_2018 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2018 + 
                              gdp_2017 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_18-ODB_14` == 3], 
                            importance=F,
                            na.action = na.omit)

rf17.clust3 <- randomForest(CPI_score_2017 ~ gender_govt_2017 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2017 + 
                              gdp_2017 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_17-ODB_14` == 3], 
                            importance=F,
                            na.action = na.omit)

rf16.clust3 <- randomForest(CPI_score_2016 ~ 
                              gender_govt_2016 + 
                              edu_2016 + 
                              # rd_2016 + 
                              life_exp_2016 + 
                              unemp_2016 + 
                              gdp_2016 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_16-ODB_14` == 3], 
                            importance=F,
                            na.action = na.omit)
rf15.clust3 <- randomForest(CPI_score_2015 ~ 
                              gender_govt_2015 + 
                              edu_2015 + 
                              # rd_2015 + 
                              life_exp_2015 + 
                              unemp_2015 + 
                              gdp_2015 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_15-ODB_14` == 3],
                            importance=F,
                            na.action = na.omit)

rf14.clust3 <- randomForest(CPI_score_2014 ~ 
                              gender_govt_2014 + 
                              edu_2014 + 
                              # rd_2014 + 
                              life_exp_2014 + 
                              unemp_2014 + 
                              gdp_2014 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_14-ODB_14` == 3],
                            importance=F, na.action = na.omit)

rf13.clust3 <- randomForest(CPI_Score_2013 ~ 
                              gender_govt_2013 + 
                              edu_2013 + 
                              # rd_2013 + 
                              life_exp_2013 + 
                              unemp_2013 + 
                              gdp_2013 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_13-ODB_13` == 3],
                            importance=F,
                            na.action = na.omit)

## Mid-rangers Cluster (Cluster 4): 
rf18.clust4 <- randomForest(CPI_score_2018 ~ gender_govt_2018 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2018 + 
                              gdp_2017 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_18-ODB_14` == 4], 
                            importance=F,
                            na.action = na.omit)
varImpPlot(rf18.clust4, main = "Variable Importance - 2018 Mid-rangers")

rf17.clust4 <- randomForest(CPI_score_2017 ~ gender_govt_2017 + 
                              edu_2017 +
                              life_exp_2017 + 
                              unemp_2017 + 
                              gdp_2017 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_17-ODB_14` == 4], 
                            importance=T,
                            na.action = na.omit)

rf16.clust4 <- randomForest(CPI_score_2016 ~ 
                              gender_govt_2016 + 
                              edu_2016 + 
                              # rd_2016 + 
                              life_exp_2016 + 
                              unemp_2016 + 
                              gdp_2016 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_16-ODB_14` == 4],
                            importance=T,
                            na.action = na.omit)

rf15.clust4 <- randomForest(CPI_score_2015 ~ 
                              gender_govt_2015 + 
                              edu_2015 + 
                              # rd_2015 + 
                              life_exp_2015 + 
                              unemp_2015 + 
                              gdp_2015 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_15-ODB_14` == 4],
                            importance=T,
                            na.action = na.omit)

rf14.clust4 <- randomForest(CPI_score_2014 ~ 
                              gender_govt_2014 + 
                              edu_2014 + 
                              # rd_2014 + 
                              life_exp_2014 + 
                              unemp_2014 + 
                              gdp_2014 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_14-ODB_14` == 4],
                            importance=T,
                            na.action = na.omit)

rf13.clust4 <- randomForest(CPI_Score_2013 ~ 
                              gender_govt_2013 + 
                              edu_2013 + 
                              # rd_2013 + 
                              life_exp_2013 + 
                              unemp_2013 + 
                              gdp_2013 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_13-ODB_13` == 4],
                            importance=T,
                            na.action = na.omit)

## Calculate the variable importances
importance(rf18.clust1)
importance(rf17.clust1)
importance(rf16.clust1)
importance(rf15.clust1)
importance(rf14.clust1)
importance(rf13.clust1)

importance(rf18.clust2)
importance(rf17.clust2)
importance(rf16.clust2)
importance(rf15.clust2)
importance(rf14.clust2)
importance(rf13.clust2)

importance(rf18.clust3)
importance(rf17.clust3)
importance(rf16.clust3)
importance(rf15.clust3)
importance(rf14.clust3)
importance(rf13.clust3)

importance(rf18.clust4)
importance(rf17.clust4)
importance(rf16.clust4)
importance(rf15.clust4)
importance(rf14.clust4)
importance(rf13.clust4)

## Put the information into data tables so they can be used later
## Cluster 1: Closed-Ups
imp.temp <- importance(rf18.clust1)
rf18.clust1.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf17.clust1)
rf17.clust1.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf16.clust1)
rf16.clust1.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf15.clust1)
rf15.clust1.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf14.clust1)
rf14.clust1.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf13.clust1)
rf13.clust1.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

## Cluster 2: Frontrunners
imp.temp <- importance(rf18.clust2)
rf18.clust2.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf17.clust2)
rf17.clust2.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf16.clust2)
rf16.clust2.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf15.clust2)
rf15.clust2.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf14.clust2)
rf14.clust2.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf13.clust2)
rf13.clust2.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

## Cluster 3: Indecisives
imp.temp <- importance(rf18.clust3)
rf18.clust3.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf17.clust3)
rf17.clust3.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf16.clust3)
rf16.clust3.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf15.clust3)
rf15.clust3.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf14.clust3)
rf14.clust3.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf13.clust3)
rf13.clust3.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

##### Cluster 4: Mid-rangers
imp.temp <- importance(rf18.clust4)
rf18.clust4.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf17.clust4)
rf17.clust4.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf16.clust4)
rf16.clust4.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf15.clust4)
rf15.clust4.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf14.clust4)
rf14.clust4.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

imp.temp <- importance(rf13.clust4)
rf13.clust4.vi <- data.table(variable = imp.temp %>% row.names(), IncMSE = imp.temp[,1], IncNodePurity = imp.temp[,2])

## Plot the Variable Importance 
# Frontrunners cluster over all years
varImpPlot(rf18.clust2, main="Variable Importance - Frontrunners 2018")
varImpPlot(rf17.clust2, main="Variable Importance - Frontrunners 2017")
varImpPlot(rf16.clust2, main="Variable Importance - Frontrunners 2016")
varImpPlot(rf15.clust2, main="Variable Importance - Frontrunners 2015")
varImpPlot(rf14.clust2, main="Variable Importance - Frontrunners 2014")
varImpPlot(rf13.clust2, main="Variable Importance - Frontrunners 2013")

# Mid-rangers cluster over all years
varImpPlot(rf18.clust4, main="Variable Importance - Mid-rangers 2018")
varImpPlot(rf17.clust4, main="Variable Importance - Mid-rangers 2017")
varImpPlot(rf16.clust4, main="Variable Importance - Mid-rangers 2016")
varImpPlot(rf15.clust4, main="Variable Importance - Mid-rangers 2015")
varImpPlot(rf14.clust4, main="Variable Importance - Mid-rangers 2014")
varImpPlot(rf13.clust4, main="Variable Importance - Mid-rangers 2013")

# Indecisives cluster over all years
varImpPlot(rf18.clust3, main="Variable Importance - Indecisives 2018")
varImpPlot(rf17.clust3, main="Variable Importance - Indecisives 2017")
varImpPlot(rf16.clust3, main="Variable Importance - Indecisives 2016")
varImpPlot(rf15.clust3, main="Variable Importance - Indecisives 2015")
varImpPlot(rf14.clust3, main="Variable Importance - Indecisives 2014")
varImpPlot(rf13.clust3, main="Variable Importance - Indecisives 2013")

# Closed-ups cluster over all years
varImpPlot(rf18.clust1, main="Variable Importance - Closed-Ups 2018")
varImpPlot(rf17.clust1, main="Variable Importance - Closed-Ups 2017")
varImpPlot(rf16.clust1, main="Variable Importance - Closed-Ups 2016")
varImpPlot(rf15.clust1, main="Variable Importance - Closed-Ups 2015")
varImpPlot(rf14.clust1, main="Variable Importance - Closed-Ups 2014")
varImpPlot(rf13.clust1, main="Variable Importance - Closed-Ups 2013")


############################################################################################################################
##
## Regression Analysis
##
############################################################################################################################

## Conduct the general, complete regression analysis for each year, 
## with ODBI as the dependent and CPI as the independent variable
regressiongeneralODB17 <- lm(ODB17 ~ CPI17)
regressiongeneralODB16 <- lm(ODB16 ~ CPI16)
regressiongeneralODB15 <- lm(ODB15 ~ CPI15)
regressiongeneralODB14 <- lm(ODB14 ~ CPI14)
regressiongeneralODB13 <- lm(ODB13 ~ CPI13)
regressiongeneralODB17
regressiongeneralODB16
regressiongeneralODB15
regressiongeneralODB14
regressiongeneralODB13

## Conduct the general, complete regression analysis for each year, 
## with CPI as the dependent and ODBI as the independent variable
regressiongeneralCPI17 <- lm(CPI17 ~ ODB17)
regressiongeneralCPI16 <- lm(CPI16 ~ ODB16)
regressiongeneralCPI15 <- lm(CPI15 ~ ODB15)
regressiongeneralCPI14 <- lm(CPI14 ~ ODB14)
regressiongeneralCPI13 <- lm(CPI13 ~ ODB13)
regressiongeneralCPI17
regressiongeneralCPI16
regressiongeneralCPI15
regressiongeneralCPI14
regressiongeneralCPI13


## Prepare the data of all variables, including the explaining variables and get the results
#2017
CPI17 <- dt.cpi.odb$CPI_score_2017
ODB17 <- dt.cpi.odb$ODB_Score_2017
GOVT17<- dt.cpi.odb.ind$gender_govt_2017
EDU17 <- dt.cpi.odb.ind$edu_2017
LIFE17 <- dt.cpi.odb.ind$life_exp_2017
UNEMP17 <- dt.cpi.odb.ind$unemp_2017
POWER <- dt.cpi.odb.ind$power_distance
GDP17 <- dt.cpi.odb.ind$gdp_2017

regression17CPI <- lm(CPI17 ~ ODB17 + GOVT17 + EDU17 + LIFE17 + UNEMP17 + POWER + GDP17, data=dt.cpi.odb.ind)
summary(regression17CPI)
regression17ODBI <- lm(CPI17 ~ ODB17 + GOVT17 + EDU17 + LIFE17 + UNEMP17 + POWER + GDP17, data=dt.cpi.odb.ind)

#2016
CPI16 <- dt.cpi.odb$CPI_score_2016
ODB16 <- dt.cpi.odb$ODB_Score_2016
GOVT16<- dt.cpi.odb.ind$gender_govt_2016
EDU16 <- dt.cpi.odb.ind$edu_2016
LIFE16 <- dt.cpi.odb.ind$life_exp_2016
UNEMP16 <- dt.cpi.odb.ind$unemp_2016
POWER <- dt.cpi.odb.ind$power_distance
GDP16 <- dt.cpi.odb.ind$gdp_2016

regression16 <- lm(CPI16 ~ ODB16 + GOVT16 + EDU16 + LIFE16 + UNEMP16 + POWER + GDP16, data=dt.cpi.odb.ind)
summary(regression16) # show results

#2015
CPI15 <- dt.cpi.odb$CPI_score_2015
ODB15 <- dt.cpi.odb$ODB_Score_2015
GOVT15<- dt.cpi.odb.ind$gender_govt_2015
EDU15 <- dt.cpi.odb.ind$edu_2015
LIFE15 <- dt.cpi.odb.ind$life_exp_2015
UNEMP15 <- dt.cpi.odb.ind$unemp_2015
POWER <- dt.cpi.odb.ind$power_distance
GDP15 <- dt.cpi.odb.ind$gdp_2015

regression15 <- lm(CPI15 ~ ODB15 + GOVT15 + EDU15 + LIFE15 + UNEMP15 + POWER + GDP15, data=dt.cpi.odb.ind)
summary(regression15) # show results

#2014
CPI14 <- dt.cpi.odb$CPI_score_2014
ODB14 <- dt.cpi.odb$ODB_Score_2014
GOVT14<- dt.cpi.odb.ind$gender_govt_2014
EDU14 <- dt.cpi.odb.ind$edu_2014
LIFE14 <- dt.cpi.odb.ind$life_exp_2014
UNEMP14 <- dt.cpi.odb.ind$unemp_2014
POWER <- dt.cpi.odb.ind$power_distance
GDP14 <- dt.cpi.odb.ind$gdp_2014

regression14 <- lm(CPI14 ~ ODB14 + GOVT14 + EDU14 + LIFE14 + UNEMP14 + POWER + GDP14, data=dt.cpi.odb.ind)
summary(regression14) # show results

#2013
CPI13 <- dt.cpi.odb$CPI_Score_2013
ODB13 <- dt.cpi.odb$ODB_Score_2013
GOVT13<- dt.cpi.odb.ind$gender_govt_2013
EDU13 <- dt.cpi.odb.ind$edu_2013
LIFE13 <- dt.cpi.odb.ind$life_exp_2013
UNEMP13 <- dt.cpi.odb.ind$unemp_2013
POWER <- dt.cpi.odb.ind$power_distance
GDP13 <- dt.cpi.odb.ind$gdp_2013

regression13 <- lm(CPI13 ~ ODB13 + GOVT13 + EDU13 + LIFE13 + UNEMP13 + POWER + GDP13, data=dt.cpi.odb.ind)
summary(regression13) # show results

