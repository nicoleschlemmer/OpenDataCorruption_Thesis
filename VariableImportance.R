
############################################################################################################################
##
## Random Forest Variable Importance
##
############################################################################################################################

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
                              # rd_2016 + 
                              life_exp_2016 + 
                              unemp_2016 + 
                              gdp_2016 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_16-ODB_14` == 2], 
                            importance=F,
                            na.action = na.omit)

rf15.clust2 <- randomForest(CPI_score_2015 ~ 
                              gender_govt_2015 + 
                              edu_2015 + 
                              # rd_2015 + 
                              life_exp_2015 + 
                              unemp_2015 + 
                              gdp_2015 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_15-ODB_14` == 2], 
                            importance=F,
                            na.action = na.omit)

rf14.clust2 <- randomForest(CPI_score_2014 ~ 
                              gender_govt_2014 + 
                              edu_2014 + 
                              # rd_2014 + 
                              life_exp_2014 + 
                              unemp_2014 + 
                              gdp_2014 + power_distance, 
                            data=dt.cpi.odb.ind[`CPI_14-ODB_14` == 2], 
                            importance=F,
                            na.action = na.omit)

rf13.clust2 <- randomForest(CPI_Score_2013 ~ 
                              gender_govt_2013 + 
                              edu_2013 + 
                              # rd_2013 + 
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

## Variable Importance Plots
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

