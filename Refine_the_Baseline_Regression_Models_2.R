library("tidymodels")
library("tidyverse")
library("stringr")

# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

ggplot(data = train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point()

lm_poly <- RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(HUMIDITY, 4)+ poly(WIND_SPEED, 4)+ poly(VISIBILITY, 3)+ 
  poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 4) + 
 `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + `20` + `21` + `22` + `23` + `3` + `4` +
  `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY 

train_fit <- lm_spec %>% 
  fit(lm_poly,  data=train_data)

summary(train_fit$fit)



# Use predict() function to generate test results for `lm_poly`

test_results <- train_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = test_data) %>%
  # Create a new column to save the true values
  mutate(truth = test_data$RENTED_BIKE_COUNT)

head(test_results)

# e.g., test_results[test_results<0] <- 
test_results[test_results<0] <- 0


# Calculate R-squared and RMSE from the test results
rsq_1<-rsq(test_results, truth = truth, estimate = .pred)

rmse_1<-rmse(test_results, truth = truth, estimate = .pred)


model_1_results<-c( rsq_1, rmse_1)
model_1_results


# Add interaction terms to the poly regression built in previous step

# HINT: You could use `*` operator to create interaction terms such as HUMIDITY*TEMPERATURE and make the formula look like:
# RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY ...

lm_interactive <- RENTED_BIKE_COUNT ~ `18`*TEMPERATURE*DEW_POINT_TEMPERATURE + RAINFALL*HUMIDITY*`4` +SOLAR_RADIATION*SNOWFALL+
                                      WIND_SPEED*VISIBILITY +`18`*TEMPERATURE* +poly(TEMPERATURE, 6) + poly(HUMIDITY, 4)+ poly(WIND_SPEED, 4)+ poly(VISIBILITY, 3)+ 
  poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 4) + 
  `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17`  + `19` + `2` + `20` + 
  `21` + `22` + `23` + `3`  + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY 

train_fit_2 <- lm_spec %>% 
  fit(lm_interactive,  data=train_data)

train_fit_2

# Print model summary
summary(train_fit_2$fit)

test_results_2 <- train_fit_2 %>%
  # Make the predictions and save the predicted values
  predict(new_data = test_data) %>%
  # Create a new column to save the true values
  mutate(truth = test_data$RENTED_BIKE_COUNT)

head(test_results_2)


test_results_2[test_results_2<0] <- 0

# Calculate R-squared and RMSE for the new model to see if performance has improved
rsq_2<-rsq(test_results_2, truth = truth, estimate = .pred)

rmse_2<- rmse(test_results_2, truth = truth, estimate = .pred)

model_2_results<-c(rsq_2, rmse_2)
model_2_results



#install.packages('glmnet')
library('glmnet')

# Fit a glmnet model using the fit() function
glmnet_spec <- linear_reg(penalty = 0.2, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  set_mode("regression")


glmnet_fit<-glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ `18`*TEMPERATURE*DEW_POINT_TEMPERATURE + RAINFALL*HUMIDITY*`4` +SOLAR_RADIATION*SNOWFALL +
                                  WIND_SPEED*VISIBILITY +`18`*TEMPERATURE* +poly(TEMPERATURE, 6) + poly(HUMIDITY, 4)+ poly(WIND_SPEED, 4)+ poly(VISIBILITY, 3)+ 
                                  poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 4) + 
                                  `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17`  + `19` + `2` + `20` + 
                                  `21` + `22` + `23` + `3`  + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY , 
                                data = train_data )

test_results_glment <- glmnet_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = test_data) %>%
  # Create a new column to save the true values
  mutate(truth = test_data$RENTED_BIKE_COUNT)

head(test_results_glment)

test_results_glment[test_results_glment<0] <- 0

rsq_3<-rsq(test_results_glment, truth = truth, estimate = .pred)

rmse_3<-rmse(test_results_glment, truth = truth, estimate = .pred)

model_3_results<-c(rsq_3, rmse_3)
model_3_results

#Making functions
model_prediction<- function(model_fit, test_data )  {
  test_results<-model_fit %>%
    predict(new_data = test_data) %>%
    mutate(truth = test_data$RENTED_BIKE_COUNT)
  test_results[test_results<0] <- 0
  return(test_results)}

model_evaluation <- function(test_results){
    rsq_model<-rsq(test_results, truth = truth, estimate = .pred)
    rmse_model<-rmse(test_results, truth = truth, estimate = .pred)
    #print(rmse)
    #print(r2)
    results<-c(rsq_model, rmse_model)
    return(results)
}

#model_4
lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

#model_4
model_4_fit <- lm_spec %>% fit(RENTED_BIKE_COUNT ~ `18`*TEMPERATURE*DEW_POINT_TEMPERATURE + RAINFALL*HUMIDITY*`4` +SOLAR_RADIATION*SNOWFALL+
                             WIND_SPEED*VISIBILITY +`18`*TEMPERATURE+RAINFALL*HUMIDITY*`18` +poly(TEMPERATURE, 6) + poly(HUMIDITY, 6)+ poly(WIND_SPEED, 2)+ poly(VISIBILITY, 3)+ 
                             poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 2) + `18`+`4` +
                             `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17`  + `19` + `2` + `20` + 
                             `21` + `22` + `23` + `3`  + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY+
                             AUTUMN*SPRING*SUMMER*WINTER*HOLIDAY*`18`*`4`*`19`, data = train_data )

results_model_4<-model_prediction(model_4_fit, test_data)
model_4_results<-model_evaluation(results_model_4)
model_4_results

#model_5
# Fit a glmnet model using the fit() function
glmnet_spec <- linear_reg(penalty = 0.2, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  set_mode("regression")


glmnet_fit<-glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ `18`*TEMPERATURE*DEW_POINT_TEMPERATURE + RAINFALL*HUMIDITY*`4` +SOLAR_RADIATION*SNOWFALL+
                     WIND_SPEED*VISIBILITY +`18`*TEMPERATURE+RAINFALL*HUMIDITY*`18` +poly(TEMPERATURE, 6) + poly(HUMIDITY, 6)+ poly(WIND_SPEED, 2)+ poly(VISIBILITY, 3)+ 
                     poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 2) + `18`+`4`+
                   `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17`  + `19` + `2` + `20` + 
                     `21` + `22` + `23` + `3`  + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY+
                     AUTUMN*SPRING*SUMMER*WINTER*HOLIDAY*`18`*`4`*`19`, data = train_data )



results_model_5<-model_prediction(glmnet_fit, test_data)
model_5_results<-model_evaluation(results_model_5)
model_5_results



rsq_rsme_data<-data.frame(model_1_results)
rsq_rsme_data<-rbind(rsq_rsme_data, model_2_results, model_3_results, model_4_results, model_5_results)
rsq_rsme_data['model']<-c("model_1", "model_2", "model_3", "model_4", "model_5")
colnames(rsq_rsme_data)[6]<-"RSME"
colnames(rsq_rsme_data)[3]<-"Rsquared"
rsq_rsme_data

#Report the best performed model in terms of rmse and rsq
#the best model is the model 4 because it has highest R-squared of 0.797 and lowest RMSE of 285.7
library(ggplot2)
#ggplot(rsq_rsme_data, aes(x=.metric, y=.estimate))+ geom_bar( stat="identity")

#data<- data.frame(model=c("model_1", "model_2", "model_3", "model_4", "model_5"),
 #                 rsq_estimate=c(0.7772220, 0.7828358, 0.7833098, 0.7971291, 0.7963428  ),
  #                rmse_estimate=c(300.1790,296.2711,296.0435, 285.7007, 286.3185 )
   #               )
#data

ggplot(rsq_rsme_data, aes(fill=RSME, x=model, y=Rsquared))+   
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = format(round(Rsquared, 3), nsmall = 2)), vjust = -0.2, size = 4)+
  ylab("R-squared")+
  ggtitle("RMSE and R-squared values of models ") 

#TODO: Create a Q-Q plot by plotting the distribution difference between the predictions generated by your best model and the true values on the test dataset.
  
results_model_4['.pred']

ggplot(results_model_4)+
  stat_qq(aes(sample=truth), color='green') +
  stat_qq(aes(sample=.pred), color='red')






