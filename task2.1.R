##...........EVALUATION TASK.............##
#QUARY: WE WANT TO SEE IF THERE IS A ANY CONNECTION BETWEEN HEALTHY DIET AND SERVIVAL RATE OF COVID19 VIRUS?
#IF ANY THEN WHAT KIND OF DIET CAN HELP FOR FIGHTING AGAINST CORONA VIRUS.

# Loading required packages

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tree)

# Read data & check NA values

food_quantity_data <- read.csv(file.choose())
food_quantity_data = read.csv(file = "E:\\Evaluation Task Softanbees\\Dataset 2 Covid_19_healty_diet/Food_Supply_Quantity_kg_Data.csv", header = T, 
                               stringsAsFactors = F)
str(food_quantity_data)
summary(food_quantity_data)
ncol(food_quantity_data)

sprintf("Total NA of data = %d", sum(is.na(food_quantity_data)))
food_quantity_data[!complete.cases(food_quantity_data), ]
sprintf("Total NA of Obesity = %d", sum(is.na(food_quantity_data$Obesity)))
sprintf("Total NA of Undernourished = %d", sum(is.na(food_quantity_data$Undernourished)))

# Preprocessing missing value in Obesity variable(Using Linear Regression)

Obesity_na_data = food_quantity_data[is.na(food_quantity_data$Obesity), 2:24]
Obesity_data = food_quantity_data[!is.na(food_quantity_data$Obesity), 2:25]

Obesity_lm = lm(Obesity ~ ., data = Obesity_data)
Obesity_step = step(Obesity_lm, scope = list(lower = ~ 1, upper = ~ .), direction = "both")
summary(Obesity_step)

Obesity_predict = predict(Obesity_step, newdata = Obesity_na_data)
Obesity_predict
food_quantity_data[is.na(food_quantity_data$Obesity), "Obesity"] = Obesity_predict



#Preprocessing missing value in Undernourished variable(using linear Regression)

Undernourished_na_data = food_quantity_data[is.na(food_quantity_data[, "Undernourished"]), 2:25]
Undernourished_data = food_quantity_data[!is.na(food_quantity_data[, "Undernourished"]), 2:26]

Undernourished_data[, "Undernourished"] = as.numeric(as.character(Undernourished_data[, "Undernourished"]))
Undernourished_data[is.na(Undernourished_data[, "Undernourished"]), "Undernourished"] = 1.25

Undernourished_lm = lm(Undernourished ~ ., data = Undernourished_data)
Undernourished_step = step(Undernourished_lm, scope = list(lower = ~ 1, upper = ~ .), direction = "both")
summary(Undernourished_step)

Undernourished_predict = predict(Undernourished_lm, newdata = Undernourished_na_data)


# Fill it missing value myself

food_quantity_data[53, ] = food_quantity_data %>% filter(Country == "French Polynesia") %>%
  mutate(Confirmed = 0.021505, 
         Deaths = 0, 
         Recovered = 0.950179, 
         Active = 0)
food_quantity_data[106, ] = food_quantity_data %>% filter(Country == "Myanmar") %>%
  mutate(Confirmed = 0.000455, 
         Deaths = 0, 
         Recovered = 0.000294, 
         Active = 0.000161)
food_quantity_data[110, ] = food_quantity_data %>% filter(Country == "New Caledonia") %>%
  mutate(Confirmed = 0.007216, 
         Deaths = 0, 
         Recovered = 0.006873, 
         Active = 0.000344)

food_quantity_data = na.omit(food_quantity_data)
sprintf("Total NA of data = %d", sum(is.na(food_quantity_data)))



# Find out relation from scatterplot
# Function for check correlation coefficient

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x ,y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Recovered with other variables

pairs(food_quantity_data %>% select(2:10, 28), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

pairs(food_quantity_data %>% select(11:20, 28), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

pairs(food_quantity_data %>% select(21:27, 28), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)


p1 = food_quantity_data %>% ggplot(aes(Recovered, Animal.Products)) + geom_point() + scale_x_log10() + geom_smooth()
p2 = food_quantity_data %>% ggplot(aes(Recovered, Vegetal.Products)) + geom_point() + scale_x_log10() + geom_smooth()
p3 = food_quantity_data %>% ggplot(aes(Recovered, Obesity)) + geom_point() + scale_x_log10() + geom_smooth()
p4 = food_quantity_data %>% ggplot(aes(Recovered, Undernourished)) + geom_point() + scale_x_log10() + geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol = 2)




# Deaths with other variables

pairs(food_quantity_data %>% select(2:10, 27), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

pairs(food_quantity_data %>% select(11:20, 27), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

pairs(food_quantity_data %>% select(21:27, 27), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

o1 = food_quantity_data %>% ggplot(aes(Deaths, Animal.fats)) + geom_point() + scale_x_log10() + geom_smooth()
o2 = food_quantity_data %>% ggplot(aes(Deaths, Animal.Products)) + geom_point() + scale_x_log10() + geom_smooth()
o3 = food_quantity_data %>% ggplot(aes(Deaths, Milk...Excluding.Butter)) + geom_point() + scale_x_log10() + geom_smooth()
o4 = food_quantity_data %>% ggplot(aes(Deaths, Vegetal.Products)) + geom_point() + scale_x_log10() + geom_smooth()
o5 = food_quantity_data %>% ggplot(aes(Deaths, Obesity)) + geom_point() + scale_x_log10() + geom_smooth()
o6 = food_quantity_data %>% ggplot(aes(Deaths, Undernourished)) + geom_point() + scale_x_log10() + geom_smooth()
grid.arrange(o1, o2, o3, o4, o5, o6, ncol = 3)



#Confrimed with other variables

pairs(food_quantity_data %>% select(2:10, 26), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

pairs(food_quantity_data %>% select(11:20, 26), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

pairs(food_quantity_data %>% select(21:26, 26), 
      lower.panel = function(x,y){points(x,y); abline(0, 1, col = "red")},
      upper.panel = panel.cor)

i1 = food_quantity_data %>% ggplot(aes(Confirmed, Animal.fats)) + geom_point() + scale_x_log10() + geom_smooth()
i2 = food_quantity_data %>% ggplot(aes(Confirmed, Animal.Products)) + geom_point() + scale_x_log10() + geom_smooth()
i3 = food_quantity_data %>% ggplot(aes(Confirmed, Milk...Excluding.Butter)) + geom_point() + scale_x_log10() + geom_smooth()
i4 = food_quantity_data %>% ggplot(aes(Confirmed, Eggs)) + geom_point() + scale_x_log10() + geom_smooth()
i5 = food_quantity_data %>% ggplot(aes(Confirmed, Vegetal.Products)) + geom_point() + scale_x_log10() + geom_smooth()
i6 = food_quantity_data %>% ggplot(aes(Confirmed, Obesity)) + geom_point() + scale_x_log10() + geom_smooth()
i7 = food_quantity_data %>% ggplot(aes(Confirmed, Undernourished)) + geom_point() + scale_x_log10() + geom_smooth()
grid.arrange(i1, i2, i3, i4, i5, i6, i7, ncol = 4)


# Make table merged

Recovered_Country = food_quantity_data[order(food_quantity_data$Recovered, decreasing = T), "Country"]
Recovered_Rate = food_quantity_data[order(food_quantity_data$Recovered, decreasing = T), "Recovered"]
Deaths_Country = food_quantity_data[order(food_quantity_data$Deaths), "Country"]
Deaths_Rate =food_quantity_data[order(food_quantity_data$Deaths), "Deaths"]
Confirmed_Country = food_quantity_data[order(food_quantity_data$Confirmed, decreasing = T), "Country"]
Confirmed_Rate = food_quantity_data[order(food_quantity_data$Confirmed, decreasing = T), "Confirmed"]

cbind(Recovered_Country, Recovered_Rate, Deaths_Country, Deaths_Rate, Confirmed_Country, Confirmed_Rate)


# Using Decision Tree with Food_Supply_Quantity_kg_data
# Function for calculating RMSE 

rmse <- function(yi, yhat_i){
  sqrt(mean((yi - yhat_i)^2))
}

food_quantity_data = food_quantity_data[,-c(1,32)]

set.seed(2006)
Recovered_data = food_quantity_data[, -c(26, 27, 29, 30)]

n = nrow(Recovered_data)
idx = 1:n
training_idx = sample(idx, n*0.6)
validation_idx = setdiff(idx, training_idx)
training_data = Recovered_data[training_idx, ]
validation_data = Recovered_data[validation_idx, ]

y_obs = validation_data$Recovered
data_tree = tree(Recovered ~ ., data = training_data)
summary(data_tree)
plot(data_tree)
text(data_tree, use.n = T)
yhat_tree = predict(data_tree, newdata = validation_data)
sprintf("RMSE = %f", rmse(y_obs, yhat_tree))


#Function for data division and decision tree application

data_preprocessed = function(data){
  
  
  origin_data = read.csv(file = data, header = T, stringsAsFactors = F)
  Obesity_na_data = origin_data[is.na(origin_data$Obesity), 2:24]
  Obesity_data = origin_data[-is.na(origin_data$Obesity), 2:25]
  
  Obesity_lm = lm(Obesity ~ ., data = Obesity_data)
  Obesity_step = step(Obesity_lm, scope = list(lower = ~ 1, upper = ~ .), direction = "both")
  
  Obesity_predict = predict(Obesity_step, newdata = Obesity_na_data)
  
  origin_data[is.na(origin_data$Obesity), "Obesity"] = Obesity_predict
  
  origin_data[53, ] = origin_data %>% filter(Country == "French Polynesia") %>%
    mutate(Confirmed = 0.021505, 
           Deaths = 0, 
           Recovered = 0.950179, 
           Active = 0)
  origin_data[106, ] = origin_data %>% filter(Country == "Myanmar") %>%
    mutate(Confirmed = 0.000455, 
           Deaths = 0, 
           Recovered = 0.000294, 
           Active = 0.000161)
  origin_data[110, ] = origin_data %>% filter(Country == "New Caledonia") %>%
    mutate(Confirmed = 0.007216, 
           Deaths = 0, 
           Recovered = 0.006873, 
           Active = 0.000344)
  
  Undernourished_na_data = origin_data[is.na(origin_data[, "Undernourished"]), 2:25]
  Undernourished_data = origin_data[!is.na(origin_data[, "Undernourished"]), 2:26]
  
  Undernourished_data[, "Undernourished"] = as.numeric(as.character(Undernourished_data[, "Undernourished"]))
  Undernourished_data[is.na(Undernourished_data[, "Undernourished"]), "Undernourished"] = 1.25
  
  Undernourished_lm = lm(Undernourished ~ ., data = Undernourished_data)
  Undernourished_step = step(Undernourished_lm, scope = list(lower = ~ 1, upper = ~ .), direction = "both")
  
  Undernourished_predict = predict(Undernourished_lm, newdata = Undernourished_na_data)
  
  origin_data[is.na(origin_data[, "Undernourished"]), "Undernourished"] = Undernourished_predict
  origin_data[, "Undernourished"] = as.numeric(origin_data[, "Undernourished"])
  origin_data[is.na(origin_data[, "Undernourished"]), "Undernourished"] = 1.25
  
  origin_data = na.omit(origin_data)
  
  origin_data = origin_data[,-c(1,32)]
  return(origin_data)
}

dt_data_recovered = function(data){
  
  set.seed(2006)
  Recovered_data = data[, -c(26, 27, 29, 30)]
  
  n = nrow(Recovered_data)
  idx = 1:n
  training_idx = sample(idx, n*0.6)
  validation_idx = setdiff(idx, training_idx)
  training_data = Recovered_data[training_idx, ]
  validation_data = Recovered_data[validation_idx, ]
  
  y_obs = validation_data$Recovered
  data_tree = tree(Recovered ~ ., data = training_data)
  (summary(data_tree))
  (plot(data_tree))
  (text(data_tree, use.n = T))
  yhat_tree = predict(data_tree, newdata = validation_data)
  sprintf("RMSE = %f", (rmse(y_obs, yhat_tree)))
  
}



