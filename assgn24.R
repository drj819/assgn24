getwd()
p='C:/Users/Admin/Downloads'
list.dirs()
list.files()
library(readr)
library(sparklyr)

library(ggplot2)
library(dplyr)
sc <- spark_connect(master = "local")

iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)
iris_tbl
select(Petal_Width, Petal_Length) %>%
  
  ml_linear_regression(Petal_Length ~ Petal_Width)

iris_tbl %>%
  
  select(Petal_Width, Petal_Length) %>%
  collect %>%
  ggplot(aes(Petal_Length, Petal_Width)) +
  
  geom_point(aes(Petal_Width, Petal_Length), size = 2, alpha = 0.5)
+ geom_abline(aes(slope = coef(lm_model)[["Petal_Width"]],
                                                                                      
intercept = coef(lm_model)[["(Intercept)"]]), color = "red") +labs(
    
  x	= "Petal Width",
    
  y	= "Petal Length",
    
  title = "Linear Regression: Petal Length ~ Petal Width",
    
subtitle = "Use Spark.ML linear regression to predict petal length as a f unction of petal width."
  )
pca_model <- tbl(sc, "iris") %>%
  select(-Species) %>%
  
rf_model <- iris_tbl %>%
  
ml_random_forest(Species ~ Petal_Length + Petal_Width, type = "classificati on")


fit <- partitions$training %>%
  ml_linear_regression(Petal_Length ~ Petal_Width)

estimate_mse <- function(df){
  sdf_predict(fit, df) %>%
    
mutate(resid = Petal_Length - prediction) %>% summarize(mse = mean(resid ^ 2)) %>% collect
  
  
}
ft_string2idx <- iris_tbl %>%
  
ft_string_indexer("Species", "Species_idx") %>%
  
ft_index_to_string("Species_idx", "Species_remap") %>%
  collect

table(ft_string2idx$Species, ft_string2idx$Species_remap)
glm_model <- beaver_tbl %>%
  
mutate(binary_response = as.numeric(activ == "Active")) 
%>% ml_logistic_regression(binary_response ~ temp)

fit <- partitions$training %>%
ml_linear_regression(mpg ~ wt + cyl)

model summary(fit)