# Exploring-Nonlinear-Patterns-A-GAM-Based-Approach



library(readr)
data <- read_csv("data.csv")
View(data)
install.packages("mgcv")
library(mgcv)

model_full <- gam(diagnosis ~ s(radius_mean) + s(texture_mean) + 
+                       s(perimeter_mean) + s(area_mean) + 
+                       s(smoothness_mean) + s(compactness_mean) + 
+                       s(concavity_mean) + s(concave_points_mean) + 
+                       s(symmetry_mean) + s(fractal_dimension_mean) + 
+                       radius_mean:texture_mean + s(radius_mean):s(texture_mean), 
+                   data = data, family = binomial)


data$diagnosis <- ifelse(data$diagnosis == "M", 1, 0)
duplicated_ids <- data$id[duplicated(data$id)]

if (length(duplicated_ids) > 0) {
+     cat("duplicated_ids :\n")
+     print(duplicated_ids)
+ } else {
+     cat("There are no duplicated_ids.\n")
+ }

data <- data[, -which(names(data) == "id")]
data <- na.omit(data)
data[is.na(data)] <- lapply(data[is.na(data)], function(x) mean(x, na.rm = TRUE))


colSums(is.na(data))
sum(is.na(data))

data[is.na(data)] <- lapply(data[is.na(data)], function(x) mean(x, na.rm = TRUE))

set.seed(123)

train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
model <- gam(
+     diagnosis ~ 
+         s(radius_mean) + s(texture_mean) + 
+         s(perimeter_mean) + s(area_mean) + 
+         te(radius_mean, texture_mean) +  # 반지름과 텍스처 간 상호작용
+         ti(compactness_mean, concavity_mean) +  # 조밀성과 오목성 간 상호작용
+         s(symmetry_mean) + s(fractal_dimension_mean),
+     data = train_data,
+     family = binomial
+ )
summary(model)

plot(model, pages = 1)


![스크린샷 2025-01-11 05-25-46](https://github.com/user-attachments/assets/4193c325-f715-4e3d-a3d4-948ab14af8ad)




predictions <- predict(model, test_data, type = "response")
library(pROC)
install.packages("pROC")

roc_curve <- roc(test_data$diagnosis, predictions)
plot(roc_curve, main = "ROC Curve")


![스크린샷 2025-01-11 05-25-52](https://github.com/user-attachments/assets/fa2cb783-0cc9-4d14-a59d-911e4dc7d9a6)



auc(roc_curve)  # AUC 값 출력
library(mgcv)

vis.gam(model, view = c("radius_mean", "texture_mean"), type = "response", plot.type = "persp")

![스크린샷 2025-01-11 05-26-04](https://github.com/user-attachments/assets/5f2470e9-5360-497f-b831-ba4b9dca544c)



install.packages("ggplot2")
library(ggplot2)


test_data$predicted <- predictions



ggplot(test_data, aes(x = diagnosis, y = predicted)) +
+     geom_jitter(width = 0.2, height = 0.2) +
+     labs(title = "Actual vs Predicted", x = "Actual Diagnosis", y = "Predicted Probability")




![스크린샷 2025-01-11 05-26-20](https://github.com/user-attachments/assets/e414eecb-1032-43f3-b646-310f719245c2)












