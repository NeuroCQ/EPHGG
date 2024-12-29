
#=======================================================

#=======================================================

rm(list = ls())

library(dplyr)

load("best_rsf_model.Rdata")

load(file = "train_data.Rdata")
load(file = "test_data.Rdata")


#===============================================================

#===============================================================


original_timepoints <- seq(0, 3, by = 0.25) 
original_tdAUC <- timeROC(
  T = train_data$Time,
  delta = train_data$OS,
  marker = predict(best_rsf_model, newdata = train_data)$predicted,
  cause = 1,
  times = original_timepoints
)$AUC

perm_importance <- data.frame(Variable = setdiff(colnames(train_data),
                                                 c("OS", "Time")),
                              Importance = NA)

for (variable in perm_importance$Variable) {
  
  permuted_data <- train_data
  
  permuted_data[[variable]] <- sample(permuted_data[[variable]])
  
  permuted_tdAUC <- timeROC(
    T = permuted_data$Time,
    delta = permuted_data$OS,
    marker = predict(best_rsf_model, newdata = permuted_data)$predicted,
    cause = 1,
    times = original_timepoints
  )$AUC
  
  importance <- mean(original_tdAUC, na.rm = TRUE) - mean(permuted_tdAUC, na.rm = TRUE)
  perm_importance$Importance[perm_importance$Variable == variable] <- importance
}

print(perm_importance)


max_importance <- max(perm_importance$Importance, na.rm = TRUE)
min_importance <- min(perm_importance$Importance, na.rm = TRUE)

perm_importance$Relative_Importance <- (perm_importance$Importance - min_importance) / (max_importance - min_importance)

perm_importance$Relative_Importance <- perm_importance$Relative_Importance + 0.05

print(perm_importance)



p <- ggplot(perm_importance, aes(x = reorder(Variable, Relative_Importance), y = Relative_Importance)) +
  geom_bar(stat = "identity", fill = "#A0C3D2") +
  geom_text(
    aes(label = round(Relative_Importance, 3)), 
    hjust = 0.4, color = "black" 
  ) +
  coord_flip() +
  labs(x = "Variable", y = "Relative Importance") +
  theme_bw()

pdf(file = "importance.pdf", height = 6, width = 6)
print(p)
dev.off()

