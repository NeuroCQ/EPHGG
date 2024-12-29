
#=======================================================

#=======================================================

rm(list = ls())

library(dplyr)

load(file = "train_data.Rdata")
load(file = "test_data.Rdata")


#===============================================================

#===============================================================


n <- nrow(train_data)
folds <- split(sample(seq_len(n)), rep(1:5, length.out = n))

# 设置需要调参的范围
ntree_values <- c(100, 200, 500, 1000)
mtry_values <- c(1, 2, 4, 6)          

# 初始化存储不同参数组合的结果
results <- data.frame(ntree = numeric(), mtry = numeric(), avg_c_index = numeric())

# 五折交叉验证调参
for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    tdAuc_list <- numeric()  
    
    # 对每个参数组合进行五折交叉验证
    for (fold in 1:length(folds)) {
      # 获取当前折的验证集索引
      validation_indices <- folds[[fold]]
      
      # 创建训练集和验证集
      cv_train_data <- train_data[-validation_indices, ]
      cv_validation_data <- train_data[validation_indices, ]
      
      # 构建随机生存森林模型
      rsf_cv_model <- rfsrc(Surv(Time, OS) ~ ., data = cv_train_data,
                            ntree = ntree, mtry = mtry)
      
      # 在验证集上进行预测
      cv_predictions <- predict(rsf_cv_model, newdata = cv_validation_data)
      cv_risk_scores <- as.numeric(cv_predictions$predicted)
      
      # 创建包含生存状态、时间和风险得分的数据框
      scores_df <- data.frame(
        OS = cv_validation_data$OS,
        Time = cv_validation_data$Time,
        risk_scores = cv_risk_scores
      )
      
      # 检查是否存在 NA 值
      scores_df <- na.omit(scores_df)
      
      
      # 使用你提供的方法计算c
      time_dependent_auc <- timeROC(
        T = scores_df$Time,     
        delta = scores_df$OS,      
        marker = scores_df$risk_scores,  
        cause = 1,           
        times = seq(0, 3, by = 0.25) 
      )
      
      AUCvalues <- mean(time_dependent_auc$AUC, na.rm = TRUE)
      
      # 保存该次交叉验证的c-index值
      tdAuc_list <- c(tdAuc_list, AUCvalues)
    }
    
    # 计算当前 ntree 和 mtry 参数组合的平均 c-index
    avg_tdAU <- mean(tdAuc_list)
    
    # 保存结果
    results <- rbind(results, data.frame(ntree = ntree, 
                                         mtry = mtry, 
                                         avg_tdAUC = avg_tdAU))
  }
}

# 打印调参结果
print(results)

# 找到最高的 c-index 对应的参数组合
best_params <- results[which.max(results$avg_tdAU), ]
print(paste("best parameters: ntree =", best_params$ntree, "mtry =", best_params$mtry))


#===============================================================

#===============================================================


# 检查数据框
head(results)

# 确保 avg_tdAUC 保留三位小数
resultsPlot <- results %>%
  mutate(avg_tdAUC = round(avg_tdAUC, 4))

# 检查修改后的数据框
head(resultsPlot)

# 绘制热图
p <- ggplot(resultsPlot, aes(x = factor(mtry), y = factor(ntree), fill = avg_tdAUC)) +
  geom_tile() +
  geom_text(aes(label = avg_tdAUC), color = "#000000", size = 4) +
  scale_fill_gradient(
    low = "#9195F6", 
    high = "#FB88B4", 
    name = "tdAUC",  
    limits = c(0.67, 0.77) 
  ) +
  labs(x = "mtry", y = "ntree", title = "Heatmap of tdAUC") +  # 修改标题
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


pdf(file = "heatmap.pdf", height = 6, width = 6)
print(p)
dev.off()


#===============================================================

#===============================================================



# 使用最佳参数组合构建最终的模型
best_rsf_model <- rfsrc(Surv(Time, OS) ~ .,
                        data = train_data, 
                        ntree = best_params$ntree, 
                        mtry = best_params$mtry,
                        importance = TRUE)

save(best_rsf_model, file = "best_rsf_model.Rdata")

#===============================================================

#===============================================================


num <- 20

# 初始化向量存储每次循环的 C-index、AUC 和 IBS 值
cindex_values <- numeric(num)
auc_values <- numeric(num)
ibs_values <- numeric(num)

# 定义 expit 函数
expit <- function(x) { 1 / (1 + exp(-x)) }
timepoints <- seq(0, 3, by = 0.25)

set.seed(123) # 设置随机种子

for (i in 1:num) {
  # 随机抽取 test_data 中 50% 的样本
  sampled_indices <- sample(1:nrow(test_data), size = floor(0.5 * nrow(test_data)))
  sampled_test_data <- test_data[sampled_indices, ]
  
  # 使用模型预测风险分数
  sampled_predictions <- predict(best_rsf_model, newdata = sampled_test_data)
  sampled_risk_scores <- as.numeric(sampled_predictions$predicted)
  
  # 创建包含测试集生存状态、时间和风险得分的数据框
  sampled_test_scores_df <- data.frame(
    OS = sampled_test_data$OS,
    Time = sampled_test_data$Time,
    risk_scores = sampled_risk_scores
  )
  
  # 计算 C-index
  sampled_cindex <- concordance.index(
    x = sampled_test_scores_df$risk_scores,
    surv.time = sampled_test_scores_df$Time,
    surv.event = sampled_test_scores_df$OS
  )$c.index
  cindex_values[i] <- sampled_cindex # 存储 C-index
  
  # 计算 AUC
  time_dependent_auc <- timeROC(
    T = sampled_test_scores_df$Time,
    delta = sampled_test_scores_df$OS,
    marker = sampled_test_scores_df$risk_scores,
    cause = 1,
    times = timepoints
  )
  auc_values[i] <- mean(time_dependent_auc$AUC, na.rm = TRUE) # 存储 AUC
  
  # 计算 Brier Scores 并计算 IBS
  brier_scores <- numeric(length(timepoints))
  for (j in seq_along(timepoints)) {
    tdroc_result <- tdROC(
      X = expit(sampled_test_scores_df$risk_scores),
      Y = sampled_test_scores_df$Time,
      delta = sampled_test_scores_df$OS,
      tau = timepoints[j],
      span = 0.1
    )
    brier_scores[j] <- as.numeric(tdroc_result$calibration_res["BrierScore"])
  }
  ibs_values[i] <- mean(brier_scores) # 存储 IBS
}

# 创建数据框
results_df <- data.frame(
  C_Index = cindex_values,
  AUC = auc_values,
  IBS = ibs_values
)

# 查看数据框的前几行
head(results_df)

results_df$IBS <- 1-results_df$IBS


#===============================================================

#===============================================================


# 定义一个函数计算中位数和95%置信区间
calculate_stats_median <- function(x) {
  n <- length(x) # 样本数量
  median_x <- median(x) # 中位数
  se <- sd(x) / sqrt(n) # 标准误
  ci_lower <- median_x - 1.96 * se # 下界
  ci_upper <- median_x + 1.96 * se # 上界
  
  return(c(median = median_x, ci_lower = ci_lower, ci_upper = ci_upper))
}

# 对每列应用这个函数
result_summary_median <- as.data.frame(t(apply(results_df, 2, calculate_stats_median)))

# 打印结果
print(result_summary_median)



#===============================================================

#===============================================================


results_long <- results_df %>%
  tidyr::pivot_longer(
    cols = everything(), 
    names_to = "Metric",
    values_to = "Value" 
  )


results_long$Metric[results_long$Metric=="IBS"] <- "1-IBS"



p <- ggplot(results_long, aes(x = Metric, y = Value, fill = Metric, colour = Metric)) +
  geom_boxplot() + 
  ylim(0.6, 0.8) +
  scale_fill_manual(values = c("#C9A7EB", "#F97B22", "#6C9BCF")) + 
  scale_color_manual(values = c("#C9A7EB", "#F97B22", "#6C9BCF")) + 
  stat_summary(
    fun = median, # 计算中位数
    geom = "text", # 以文字形式显示
    aes(label = round(..y.., 3)), # 显示小数点后三位
    vjust = -1, # 文字位置向上偏移
    colour = "black" # 文字颜色为黑色
  ) +
  labs(x = "", y = "Value") +
  theme_bw() + 
  theme(
    legend.position = "none"
  )

pdf(file = "metric.pdf", height = 6, width = 6)
print(p)
dev.off()

