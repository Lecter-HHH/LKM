#' logit_auto_cor:Batch build logistic regression models with all possible combinations of covariates.
#'
#' @param outc outc should be a vector containing all outcome variables, such as c("A","B").
#' @param exp exp should be a vector containing all exposure variables, such as c("C","D").
#' @param cor cor should be a vector containing all covariates, such as c("E","F").
#' @param data data should be a data.frame containing all relevant variables.
#' @param only_sig only_sig should be TRUE or FALSE value.
#'
#' @return
#' @export
#'
#' @examples
logit_auto_cor <- function(outc,exp,cor,data,only_sig){
  # outc 应该输入一个字符串向量表示结局指标有哪些，如c("MMSE")
  # exp 应该输入一个字符串向量表示暴露指标有哪些，如c("TBIL")
  # cor 应该输入一个list,指定要调整的特定协变量集，如list(c("A"),c("A","B"))
  # data 应输入一个包含所有用到数据的数据框

  # --------------- 模块一：生成协变量组合---------------
  cor0 <- cor # 待输入的拟调整的协变量
  n <- length(cor0) # 获取长度
  cor1 <- list() # 生成空列表
  # 对所有协变量进行排列组合
  for (length_limit in 1:n) {
    combos <- combn(1:n, length_limit)
    for (i in 1:ncol(combos)) {
      index <- combos[, i]
      cor1 <- c(cor1,list(cor0[index]))
    }
  }
  # --------------- 模块二：循环建模---------------
  result <- data.frame()

  # 加载包
  library(dplyr)

  for(i in 1:length(outc)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor1)){
        # 构建公式
        formula <- paste(outc[i],"~",exp[j],"+",
                         paste(cor1[[k]], collapse = "+", sep = " "),
                         sep = "")
        # 构建模型
        model <- glm(formula, data = data, family = "binomial")
        # 提取效应值 和 P值, 考虑到exp可能是连续、也可能是分类，所以使用grep匹配
        result0 <- summary(model)[["coefficients"]] %>% as.data.frame()
        result0 <- result0[grep(exp[j], rownames(result0)),]
        # 取对数，计算OR值
        result0$OR <- exp(result0$Estimate)
        # 提取95%CI并合并
        ci <- confint(model) %>% as.data.frame()
        # 取对数，计算OR值95%CI
        ci <- ci[grep(exp[j], rownames(ci)),] %>% exp()
        result0 <- cbind(result0,ci)
        # 变量更名
        names(result0)[4:7] <- c("p","OR","CI_lower","CI_upper")
        # 生成规范 beta(95%CI)
        result0$effects <- paste(sprintf("%.2f", result0$OR)," (",
                                 sprintf("%.2f", result0$CI_lower),", ",
                                 sprintf("%.2f", result0$CI_upper),")",
                                 sep = "")
        # 添加结局指标
        result0$outcome <- outc[i]
        # 添加协变量
        result0$corname <- paste(cor1[[k]], collapse = "+", sep = " ")
        # 提取子集
        result0 <- result0[,c("effects","p","outcome","corname","OR",
                              "CI_lower","CI_upper")]
        # 合并最终结果
        result <- rbind(result0, result)
      }
    }
  }
  # --------------- 模块三：输出结果---------------
  if(only_sig == TRUE){
    return(filter(result,p<0.05)) # 只返回统计学显著的结果
  } else {
    return(result) # 返回所有结果
  }
}
