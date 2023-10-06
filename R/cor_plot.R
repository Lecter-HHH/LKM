#' cor_plot: Frequency of occurrence of key covariates.
#'
#' @param exp  "exp" should be a character indicating the exposure variable, such as "A".
#' @param outc "outc" should be a character indicating the outcome variable, such as "B".
#' @param result "result" should be a data.frame obtained by the lm_cor/lm_auto_cor/logit_cor/logit_auto_cor functions.
#'
#' @return
#' @export
#'
#' @examples
cor_plot <- function(exp,outc,result){
  # exp 在这里是一个字符串，如"TBIL"
  # outc 在这里是一个字符串，如"MMSE"
  # result 在这里应传入lm/logit系列函数的数据框结果


  # 加载必要的包
  library(stringr)
  library(ggplot2)
  # 取交集
  index <- intersect(grep(exp, result$exposure),
                     grep(outc, result$outcome))
  # 锁定子集
  result_linsh <- result[index,]
  # 将corname列拆分成单词，并计算频率
  cor_freq <- table(unlist(str_split(result_linsh$corname, "\\+"))) %>%
    as.data.frame()
  # 为数据框添加列名
  colnames(cor_freq) <- c("Covariates", "Frequency")
  # 按出现频率降序排序
  cor_freq$Covariates <- reorder(cor_freq$Covariates, -cor_freq$Frequency)
  cor_freq$group <- paste("Exposure : ",exp,"    Outcome : ", outc, sep = " ")
  # 协变量排行榜绘图
  p <- ggplot(data = cor_freq,mapping = aes(x = Covariates,y = Frequency))+
    geom_bar(stat = "identity")+
    geom_text(aes(label = Frequency), vjust = -0.5)+
    facet_grid(~group)
  labs(y="Frequency of key covariates occurrence")
  return(p)
}
