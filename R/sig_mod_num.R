#' sig_mod_num:Across all models, count the number of models in which the exposure variable was statistically significant.
#'
#' @param exp exp should be a vector containing all exposure variables, such as c("C","D").
#' @param outc outc should be a vector containing all outcome variables, such as c("A","B").
#' @param result "result" should be a data.frame obtained by the lm_cor/lm_auto_cor/logit_cor/logit_auto_cor functions.
#'
#' @return
#' @export
#'
#' @examples
sig_mod_num <- function(exp,outc,result){
  # exp 应输入暴露变量向量，例如c("TBIL")
  # outc 应输入结局变量向量，例如c("MMSE")
  # result 应输入从lm/logit系列函数得到的结果数据框

  # 准备空数据框
  mod_num <- data.frame()

  # 循环计数各个暴露变量有意义的模型数量
  for(x in exp){
    for(y in outc){
      library(stringr)
      num <- nrow(filter(result, x%in%exposure, outcome==y))
      mod_num0 <- data.frame(exposure=x,outcome=y,sig_mod_num=num)
      mod_num <- rbind(mod_num, mod_num0)
    }
  }
  # ----------输出图片--------
  library(ggplot2)
  p <- ggplot(data=mod_num, mapping=aes(x=exposure,y=sig_mod_num))+
    geom_bar(stat = "identity")+
    geom_text(aes(label = sig_mod_num), vjust = -0.5)+
    labs(x="Exposure",y="Number of statistically significant models")+
    facet_grid(~outcome)
  print(p)
}
