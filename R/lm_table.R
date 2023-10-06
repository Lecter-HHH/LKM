#' lm_table: Output linear regression results as a table
#'
#' @param outc "outc" should be a vector containing all outcome variables, such as c("A","B").
#' @param exp "exp" should be a vector containing all exposure variables, such as c("C","D").
#' @param cor "cor" should be a list containing a specific combination of covariates, such as list(c("E"),c("E","F")).
#' @param data "data" should be a data.frame containing all relevant variables.
#'
#' @return
#' @export
#'
#' @examples
lm_table <- function(outc,exp,cor,data){
  library(gtsummary)
  library(dplyr)
  for(i in 1:length(outc)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        formula <- paste(outc[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- lm(formula, data = data)
        p <- tbl_regression(model, exponentiate = F, # 这里非指数化，求Beta值
                            pvalue_fun = function(x) style_pvalue(x, digits = 3),
        ) %>% modify_caption(paste("Model :",formula))
        print(p)
      }
    }
  }
}
