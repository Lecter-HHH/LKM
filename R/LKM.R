# ------------------------------------------------------------------------------
lm_summary <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        formula <- paste(y[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- lm(formula, data = data)
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        print(paste("Below Model :",formula,"    @ Copyright:LK"))
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        print(summary(model))
      }
    }
  }
}

# ------------------------------------------------------------------------------
lm_ci <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        formula <- paste(y[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- lm(formula, data = data)
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        print(paste("Below Model :",formula,"    @ Copyright:LK"))
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        df <- cbind(coef(model),confint(model))
        df <- as.data.frame(df)
        df$'β (95%CI)' <- paste(round(df[,1],2)," (",
                         round(df[,2],2),", ",
                         round(df[,3],2),")",sep = "")
        df$Variable <- rownames(df)
        print(df[,c("Variable","β (95%CI)")])
      }
    }
  }
}
# ------------------------------------------------------------------------------

logit_summary <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        formula <- paste(y[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- glm(formula, data = data, family = "binomial")
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        print(paste("Below Model :",formula,"    @ Copyright:LK"))
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        print(summary(model))
      }
    }
  }
}
# ------------------------------------------------------------------------------

logit_ci <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        formula <- paste(y[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- glm(formula, data = data, family = "binomial")
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        print(paste("Below Model :",formula,"    @ Copyright:LK"))
        cat("===============================================================\n")
        cat("===============================================================\n")
        cat("===============================================================\n")
        df <- cbind(exp(coef(model)),exp(confint(model)))
        df <- as.data.frame(df)
        df$'OR (95%CI)' <- paste(round(df[,1],2)," (",
                                round(df[,2],2),", ",
                                round(df[,3],2),")",sep = "")
        df$Variable <- rownames(df)
        print(df[,c("Variable","OR (95%CI)")])

      }
    }
  }
}

# ------------------------------------------------------------------------------
lm_table <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        library(dplyr)
        formula <- paste(y[i],"~",exp[j],"+",
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

# ------------------------------------------------------------------------------
logit_table <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        library(dplyr)
        formula <- paste(y[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- glm(formula, data = data, family = "binomial")
        p <- tbl_regression(model, exponentiate = T, #这里指数化，求OR值
                            pvalue_fun = function(x) style_pvalue(x, digits=3),
        ) %>% modify_caption(paste("Model :",formula))
        print(p)
      }
    }
  }
}
# ------------------------------------------------------------------------------

logit_test <- function(y,exp,cor,data){
  for(i in 1:length(y)){
    for(j in 1:length(exp)){
      for(k in 1:length(cor)){
        library(dplyr)
        formula <- paste(y[i],"~",exp[j],"+",
                         paste(cor[[k]], collapse = "+", sep = " "),sep = "")
        model <- glm(formula, data = data, family = "binomial")
        p <- tbl_regression(model, exponentiate = T, #这里指数化，求OR值
                            pvalue_fun = function(x) style_pvalue(x, digits=3),
        ) %>% modify_caption(paste("Model :",formula))
        print(p)
      }
    }
  }
}










