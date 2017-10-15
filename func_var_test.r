################################################################################
#function for calculating test statistics for potential variables for model use
################################################################################
var_test <- function(data, seg_col, formula){
  library(car) # for VIF
  df <- data # need to set prior to running function
  segment_list <- as.character(unique(df[, seg_col])) #loop to automatically run everything for each segment
  list_of_df <- vector("list", length(segment_list))
  list_of_chi <- vector("list", length(segment_list))
  for (k in 1:length(segment_list)){
    segment <- segment_list[k]
    
    df_seg <- df[df$seg == segment, ] # create temporary dataset with only a particular segment
    
    glm_mod <- glm(formula, data = df_seg, family = binomial)
    glm_chi <- anova(glm_mod, test = "Chisq")
    
    
    chi_df <- as.data.frame(matrix(ncol = length(names(glm_chi))+1, 
                                   nrow = length(row.names(glm_chi))
                                   )) #remove from all(??) loops (move to top of function)
    
    for (j in 1:length(names(glm_chi))){
      names(chi_df) <- c(segment, names(glm_chi))
      if (j == 1)
        chi_df[, j] <- row.names(glm_chi)
      if (j != 2)
        chi_df[, j+1] <- glm_chi[j]
    }
    
    glm_vif <- data.frame(vif(glm_mod)) # calculate VIF
      colnames(glm_vif) <- "VIF"
    glm_coeff <- as.data.frame(glm_mod$coefficients) # logistic regresion
      colnames(glm_coeff) <- "coeff"
    glm_vif_adj <- rbind(c(NA, NA), glm_vif) # put in new row at the top
    glm_result <- cbind(chi_df, glm_coeff, glm_vif_adj)
    
    
    df_pred <- scored_2015_m[scored_2015_m$seg == segment, ]
    predicted <- as.data.frame(predict(glm_mod, df_pred, type = "response"))
      names(predicted) <- "predicted"
    
    list_of_df[[k]] <- merge(predicted, df_pred, by = "row.names") # k is for the number of segments
    list_of_chi[[k]] <- glm_result
  }
  list_of_lists <- c(list_of_df, list_of_chi)
  return(list_of_lists)
}