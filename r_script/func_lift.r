##################################
#function for calculating "lift"
##################################
lift <- function(df, col, factor){
  colnames(df)[names(df) %in% col] <- "probability" #change column name to make code easier to maintain
  
  decile <- as.data.frame(quantile(df[, "probability"], probs = seq(0, 1, 0.10)))
  colnames(decile) <- paste("probability", "bin", sep = "_")
  
  list_decile <- c(-Inf, decile[2:(nrow(decile)-1), ], Inf) #create bins but need to remove first and last to accomodate -Inf and Inf (to create 10 bins)
  
  df_a <- as.data.frame(cut(df[, "probability"], list_decile))
    colnames(df_a) <- "bin_value"
  df_b <- as.data.frame(cut(df[, "probability"], list_decile, labels = c(1:(nrow(decile)-1))))
    colnames(df_b) <- "bin_rank"
  
  df_1 <- cbind(df_a, df_b, df) #merge bin values, bin number, and data
  
  #segment <- column
  list_suff <- c("length", "min", "max", "mean")
  agg_df_list <- vector("list", length = length(list_suff))
  agg_df_list[[1]] <- setNames(aggregate(default_18_install ~ bin_value + bin_rank, data = df_1, FUN = sum), c("bin_value", "bin_rank", "num_default_18_install"))
  for (k in 2:(length(list_suff)+1)) {
    agg_df_list[[k]] <- setNames(aggregate(probability ~ bin_value, data = df_1, FUN = list_suff[k-1]), c("bin_value", list_suff[k-1])) #k-1 to refer to agg_df_list, which is k-1 long
  }
  
  agg_final <- Reduce(function(...) merge(..., by = "bin_value", all = T), agg_df_list) #need to have the same column key for all dataframes
  agg_final$bin_rank <- as.numeric(agg_final$bin_rank) #convert to numeric, may be factor
  agg_final <- agg_final[with(agg_final, order(-bin_rank)), ] #reverse order based in decile rank

  #create other key measures
  agg_final$default_rate <- with(agg_final, num_default_18_install/length)
  agg_final$cumulative_default_count <- cumsum(agg_final$num_default_18_install)
  agg_final$capture <- with(agg_final, cumsum(num_default_18_install/sum(num_default_18_install)))
  agg_final <- agg_final[, c("bin_value", "bin_rank", "length",  "num_default_18_install", "cumulative_default_count", "min", "max", "mean", "default_rate", "capture")]
  names(agg_final)[names(agg_final) %in% "length"] <- "count"
  names(agg_final)
  #print(agg_final)
  return(agg_final)
}

# example

