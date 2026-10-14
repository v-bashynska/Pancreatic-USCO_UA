if(!require("corrplot")) install.packages("corrplot")
library(corrplot)
setwd()
Data = read.csv()
##################################################################################
## Spearman correlation: non-parametric, however assumes a monotonic relationship ##
####################################################################################
if(!require("Hmisc")) install.packages("Hmisc")
library(Hmisc)
rcorrData_spear<- Hmisc::rcorr(as.matrix(correlation_dataset_completerate0.5_colnames), type="spearman")
write.csv(rcorrData_spear[["r"]], file = "Cor_Spearman_Data_rcorr_r.csv", quote=FALSE)
write.csv(rcorrData_spear[["n"]], file = "Cor_Spearman_Data_rcorr_n.csv", quote=FALSE)
write.csv(rcorrData_spear[["P"]], file = "Cor_Spearman_Data_rcorr_p.csv", quote=FALSE)

rcorrData_spear_r = as.matrix(rcorrData_spear[["r"]])
rcorrData_spear_p = as.matrix(rcorrData_spear[["P"]])

# need to retain only valid columns
valid_cols <- apply(rcorrData_spear_r, 2, function(x) {
  sum(!is.na(x) & x != 1) > 0 }) # At least one valid non-diagonal correlation (we exclude those that are only correlatted to themselves)


filtered_rcorrData_spear_r <- rcorrData4_spear_r[valid_cols, valid_cols]
filtered_rcorrData_spear_p <- rcorrData4_spear_p[valid_cols, valid_cols]


#delete NAs for the plot
filtered_rcorrData4_spear_r_r0NAs = filtered_rcorrData4_spear_r
filtered_rcorrData4_spear_r_r0NAs[is.na(filtered_rcorrData4_spear_r_r0NAs)] <- 0
filtered_rcorrData4_spear_p_p0NAs = filtered_rcorrData4_spear_p
filtered_rcorrData4_spear_p_p0NAs[is.na(filtered_rcorrData4_spear_p_p0NAs)] <- 1

#filtered corr matrices
png("corrplotSpearman_pancreatic-PrGn-filtered.png", width=2000, height=2200, pointsize=10)
corrplot(rcorr_filteredData4_spear_r_r0NAs.filtered, method = "square", type = "lower", col = COL2('PRGn', 20), is.corr=TRUE, order="hclust", p.mat = rcorr_filteredData4_spear_p0NAs.filtered, sig.level = 0.05, insig = "blank", tl.col = "#252525")
dev.off()

#################################################################################################################
#The same but with Kendall correlation - as it also does not assume that you have linear relationships between the data and (as Spearman) no normality ##
###################################################################################################################
if(!require("corpcor")) install.packages("corpcor")
library(corpcor)
kendall_corr_pvalues <- function(df) {
  n <- ncol(df)
  # Initialize matrices to store correlation coefficients and p-values
  corr_matrix <- matrix(NA, ncol=n, nrow=n)
  pvalue_matrix <- matrix(NA, ncol=n, nrow=n)
  colnames(corr_matrix) <- colnames(df)
  rownames(corr_matrix) <- colnames(df)
  colnames(pvalue_matrix) <- colnames(df)
  rownames(pvalue_matrix) <- colnames(df)
  
  # Compute Kendall correlation and p-values
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        corr_matrix[i, j] <- 1
        pvalue_matrix[i, j] <- 0
      } else {
        # Drop missing values pairwise
        valid <- complete.cases(df[, i], df[, j])
        if (sum(valid) > 1) {  # Ensure there are enough observations
          test <- cor.test(df[valid, i], df[valid, j], method = "kendall")
          corr_matrix[i, j] <- test$estimate
          pvalue_matrix[i, j] <- test$p.value
        } else {
          corr_matrix[i, j] <- NA
          pvalue_matrix[i, j] <- NA
        }
      }
    }
  }
  
  return(list(correlation = corr_matrix, pvalue = pvalue_matrix))
  write.csv(corr_matrix, file="Kendall_corr_r.csv")
  write.csv(pvalue_matrix, file="Kendal_corr_p.csv")
}

result_Kendall_corpcor_data <- kendall_corr_pvalues(correlation_dataset_completerate0.5_colnames)
Kendallcorrelation_matrix <- result_Kendall_corpcor_data4$correlation
Kendallpvalue_matrix <- result_Kendall_corpcor_data4$pvalue
write.csv(Kendallcorrelation_matrix, file="Kendall_corpcor_r_pancreatic.csv", quote = FALSE)
write.csv(Kendallpvalue_matrix, file="Kendall_corpcor_p_pancreatic.csv", quote = FALSE)

#delete columns without correlations from the correlation matrix
valid_cols_Kendall <- apply(Kendallcorrelation_matrix, 2, function(x) {
  sum(!is.na(x) & x != 1) > 0 }) # At least one valid non-diagonal correlation (we exclude those that are only correlatted to themselves)
valid_cols_Kendall.filt <- apply(Kendallcorrelation_matrix.filt, 2, function(x) {
  sum(!is.na(x) & x != 1) > 0 }) # At least one valid non-diagonal correlation (we exclude those that are only correlatted to themselves)
filtered_Kendallcorrelation_matrix_r <- Kendallcorrelation_matrix [valid_cols_Kendall, valid_cols_Kendall]
filtered_Kendallcorrelation_matrix_p <- Kendallpvalue_matrix [valid_cols_Kendall, valid_cols_Kendall]
filtered_Kendallcorrelation_matrix_r_0NAs = filtered_Kendallcorrelation_matrix_r
filtered_Kendallcorrelation_matrix_r_0NAs[is.na(filtered_Kendallcorrelation_matrix_r_0NAs)] <- 0 # delete remaining NAs for the plot
filtered_Kendallcorrelation_matrix_p_0NAs = filtered_Kendallcorrelation_matrix_p
filtered_Kendallcorrelation_matrix_p_0NAs[is.na(filtered_Kendallcorrelation_matrix_p_0NAs)] <- 1
#plot
png("corrplotKendall_pancreatic-PrGn-filtered.png", width=2000, height=2200, pointsize=10)
corrplot(filtered_Kendallcorrelation_matrix_r_0NAs, method = "square", type = "lower", is.corr=TRUE, order="hclust", col = COL2('PRGn', 20), p.mat = filtered_Kendallcorrelation_matrix_p_0NAs, sig.level = 0.05, insig = "blank", tl.col = "#252525", tl.srt = 90)
dev.off()

