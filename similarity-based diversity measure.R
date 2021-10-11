## The function SimilarityBasedDiversity takes the 
## 1) "venuecat_matrix", which is a matrix of counts of amenities per venue category for each spatial unit, and
## 2) "totvenuesimilarity", which is a similarity measure based on shortest path matrix of all category nodes (McInnes BT, Pedersen T, Li Y, Melton GB and Pakhomov SV (2014) U-path: An undirected path-based measure of semantic similarity. AMIA - Annual Symposium proceedings. AMIA Symposium 2014: 882-891.)
## and calculates a similarity-adjusted diversity measurement. 
## The algorithm works by redistributing a nominal distribution over categories (venuecat_matrix) 
## according to the similarity measure between categories (given by totvenuesimilarity)
## and then measure Shannon entropy over this new distribution.

SimilarityBasedDiversity = function(venuecat_matrix, totvenuesimilarity){
  SBD_df = venuecat_matrix
  SBD_df[, c("ncat", "entropysum", "rowentropy")] = ""
  ComputationMatrix = venuecat_matrix
  colnameset <- colnames(venuecat_matrix)
  entropy <- function(mat) {
    freqs <- mat/rowSums (mat)
    entropy <- - rowSums (freqs * log2(freqs+0.000000001))
    entropy <- round (entropy, digits = 3)
    return (entropy)
  }
  for(n in 1:nrow(venuecat_matrix)){
    localvenuesubset <- which(venuecat_matrix[n,1:ncol(venuecat_matrix)] != 0)
    if(length(localvenuesubset) > 1){
      colnameset_local <- colnameset[localvenuesubset]
      localvenuesubset_entries <- venuecat_matrix[n, localvenuesubset]
      similarity_rows <- which(totvenuesimilarity$columnnames %in% colnameset_local[1:length(colnameset_local)])
      similarity_columns <- which(colnames(totvenuesimilarity) %in%  colnameset_local[1:length(colnameset_local)])
      for(g in 1:length(localvenuesubset)){
        ComputationMatrix[n,localvenuesubset[g]] <-  localvenuesubset_entries[g] * (sum(totvenuesimilarity[similarity_rows[g], similarity_columns]))
        for(p in 1:length(localvenuesubset)){
          if(p != g){
            ComputationMatrix[n,localvenuesubset[p]] <- (localvenuesubset_entries[p] * (1- totvenuesimilarity[similarity_rows[g], similarity_columns[p]]))
          }
        }
        SBD_df[n, localvenuesubset[g]] <- entropy(ComputationMatrix[n,])
      }
      SBD_df$ncat[n] <- length(localvenuesubset)
      SBD_df$entropysum[n] <- sum(SBD_df[n, 1:ncol(venuecat_matrix)])
      SBD_df$rowentropy[n] <- as.numeric(SBD_df$entropysum[n])/ as.numeric(SBD_df$ncat[n])
    }
    else if(length(localvenuesubset) == 1){
      SBD_df$ncat[n] <- length(localvenuesubset)
      SBD_df$entropysum[n] <- 0
      SBD_df$rowentropy[n] <- 0
    }
    if(n %% 100 == 0){
      print(paste("calculated ", n, " of ", nrow(venuecat_matrix)))
    }
  }
  return(SBD_df)
}

## Example application

# setting the directory
setwd("C:/Dokumente/Master Thesis/Master_these _ Latest/Data")

# reading shortest path matrix of all category nodes
totvenuesimilarity <- read.csv("totvenuesimilarity.csv", header=TRUE)
totvenuesimilarity <- subset(totvenuesimilarity, select = -c(X))
max(totvenuesimilarity[1:937, 1:937]) ## maximum shortest path is 9

# redistribute for 0 to be maximum dissimilarity and 1 maximum similarity (for standardization)
totvenuesimilarity[1:937, 1:937] <-(1-(totvenuesimilarity[1:937, 1:937]/9))

# reading the matrix of counts of amenities per venue category for each spatial unit
venuecat_matrix = read.csv("/Budapest/Budapestvenuecat_matrix_pre2017.csv", header = TRUE)

# make sure that the dataset is clean, so no rownumber is in first column (apply below code if it is)
# venuecat_matrix <- subset(venuecat_matrix, select = -c(X))

# applying the algorithm
Budapest2017 = SimilarityBasedDiversity(venuecat_matrix = venuecat_matrix, totvenuesimilarity = totvenuesimilarity)

# writing the final dataset
write.csv(Budapest2017, "Budapest2017_dissimilardiversity_matrix.csv")

