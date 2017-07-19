# Note that this function has a flaw: it only searches 8 cells in each ring, no
# matter how big. As a result it over-estimates the distance to rural cells.

nearest_contrast <- function(mat, k) {
  n_urban <- sum(mat)
  out_arr <- array(rep(NA, 2 * k * n_urban), dim = c(k, 2, n_urban))
  dimnames(out_arr)[[3]] <- rep(NA, n_urban)
  urban_ij <- which(mat == 1, arr.ind = TRUE)

  for (g in 1:n_urban) {
    k_ijg <- c()
    k_found <- 0
    hop <- 0
    while (k_found < k) {
      hop <- hop + 1
      if (urban_ij[g, 1] >= hop + 1) {
        L_neighbor <- mat[urban_ij[g, 1] - hop, urban_ij[g, 2]]
        if (!L_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1] - hop, urban_ij[g, 2]))
        }
      }
      if ((urban_ij[g, 1] >= hop + 1) && (urban_ij[g, 2] <= dim(mat)[2] - hop)) {
        UL_neighbor <- mat[urban_ij[g, 1] - hop, urban_ij[g, 2] + hop]
        if (!UL_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1] - hop, urban_ij[g, 2] + hop))
        }
      }
      if (urban_ij[g, 2] <= dim(mat)[2] - hop) {
        U_neighbor <- mat[urban_ij[g, 1], urban_ij[g, 2] + hop]
        if (!U_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1], urban_ij[g, 2] + hop))
        }
      }
      if ((urban_ij[g, 2] <= dim(mat)[2] - hop) && (urban_ij[g, 1] <= dim(mat)[1] - hop)) {
        UR_neighbor <- mat[urban_ij[g, 1] + hop, urban_ij[g, 2] + hop]
        if (!UR_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1] + hop, urban_ij[g, 2] + hop))
        }
      }
      if (urban_ij[g, 1] <= dim(mat)[1] - hop) {
        R_neighbor <- mat[urban_ij[g, 1] + hop, urban_ij[g, 2]]
        if (!R_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1] + hop, urban_ij[g, 2]))
        }
      }
      if ((urban_ij[g, 1] <= dim(mat)[1] - hop) && (urban_ij[g, 2] >= 1 + hop)) {
        LwR_neighbor <- mat[urban_ij[g, 1] + hop, urban_ij[g, 2] - hop]
        if (!LwR_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1] + hop, urban_ij[g, 2] - hop))
        }
      }
      if (urban_ij[g, 2] >= 1 + hop) {
        Lw_neighbor <- mat[urban_ij[g, 1], urban_ij[g, 2] - hop]
        if (!Lw_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1], urban_ij[g, 2] - hop))
        }
      }
      if ((urban_ij[g, 1] >= 1 + hop) && (urban_ij[g, 2] >= 1 + hop)) {
        LwL_neighbor <- mat[urban_ij[g, 1] - hop, urban_ij[g, 2] - hop]
        if (!LwL_neighbor) {
          k_ijg <- append(k_ijg, c(urban_ij[g, 1] - hop, urban_ij[g, 2] - hop))
        }
      }
      k_found <- length(k_ijg)/2
    }
    k_ijg <- matrix(k_ijg[1:(2 * k)], ncol = 2)
    out_arr[ , , g] <- k_ijg
    dimnames(out_arr)[[3]][g] <- paste0(hop)
  }
  return(out_arr)
}


# example
# set.seed(13)
# mat <- matrix(sample(c(0, 1), size = 100, replace = TRUE, prob = c(.9, .1)), nrow = 10)
# image(mat)
# nearest_contrast(m, k = 10)


# Check max hop from urban to rural
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)
is_urban_mat <- matrix(is_urban, nrow = 288)

k <- 2
nn_arr <- nearest_contrast(is_urban_mat, k = k)
max_ring <- strtoi(dimnames(nn_arr)[[3]])
df <- data.frame(max_ring)
library(tidyverse)
ggplot(df, aes(x = factor(max_ring))) +
  geom_bar() +
  theme_bw() +
  xlab(paste0("Distance to nearest k=", k, " rural cells"))

