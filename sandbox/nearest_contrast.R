# test matrix
set.seed(13)
m <- matrix(sample(c(0, 1), size = 100, replace = TRUE, prob = c(.9, .1)), nrow = 10)
image(t(m))

k = 5
n_urban <- sum(m)
out_arr <- array(rep(NA, 2 * k * n_urban), dim = c(k, 2, n_urban))
urban_ij <- which(m == 1, arr.ind = TRUE)

for (g in 1:n_urban) {
  while (k_found) {
    hop <- 1
    if (urban_ij[g, 1] >= hop + 1) {
      L_neighbor <- m[urban_ij[g, 1] - hop, urban_ij[g, 2]]
      if (L_neighbor == F) {
        out_arr[1, , g] <-
      }
    }
  }
}
