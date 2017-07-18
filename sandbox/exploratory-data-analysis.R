library(GGally)

# load data
load(url("https://www.dropbox.com/s/v0cal2s54mizlfo/urban.rda?dl=1"))

# find urban gridcells
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)

# make plots for 3 random urban gridcells
set.seed(304)
n <- 3
rand_cells <- sample(1:sum(is_urban), size = n)

for (i in 1:n) {
  g <- as.data.frame(urban[which(is_urban)[rand_cells[i]], , ])
  png(file = paste0("cell", rand_cells[i], ".png"), width = 1300, height = 1200)
  print(ggpairs(g))
  dev.off()
}


# model a few gridcells

# slice off cells of interest and scale vars
urban_subset <- urban[which(is_urban)[rand_cells], , ] %>%
  alply(1)
urban_subset <- map(urban_subset, ~apply(.x, MARGIN = 2, scale))


# fit several SLRs
m <- map(urban_subset, ~lm(Tu ~ ., data = as.data.frame(.x)))

# cross-validation
k <- 5
u <- urban_subset[[1]]
for (i in 1:k) {
  test <- u
}


#

df <- as.data.frame(urban_subset[[1]])
ggplot(df, aes(x = TH, y = Tu)) +
  geom_point() +
  geom_smooth(method = "lm")
df_res <- data.frame(res = m[[1]]$res, time_ind = 1:dim(urban_subset[[1]])[1])
ggplot(df_res, aes(x = time_ind, y = res)) +
  geom_point()

models <- apply(urban_subset, MARGIN = 1, FUN = SLR, var = "TH")


