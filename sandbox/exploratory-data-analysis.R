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
  png(file = paste0("cell", i, ".png"), width = 1300, height = 1200)
  print(ggpairs(g))
  dev.off()
}

