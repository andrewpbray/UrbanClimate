# load data
load(url("https://www.dropbox.com/s/v0cal2s54mizlfo/urban.rda?dl=1"))

# find urban gridcells
is_urban <- (rowSums(!is.na(urban[, , "Tu"])) > 0)

# make plots for 3 random urban gi
set.seed(304)
rand_cells <- sample(1:sum(is_urban), size = 3)
g1 <- as.data.frame(urban[which(is_urban)[rand_cells[1]], , ])
p1 <- ggpairs(g1)
g2 <- as.data.frame(urban[which(is_urban)[rand_cells[2]], , ])
p2 <- ggpairs(g2)
g3 <- as.data.frame(urban[which(is_urban)[rand_cells[3]], , ])
p3 <- ggpairs(g3)

png(file = "figs/cell1.png", width = 1100, height = 1200)
p1
dev.off()
png(file = "figs/cell2.png", width = 1100, height = 1200)
p2
dev.off()
png(file = "figs/cell3.png", width = 1100, height = 1200)
p3
dev.off()
