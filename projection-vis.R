library(ggplot2)
library(dplyr)

# movie data
movies <- read.table('data/movies.txt', sep='\t', header=F, quote='"',
                     col.names=c(
  'id', 'title', 'unknown', 'action', 'adventure', 'animation', 'childrens', 'comedy',
  'crime', 'documentary', 'drama', 'fantasy', 'filmnoir', 'horror', 'musical', 'mystery',
  'romance', 'scifi', 'thriller', 'war', 'western'))

# rating data
data <- read.table('data/data.txt', sep='\t', header=F, col.names=c('i', 'j', 'y'))

# compute # of ratings for each movie, and extract the most popular 10
movies$n <- unlist(lapply(movies$id, function(id) sum(data$j == id)))
popular10 <- arrange(movies, -n)[1:10,]

# remove movies with 0 ratings
movies <- movies[movies$n > 0,]

# compute mean rating for each movie, and extract the best rated ones with >50 reviews
movies$r <- unlist(lapply(movies$id, function(id) mean(data$y[data$j == id])))
best10 <- arrange(movies[movies$n>mean(movies$n),], -r)[1:10,]

# our choices for movies in each of our chosen categories
action10 <- c(
  50, 172, 181, # star wars
  228, 229, 230, # star trek
  174, 210, # indiana jones
  82, 252) # jurassic park
animation10 <- c(1, 71, 95, 99, 114, 206, 422, 542, 588, 596)
musical10 <- c(132, 143, 419, 420, 432, 538, 543, 473, 21, 155)
any10 <- c(
  29, 231, 254, 403, # batman
  96, 195, # Terminator
  144, 226, 550, # Die Hard
  11)

# bind the projection coordinates to the movie information dataframe
bind_proj <- function(proj) {
  m <- data.frame(movies)
  m$x1 <- unlist(lapply(m$id, function(id) {
    r <- which(proj$id == id)
    if (length(r) != 1) {
      NA
    } else {
      proj$x1[r]
    }}))
  m$x2 <- unlist(lapply(m$id, function(id) {
    r <- which(proj$id == id)
    if (length(r) != 1) {
      NA
    } else {
      proj$x2[r]
    }}))
  m
}

# plot one group of ids
make_one <- function(m, ids, title) {
  a <- m$id %in% ids
  ggplot(m[a,], aes(x=x1, y=x2, label=m$title[a])) +
    geom_text(hjust='right', vjust='top') +
    geom_point() +
    labs(x="", y="", title=title)
}

# make all necessary plots for one factorization method
make_plots <- function(proj, method) {
  m <- bind_proj(proj)
  list(
    any = make_one(m, any10, paste(method, "Batman vs Terminator vs Die Hard")),
    popular = make_one(m, popular10$id, paste(method, "Top 10 Popular Movies")),
    best = make_one(m, best10$id, paste(method, "Top 10 Best Movies")),
    action = make_one(m, action10, paste(method, "Action Movies")),
    animation = make_one(m, animation10, paste(method, "Animations")),
    musical = make_one(m, musical10, paste(method, "Musicals")))
}

# save plots
save_one <- function(a, n, pre) {
  ggsave(paste('plots/', pre, '_', n, '.png', sep=""), a)
}
save_plots <- function(p, pre) {
  for (n in names(p)) {
    save_one(p[[n]], n, pre)
  }
}

# create plots for method 2
p2 <- make_plots(read.csv('movies-2d.csv'), '[Method 2]')
p2a <- list()
p2a$any <- p2$any + lims(x=c(-.3, .7))
p2a$popular <- p2$popular + lims(x=c(-.5,.5))
p2a$best <- p2$best + lims(x=c(-.75, .13))
p2a$action <- p2$action + lims(x=c(-.2, .45))
p2a$musical <- p2$musical + lims(x=c(-.7, .65))
p2a$animation <- p2$animation + lims(x=c(-1.1, .6))
save_plots(p2a, 'm2')

# create plots for method 3 (surpriselib)
proj3 <- read.csv('surprise2.csv')
proj3 <- data.frame(
  id = 1:length(proj3$Column1),
  x1 = proj3$Column1,
  x2 = proj3$Column2)
p3 <- make_plots(proj3, '[Surprise]')
p3a <- list()
p3a$any <- p3$any + lims(x=c(-1.1, .25))
p3a$popular <- p3$popular + lims(x=c(-.8, 1.1))
p3a$best <- p3$best + lims(x=c(-1.5, 1))
p3a$action <- p3$action + lims(x=c(-.8, 1))
p3a$musical <- p3$musical + lims(x=c(-1.2, .8))
p3a$animation <- p3$animation + lims(x=c(-1.5, .7))
save_plots(p3a, 'm3')

# create plots for method 1
p1 <- make_plots(read.csv('method1.csv'), '[Method 1]')
p1a <- list()
p1a$any <- p1$any + lims(x=c(.9, 2.2))
p1a$popular <- p1$popular + lims(x=c(1.5, 2.4))
p1a$best <- p1$best + lims(x=c(2.225, 2.4))
p1a$action <- p1$action + lims(x=c(1.3, 2.4))
p1a$musical <- p1$musical + lims(x=c(1.3, 2.1))
p1a$animation <- p1$animation + lims(x=c(1.15, 2.4))
save_plots(p1a, 'm1')
