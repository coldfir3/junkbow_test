rm(list = ls())
library(tidyverse)

df = read.csv("~/dispersion.csv", header = TRUE) %>% as.tibble
names(df) <- c('Weapon', 'Distance', 'Damage')

df <- mutate(df, Distance = (8 - Distance)*4) %>% split(.$Weapon)

df$Junkbow$Hits <- df$Junkbow$Damage/17
df$Fafnir$Hits <- df$Fafnir$Damage/19
df$Nidhogg$Hits <- df$Nidhogg$Damage/23

df <- df %>% reduce(rbind) %>% mutate(Distance = as.factor(Distance))

## simulation ##

position <- function(x, alpha, beta){
  r = x * tan(alpha * pi/180)
  x <- r*cos(beta * pi/180)
  y <- r*sin(beta * pi/180)
  c(x,y)
}
hit <- function(x, alpha, beta, window = c(1,1)){
  p <- position(x, alpha, beta)
  all(abs(p) < window)
}
set.seed(1)
dfs <- NULL
for(j in 1:25){
  hits <- 0
  for(i in 1:8){
    alpha = abs(rnorm(1,0,6))
    beta = runif(1,0,360)
    x <- 0:8*4
    hits <- hits + map_lgl(x, hit, alpha, beta)
  }
  dfs <- rbind(dfs, tibble(Weapon = 'Junkbow', Distance = as.factor(x), Hits = hits))
}
for(j in 1:25){
  hits <- 0
  for(i in 1:8){
    alpha = abs(rnorm(1,0,5))
    beta = runif(1,0,360)
    x <- 0:8*4
    hits <- hits + map_lgl(x, hit, alpha, beta)
  }
  dfs <- rbind(dfs, tibble(Weapon = 'Fafnir', Distance = as.factor(x), Hits = hits))
}
for(j in 1:25){
  hits <- 0
  for(i in 1:8){
    alpha = abs(rnorm(1,0,4))
    beta = runif(1,0,360)
    x <- 0:8*4
    hits <- hits + map_lgl(x, hit, alpha, beta)
  }
  dfs <- rbind(dfs, tibble(Weapon = 'Nidhogg', Distance = as.factor(x), Hits = hits))
}

df$Src = 'Experiment'
dfs$Src = 'Simulation'
df$Weapon <- factor(df$Weapon, levels=c("Junkbow","Fafnir","Nidhogg")) 

df %>% mutate(Hits = as.factor(Hits)) %>%
  group_by(Distance, Weapon, Hits) %>% tally() %>%
  ggplot(aes(x = Distance, y = Hits, fill = n, label = n)) +
  geom_tile(col = 'white') +
  geom_text(col = 'white') +
  facet_grid(.~Weapon) +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white')) +
  viridis::scale_fill_viridis()


rbind(select(df, -Damage), dfs) %>% 
  mutate(Hits = as.factor(Hits)) %>%
  group_by(Distance, Weapon, Hits, Src) %>% tally() %>%
  ggplot(aes(x = Distance, y = Hits, fill = n, label = n)) +
  geom_tile(col = 'white') +
  geom_text(col = 'white') +
  facet_grid(Src~Weapon) +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white')) +
  viridis::scale_fill_viridis()

df %>%
  ggplot(aes(x = Distance, y = Hits)) +
  geom_jitter(alpha = 0.5, col = 'white', width = 0.15, height = 0.15)  +
  facet_grid(.~Weapon) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white'))


rbind(select(df, -Damage), dfs) %>% 
  ggplot(aes(x = Distance, y = Hits, fill = Src)) +
  geom_boxplot(col = 'white') + 
  facet_grid(Weapon~.) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white'))



x <- 0:90
dfss <- NULL
for(alpha in qnorm(seq(0.01,0.99,0.01), 0, 6)){
  r <- map(x, function(x) x * tan(alpha * pi/180)) %>% map(1) %>% reduce(c)
  dfss <- rbind(dfss, tibble(x = x, r = r, alpha = alpha, Weapon = 'Junkbow'))
}
for(alpha in qnorm(seq(0.01,0.99,0.01), 0, 5)){
  r <- map(x, function(x) x * tan(alpha * pi/180)) %>% map(1) %>% reduce(c)
  dfss <- rbind(dfss, tibble(x = x, r = r, alpha = alpha, Weapon = 'Fafnir'))
}

for(alpha in qnorm(seq(0.01,0.99,0.01), 0, 4)){
  r <- map(x, function(x) x * tan(alpha * pi/180)) %>% map(1) %>% reduce(c)
  dfss <- rbind(dfss, tibble(x = x, r = r, alpha = alpha, Weapon = 'Nidhogg'))
}

dfss$Weapon <- factor(dfss$Weapon, levels=c("Junkbow","Fafnir","Nidhogg")) 

dfss %>% ggplot(aes(x = x, y = r)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
  facet_grid(Weapon~.) +
  coord_cartesian(xlim = c(0, 45)) +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(colour = 'white'),
        axis.text = element_text(colour = 'white')) +
  viridis::scale_fill_viridis(option = "A")


