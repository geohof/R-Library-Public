#require(animation)
require(stringr)
sim.col <- "limegreen"
Plot <- function(stage){
x <- seq(from = 0, to = 1, length.out = 1001)

mean <- .2
st.dev <- .1
z <- mean * (1 - mean) / (st.dev ^ 2) - 1
alpha <- mean * z
beta <- (1 - mean) * z

tiv <- 1e2
ded <- .2
lim <- .5

y <- pmin(lim, pmax(0, x - ded)) 


p <- dbeta(x, alpha, beta)

n <- 10

if(stage == -11){
  r = numeric(0)
}else if(stage == -10){
  r = .1
}else if(stage == -9){
  r = c(.1, .4)
}else if(stage == -8){
  r = c(.1, .4, .8)
}else if(stage == -5){
  r = numeric(0)
}else if(stage >= 0){
  n <- stage
  r <- rbeta(n, alpha, beta)
}

s <- pmin(lim, pmax(0, r - ded)) 

if(stage >= 0){
  s0 <- s[s>0 & s< lim]
  d <- density(s, from = 0.03, to = lim, adjust = 1)
  d.f <- d$x>0 & d$x < lim
  p.f <- x>=ded +.03 & x<= ded + lim
}

par(cex.axis = 1.6, cex.lab = 2)
layout(matrix(c(1, 6, 6, 2, 6, 6, 3, 4, 5), nrow = 3), widths = c(1,2,4), heights = c(4,2,1))
par(mar = c(0, 5, 0, 0) + 0.1)
plot(x,y, xaxt = "n", yaxt = "n", type = "n", ylab = "Gross loss")
axis(2, seq(0, 1, length.out = 6), c(0, str_c((1:4) * 2, "00k"), "1m"), lwd = 5)
points(rep(.5, length(s)), s, pch = 16, 
       col = sim.col, cex = 2)


par(mar = c(0, 0, 0, 0) + 0.1)
m <- max(p[p.f])
plot(c(0, m), c(0,lim), xaxt = "n", yaxt = "n", type = "n")

if(stage >=-5)
  lines(p[p.f], x[p.f] - ded, lwd = 5, col = "blue")
point.mass1 <- pbeta(ded, alpha, beta)
point.mass2 <- sum(s == 0) / n
if(stage >=0)
  rect(xleft = 0, xright = m * point.mass2, ybottom = -.0, ytop = .015, col = sim.col, lwd = 2)
if(stage >=-5)
  rect(xleft = 0, xright = m * point.mass1, ybottom = -.015, ytop = .0, col = "blue", lwd = 2)

# d2 <- density(s, adjust = 1)
# lines(d2$y, d2$x, lwd = 5, col = sim.col)
if(stage >= 0)
  lines(d$y[d.f], d$x[d.f], lwd = 5, col = sim.col)
#lines(density(s))

par(mar = c(0, 0, 0, 0) + 0.1)
plot(x,y, xaxt = "n", yaxt = "n", type="n")
lines(x,y, lwd = 4)
points(r, s, pch = 16, 
       col = sim.col, cex = 2)

par(mar = c(0, 0, 0, 0) + 0.1)
plot(x,p, xaxt = "n", yaxt = "n", type = "n")
if(stage >=-5)
  lines(x, p, lwd = 5, col = "blue")
if(stage >=0)
  lines(density(r), lwd = 5, col = sim.col)


par(mar = c(5, 0, 0, 0) + 0.1)
plot(x,y, yaxt = "n", axes = FALSE, type = "n", xlab = "Ground-up loss")
box()
axis(1, seq(0, 1, length.out = 6), c(0, str_c((1:4) * 2, "00k"), "1m"), lwd = 5)
points(r, rep(lim / 2, length(r)), pch = 16, 
       col = sim.col, cex = 2)
barplot(horiz = TRUE, height = stage, xlim = c(10, 1E6), log = "x", xlab = "Sample size")

}

Png <- function(stage){
png(filename = str_c("st", stage, ".png"), width = 720, height = 720, res = 120)
Plot(stage)
dev.off()
}

Png(10)
Png(100)
Png(1000)
Png(10000)
Png(100000)
Png(1000000)




png(filename = "error.png", width = 720, height = 720, res = 120)

par(cex.axis = 1.6, cex.lab = 2)
par(mar = c(5, 5, 4, 2))
mean <- .2
st.dev <- .2
z <- mean * (1 - mean) / (st.dev ^ 2) - 1
alpha <- mean * z
beta <- (1 - mean) * z

tiv <- 1e2
ded <- .2
lim <- .5

sample.size <- c(10, 100, 1000, 10000, 1000000, 1000000)
num.x <- length(sample.size)
plot(c(0.5, num.x + .5), c(0, .2), typ = "n", xaxt = "n", yaxt = "n", 
     xlab = "Sample size", ylab = "Mean Loss")
axis(2, seq(0, 1, length.out = 11), c(0, str_c((1:9), "00k"), "1m"), lwd = 5)
axis(1, labels = c("10", "100", "1k", "10k", "100k", "1m"), at = 1:num.x)
n <- 1e6
r <- rbeta(n, alpha, beta)
s <- pmin(lim, pmax(0, r - ded))
m <- mean(s)
lines(c(.5, num.x + .5), c(m, m), lwd = 4, col = "blue")

for(i in 1:num.x){
  n <- sample.size[i]
  r <- rbeta(n, alpha, beta)
  s <- pmin(lim, pmax(0, r - ded))
  m <- mean(s)
  std <- sd(s)
  l <- 2 * std / sqrt(n)
  rect(xleft = i-.4, xright = i+.4, ybottom = m - l, ytop = m + l, lwd = 4, border = sim.col)
  lines(x = c(i - .4, i + .4), y = c(m, m), lwd = 4, col = sim.col)
}

dev.off()









require(animation)
saveGIF(ani.width=720, ani.height=720, interval=.1,  res = 120,
    		movie.name="RPAnimation.gif",
				outdir=paste(getwd(),"/Animation", sep=""), 
  {
  for(i in 1:60){
    n <- floor(10 * 1.2 ^ i)
    n <- min(100000, n)
    Plot(stage = n)
  }
  ani.pause(interval = 3)
  
}
)



