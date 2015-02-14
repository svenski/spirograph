
library(ggplot2)
library(animation)

main <- function() {

  earth.r <- 2
  earth.omega <- 0.05 # rad/time unit

  moon.omega <- earth.omega*12
  moon.r <- 0.2

  earth.start <- c(earth.r, 0, 1)

  moon.start <- c(earth.r + moon.r, 0, 1)

  saveGIF(tracePositions(), "earth.moon.gif", interval=0.1)

}

tracePositions <- function() {
  lapply(seq(0, 100, by = 0.5), function(x) { plotEarthMoonAt(x) })
}

plotEarthMoonAt <- function(tt) {

  earth.pos <- pos.at(tt, earth.omega, earth.start)
  moon.pos <- moon.pos.at(tt, earth.omega, earth.r, earth.start, moon.omega, moon.start)

  p <- ggplot() + 
  geom_segment(aes_string(x = 0, y = 0,
                   xend = earth.pos[1], 
                   yend = earth.pos[2])) + 
  geom_segment(aes_string(x = earth.pos[1], 
                   y = earth.pos[2],
                   xend = moon.pos[1], 
                   yend = moon.pos[2]), colour = "red") + 
  xlim(-3, 3) + ylim(-3, 3)

  print(p)
}

moon.pos.at <- function(t, earth.omega, earth.r, earth.start, moon.omega, moon.start) {

  earth.pos <- rotate(earth.omega, t) %*% earth.start 
  earth.trans <- earth.pos[1:2]
  moon.pos.rel.earth <- rotate(moon.omega, t) %*%  (moon.start - earth.start)
  moon.pos.rel.earth[3] <- 1

  tt <- translate(earth.trans)
  moon.pos <- translate(earth.trans) %*% moon.pos.rel.earth
}

pos.at <- function(t, omega, start) {
  rotate(omega, t) %*% start
}

translate <- function(v) {
  mm <- matrix(c(1, 0, v[1], 
                 0, 1, v[2], 
                 0, 0, 1), nrow =3, byrow = TRUE)
}

rotate <- function(omega, t) {
  tt <- omega*t
  mm <- matrix(c(cos(tt), -sin(tt), 0, 
                 sin(tt), cos(tt),  0, 
                 0,       0,        1), 
               nrow = 3, byrow = TRUE)
}
