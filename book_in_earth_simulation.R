R0 <- 6371e3
G <- 6.67428e-11 # m^(3)*kg^(-1)*s^(-2)
e_dens_single <- function(r) {
  x <- abs(r / R0)
  if (abs(r) < 1221.5e3) q <- 13.0885 - 8.8381*x^2
  else if (abs(r) < 3480e3) q <- 12.5815 - 1.2638*x - 3.6426*x^2 - 5.5281*x^3
  else if (abs(r) < 5701e3) q <- 7.9565 -6.4761*x + 5.5283*x^2 - 3.0807*x^3
  else if (abs(r) < 5771e3) q <- 5.3197 - 1.4836*x
  else if (abs(r) < 5971e3) q <- 11.2494 - 8.0298*x
  else if (abs(r) < 6151e3) q <- 7.1089 - 3.8045*x
  else if (abs(r) < 6346.6e3) q <- 2.6910 + 0.6924*x
  else if (abs(r) < 6356e3) q <- 2.9
  else if (abs(r) < 6368e3) q <- 2.6
  else if (abs(r) <= 6371e3) q <- 1.020
  else q <- 0
  return(q)
}

e_dens <- function(r) {
  vapply(r, e_dens_single, numeric(1L))
}

g_field_single <- function(r) {
  if(r == 0) {
    g <- 0
  } else {
    int_fun <- function(x) x^2*e_dens(x)*1000 # zamiana jednostek z g/cm3 na kg/m3
    M <- 4 * pi * integrate(int_fun, lower = 0, upper = r)$value
    g <- - G * M/ r^2
    return(g)
  }
}

g_field <- function(r) {
  vapply(r, g_field_single, numeric(1L))
}

library(ggplot2)
N_R0 <- 1
q <- ggplot(data = data.frame(x = 0), aes = (x = x)) + 
  #geom_vline(xintercept = R0, color = "green", size = 1.0) +
  #geom_hline(yintercept = e_dens(R0), color = "blue", size = 1.0) +
  stat_function(fun = e_dens, size = 1.4, color = "lightsalmon2", n = 10001) + 
  ggtitle("Gestosc Ziemi") + 
  ylab("Gestosc Ziemi [g/cm3]") + 
  xlab("Odleglosc od srodka Ziemi r") + 
  scale_y_continuous(breaks = seq(0,20,2), limits = c(0, e_dens(0))) +
  scale_x_continuous(limits = c(0, N_R0 * R0), breaks = seq(0, 2*N_R0*R0, 1000e3), labels = paste(seq(0, 2*N_R0*R0/1000, 1000), "km"))
print(q)

N_R0 <- 2
g <- ggplot(data = data.frame(x = 0), aes = (x = x)) + 
  geom_vline(xintercept = R0, color = "green", size = 1.2) +
  stat_function(fun = function(x) -g_field(x), size = 1.5, color = "lightsalmon2", n = 10001) + 
  ggtitle("Sila grawitacji Ziemi Fg(r) dzialajaca na cialo o masie 1 kg") + 
  ylab("Sila Fg [N]") + 
  xlab("Odleglosc od srodka Ziemi r [m]") + 
  scale_x_continuous(limits = c(0, N_R0 * R0), breaks = seq(0, 2*N_R0*R0, 1000e3), labels = paste(seq(0, 2*N_R0*R0/1000, 1000), "km"))
print(g)

# FDTD
dt <- 1 # s
simul_time <- 25 # min
get_next_r <- function(r, rp, dt, g_fun) {
  2 * r - rp + g_fun(r) * dt ^ 2
}
t <- seq(-dt, simul_time*60, dt)
N <- length(t)
pos <- numeric(N)
pos[1] <- R0
pos[2] <- R0
for(k in 3:N) {
  pos[k] <- get_next_r(pos[k-1], pos[k-2], dt, g_field_single)
}
zero_time <- t[which.min(abs(pos))[1]]
zero_time_str <- paste(round(zero_time/60), "min", round(zero_time) - 60*round(zero_time/60), "s")

N_R0 <- 1
brks <- seq(-2*round(N_R0*R0, -6), 2*N_R0*R0, 1000e3)
rt <- ggplot(data = data.frame(r = pos, t = t), aes(x = t, y = r)) + 
  geom_line(size = 1.4, color = "lightsalmon2") + 
  stat_function(fun = function(x) R0 * cos(sqrt(abs(g_field_single(R0))/R0) * x), color = "blue") +
  geom_point(size = 3, aes(x = zero_time, y = 0)) +
  geom_text(aes(x = zero_time, y = 0, label = zero_time_str), nudge_y = 5e5) +
  ggtitle("Polozenie ksiazki spadajacej do srodka Ziemi w funkcji czasu") + 
  ylab("Polozenie ksiazki") + 
  xlab("Czas t [min]") +
  scale_x_continuous(breaks = 0:simul_time*60, labels = 0:simul_time, minor_breaks = NULL)+
  scale_y_continuous(limits = c(min(pos), N_R0 * R0), breaks = brks, labels = paste(brks/1000, "km"))
print(rt)

# FDTD - periodic
simul_time <- 200 # min
t <- seq(-dt, simul_time*60, dt)
N <- length(t)
pos <- numeric(N)
pos[1] <- R0
pos[2] <- R0
for(k in 3:N) {
  pos[k] <- get_next_r(pos[k-1], pos[k-2], dt, g_field_single)
}

N_R0 <- 1
brks <- seq(-2*round(N_R0*R0, -6), 2*N_R0*R0, 1000e3)
rt <- ggplot(data = data.frame(r = pos, t = t), aes(x = t, y = r)) + 
  geom_line(size = 1.4, color = "lightsalmon2") + 
  # stat_function(fun = function(x) R0 * cos(sqrt(abs(g_field_single(R0))/R0) * x), color = "blue") +
  ggtitle("Polozenie ksiazki spadajacej do srodka Ziemi w funkcji czasu") + 
  ylab("Polozenie ksiazki") + 
  xlab("Czas t [min]") +
  scale_x_continuous(breaks = seq(0,simul_time,10)*60, labels = seq(0,simul_time,10), minor_breaks = NULL)+
  scale_y_continuous(limits = c(min(pos), N_R0 * R0), breaks = brks, labels = paste(brks/1000, "km"))
print(rt)
