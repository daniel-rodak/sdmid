# Uzywamy jednostek SI
R0 <- 6378140 # m
M0 <- 5.9722e24 # kg
G <- 6.67428e-11 # m^(3)*kg^(-1)*s^(-2)
V <- function(r) (4/3) * pi * r^3
Fg <- function(r, m = 1, R0 = 6378140, M0 = 5.9722e24, G = 6.67428e-11) {
  grav <- ifelse(r > R0, G * m * M0 / r ^ 2, G * m * M0 * r / (R0 ^ 3))
  return(grav)
}

library(ggplot2)
N_R0 <- 4
g <- ggplot(data = data.frame(x = 0), aes = (x = x)) + 
  geom_vline(xintercept = R0, color = "green", size = 1.2) +
  stat_function(fun = Fg, size = 1.5, color = "lightsalmon2") + 
  ggtitle("Sila grawitacji Ziemi Fg(r) dzialajaca na cialo o masie 1 kg") + 
  ylab("Sila Fg [N]") + 
  xlab("Odleglosc od srodka Ziemi r [m]") + 
  scale_x_continuous(breaks = R0 * 0:N_R0, labels = c(0, "R0", paste(2:N_R0, "R0", sep = "*")), limits = c(0, N_R0 * R0))
g

t_k=pi/2*sqrt(R0^3/(G*M0))
print(paste("Czas spadania ksiazki na Ziemie to",
            round(t_k), "s =",
            round(t_k/60), "min", round(t_k) - 60*round(t_k/60), "s."))
