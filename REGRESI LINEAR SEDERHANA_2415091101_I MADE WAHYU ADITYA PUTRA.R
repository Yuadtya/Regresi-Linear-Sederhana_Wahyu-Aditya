#UAS STATISTIK DAN PROBABILITAS
#2415091101_I MADE WAHYU ADITYA PUTRA_SI 1 DENPASAR
#REGRESI LINEAR SEDERHANA

library(ggplot2)

# Mengatur seed
set.seed(123)

#Data
n <- 1000  # Jumlah observasi
X <- rnorm(n, mean = 50, sd = 10)  # Variabel independen
Y <- 5 + 2 * X + rnorm(n, mean = 0, sd = 10)  # Variabel dependen dengan noise

# Menggabungkan data ke dalam data frame
data <- data.frame(X, Y)

# Regresi linear
model <- lm(Y ~ X, data = data)

# Uji normalitas dengan Shapiro-Wilk
shapiro_test <- shapiro.test(residuals(model))

# Plot residual untuk memeriksa homoscedasticity
plot(model$fitted.values, residuals(model), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Plot Residuals")
abline(h = 0, col = "red")

#Hasil uji normalitas
print(shapiro_test)

#Ringkasan model
summary(model)


ggplot(data, aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regresi Linear Sederhana", x = "Variabel Independen (X)", y = "Variabel Dependen (Y)") +
  theme_minimal()  

