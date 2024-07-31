#Code to plot best fit line for the given data points and find the concentration of the unknown 

x_values <- c(0, 1, 2, 3, 4, 5)
y_values <- c(0, 0.051, 0.135, 0.208, 0.349, 0.432)


# Fit a linear regression line
fit <- lm(y_values ~ x_values)

# Extract the slope and intercept of the line
slope <- coef(fit)[2]
intercept <- coef(fit)[1]

# Calculate the x-value corresponding to y = 0.266
y_new <- 0.266
x_new <- (y_new - intercept) / slope
x_new

# Plot the points
plot(x_values, y_values, type = "o", col = "blue", 
     xlim = c(0, 6), ylim = c(0, .6), 
     xlab = "Concentration of Potassium (mmol)", ylab = "Absorbance at 530 nm", 
     main = "potassium estimation standard curve")

# Add the best-fit line
abline(fit, col = "red")

# Add labels to each point
text(x_values, y_values, labels = y_values, pos = 3)

# Add the point corresponding to y = 0.266
points(x_new, y_new, col = "green", pch = 19)

# Draw a dashed line from the point to the best-fit line
segments(x_new, y_new, x_new, slope * x_new + intercept, lty = 2)

# Add a horizontal dashed line at the y-value corresponding to y = 0.266
abline(h = y_new, col = "black", lty = 2)

# Add a vertical dashed line from the x-axis to the point (x_new, y_new)
segments(x_new, 0, x_new, y_new, lty = 2, col = "black")

# Add a legend
legend("topright", legend = c("Best-fit line", "Point corresponding to y = 0.266"), 
       col = c("red", "green"), pch = c(NA, 19), lty = c(1, 2), cex = 0.5)

