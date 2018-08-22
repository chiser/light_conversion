
## Info comes from here: http://www.dfisica.ubi.pt/~hgil/Fotometria/HandBook/ch07.html

## 1 Watt = 517.031 lm at 590 nm

## 1 lux = 1 lm/m2

## Therefore: 517.031 lux = 517.031 lm/m2 = 1 Watt/m2 = 1000mW/mm2 = 10mW/cm2

#### Converter function ####
lux_watt_converter <- function(lux, photopic_conversion = 517.031) {
  
  watt_squaredmeter = lux / photopic_conversion
  
  m_watt_squaredcm = watt_squaredmeter*10
  
  return(m_watt_squaredcm)
}


## Make a sequence fo lux, and convert it to watts
lux_scale <- seq(0,25000,1000)
watt_scale <- vector("numeric", length = length(lux_scale))

for (i in 1:length(lux_scale)){
  
  watt_scale[i] <- lux_watt_converter(lux_scale[i])
  
}
  
plot(lux_scale ~ watt_scale , xlab = "mW/cm²", ylab = "lux",type="l", lwd = 3)  

## Import calibration measurements
Tmaze <- read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = "," )
Joystick <- read.csv(file.choose(), header = TRUE, sep = ";", quote = "\"",dec = "," )

r=0.05  # the radius in cm of a 1mm diameter light guide

area_light_guide <- pi*r^2 # light guide area in cm²

Joystick$mW.cm2 <- Joystick$uW/(area_light_guide*1000)

## Tmaze fit compared to theoretical from formula above
plot(lux_scale ~ watt_scale , xlab = "mW/cm²", ylab = "lux",type="l", lwd = 3, main = "Tmaze")  
points(Tmaze$Lux~Tmaze$mW.cm2 , xlab = "mW/cm²", ylab = "lux", pch = 16, cex = 1.3,col = "red")
abline(lm(Tmaze$Lux~Tmaze$mW.cm2), col="red", lwd = 3) # regression line (y~x)

Tmaze_fit <- lm(Tmaze)
summary(Tmaze_fit)

text(100,25000,paste("y(mW/cm²) =", Tmaze_fit$coefficients[2], "x(lux)", Tmaze_fit$coefficients[1]))
text(100,24000,"Adjusted R-squared:  0.9861")
legend(0,23000,c("Theoretical","Measurement fit"),fill = c("black","red"))

## Joystick fit compared to theoretical from formula above
plot(lux_scale[1:3] ~ watt_scale[1:3] , xlab = "mW/cm²", ylab = "lux",type="l", lwd = 3, main = "Joystick") 
points(Joystick$Lux ~ Joystick$mW.cm2, pch = 16, cex = 1.3, col = "blue")
abline(lm(Joystick$Lux ~ Joystick$mW.cm2), col="blue", lwd = 3) # regression line (y~x)

Joystick_fit <- lm(Joystick$mW.cm2 ~ Joystick$Lux)
summary(Joystick_fit)

text(8.5,2000,paste("y(mW/cm²) =", Joystick_fit$coefficients[2], "x(lux) +", Joystick_fit$coefficients[1]))
text(4,1900,"Adjusted R-squared:  0.9962")
legend(0,1800,c("Theoretical","Measurement fit"),fill = c("black","blue"))


