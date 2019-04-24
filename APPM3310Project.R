library(ggplot2)
library(matrixcalc)
theme_set(theme_minimal())

# Load and inspect data
df <- read.csv("~/Downloads/matrix_data.csv")
head(df)
dim(df)
summary(df)

# Convert dates for time series plotting
for(i in 1:nrow(df)){
  yDay <- df$Yday[i]
  numYear <- substring(yDay, 1, 4)
  numDay <- as.integer(substring(yDay, 5, nchar(yDay)))
  year <- paste(numYear,"-01-01",sep="")
  stamp <- format(as.Date(numDay, origin=year))
  df$Yday[i] <- stamp
}

head(df)

# Subset the data
dfSS <- subset(df, Yday > as.Date("2010-01-01"))

# Check the data
head(dfSS)
dim(dfSS)

# Plot the data
ggplot(data = dfSS, aes(x = Yday, y = Extent, group = 1))+
  geom_line(color = "#00AFBB", size = 1)

# Construct trajectory matrix
n <- 15
m <- 5
k <- n - m + 1
T <- matrix(NA, nrow=m, ncol=k)
extents <- df$Extent
for (i in 1:m) {
  T[i,] <- extents[i:(k+i-1)]
}
print(dim(T))
print(T)

# Create lag-covariance matrix R=XX^T
R <- T %*% t(T)
print(R)

# Compute Singular Value Decomposition (SVD)
SVD <- svd(R)
print(SVD)
