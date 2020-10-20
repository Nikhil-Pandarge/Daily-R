library(datasets)
data("iris")
library(dplyr)

?iris

iris %>%
  filter(Species == "virginica") %>%
  mutate(mean_Length = mean(Sepal.Length))

tail(iris)

apply(iris[,1:4], 1, mean)

data("mtcars")
?mtcars

with(mtcars,tapply(mpg,cyl,mean))

mean(mtcars$mpg,mtcars$cyl)

sapply(split(mtcars$mpg,mtcars$cyl),mean)

tapply(mtcars$mpg,mtcars$cyl,mean)

tapply(mtcars$cyl,mtcars$mpg,mean)

sapply(mtcars,cyl,mean)

apply(mtcars, 2, mean)

debug(ls)

library(dplyr)
horsepower_4 <- mtcars %>%
  filter(cyl == 4) %>%
  lapply(hp,mean)

head(mtcars)
tail(mtcars)
mean(mtcars[mtcars$cyl == "8",]$hp) - mean(mtcars[mtcars$cyl =="4",]$hp)
sapply(split(mtcars$mpg,mtcars$cyl),mean)

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

set.seed(1)
rpois(5,2)

set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
system.time(2+3*2^6)
ipv4_address <- "11000000101010000000000100000101"
nchar(ipv4_address)

library(ggplot2)
library(datasets)
data("mtcars")
# Change the color aesthetic to a size aesthetic
ggplot(mtcars, aes(wt, mpg, color = disp,size=disp)) +
  geom_point() + geom_smooth()

data("mtcars")

library(dplyr)
mtcars <- mtcars %>% 
  mutate(fcyl = factor(cyl), fam = factor(am))
ggplot(mtcars,aes(x = mpg, y = fcyl)) + geom_point()

head(mtcars)

ggplot(mtcars,aes(x = fcyl, y = mpg)) + geom_point()

ggplot(mtcars,aes(x = wt, y = mpg, fill = fcyl)) + geom_point(shape = 1, size = 4)

ggplot(mtcars,aes(x = wt, y = mpg, fill = fcyl)) + geom_point(shape = 21, size = 4, alpha = 0.6)

ggplot(mtcars,aes(x = wt, y = mpg, fill = fcyl, color = fam)) + geom_point(shape = 21, size = 4, alpha = 0.6)

palette <- c(automatic = "#377EB8", manual = "#E41A1C")

# Set the position
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar(position = 'dodge') +
  labs(x = "Number of Cylinders", y = "Count")
  scale_fill_manual("Transmission", values = palette)

ggplot(mtcars, aes(mpg, 0)) +
    geom_jitter() +
    # Set the y-axis limits
    ylim(-2,2)
# Plot price vs. carat, colored by clarity
plt_price_vs_carat_by_clarity <- ggplot(diamonds, aes(carat, price, color = clarity))

# Set transparency to 0.5
plt_price_vs_carat_by_clarity + geom_point(alpha = 0.5, shape = 16)

plt_price_vs_carat_by_clarity

# Plot base
plt_mpg_vs_fcyl_by_fam <- ggplot(mtcars, aes(fcyl, mpg, color = fam))

# Default points are shown for comparison
plt_mpg_vs_fcyl_by_fam + geom_point()

# Alter the point positions by jittering, width 0.3
plt_mpg_vs_fcyl_by_fam + geom_point(position = position_jitter(width = 0.3))

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  # Use a jitter position function with width 0.1
  geom_point(alpha = 0.5,position = position_jitter(width = 0.1))

ggplot(mtcars, aes(mpg, fill = fam)) +
  # Change the position to dodge
  geom_histogram(binwidth = 1)
library(carData)
data("Vocab")
# Plot education, filled by vocabulary
ggplot(Vocab, aes(education, fill = vocabulary)) +
  # Add a bar layer with position "fill"
  geom_bar(position = "fill")

# Plot education, filled by vocabulary
ggplot(Vocab, aes(education, fill = vocabulary)) +
  # Add a bar layer with position "fill"
  geom_bar(position = "fill") +
  # Add a brewer fill scale with default palette
  scale_fill_brewer()
data("economics")
head(economics)

ggplot(economics,aes(date,unemploy)) + geom_line()
