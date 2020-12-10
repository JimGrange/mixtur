
# install the hexSticker package
# remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)
library(ggplot2)
library(RColorBrewer)
library(mixtur)

# get the density data for different values of k
k <- seq(from = 2, to = 0.2, length.out = 8)
k <- exp(k)
x <- seq(from = -pi, to = pi, length.out = 500)
y <- vonmisespdf(x, 0, k[1])
# pass all to a data frame
data <- data.frame(
  x = x, 
  y = y, 
  k = k[1]
)

# loop and add to data frame other values for k
for(i in 2:length(k)){
  y <- vonmisespdf(x, 0, k[i])
  curr_data <- data.frame(
    x = x, 
    y = y, 
    k = k[i]
  )
  data <- rbind(data, curr_data)
}

# do the plot!
plot <- ggplot(data, aes(x = x, y = y, group = k)) + 
  geom_line(aes(col = as.factor(k)), 
            size = 0.4) + 
  scale_color_brewer(palette = "Set3") +
  theme_void() + 
  theme_transparent() + 
  theme(legend.position = "none")

# save it as a hex sticker
sticker(plot, package="mixtur", 
        h_color = "#0063B2",
        spotlight = FALSE,
        white_around_sticker = FALSE,
        p_size = 10, 
        p_y = 1.5,
        s_x = 1, 
        s_y = 0.82, 
        s_width = 1.3, 
        s_height = 1,
        filename = "mixtur_logo.png", 
        dpi = 1200)





