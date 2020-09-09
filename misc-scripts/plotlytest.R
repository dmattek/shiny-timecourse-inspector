require(ggplot2)
require(plotly)

df = data.frame(x = rep(1:10, 10),
                y = rep(1:10, each = 10),
                id = rep(1:10, each = 10),
                r = rnorm(100))

p1=ggplot(df, aes(x = x, 
                  y = y,
                  group = id, label = id, label2 = r)) +
  geom_line()

ggplotly(p1)

ggplotly(p1, tooltip=c("x", "y", "label", "label2"))

