ggplot() +
  geom_point(size = 1, colour = "foo", pch = 2)

foo <- "geom_col(size = 3)"

ggplot() +
  geom_point(aes(1, 5)) +
  geom_line(linewidth = 50, aes(x = thing, y = thang), "another_not_aes_arg")

ggplot() +
  geom_label_repel(aes = aes(x = ee, y = yy))

ggplot() +
  geom_point()
