library(ggplot2)

# 2
Zx2 = c(64, 40, 30, 71, 55, 31, 61, 42, 57)
Zy2 = c(66, 79, 98, 65, 76, 83, 68, 80, 72)
groupA = matrix(c(Zx2, Zy2),nrow=9,ncol=2)
homework2.data = data.frame(Zx2, Zy2)
cor(Zx2,Zy2)
scatter = ggplot(homework2.data, aes(x = Zx2, y = Zy2))+geom_point()
scatter

# 4
Zx4 = c(5, 3, 4, 10, 15)
Zy4 = c(25, 20, 21, 35, 38)
cor(Zx4,Zy4)

# 5
Zx5 = c(17, 13, 12, 15, 16, 14, 16, 16, 18, 19)
Zy5 = c(94, 73, 59, 80, 93, 85, 66, 79, 77, 91)
cor(Zx5,Zy5)
lm(Zy5~Zx5)
