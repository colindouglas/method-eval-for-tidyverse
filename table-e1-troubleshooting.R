alpha <- 0.05

clsi_n <- c(seq(20, 100, by = 10), 150, 200, 250, 300, 400, 500, 1000)
paper_n <- c(50, 60, 70, 80, 90, 100, 150, 200, 250, 300)
minus_n <- c( 5,  6,  7,  8,  8,   9,  12,  16,  19,  22)


paper_n - floor(paper_n * 0.95)


df <- tibble(
  N = paper_n,
  minus_n,
  boundary = N - minus_n,
  boundary/N)


ggplot(df, aes(x = N, y = minus_n)) +
  geom_point() +
  geom_line()






stdev <- 1



upper <- stdev*sqrt((df$n-1)/qchisq(p = alpha/2, df = df$n-1))
lower <- stdev*sqrt((df$n-1)/qchisq(p = 1-alpha/2, df = df$n-1))
                                    