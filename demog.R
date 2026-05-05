library(readr)

nascidos_vivos <- read_csv2("nascidos-vivos-curitiba.csv",
                            skip = 5,
                            locale = locale(encoding = "latin1"))

nascidos_vivos <- nascidos_vivos[1:(nrow(nascidos_vivos) - 10), ]


obitos <- read_csv2("mortalidade-curitiba.csv",
                                      skip = 5,
                                      locale = locale(encoding = "latin1"))

obitos <- obitos[1:(nrow(obitos) - 8), ]
