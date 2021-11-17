mod1 = lm(price ~ poly(length, degree = 2, raw = F) + 
          poly(width, degree = 2, raw = F) +
          poly(curb_weight, degree = 2, raw = F) +
          poly(engine_size, degree = 1, raw = F) +
          poly(horsepower, degree = 1, raw = F) +
          poly(city_mpg, degree = 4, raw = F) +
          poly(highway_mpg, degree = 3, raw = F) +
          compression_ratio + num_cylinders +
          engine_type + engine_location + drive_wheels +
          body_style + aspiration + make, df)


mod1 |> summary()

mod2 = lm(price ~ poly(length, degree = 2, raw = F) + 
                   poly(width, degree = 2, raw = F) +
                   poly(curb_weight, degree = 2, raw = F) +
                   poly(engine_size, degree = 1, raw = F) +
                   poly(horsepower, degree = 1, raw = F) +
                   poly(city_mpg, degree = 4, raw = F) +
                   poly(highway_mpg, degree = 3, raw = F) +
                   compression_ratio + num_cylinders +
                   engine_type + engine_location + drive_wheels +
                   body_style + aspiration, df)

mod2 |> summary()


step(mod1, direction = "both")
step(mod2, direction = "both")



aux = residual_graph_analyses(step(mod2, direction = "both"))
aux

aux2 = model_hyphotesys_analyses(step(mod2, direction = "both"))
aux2[1]
aux2[2]
aux2[3]
aux2[5]
