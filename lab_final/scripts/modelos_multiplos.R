library(MASS)

mod1 = lm(price ~ poly(length, degree = 2, raw = F) + 
          poly(width, degree = 2, raw = F) +
          poly(curb_weight, degree = 2, raw = F) +
          poly(engine_size, degree = 1, raw = F) +
          poly(horsepower, degree = 1, raw = F) +
          poly(city_mpg, degree = 4, raw = F) +
          poly(highway_mpg, degree = 3, raw = F) +
          compression_ratio + num_cylinders +
          engine_type + engine_location + drive_wheels +
          body_style + aspiration, df)

mod1 |> summary()

mod2 = lm(price ~ poly(length, degree = 2, raw = F) + 
            poly(width, degree = 2, raw = F) +
            poly(curb_weight, degree = 2, raw = F) +
            poly(engine_size, degree = 1, raw = F) +
            poly(horsepower, degree = 1, raw = F) +
            poly(city_mpg, degree = 3, raw = F) +
            poly(highway_mpg, degree = 3, raw = F) +
            compression_ratio + num_cylinders +
            engine_type + engine_location + drive_wheels +
            body_style + aspiration, df)


mod3 = lm(price ~ poly(length, degree = 2, raw = F) + 
            poly(width, degree = 2, raw = F) +
            poly(curb_weight, degree = 2, raw = F) +
            poly(engine_size, degree = 1, raw = F) +
            poly(horsepower, degree = 1, raw = F) +
            poly(city_mpg, degree = 2, raw = F) +
            poly(highway_mpg, degree = 3, raw = F) +
            compression_ratio + num_cylinders +
            engine_type + engine_location + drive_wheels +
            body_style + aspiration, df)

ft = olsrr::ols_step_both_p(mod1)
ft$sbc

ft2 = olsrr::ols_step_both_p(mod2)
ft2$sbc

ft3 = olsrr::ols_step_both_p(mod3)
ft3$sbc
ft3$predictors

residual_graph_analyses(mod1)
model_hyphotesys_analyses(mod1)

df1 = df[]


bc = boxcox(lm(price ~ 
                 poly(width, degree = 2, raw = F) + engine_location +
                 poly(city_mpg, degree = 2, raw = F) + engine_type +
                 drive_wheels + body_style +
                 num_cylinders + compression_ratio + aspiration +
                 wheel_base + num_doors, df1))

lambda = bc$x[which.max(bc$y)]
df['y_boxcox'] = ((df$price^lambda)-1)/lambda




mod_fin = lm(y_boxcox ~ 
               poly(width, degree = 1, raw = F) + engine_location +
               poly(city_mpg, degree = 2, raw = F) + engine_type +
               drive_wheels + body_style +
               num_cylinders + compression_ratio + aspiration +
               wheel_base + num_doors, df[-c(42),])

mod_fin |> summary()

model_hyphotesys_analyses(mod_fin)


residual_graph_analyses(lm(mod_fin))
