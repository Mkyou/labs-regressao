
step(lm(price ~ ., df), direction = "backward")

df$num_cylinders

fitbetter = lm(formula = price ~ symboling + make + aspiration + body_style + 
                 wheel_base + length + width + height + curb_weight + engine_type + 
                 engine_size + fuel_system + bore + stroke + 
                 compression_ratio + peak_rpm + highway_mpg, data = df)
summary(fitbetter)
plot(fitbetter)


aux = residual_graph_analyses(fitbetter)
aux

aux2 = model_hyphotesys_analyses(fitbetter)
aux2[1]
aux2[2]
aux2[3]
aux2[5]