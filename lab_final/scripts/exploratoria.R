# sumário da nova base de dados
df |> summary()

#aparentemente o número de portas ainda tem "?" nos níveis, mesmo sem
#nenhuma observação com esse valor
df$num_doors

#removendo esse level do fator
df$num_doors = df$num_doors |> droplevels()
df |> summary()

#a variável make tem quase 50% das observações iguais a "other"
#na variável fuel_system é só uma

