library(sidrar)
library(data.table)

### Variáveis selecionadas das empresas, por atividades da indústria, 
### do setor de eletricidade e gás e dos serviços selecionados

### Load data
df5018 = get_sidra(5018, variable = "allxp", period = "last", geo = "Brazil",
                   geo.filter = NULL, classific = "all", category = "all", header = TRUE,
                   format = 4, digits = "default", api = NULL)

### Organize data
df_geral = df5018[,5:11] # select variables
names(df_geral)[3] <- "cod_setor" # rename variable
names(df_geral)[4] <- "setor" # rename variable
names(df_geral)[1] <- "var_cod" # rename variable
names(df_geral)[6] <- "medida" # rename variable
names(df_geral)[7] <- "total" # rename variable
names(df_geral)[2] <- "variavel" # rename variable

nomes_completos = c("Telecom", "Atividades dos Serviços de TI", 
          "Desenvolvimento e Licenciamento de programas customizados",
          "Desenvolvimento e Licenciamento de programas não-customizados",
          "Outros serviços de TI",
          "Tratamento de dados",
          "Serviços de Arquitetura e Engenharia",
          "Pesquisa e Desenvolvimento")

### Function por variável
var_fun = function(var, title) {
      names = c("TE", "AT", 
                "DC",
                "DN",
                "OS",
                "TD",
                "SA",
                "PD")
      vec_setor = c(129402, 129403, 129404, 39420, 129405, 129406, 129407, 129408, 129409, 129410)
      x = subset(df_geral, var_cod == var & cod_setor == vec_setor,
                 select = c(setor, medida, total))
      is.na(x$cod_sector)
      x$freq = x$total / sum(x$total) * 100
      print(x[,-2])
      barplot(x$freq, names.arg = names, xlab = "Setores",
              ylab = "Percentual(%)", ylim = c(0,100), main = title, 
              ps = .7, cex.axis = .8)
}

var_fun(5977, "Empresas que inovaram")
                                           
