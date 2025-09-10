library(dplyr)
library(xlsx)
library(tidyr)
library(plyr)
library(stringr)
library(purrr)
library(tidyverse)


############## Transparència Algorítmica angles

aa <- read.xlsx2("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/Academico_AcaoAfirmativa_Completa.xlsx",
                sheetName = "acoes_afirmativas")

aa$cursoId <- as.numeric(as.character(aa$cursoId))

####
aa1 <- gsub("BARÃO DE GERALDO", "CAMPINAS", aa$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("BARRA DO PIRAI", "BARRA DO PIRAÍ", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("CANINDE DE SÃO FRANCISCO", "CANINDÉ DE SÃO FRANCISCO", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("CONCEICÃO DA BREJAUBA", "GONZAGA", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("HONORÓPOLIS", "CAMPINA VERDE", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("IUIÚ", "IUIU", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("LIMEIRA D'OESTE", "LIMEIRA DO OESTE", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("OURO PRETO D'OESTE", "OURO PRETO DO OESTE", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("SÃO BENEDITO DAS AREIAS", "MOCOCA", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("SÃO SEBASTIÃO DO PONTAL", "CARNEIRINHO", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("DIAS D´ÁVILA", "DIAS D'ÁVILA", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"
aa1 <- gsub("ALVORADA DO OESTE", "ALVORADA D'OESTE", aa1$Nome_Município)
aa1 <- data.frame(aa1)
names(aa1)[1] <- "Nome_Município"

aa <- aa %>%
  add_column(add_column = aa1$Nome_Município)


#aa <- aa[!grepl("ERRO ERRO ERRO", aa$Nome_Município),]

aa <- aa %>%
  modify_if(is.character, toupper)

aa <- aa[ -c(9) ]

colnames(aa)[8] <- "UF"
colnames(aa)[10] <- "Nome_Município"


########

municipios <- read.xlsx2("/Users/wemigliari/Downloads/DTB_2024/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls",
                 sheetName = "DTB_Municípios")

municipios <- municipios[ -c(3:6) ]

municipios <- municipios %>%
  modify_if(is.character, toupper)

### Merging dataframes
municipios2 <- merge(x = aa, y = municipios, by = c("Nome_Município", "UF"), all.x = TRUE)

#### Usar dataframe 'municipios2'para calcular as frequências
municipios2 <- municipios2[ -c(12) ] ## Total de entradas 8285

#### Filtering dataframe - LI_PPI/309, LB_PPI/198,  PPI (-$1.5)/1801,   PPI (#$)/1805,   PPI (@)/20, LB_Q/3 - Total de entradas 4136


municipios_PPI_ <- municipios2 %>%
  filter(str_detect(Acao.Afirmativa, "PPI"))


municipios_LB_Q <- municipios2 %>%
  filter(str_detect(Acao.Afirmativa, "LB_Q"))

municipios2 <- rbind(municipios_PPI_, municipios_LB_Q)


####

#### Dataframe 'municipios21' para a plotagem do mapa - municipios que mais enviam alunos para a UFTM-Uberaba por ano
municipios21 <- municipios2 %>% 
  group_by(Ano, Nome_Município) %>% 
  add_count(Ano, Nome_Município)

municipios21 <- data.frame(municipios21)

municipios21 <- municipios21[!duplicated(municipios21), ]


write.xlsx(municipios21, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/naturalidade.xlsx')

#### Tabela com a primera linha de cada ano e o total de alunos por municipio de origem

municipios22 <- municipios21 %>% 
  group_by(Nome_Município, Ano, n) %>% 
  slice(1)

municipios22 <- municipios22[ -c(2, 4:10) ]

municipios22 <- data.frame(municipios22)

# Wider format

municipios23 <- municipios22 %>%
  pivot_wider(
    names_from = Ano,
    values_from = n,
    values_fill = 0
  )


municipios23 <- municipios23[,c(7, 4, 8, 15, 6, ,)]

  
write.xlsx(municipios23, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/municipio_origem_mapa.xlsx')



####

write.xlsx(municipios21, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/municipio_origem.xlsx')

##### Frequência semestre

municipios3 <- municipios2


frequencia <- municipios3 %>% 
  group_by(Ano, SI) %>% 
  add_count(Ano, SI)

frequencia <- frequencia[ -c(1,4:10) ]

frequencia <- data.frame(frequencia)

frequencia <- frequencia[!duplicated(frequencia), ]

frequencia <- frequencia[order(frequencia$Ano, frequencia$SI), ]


write.xlsx(frequencia, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/frequencia.xlsx')


##### Frequência sexo

frequencia_sexo <- municipios3 %>% 
  group_by(Ano, Sexo) %>% 
  add_count(Ano, Sexo)

frequencia_sexo <- frequencia_sexo[ -c(1,3,5:10) ]

frequencia_sexo  <- data.frame(frequencia_sexo )

frequencia_sexo  <- frequencia_sexo [!duplicated(frequencia_sexo ), ]

frequencia_sexo  <- frequencia_sexo [order(frequencia_sexo$Ano, frequencia_sexo$n), ]


write.xlsx(frequencia_sexo, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/frequencia_sexo.xlsx')

##### Frequência sexo


frequencia_aa <- municipios2 %>% 
  group_by(Ano, Acao.Afirmativa) %>% 
  add_count(Ano, Acao.Afirmativa)

frequencia_aa <- frequencia_aa[ -c(1,3:7, 9:10) ]

frequencia_aa  <- data.frame(frequencia_aa)

frequencia_aa  <- frequencia_aa[!duplicated(frequencia_aa), ]

frequencia_aa  <- frequencia_aa[order(frequencia_aa$Ano, frequencia_aa$n), ]

write.xlsx(frequencia_aa, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/frequencia_acao_afirmativa.xlsx')


##### Número de alunos por curso e ano


frequencia_curso_ano <- municipios3 %>% 
  group_by(Ano, cursoNome) %>% 
  add_count(Ano, cursoNome)

frequencia_curso_ano <- frequencia_curso_ano[ -c(1,3:6, 8:10) ]

frequencia_curso_ano  <- data.frame(frequencia_curso_ano)

frequencia_curso_ano  <- frequencia_curso_ano[!duplicated(frequencia_curso_ano), ]

frequencia_curso_ano  <- arrange(frequencia_curso_ano, Ano, desc(n))

write.xlsx(frequencia_curso_ano, '/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Rosemberg/Dados_AA/frequencia_curso_ano.xlsx')



##### Finding duplicates
# Using base R functions to find duplicate values
duplicates <- aa[duplicated(aa), ]
duplicate_counts <- table(duplicates[duplicated(duplicates), ])

duplicates

#####

aa <- unique(aa[ , 1:8])