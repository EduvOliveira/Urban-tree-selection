#' ---
# Projeto: Arborização urbana Sergipe 
# by Eduardo V. S. Oliveira
# 09/02/2026
#' ---

#####Listagem de espécies nativas recomendadas#####

### Obtendo uma lista espécies que maximizam o sequestro e a atratividade da fauna nos municípios de Sergipe


setwd("F:/R_analises/ML_Urban_Forest/carbon")

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)


# Entrando com os dados

spp_att<-read.xlsx("traits_FINAL.xlsx")

spp_city<-read.xlsx("lista_spp_por_municipio-PRE.xlsx")


# Transformar tabela da lista de espécies

mun_long <- spp_city %>% 
  separate_rows(especies_presentes, sep = ",\\s*") %>%
  mutate(species = str_replace_all(especies_presentes, "_", " ")) %>%
  select(municipio, species)

mun_long

# Preparar tabela de traits

traits2 <- spp_att %>%
  rename(
    Height_max = H,
    DAP_max = DBH,
    Wood_density = Dwood
  )


traits2<-traits2[1:369,]

# Juntar ocorrências e atributos

mun_traits <- mun_long %>%
  left_join(traits2, by = "species")


# Aplicar a função por município

ranking_mun <- mun_traits %>%
  group_by(municipio) %>%
  group_modify(~ select_carbon_species(.x)) %>%
  ungroup()

write.xlsx(ranking_mun, "ranking_carbon-mun.xlsx")

# Selecionando as 50+

top_carbon <- ranking_mun %>%
  group_by(municipio) %>%
  slice_max(carbon_score, n = 50) %>%
  ungroup()

write.xlsx(top_carbon, "ranking_carbon-50spp_mun.xlsx")

# Incluindo a atratividade da fauna

ranking_biotic <- ranking_mun %>%
  filter(SD == "biotic") %>%
  arrange(municipio, desc(carbon_score))

write.xlsx(ranking_biotic, "ranking_biotic-mun.xlsx")

# Selecionando as 50+

top_biotic <- ranking_biotic %>%
group_by(municipio) %>%
  slice_head(n = 50) %>%
  ungroup()

write.xlsx(top_biotic, "ranking_biotic-50spp_mun.xlsx")

# Gráficos/resultados

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Preparação dos dados

ranking_mun<-read.xlsx("ranking_carbon-mun.xlsx")

ranked <- ranking_mun %>% 
  group_by(municipio) %>% 
  arrange(desc(carbon_score), .by_group = TRUE) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()

# Heatmap - top 10 por município

top10 <- ranked %>% 
  filter(rank <= 10)

# Ordenar espécies por desempenho

ordem_sp <- top10 %>% 
  group_by(species) %>% 
  summarise(media = mean(carbon_score, na.rm = TRUE)) %>% 
  arrange(desc(media)) %>% 
  pull(species)

# plot

ggplot(top10, aes(y = municipio,
                  x = factor(species, levels = ordem_sp),
                  fill = carbon_score)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Carbon score") +
  labs(y = "Municipality", x = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 6)
  )

# 20 Munícipios por score

top_mun <- ordem_mun[1:20]

top20_filtrado <- top10 %>%
  filter(municipio %in% top_mun)

top20_filtrado$municipio <- factor(top20_filtrado$municipio,
                                   levels = top_mun)

ggplot(top20_filtrado, aes(x = municipio,
                           y = factor(species, levels = ordem_sp),
                           fill = carbon_score)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Carbon score") +
  labs(x = "Municipality", y = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 6)
  )

# Por população

top_mun_pop <- c(
  "Aracaju",
  "Nossa Senhora Do Socorro",
  "Itabaiana",
  "Lagarto",
  "São Cristóvão",
  "Estância",
  "Tobias Barreto",
  "Barra Dos Coqueiros",
  "Simão Dias",
  "Nossa Senhora Da Glória"
)

top10_pop <- top10 %>%
  filter(municipio %in% top_mun_pop)

top10_pop$municipio <- factor(top10_pop$municipio,
                              levels = top_mun_pop)

top10_pop <- top10_pop %>%
  mutate(municipio = recode(municipio,
                            "Barra Dos Coqueiros" = "Barra dos Coqueiros","Nossa Senhora Do Socorro" = "Nossa Senhora do Socorro", "Nossa Senhora Da Glória" = "Nossa Senhora da Glória"))



ggplot(top10_pop, aes(x = municipio,
                      y = factor(species, levels = ordem_sp),
                      fill = carbon_score)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Carbon score") +
  labs(x = "Municipality", y = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 6)
  )

# Cidades nas linhas

top10_pop <- top10 %>%
  filter(municipio %in% top_mun_pop) %>%
  mutate(
    municipio = recode(municipio,
                       "Barra Dos Coqueiros" = "Barra dos Coqueiros",
                       "Nossa Senhora Do Socorro" = "Nossa Senhora do Socorro",
                       "Nossa Senhora Da Glória" = "Nossa Senhora da Glória"
    )
  )

ordem_mun <- c(
  "Aracaju",
  "Nossa Senhora do Socorro",
  "Itabaiana",
  "Lagarto",
  "São Cristóvão",
  "Estância",
  "Tobias Barreto",
  "Barra dos Coqueiros",
  "Simão Dias",
  "Nossa Senhora da Glória"
)


top10_pop$municipio <- factor(
  top10_pop$municipio,
  levels = rev(ordem_mun)
)


g1<-ggplot(top10_pop, aes(x = factor(species, levels = ordem_sp),
                      y = municipio,
                      fill = carbon_score)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Carbon stock\nscore") +
  labs(x = "Species", y = "Municipality") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,size =10),
    axis.text.y = element_text(size = 10)
  ) +theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                               face = "italic"),legend.position = "none"
  )+theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))

g1


dev.copy(device = jpeg, file = "Fig.2.jpeg", width = 4400, height = 3300, res = 600)
dev.off()


# Frequencia nos municípios

freq_top10 <- ranked %>% 
  filter(rank <= 20) %>% 
  count(species, sort = TRUE) %>%
  slice_head(n = 20)   


g3<-ggplot(freq_top10,
       aes(x = n,
           y = fct_reorder(species, n))) +
  geom_col() +
  labs(x = "Number of municipalities",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "italic")) +
  scale_x_continuous(breaks = seq(0, max(freq_top10$n), by = 5))+theme(
    axis.title.y = element_text(margin = margin(r = 10))
  )+scale_x_continuous(
    limits = c(0, ceiling(max(freq_top10$n) / 10) * 10),
    breaks = seq(0, ceiling(max(freq_top10$n) / 10) * 10, by = 10)
  )+theme(axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),)

# Adicionar numeros às colunas

ggplot(freq_top10,
       aes(x = n,
           y = fct_reorder(species, n))) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  labs(x = "Number of municipalities",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6, face = "italic"))


dev.copy(device = jpeg, file = "Fig.3.jpeg", width = 4400, height = 3300, res = 600)
dev.off()


# Tabela completa para material suplementar

supp_table <- ranked %>% 
  arrange(municipio, rank)

write.csv(supp_table,
          "Table_SX_species_ranking_by_municipality.csv",
          row.names = FALSE)

# Entrando com dados de espécies ordenadas por carbono + biotico

ranking<-read.xlsx("ranking_biotic-mun.xlsx")

dir()

ranked2 <- ranking %>% 
  group_by(municipio) %>% 
  arrange(desc(carbon_score), .by_group = TRUE) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()

# Heatmap - top 10 por município

top10 <- ranked2 %>% 
  filter(rank <= 10)

ord2 <- top10 %>% 
  group_by(species) %>% 
  summarise(media = mean(carbon_score, na.rm = TRUE)) %>% 
  arrange(desc(media)) %>% 
  pull(species)


# Por população

top_mun_pop <- c(
  "Aracaju",
  "Nossa Senhora Do Socorro",
  "Itabaiana",
  "Lagarto",
  "São Cristóvão",
  "Estância",
  "Tobias Barreto",
  "Barra Dos Coqueiros",
  "Simão Dias",
  "Nossa Senhora Da Glória"
)

top10_pop <- top10 %>%
  filter(municipio %in% top_mun_pop)

top10_pop$municipio <- factor(top10_pop$municipio,
                              levels = top_mun_pop)

top10_pop <- top10_pop %>%
  mutate(municipio = recode(municipio,
                            "Barra Dos Coqueiros" = "Barra dos Coqueiros","Nossa Senhora Do Socorro" = "Nossa Senhora do Socorro", "Nossa Senhora Da Glória" = "Nossa Senhora da Glória"))

ggplot(top10_pop, aes(x = municipio,
                      y = factor(species, levels = ord2),
                      fill = carbon_score)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Carbon score") +
  labs(x = "Municipality", y = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 6)
  )

# Cidades nas linhas

g2<-ggplot(top10_pop, aes(x = factor(species, levels = ord2),
                          y = municipio,
                          fill = carbon_score)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Carbon stock\nscore") +
  labs(x = "Species", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size=10),
    axis.text.y = element_text(size = 7)
  ) +theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,
                               face = "italic"),axis.text.y = element_blank() 
  )+theme(legend.title = element_text(size = 11),
              legend.text = element_text(size = 10),axis.title.x = element_text(size = 12))

g2

library(patchwork)

fig <- g1 + g2 +
  plot_annotation(tag_levels = "A")

fig


dev.copy(device = jpeg, file = "Fig.2new.jpeg", width = 5000, height = 3300, res = 600)
dev.off()


# Frequencia nos municípios

freq_top10 <- ranked2 %>% 
  filter(rank <= 20) %>% 
  count(species, sort = TRUE) %>%
  slice_head(n = 20)   


g4<-ggplot(freq_top10,
           aes(x = n,
               y = fct_reorder(species, n))) +
  geom_col() +
  labs(x = "Number of municipalities",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "italic")) +
  scale_x_continuous(breaks = seq(0, max(freq_top10$n), by = 5))+theme(
    axis.title.y = element_text(margin = margin(r = 10))
  )+scale_x_continuous(
    limits = c(0, ceiling(max(freq_top10$n) / 10) * 10),
    breaks = seq(0, ceiling(max(freq_top10$n) / 10) * 10, by = 10)
  )+theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 12))

g4


fig2 <- g3 + g4 +
  plot_annotation(tag_levels = "A")

fig2


dev.copy(device = jpeg, file = "Fig.3new.jpeg", width = 5000, height = 3300, res = 600)
dev.off()


# Tabela completa para material suplementar

supp_table <- ranked2 %>% 
  arrange(municipio, rank)

write.csv(supp_table,
          "Table_SX_species_ranking_biotic_by_municipality.csv",
          row.names = FALSE)

#####Organizando as tabelas para o FigShare

# Entrando com os dados

setwd("C:/Users/eduar/Documents/R/ML_Urban_Forest/saves/supplementary")

spp_city<-read.xlsx("Table_S4_species_SSP245.xlsx")

# Transformar tabela da lista de espécies

mun_long <- spp_city %>% 
  separate_rows(especies_adequadas, sep = ",\\s*") %>%
  mutate(species = str_replace_all(especies_adequadas, "_", " ")) %>%
  select(municipio, species)

head(mun_long)

# Tabela completa para material suplementar

supp_table <- mun_long %>% 
  arrange(municipio, species)

write.xlsx(supp_table,
          "Table_S5_species_SSP585.xlsx",rowNames = FALSE)

# Ajustes dos nomes

setwd("C:/Users/eduar/Documents/R/ML_Urban_Forest/saves/supplementary/ok")

spp_city<-read.xlsx("Table_S9_species_SSP585_2080-2100.xlsx")

unique(spp_city$municipalities)

library(dplyr)
library(stringr)

spp_city <- spp_city %>%
  mutate(
    municipalities = municipalities %>%
      str_replace_all("\\bDo\\b", "do") %>%
      str_replace_all("\\bDa\\b", "da") %>%
      str_replace_all("\\bDas\\b", "das") %>%
      str_replace_all("\\bDos\\b", "dos") %>%
      str_replace_all("\\bDe\\b", "de")
  )

head(spp_city)

write.xlsx(spp_city,
           "Table_S9_species.xlsx",rowNames = FALSE)

