install.packages("readr")
install.packages("readxl")
install.packages("tidyr")
install.packages("tibble")
install.packages("stringr")
install.packages("dplyr")

Tible_1 <- read.csv("data_AE_Anastasiia_and_Natalia.csv")
clinical_data <- read_delim("E-MTAB-3732.sdrf.txt")

#Таблица с экспрессией без Наташиного гена
expression <- Tible_1[ -11 , ]


#Таблица по нормальным образцам

clinical_data1 <- clinical_data[!clinical_data$`Characteristics[organism part]` != 	
                                  'pancreas', ]
normal <- clinical_data1[!clinical_data1$`Characteristics[disease]` != 'normal', ]

#Таблица по опухолевым образцам

cancer <- clinical_data1[!clinical_data1$`Characteristics[disease]` != 'pancreatic cancer', ]
cancer1 <- clinical_data1[!clinical_data1$`Characteristics[disease]` != 'pancreatic carcinoma', ]
cancer1 <- clinical_data1[!clinical_data1$`Characteristics[disease]` != 'pancreatic adenocarcinoma', ]
cancer1 <- clinical_data1[!clinical_data1$`Characteristics[disease]` != 'pancreatic ductal adenocarcinoma', ]
cancer <- full_join(cancer, cancer1)


#Таблица по экспрессии здоровых образцов

healthy_samples <- expression %>% select("Sample.557", "Sample.558" ,  "Sample.565" ,  "Sample.3313" , "Sample.4230" , "Sample.4756" , "Sample.5620",  "Sample.6133" ,
                                         "Sample.6139" ,
                                         "Sample.6140" , "Sample.7021" , "Sample.7504" , "Sample.7510" , "Sample.7511" , "Sample.8941" , "Sample.8942" , "Sample.9794" ,
                                         "Sample.10284",
                                         "Sample.10285", "Sample.10290" ,"Sample.11195", "Sample.11686" ,"Sample.11687", "Sample.11688", "Sample.11689", "Sample.11694",
                                         "Sample.11695",
                                         "Sample.11696", "Sample.12604", "Sample.13117", "Sample.13118", "Sample.13120","Sample.14519", "Sample.14524", "Sample.14525",
                                         "Sample.14526",
                                         "Sample.14527", "Sample.15391", "Sample.15890", "Sample.15897", "Sample.17295", "Sample.17302", "Sample.17303", "Sample.18180",
                                         "Sample.18705",
                                         "Sample.18708", "Sample.18709", "Sample.18710", "Sample.18711", "Sample.20094", "Sample.20095", "Sample.21461", "Sample.21462",
                                         "Sample.21467",
                                         "Sample.21468", "Sample.22857", "Sample.24238", "Sample.24243", "Sample.24244", "Sample.25636", "Sample.25637", "Sample.25642",
                                         "Sample.26516",
                                         "Sample.27048", "HGNC.symbol" )  



cancer_samples <- expression %>% select("Sample.559","Sample.560","Sample.561","Sample.1436","Sample.1898","Sample.1967","Sample.2034","Sample.2035","Sample.2512",
                                        "Sample.2513","Sample.2572","Sample.2826","Sample.3314","Sample.3315","Sample.3316","Sample.3636","Sample.3650","Sample.3686",
                                        "Sample.3965","Sample.4757","Sample.4830","Sample.4831","Sample.5101","Sample.5367","Sample.5368","Sample.5621","Sample.5622",
                                        "Sample.6134","Sample.6135","Sample.6136","Sample.6454","Sample.6470","Sample.6728","Sample.6777","Sample.7018","Sample.7019",
                                        "Sample.7020","Sample.7505","Sample.7506","Sample.7507","Sample.8404","Sample.8405","Sample.8406","Sample.9222","Sample.9274",
                                        "Sample.9275","Sample.9491","Sample.9534","Sample.9793","Sample.9795","Sample.10286","Sample.10287","Sample.10288","Sample.10600",
                                        "Sample.10630","Sample.11194","Sample.11196","Sample.11197","Sample.11690","Sample.11691","Sample.11692","Sample.12005","Sample.12006",
                                        "Sample.12025","Sample.12293","Sample.12605","Sample.12606","Sample.13119","Sample.13420","Sample.13464","Sample.13709","Sample.14007"
                                        ,"Sample.14008","Sample.14520","Sample.14521","Sample.14590","Sample.14798","Sample.15117","Sample.15390","Sample.15392","Sample.15393"
                                        ,"Sample.15891","Sample.15892","Sample.16193","Sample.16212","Sample.16213","Sample.16474","Sample.16475","Sample.16794","Sample.16795"
                                        ,"Sample.17216","Sample.17296","Sample.17593","Sample.17594","Sample.18178","Sample.18179","Sample.18769","Sample.19006","Sample.19007"
                                        ,"Sample.19008","Sample.19031","Sample.19303","Sample.19512","Sample.19567","Sample.20088","Sample.20089","Sample.20090","Sample.20653"
                                        ,"Sample.20654","Sample.20655","Sample.20900","Sample.20952","Sample.20953","Sample.20954","Sample.20955","Sample.21398","Sample.21463"
                                        ,"Sample.21765","Sample.21782","Sample.21783","Sample.21812","Sample.22335","Sample.22852","Sample.22853","Sample.23149","Sample.23150"
                                        ,"Sample.23175","Sample.23176","Sample.23438","Sample.23439","Sample.23478","Sample.23720","Sample.23721","Sample.24239","Sample.24240"
                                        ,"Sample.24579","Sample.24843","Sample.25638","Sample.25932","Sample.26229","Sample.26231","Sample.26277","Sample.26517","Sample.27044"
                                        ,"Sample.27045","Sample.27046","Sample.27332","Sample.27347","Sample.27348","Sample.27364","Sample.27607","Sample.27656","Sample.26232",
                                        "Sample.2514","Sample.3661","Sample.9221","Sample.10631","Sample.10862","Sample.12038","Sample.13052","Sample.13707","Sample.14825",
                                        "Sample.15075","Sample.17628","Sample.21791","Sample.25552","Sample.26966","Sample.12456","Sample.20806", "HGNC.symbol")


#нашли медиану среди повторяющихся образцов (нормальные образцы)
data_expr_updated <- healthy_samples %>%
  pivot_longer(starts_with("Sample"), names_to = "Samples", values_to = "Expression") 

new_results <- data_expr_updated %>%
  group_by(HGNC.symbol, Samples) %>%
  summarise(Expression = median(Expression, na.rm = T)) %>%
  pivot_wider(names_from = `HGNC.symbol`, values_from = Expression)

#медиана раковых клеток
data_expr_cancer <- cancer_samples %>%
  pivot_longer(starts_with("Sample"), names_to = "Samples", values_to = "Expression") 

new_results_cancer <- data_expr_cancer %>%
  group_by(HGNC.symbol, Samples) %>%
  summarise(Expression = median(Expression, na.rm = T)) %>%
  pivot_wider(names_from = `HGNC.symbol`, values_from = Expression)



#корреляционные матрицы для двух таблиц

install.packages("rstatix")

cor_normal <- new_results %>% select(-1) %>% cor_mat(method = "spearman")
cor_cancer <- new_results_cancer %>% select(-1) %>% cor_mat(method = "spearman")

#кореллограммы

install.packages("corrplot")

cor_plot(cor_normal,method = "circle",type = "lower", significant.level = 0.05 )
cor_plot(cor_cancer,method = "circle",  type = "lower", significant.level = 0.05 )

#таблицы с информацией о корреляции

cor_normal2 <- new_results %>% select(-1) %>% cor_test(method = "spearman")
cor <- filter(cor_normal2, p <= 0.05)

cor_cancer2 <- new_results_cancer %>% select(-1) %>% cor_test(method = "spearman")
corc <- filter(cor_cancer2, p <= 0.05)                      




#построение сетей взаимодействия

install.packages("tidygraph")
install.packages("ggraph")
install.packages("ggplot2")

my_corc <- corc %>%
  select(-4, -5, -6)
my_cor <- cor %>%
  select(-4, -5, -6)

cbio.graph.c <- as_tbl_graph(my_corc, directed = FALSE)
cbio.graph.n <- as_tbl_graph(my_cor, directed = FALSE)

network_cbio <- ggraph(cbio.graph.c) +
  geom_edge_link(aes(color = cor, width = cor))  +
  geom_node_point(size = 1)  +
  geom_node_text(aes(label = name), size = 4, repel = TRUE)  +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave(filename = "cor_network_AE_cancer_ov.png", 
       plot = network_cbio, scale = 1)

network_cbio_n <- ggraph(cbio.graph.n) +
  geom_edge_link(aes(color = cor, width = cor))  +
  geom_node_point(size = 1)  +
  geom_node_text(aes(label = name), size = 4, repel = TRUE)  +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave(filename = "cor_network_AE_normal_ov.png", 
       plot = network_cbio_n, scale = 10)




#анализ и визуализация корреляции

#центральные вершины генов и модули сети в здоровых тканях

cbio.graph.n <- cbio.graph.n %>% activate(edges) %>% mutate (cor_abs = abs(cor))

cbio.graph.n <- cbio.graph.n %>% activate(nodes) %>%
     mutate(hub=centrality_hub(weights = cor_abs, scale = TRUE, options = igraph::arpack_defaults)) %>%
  mutate (betweenness = centrality_betweenness(
    weights = cor_abs ,
    directed = FALSE,
    cutoff = NULL,
    normalized = FALSE
  ))  %>% mutate (group = group_edge_betweenness(weights = cor_abs, directed = FALSE, n_groups = NULL)) 
  

Hub <- cbio.graph.n %>% activate(nodes) %>% as.tibble()

#верхние 5% по хаб и битвинесс

per95 = quantile(Hub$hub, 0.95)
per95b = quantile(Hub$betweenness, 0.95)

new <- filter(Hub, hub >= 0.978)
new2 <- filter(Hub, betweenness >= 39.4)
new <- full_join(new, new2)

cbio_n <- ggraph(cbio.graph.n) +
  geom_edge_link(aes(color = cor, width = cor/2))  +
  geom_node_point(aes(size = log(hub)))  +
  geom_node_text(aes(label=ifelse(group == 1, name, NA), size = betweenness), repel = TRUE)  +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave(filename = "normal.png", 
       plot = cbio_n, scale = 10)

#центральные вершины генов и модули сети в опухолевых

cbio.graph.c <- cbio.graph.c %>% activate(edges) %>% mutate (cor_abs = abs(cor))

cbio.graph.c <- cbio.graph.c %>% activate(nodes) %>%
  mutate(hub=centrality_hub(weights = cor_abs, scale = TRUE, options = igraph::arpack_defaults)) %>%
  mutate (betweenness = centrality_betweenness(
    weights = cor_abs ,
    directed = FALSE,
    cutoff = NULL,
    normalized = FALSE
  )) %>% mutate (group = group_edge_betweenness(weights = cor_abs, directed = FALSE, n_groups = NULL))  


Hc <- cbio.graph.c %>% activate(nodes) %>% as.tibble() 

#верхние 5% по хаб и битвинесс

per = quantile(Hc$hub, 0.95)
perc = quantile(Hc$betweenness, 0.95)

can <- filter(Hc, hub >= 0.993)
new2 <- filter(Hc, betweenness >= 51.2)
can <- full_join(can, new2)

cancerbio <- ggraph(cbio.graph.c) +
  geom_edge_link(aes(color = cor, width = cor/2))  +
  geom_node_point(aes(size = log(hub)))  +
  geom_node_text(aes(label = ifelse(group == 1, name, NA), size = betweenness), repel = TRUE)  +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave(filename = "cancer.png", 
       plot = cancerbio, scale = 10)

write_excel_csv(Hc, "Hc.csv")
write_excel_csv(Hub, "Hub.csv")
#почитать статью про кластеризацию

#построение дендрограммы 

install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("dendextend")

cor_normal1 <- column_to_rownames(cor_normal, var = "rowname")
d <- dist(cor_normal1, method = "euclidean")
hc1 <- hclust(d, method = "complete" ) 
plot(hc1, cex = 0.6, hang = -1)

cor_cancer1 <- column_to_rownames(cor_cancer, var = "rowname")
dcancer <- dist(cor_cancer1, method = "euclidean")
hc2 <- hclust(dcancer, method = "complete" ) 
plot(hc2, cex = 0.6, hang = -1)

tanglegram ( hc1, hc2)

cor_cophenetic(hc1, hc2) #0,1684296




























