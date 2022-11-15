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


cancer %>% select(any_of('Source Name'))
print(cancer$`Source Name`)


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





















