#### Rotina desenvolvida para conferir o funcionamento do EMS a partir dos dados do outputs da simulacao ----

# dir = "diretorio do arquivo de saida (ems_model_out.csv)"
# file = "nome do arquivo csv de saida, com os valores do objeto Output:Variable"

setwd(dir)
df <- read.csv(file)

### DORMITORIO 1 ----

## Se: Ocup Dorm1 & OcupSala = 0 -> SchVal VN Dorm1 = 0
check <- subset(df, df$DORMITORIO1.People.Occupant.Count....TimeStep. == 0 & df$SALA1.People.Occupant.Count....TimeStep. == 0)
unique(check$VN_DORM1.Schedule.Value....TimeStep.)  # Todos os valores sao 0, ok!

## Se: Ocup Dorm1 = 0 -> SchVal HVAC Dorm1 = 0
check <- subset(df, df$DORMITORIO1.People.Occupant.Count....TimeStep. == 0)
unique(check$HVAC_DORM1.Schedule.Value....TimeStep.)  # Todos os valores sao 0, ok!

## Se: SchVal VN Dorm1 = 1 -> Sch Val HVAC Dorm1 = 0
## Se: Sch Val HVAC Dorm1 = 1 -> SchVal VN Dorm1 = 0
nrow(subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. > 0 & df$VN_DORM1.Schedule.Value....TimeStep. > 0))  
# nenhuma linha atende a condicao, ok!

## Se: Ocup Dorm1 > 0 -> TempOp Dorm1 < 26
## Se: Ocup Dorm1 > 0 -> TempOp Dorm1 > 16
nrow(subset(df, df$DORMITORIO1.People.Occupant.Count....TimeStep. > 0 & df$DORM1.Zone.Operative.Temperature..C..TimeStep. > 26))
# Apenas 5 timesteps, sempre no inicio da ocupacao, ok!
nrow(subset(df, df$DORMITORIO1.People.Occupant.Count....TimeStep. > 0 & df$DORM1.Zone.Operative.Temperature..C..TimeStep. < 16))
# nenhuma linha atende a condicao, ok!

## Se: TempExt < 19 -> SchVal VN Dorm1 = 0
nrow(subset(df, df$VN_DORM1.Schedule.Value....TimeStep. > 0 & df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep. < 19))
# nenhuma linha atende a condicao, ok!

## Se: SchVal HVAC Dorm1 = 1 -> TempOp Dorm1 < 26;
## Se: SchVal HVAC Dorm1 = 1 -> TempOp Dorm1 > 18
nrow(subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. > 0 & df$DORM1.Zone.Operative.Temperature..C..TimeStep. > 26))
nrow(subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. > 0 & df$DORM1.Zone.Operative.Temperature..C..TimeStep. < 18))
# HVAC funciona baseado na Temp do ar, e nao na Temp operativa. Por isso 480 timesteps tem Temp operativa inferior a 18.
# Ao checar com Temp_ar:
nrow(subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. > 0 & df$DORM1.Zone.Mean.Air.Temperature..C..TimeStep. < 18))
# 222 timesteps ainda estao com T_ar abaixo de 18
check <- subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. > 0 & df$DORM1.Zone.Mean.Air.Temperature..C..TimeStep. < 18)
min(check$DORM1.Zone.Mean.Air.Temperature..C..TimeStep.)
# Na pratica, eh um problema de arredondamento ou algo do genero, pois o min = 18
nrow(subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. > 0 & df$DORM1.Zone.Mean.Air.Temperature..C..TimeStep. < 17.999))
# suspeita confirmada!

## Se: SchVal VN Dorm1 = 0 -> OpenFacDoorWindow Dorm1 = 0
check <- subset(df, df$VN_DORM1.Schedule.Value....TimeStep. == 0)
unique(check$PORTAINT_DORM1SALA.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)  # Todos os valores sao 0, ok!
unique(check$JANQUARTO1_SUL.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)  # Todos os valores sao 0, ok!

## Se: SchVal HVAC Dorm1 = 0 -> Heating/Cooling Dorm1 = 0
check <- subset(df, df$HVAC_DORM1.Schedule.Value....TimeStep. == 0)
unique(check$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.Total.Cooling.Energy..J..TimeStep.)  # Todos os valores sao 0, ok!
unique(check$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.Total.Heating.Energy..J..TimeStep.)  # Todos os valores sao 0, ok!

### DORMITORIO 2 ----

## Se: Ocup Dorm2 & OcupSala = 0 -> SchVal VN Dorm2 = 0
check <- subset(df, df$DORMITORIO2.People.Occupant.Count....TimeStep. == 0 & df$SALA1.People.Occupant.Count....TimeStep. == 0)
unique(check$VN_DORM2.Schedule.Value....TimeStep.)

## Se: Ocup Dorm2 = 0 -> SchVal HVAC Dorm2 = 0
check <- subset(df, df$DORMITORIO2.People.Occupant.Count....TimeStep. == 0)
unique(check$HVAC_DORM2.Schedule.Value....TimeStep.)

## Se: SchVal VN Dorm2 = 1 -> Sch Val HVAC Dorm2 = 0
## Se: Sch Val HVAC Dorm2 = 1 -> SchVal VN Dorm2 = 0
nrow(subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. > 0 & df$VN_DORM2.Schedule.Value....TimeStep. > 0))

## Se: Ocup Dorm2 > 0 -> TempOp Dorm2 < 26
## Se: Ocup Dorm2 > 0 -> TempOp Dorm2 > 16
nrow(subset(df, df$DORMITORIO2.People.Occupant.Count....TimeStep. > 0 & df$DORM2.Zone.Operative.Temperature..C..TimeStep. > 26))
# Apenas 6 timesteps, sempre no inicio da ocupacao, ok!
nrow(subset(df, df$DORMITORIO2.People.Occupant.Count....TimeStep. > 0 & df$DORM2.Zone.Operative.Temperature..C..TimeStep. < 16))
# nenhuma linha atende a condicao, ok!

## Se: TempExt < 19 -> SchVal VN Dorm2 = 0
nrow(subset(df, df$VN_DORM2.Schedule.Value....TimeStep. > 0 & df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep. < 19))
# nenhuma linha atende a condicao, ok!

## Se: SchVal HVAC Dorm2 = 1 -> TempOp Dorm2 < 26;
## Se: SchVal HVAC Dorm2 = 1 -> TempOp Dorm2 > 18
nrow(subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. > 0 & df$DORM2.Zone.Operative.Temperature..C..TimeStep. > 26))
nrow(subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. > 0 & df$DORM2.Zone.Operative.Temperature..C..TimeStep. < 18))
# HVAC funciona baseado na Temp do ar, e nao na Temp operativa. Por isso 480 timesteps tem Temp operativa inferior a 18.
# Ao checar com Temp_ar:
nrow(subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. > 0 & df$DORM2.Zone.Mean.Air.Temperature..C..TimeStep. < 18))
# 202 timesteps ainda estao com T_ar abaixo de 18
check <- subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. > 0 & df$DORM2.Zone.Mean.Air.Temperature..C..TimeStep. < 18)
min(check$DORM2.Zone.Mean.Air.Temperature..C..TimeStep.)
# Na pratica, eh um problema de arredondamento ou algo do genero, pois o min = 18
nrow(subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. > 0 & df$DORM2.Zone.Mean.Air.Temperature..C..TimeStep. < 17.999))
# suspeita confirmada!

## Se: SchVal VN Dorm2 = 0 -> OpenFacDoorWindow Dorm2 = 0
check <- subset(df, df$VN_DORM2.Schedule.Value....TimeStep. == 0)
unique(check$PORTAINT_DORM2SALA.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)
unique(check$JANQUARTO2_LESTE.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)

## Se: SchVal HVAC Dorm2 = 0 -> Heating/Cooling Dorm2 = 0
check <- subset(df, df$HVAC_DORM2.Schedule.Value....TimeStep. == 0)
unique(check$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.Total.Cooling.Energy..J..TimeStep.)
unique(check$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.Total.Heating.Energy..J..TimeStep.)

### SALA ----

## Se: OcupSala = 0 -> SchVal VN Sala = 0
check <- subset(df, df$SALA1.People.Occupant.Count....TimeStep. == 0)
unique(check$VN_SALA.Schedule.Value....TimeStep.)

## Se: Ocup Sala = 0 -> SchVal HVAC Sala = 0
check <- subset(df, df$SALA1.People.Occupant.Count....TimeStep. == 0)
unique(check$HVAC_SALA.Schedule.Value....TimeStep.)

## Se: SchVal VN Sala = 1 -> Sch Val HVAC Sala = 0
## Se: Sch Val HVAC Sala = 1 -> SchVal VN Sala = 0
nrow(subset(df, df$HVAC_SALA.Schedule.Value....TimeStep. > 0 & df$VN_SALA.Schedule.Value....TimeStep. > 0))

## Se: Ocup Sala > 0 -> TempOp Sala < 26
## Se: Ocup Sala > 0 -> TempOp Sala > 16
nrow(subset(df, df$SALA1.People.Occupant.Count....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. > 26))
nrow(subset(df, df$SALA1.People.Occupant.Count....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. < 16))
# Apesar de 987 timesteps com Temp operativa > 26, sao quase que na totalidade timesteps onde HVAC esta ligado e Temp_ar < 26
check <- subset(df, df$SALA1.People.Occupant.Count....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. > 26)
nrow(subset(check, check$HVAC_SALA.Schedule.Value....TimeStep. == 0))
nrow(subset(check, check$SALA.Zone.Mean.Air.Temperature..C..TimeStep. < 26))

## Se: TempExt < 19 -> SchVal VN Sala = 0
nrow(subset(df, df$VN_SALA.Schedule.Value....TimeStep. > 0 & df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep. < 19))
# nenhuma linha atende a condicao, ok!

## Se: SchVal HVAC Sala = 1 -> TempOp Sala < 26;
## Se: SchVal HVAC Sala = 1 -> TempOp Sala > 18
nrow(subset(df, df$HVAC_SALA.Schedule.Value....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. > 26))
nrow(subset(df, df$HVAC_SALA.Schedule.Value....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. < 18))
# Analise analoga a dos dormitorios:
check <- subset(df, df$HVAC_SALA.Schedule.Value....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. > 26)
nrow(subset(check, check$SALA.Zone.Mean.Air.Temperature..C..TimeStep. > 26))
check <- subset(df, df$HVAC_SALA.Schedule.Value....TimeStep. > 0 & df$SALA.Zone.Operative.Temperature..C..TimeStep. < 18)
nrow(subset(check, check$SALA.Zone.Mean.Air.Temperature..C..TimeStep. < 17.999))

## Se: SchVal VN Dorm1 = 0 -> OpenFacDoorWindow Sala = 0
check <- subset(df, df$VN_SALA.Schedule.Value....TimeStep. == 0)
unique(check$PORTAINT_DORM1SALA.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)
unique(check$PORTAINT_DORM2SALA.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)
unique(check$JANSALA_OESTE.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)
unique(check$JANSALA_SUL.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)
unique(check$PORTAINT_BWCSALA.AFN.Surface.Venting.Window.or.Door.Opening.Factor....TimeStep.)

## Se: SchVal HVAC Dorm1 = 0 -> Heating/Cooling Sala = 0
check <- subset(df, df$HVAC_SALA.Schedule.Value....TimeStep. == 0)
unique(check$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.Total.Cooling.Energy..J..TimeStep.)
unique(check$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Supply.Air.Total.Heating.Energy..J..TimeStep.)
