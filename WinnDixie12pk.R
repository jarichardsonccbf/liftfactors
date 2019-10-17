# filter returned items (maybe not), billqty does not reflect negative netvalue, multiply by -1

# filter zf2 

# Winn-Dixie 24 pack

library(data.table)
library(tidyverse)


cust <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/CCF_CustomerMaster.csv")
cust$id <- as.numeric(cust$CustomerNumber)
cust <- cust[!is.na(cust$id),]
cust <- cust %>% select(Soldto = id, PrimaryGroupDesc,PrimaryGroup, DeliveringPlant, SalesOffice, SalesOfficeDesc)  

# FILTER CUSTOMER
cust <- cust %>% 
  filter(PrimaryGroupDesc == "WINN DIXIE NON HEART")

  
prod <- fread("data/products.csv") %>% 
  select(MaterialNo, Name, Product, BrandCategory, BrandFlavor, BrandGroup, Package, PackageCategory, Size, Product)

# FILTER PRODUCT
prod <- prod %>% 
  filter(grepl('12Z CN 12FP', Product)) %>% 
  select(-c(Product, Package, PackageCategory, Size))
  

front.2018 <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/SuggestedOrder2018P1_P6.csv") %>%
  select(BillingDocNo, BillItem, MaterialNo, BillType, Soldto, BillQty, NetValue, FiscalYear, FiscalYearQuarter, FiscalYearPeriod, FiscalYearWeek) %>%
  filter(BillType == "ZF2",
         FiscalYearPeriod != "201806") %>% 
  right_join(cust, by ="Soldto")

back.2018 <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/SuggestedOrder2018P6_P12.csv") %>%
  select(BillingDocNo, BillItem, MaterialNo, BillType, Soldto, BillQty, NetValue, FiscalYear, FiscalYearQuarter, FiscalYearPeriod, FiscalYearWeek) %>%
  filter(BillType == "ZF2") %>% 
  right_join(cust, by ="Soldto")

front.2019 <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/SuggestedOrder2019P1_P8.csv") %>% 
  select(BillingDocNo, BillItem, MaterialNo, BillType, Soldto, BillQty, NetValue, FiscalYear, FiscalYearQuarter, FiscalYearPeriod, FiscalYearWeek) %>% 
  filter(BillType == "ZF2") %>% 
  right_join(cust, by ="Soldto")

back.2019 <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/SuggestedOrderQ4.csv")%>% 
  select(BillingDocNo, BillItem, MaterialNo, BillType, Soldto, BillQty, NetValue, FiscalYear, FiscalYearQuarter, FiscalYearPeriod, FiscalYearWeek) %>% 
  filter(BillType == "ZF2") %>% 
  right_join(cust, by ="Soldto")

all <- rbind(front.2018, back.2018, front.2019, back.2019) %>% 
  right_join(prod, by = "MaterialNo") %>% 
  mutate(FiscalYearWeek = as.factor(FiscalYearWeek))

rm(front.2018, back.2018, front.2019, back.2019, cust, prod)


# generate calendar 
# set promo -1 week out

promo.cal <- data.frame(FiscalYearWeek = c("201813",
                                           "201821",
                                           "201826",
                                           "201843",
                                           "201846",
                                           "201905",
                                           "201916",
                                           "201921",
                                           "201926",
                                           "201935",
                                           "201947",
                                           "201951"),
                        Promo = c("yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes",
                                  "yes")
                        )

all <- all %>% 
  left_join(promo.cal, "FiscalYearWeek")

all$Promo <- fct_explicit_na(all$Promo, "no")

a <- all %>%
  group_by(SalesOfficeDesc, Soldto, Promo) %>% 
  na.omit() %>% 
  summarise(ave = mean(BillQty),
            sd = sd(BillQty))

b <- all %>% 
  group_by(Soldto, Promo) %>% 
  na.omit() %>% 
  summarise(ave = mean(BillQty)) %>% 
  pivot_wider(names_from = Promo, values_from = ave) %>% 
  na.omit() %>% 
  mutate(lift.factor = yes/no)
