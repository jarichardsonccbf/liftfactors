# filter returned items (maybe not), billqty does not reflect negative netvalue, multiply by -1

# filter zf2 

# Walgreens 2/$2.50 honest tea

library(data.table)
library(tidyverse)


cust <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/CCF_CustomerMaster.csv")
cust$id <- as.numeric(cust$CustomerNumber)
cust <- cust[!is.na(cust$id),]
cust <- cust %>% select(Soldto = id, PrimaryGroupDesc,PrimaryGroup, DeliveringPlant, SalesOffice, SalesOfficeDesc)  

# FILTER CUSTOMER
cust <- cust %>% 
  filter(PrimaryGroupDesc == "WALGREEN")

  
prod <- fread("data/products.csv") %>% 
  select(MaterialNo, Name, Product, BrandCategory, BrandFlavor, BrandGroup, Package, PackageCategory, Size, Product)

# FILTER PRODUCT
prod <- prod %>% 
  filter(grepl('16.9Z', Name)) %>%
  filter(BrandCategory == "Honest-KO",
         BrandGroup == "Tea") %>% 
  select(-c(Product, Package, PackageCategory, Size))

back.2018 <- fread("C:/Users/FL014036/Coke Florida/Bogdan Visinescu - COPA/SuggestedOrder2018P6_P12.csv") %>%
  select(BillingDocNo, BillItem, MaterialNo, BillType, Soldto, BillQty, NetValue, FiscalYear, FiscalYearQuarter, FiscalYearPeriod, FiscalYearWeek) %>%
  filter(BillType == "ZF2") %>% 
  right_join(cust, by ="Soldto")

back.2018 <- back.2018 %>% 
  right_join(prod, by = "MaterialNo") %>% 
  mutate(FiscalYearWeek = as.factor(FiscalYearWeek))

rm(cust, prod)

# generate calendar 
# set promo -1 week out

promo.cal <- data.frame(FiscalYearWeek = c("201828",
                                           "201831",
                                           "201833"),
                        Promo = c("yes",
                                  "yes",
                                  "yes")
                        )

all <- back.2018 %>% 
  left_join(promo.cal, "FiscalYearWeek")

all$Promo <- fct_explicit_na(all$Promo, "no")

all %>% 
  na.omit() %>% 
  group_by(Promo) %>% 
  summarise(ave = mean(BillQty),
            sd = sd(BillQty))

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
