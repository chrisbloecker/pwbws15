module Model
  where

type Prices = [Price]
type Price  = Int

type Amounts = [Amount]
type Amount  = Int

type Plans    = [Plan]
type Plan     = (Products, Products, CoolingLiquid)
type Products = [Product]
type Product  = Int

type PlanId = Int

type CoolingLiquid = Amount
