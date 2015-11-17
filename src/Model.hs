module Model
  where

type Prices = [Price]
type Price  = Int

type Availabilities = [Availability]
type Availability   = Int

type Plans    = [Plan]
type Plan     = (Products, Products, CoolingLiquid)
type Products = [Product]
type Product  = Int

type CoolingLiquid = Availability
