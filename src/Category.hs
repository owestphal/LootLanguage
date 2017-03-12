module Category (
  Category,
  union,
  unionAll,
  intersect,
  implementCategory,
  optimize,
  --
  RarityLevel (..),
  SocketColor (..),
  ColorList (..),
  ItemClass (..),
  --
  nothing,
  everything,
  itemLevel,
  dropLevel,
  quality,
  rarity,
  itemClass,
  baseType,
  baseTypes,
  sockets,
  linkedSockets,
  socketGroup,
  width,
  height
  ) where

import           Data.List                    (nub)

import           Text.ParserCombinators.ReadP as ReadP
import           Text.Read                    as Read

-- primitive categories
data PrimCat = ItemLevel Ordering Int
             | DropLevel Ordering Int
             | Quality Ordering Int
             | Rarity Ordering RarityLevel
             | Class ItemClass
             | BaseType [String]
             | Sockets Ordering Int
             | LinkedSockets Ordering Int
             | SocketGroup ColorList
             | Height Ordering Int
             | Width Ordering Int
             deriving (Eq,Show)

-- TODO: check for valid values

data RarityLevel = Normal | Magic | Rare | Unique
                 deriving (Eq,Show,Read,Enum)

data SocketColor = Red | Green | Blue | White
                 deriving (Eq,Enum)

instance Show SocketColor where
  show Red = "R"
  show Green = "G"
  show Blue = "B"
  show White = "W"

newtype ColorList = ColorList [SocketColor]
                  deriving Eq

instance Show ColorList where
  show (ColorList xs) = concatMap show xs

data ItemClass = ActiveSkillGems
               | Amulets
               | Belts
               | BodyArmour
               | Boots
               | Bows
               | Claws
               | Daggers
               | DivinationCard
               | FishingRods
               | Gloves
               | Helmets
               | HybridFlasks
               | Jewel
               | LifeFlasks
               | ManaFlasks
               | MapFragments
               | Maps
               | OneHandAxes
               | OneHandMaces
               | OneHandSwords
               | QuestItems
               | Quivers
               | Rings
               | Sceptres
               | Shields
               | StackableCurrency
               | Staves
               | SupportSkillGems
               | ThrustingOneHandSwords
               | TwoHandAxes
               | TwoHandMaces
               | TwoHandSwords
               | UtilityFlasks
               | Wands
               deriving (Eq, Enum)

instance Show ItemClass where
  show ActiveSkillGems = "Active Skill Gems"
  show Amulets = "Amulets"
  show Belts = "Belts"
  show BodyArmour = "Body Armour"
  show Boots = "Boots"
  show Bows = "Bows"
  show Claws = "Claws"
  show Daggers = "Daggers"
  show DivinationCard = "Divination Card"
  show FishingRods = "Fishing Rods"
  show Gloves = "Gloves"
  show Helmets = "Helmets"
  show HybridFlasks = "Hybrid Flasks"
  show Jewel = "Jewel"
  show LifeFlasks = "Life Flasks"
  show ManaFlasks = "Mana Flasks"
  show MapFragments = "Map Fragments"
  show Maps = "Maps"
  show OneHandAxes = "One Hand Axes"
  show OneHandMaces = "One Hand Maces"
  show OneHandSwords = "One Hand Sword"
  show QuestItems = "Quest Items"
  show Quivers = "Quivers"
  show Rings = "Rings"
  show Sceptres = "Sceptres"
  show Shields = "Shields"
  show StackableCurrency = "Currency"
  show Staves = "Staves"
  show SupportSkillGems = "Support Skill Gems"
  show ThrustingOneHandSwords = "Thrusting One Hand Swords"
  show TwoHandAxes = "Two Hand Axes"
  show TwoHandMaces = "Two Hand Maces"
  show TwoHandSwords = "Two Hand Swords"
  show UtilityFlasks = "Utility Flasks"
  show Wands = "Wands"

instance Read ItemClass where
  readPrec = Read.choice $ map (read'.show) [ActiveSkillGems .. Wands]

read' :: String -> ReadPrec ItemClass
read' x = lift $ do
  _ <- ReadP.string x
  return $ readItemClass x

readItemClass :: String -> ItemClass
readItemClass "Active Skill Gems" = ActiveSkillGems
readItemClass "Amulets" = Amulets
readItemClass "Belts" = Belts
readItemClass "Body Armour" = BodyArmour
readItemClass "Boots" = Boots
readItemClass "Bows" = Bows
readItemClass "Claws" = Claws
readItemClass "Daggers" = Daggers
readItemClass "Divination Card" = DivinationCard
readItemClass "Fishing Rods" = FishingRods
readItemClass "Gloves" = Gloves
readItemClass "Helmets" = Helmets
readItemClass "Hybrid Flasks" = HybridFlasks
readItemClass "Jewel" = Jewel
readItemClass "Life Flasks" = LifeFlasks
readItemClass "Mana Flasks" = ManaFlasks
readItemClass "Map Fragments" = MapFragments
readItemClass "Maps" = Maps
readItemClass "One Hand Axes" = OneHandAxes
readItemClass "One Hand Maces" = OneHandMaces
readItemClass "One Hand Sword" = OneHandSwords
readItemClass "Quest Items" = QuestItems
readItemClass "Quivers" = Quivers
readItemClass "Rings" = Rings
readItemClass "Sceptres" = Sceptres
readItemClass "Shields" = Shields
readItemClass "Currency" = StackableCurrency
readItemClass "Staves" = Staves
readItemClass "Support Skill Gems" = SupportSkillGems
readItemClass "Thrusting One Hand Swords" = ThrustingOneHandSwords
readItemClass "Two Hand Axes" = TwoHandAxes
readItemClass "Two Hand Maces" = TwoHandMaces
readItemClass "Two Hand Swords" = TwoHandSwords
readItemClass "Utility Flasks" = UtilityFlasks
readItemClass "Wands" = Wands
readItemClass _ = undefined

-- "real" categories
data Category = Empty | All | Category [[PrimCat]]
  deriving (Show,Eq)

conditions :: Category -> [[PrimCat]]
conditions (Category xss) = xss
conditions Empty = []
conditions All = undefined

union :: Category -> Category -> Category
union Empty x = x
union x Empty = x
union All _ = All
union _ All = All
union x y = Category (conditions x ++ conditions y)

intersect :: Category -> Category -> Category
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect All x = x
intersect x All = x
intersect x y = Category (conditions x `cross` conditions y)
  where xss `cross` yss = [ xs ++ ys | xs <- xss, ys <- yss]

-- utility
unionAll :: [Category] -> Category
unionAll = foldl union Empty

primitive :: PrimCat -> Category
primitive p = Category [[p]]

-- optimization/simplification functions
-- optimizes tries to find obviously empty conditions and removes them
optimize :: Category -> Category
optimize = filterClassConflicts

filterClassConflicts :: Category -> Category
filterClassConflicts (Category xss) = Category $ filter (not.hasClassConflict) xss
filterClassConflicts x = x

hasClassConflict :: [PrimCat] -> Bool
hasClassConflict = (>1).length.nub.filter isClassCond
  where isClassCond (Class _) = True
        isClassCond _         = False

joinBaseTypes :: Category -> Category
joinBaseTypes Empty = Empty
joinBaseTypes All = All
joinBaseTypes (Category xss) = Category $ map (unifyBaseType ([],BaseType [])) xss
  where unifyBaseType (others, bt) [] = bt:others
        unifyBaseType (others, BaseType bts) (BaseType x:xs) = unifyBaseType (others, BaseType (x++bts)) xs
        unifyBaseType (others, bt) (x:xs) = unifyBaseType (x:others, bt) xs

-- generation functions
nothing :: Category
nothing = Empty
everything :: Category
everything = All

itemLevel :: Ordering -> Int -> Category
dropLevel :: Ordering -> Int -> Category
quality :: Ordering -> Int -> Category
rarity :: Ordering -> RarityLevel -> Category
itemClass :: ItemClass -> Category
baseType :: String -> Category
baseTypes :: [String] -> Category
sockets :: Ordering -> Int -> Category
linkedSockets :: Ordering -> Int -> Category
socketGroup :: ColorList -> Category
height :: Ordering -> Int -> Category
width :: Ordering -> Int -> Category
itemLevel o n = primitive $ ItemLevel o n
dropLevel o n = primitive $ DropLevel o n
quality o n = primitive $ Quality o n
rarity o r = primitive $ Rarity o r
itemClass c = primitive $ Class c
baseType t = primitive $ BaseType [t]
baseTypes ts = primitive $ BaseType ts
sockets o n = primitive $ Sockets o n
linkedSockets o n = primitive $ LinkedSockets o n
socketGroup cs = primitive $ SocketGroup cs
height o n = primitive $ Height o n
width o n = primitive $ Width o n

-- create filter code for category
-- each list element goes into an extra Show-block
implementCategory :: Category -> [String]
implementCategory Empty = []
implementCategory All = [""]
implementCategory c = [ concatMap implementPrimCat xs | xs <- conditions c ]

implementPrimCat :: PrimCat -> String
implementPrimCat (ItemLevel ord n) = orderedParameter "ItemLevel" ord n
implementPrimCat (DropLevel ord n) = orderedParameter "DropLevel" ord n
implementPrimCat (Quality ord n) = orderedParameter "Quality" ord n
implementPrimCat (Rarity ord r) = orderedParameter "Rarity" ord r
implementPrimCat (Class c) = genericParameter "Class" (show c)
implementPrimCat (BaseType ts) = parameter "BaseType" $ unwords $ map show ts
implementPrimCat (Sockets ord n) = orderedParameter "Sockets" ord n
implementPrimCat (LinkedSockets ord n) = orderedParameter "LinkedSockets" ord n
implementPrimCat (SocketGroup cs) = genericParameter "SocketGroup" cs
implementPrimCat (Height ord n) = orderedParameter "Height" ord n
implementPrimCat (Width ord n) = orderedParameter "Width" ord n

genericParameter :: Show a => String -> a -> String
genericParameter keyword arg = parameter keyword (show arg)

orderedParameter :: Show a => String -> Ordering -> a -> String
orderedParameter keyword ord arg = parameter keyword (comperator ord ++ show arg)

parameter :: String -> String -> String
parameter keyword arg = keyword ++ " " ++ arg ++ newline

comperator :: Ordering -> String
comperator LT = "< "
comperator EQ = " "
comperator GT = "> "

newline :: String
newline = "\n"