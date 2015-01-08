module SpaceAge where

import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune 
    deriving (Show, Eq, Ord, Read, Bounded)

type Ratio = Float

type PlanetMap = Map.Map Planet Ratio

planets :: PlanetMap
planets = Map.fromList 
    [(Earth, 1)
    ,(Mercury, 0.2408467)
    ,(Venus, 0.61519726)
    ,(Mars, 1.8808158)
    ,(Jupiter, 11.862615)
    ,(Saturn, 29.447498)
    ,(Uranus, 84.016846)
    ,(Neptune, 164.79132)
    ]

ageOn ::  Planet -> Ratio -> Ratio
ageOn planetType seconds = fromJust $ (/) <$> Just (seconds/31557600) <*> Map.lookup planetType planets
