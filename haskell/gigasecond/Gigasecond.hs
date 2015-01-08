module Gigasecond (fromDay) where

import Data.Time

dayDelta ::  Integer
dayDelta = floor $ 10^9 / (24 * 60 * 60)

fromDay ::  Day -> Day
fromDay d = addDays dayDelta d
