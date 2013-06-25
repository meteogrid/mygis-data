
type Minute = Int
type Horizons = [Minute]

mkHorizon :: Integral a => a -> Maybe (DimensionValue Minute)
mkHorizon m | m >= 0     = Just . Horizon . fromIntegral $ m
            | otherwise  = Nothing

fromList []     = []
fromList (x:xs) = case mkHorizon x of
                       Just v  -> Just (v:fromList xs)
                       Nothing -> Nothing
