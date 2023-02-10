pay :: Float -> Float -> Float -> (Float,Float)
pay rate hrs pto
    | hrs >= 37                      = (rate * hrs, pto)
    | (hrs + pto) <= 37              = ((hrs + pto) * rate, 0.0)
    | otherwise                      = (37 * rate, pto - (37 - hrs))