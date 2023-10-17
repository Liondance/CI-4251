-- Examples using 'let' and 'where'

parabola (a, b, c) = (\x -> a * x^2 + b * x + c)

-- roots using 'let'
rootsl :: Floating r => (r, r, r) -> (r, r)
rootsl (a, b, c) =   let
                        radical = sqrt(b^2 - 4 * a * c)
                        den = 2 * a
                    in
                        ( (-b - radical) / den, (-b + radical) / den)

-- roots using 'where'
rootsw :: Floating r => (r, r, r) -> (r, r)
rootsw (a, b, c) = ( (-b - radical) / den, (-b + radical) / den)
                    where
                        radical = sqrt(b^2 - 4 * a * c)
                        den = 2 * a
