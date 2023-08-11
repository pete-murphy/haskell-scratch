{-# LANGUAGE TransformListComp #-}

import GHC.Exts

employees :: [(String, String, Int)]
employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60) ]

output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept using groupWith
         , then sortWith by (sum salary)
         , then take 2 ]

main = print output

-- [("Yale",60),("Ed",85)] 
