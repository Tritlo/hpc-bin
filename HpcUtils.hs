module HpcUtils where

import Trace.Hpc.Util
import qualified HpcMap as Map

-- turns \n into ' '
-- | grab's the text behind a HpcPos; 
grabHpcPos :: Map.Map Int String -> HpcPos -> String
grabHpcPos hsMap span = 
         case lns of
           [ln] -> (take ((c2 - c1) + 1) $ drop (c1 - 1) ln)
           _ -> let lns1 = drop (c1 -1) (head lns) : tail lns
                    lns2 = init lns1 ++ [take (c2 + 1) (last lns1) ]
                 in foldl1 (\ xs ys -> xs ++ "\n" ++ ys) lns2
  where (l1,c1,l2,c2) = fromHpcPos span
        lns = map (\ n -> case Map.lookup n hsMap of
                           Just ln -> ln
                           Nothing -> error $ "bad line number : " ++ show n
                  ) [l1..l2]

