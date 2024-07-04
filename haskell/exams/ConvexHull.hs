module ConvexHull (convexHull)  where
import Data.List -- for sortBy
import Data.Complex (polar)
import GHC.IO.Exception (stackOverflow)

points = [(-2.0, 3.0), ( 2.0, 2.0), (-1.0, 1.0), (-2.0,-1.5), ( 4.0,-1.0), ( 1.0,-3.0)]

comparePoints :: RealFloat a => (a,a) -> (a,a) -> (a,a)
comparePoints (x1, y1) (x2, y2) | y1 == y2 = if x1 > x2 then (x1, y1) else (x2, y2)
                          | otherwise = if y1 < y2 then (x1, y1) else (x2, y2)

findP :: RealFloat a => [(a,a)] -> (a, a)
findP points = foldl comparePoints (head points) points

getAngle :: RealFloat a => (a,a) -> (a, a) -> a
getAngle (x1, y1) (x2, y2) = let angle = atan2 (y2 - y1) (x2 - x1)
                             in if angle < 0 then angle + (2 * pi) else angle

processSorting :: RealFloat a => [(a,a)] -> (a, a) -> [(a,a)]
processSorting points p0 = let filteredPoints = filter (\x -> (x /= p0)) points
                               f a b =  if (getAngle p0 a) > (getAngle p0 b) then GT else LT
                           in p0 : (sortBy f filteredPoints) 

isLeft :: RealFloat a => (a,a) -> (a,a) -> (a,a) -> Bool
isLeft (x1, y1) (x2, y2) (x3, y3) = let first = (x2 - x1) * (y3 - y1)
                                        second = (y2 - y1) * (x3 - x1)
                                        c = first - second
                                    in if c > 0 then True else False

processPointsHelper :: RealFloat a => [(a,a)] -> [(a,a)] -> [(a,a)]
processPointsHelper sortedPoints stack = let p2 = head sortedPoints
                                             p1 = head stack
                                             p0 = head (tail stack)
                                             left = isLeft p0 p1 p2
                                         in if left == True 
                                            then processPoints (tail sortedPoints) (p2 : stack) 
                                            else processPoints sortedPoints (tail stack)

processPoints :: RealFloat a => [(a,a)] -> [(a,a)] -> [(a,a)]
processPoints [] stack = reverse stack
processPoints sortedPoints stack = if (length stack) < 2 
                                    then processPoints (tail sortedPoints) (head sortedPoints : stack)  
                                    else processPointsHelper sortedPoints stack

convexHull :: RealFloat a => [(a,a)] -> [(a,a)]
convexHull points = let p0 = findP points
                        sortedPoints = processSorting points p0
                        in processPoints sortedPoints []