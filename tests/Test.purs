module Test where

data T = T | N   -- | M | Q


-- t :: (T,T) -> T
-- t (T,T) = T
-- t (T,N) = T
-- t (N,N) = N
-- t (T,N) = T
-- t _ = N
t :: (T,T,T,T,T,T,T) -> T
t (T,N,N,T,N,T,T) = T
t (T,T,N,T,N,T,T) = T
t (T,T,T,T,N,T,T) = T
t (T,T,T,T,T,T,T) = T



-- k :: (T,T,T,T,T,T)
-- k =  (T,T,T,T,T,T)
-- t (N,N,N,N,T,T,N) = T
-- t (_,_,_,_,_,_,T) = T
-- t (_,_,T) = T


-- t (N,T) = N

-- t (y,Q) = y
-- t (N,x) = N
-- t (Q,x) = N

-- j :: List T -> List T
-- j [] = []
-- j [T|y] = []
-- j [N|y] = []
-- j [M|y] = []
-- j [Q|y] = []
