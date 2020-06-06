------------------------------------------------------------------------------
-- Modulo      :  QueueData
-- Programador :
-- Estabilidad :  experimental
-- Portabilidad:  experimental
--
-- Optativa: Programacion Funcional en Haskell
-- Este Modulo implementa Colas/Filas Finitas (FIFO)
------------------------------------------------------------------------------
module QueueData where

-- *** 0) Definicion del tipo de datos Cola ----------------------------------

-- | Estructura polimórfica (parametrizada por 'a') con un único constructor 'Q'
data QData a = Q [a] deriving (Eq, Ord)

-- | Estructura polimórifca (parametrizada por 'a') recursiva con dos constructores
-- 'EmptyQR' representa la Queue vacia
-- 'QR' es un registro que almacena un elemento de tipo 'a' y otra Queue como 'resto' (parte recursiva)
data QDataR a = EmptyQR | QR {prim :: a, resto :: QDataR a} deriving (Eq, Ord)

-- | Estructura polimórfica (parametrizada por 'a') recursiva con dos constructores
-- 'EmptyQOp' representa la Queue vacia
-- El operador '(:-:)' construye una nueva Queue con un elemento de tipo 'a' y otra Queue (parte recursiva)
-- El operador '(:-:)' tiene prioridad '5' y es asociativo a derecha, es decir:
-- >>> 5 :-: 3 :-: 1 :-: EmptyQOp -- es igual a...
-- 5 :-: (3 :-: (1 :-: EmptyQOp))
infixr 5 :-:

data QDataOp a = EmptyQOp | a :-: (QDataOp a) deriving (Eq, Ord, Show)

-- *** 1) Constructores -------------------------------------------------------

-- ** 1.1 Construccion de constantes

emptyQ :: QData a
emptyQ = Q []

emptyQR :: QDataR a
emptyQR = EmptyQR

emptyQOp :: QDataOp a
emptyQOp = EmptyQOp

-- ** 1.2 Construccion por transformacion de estructuras de tipo Cola a tipo Lista y viceversa

ltoQ :: [a] -> QData a
ltoQ = Q

qtoL :: QData a -> [a]
qtoL (Q li) = li

ltoQR :: [a] -> QDataR a
ltoQR [] = EmptyQR
ltoQR (x : xs) = QR {prim = x, resto = ltoQR xs}

qRtoL :: QDataR a -> [a]
qRtoL EmptyQR = []
qRtoL (QR x xs) = x : qRtoL xs

ltoQOp :: [a] -> QDataOp a
ltoQOp = foldr (:-:) EmptyQOp

qOptoL :: QDataOp a -> [a]
qOptoL EmptyQOp = []
qOptoL (x :-: xs) = x : qOptoL xs

-- *** 3) Funciones de manipulacion de colas -----------------------------------

-- | Cola - Last In First Out
insertQ :: Eq a => a -> QData a -> QData a
insertQ x (Q xs) = Q (x : xs)

supresQ :: QData a -> QData a
supresQ (Q (_ : xs)) = Q xs
supresQ (Q []) = error "empty Queue"

copyQ :: QData a -> a
copyQ (Q (x : _)) = x
copyQ (Q []) = error "empty Queue"

isEmptyQ :: QData a -> Bool
isEmptyQ (Q xs) = null xs

-- 4) Funciones utiles sobre Estructuras en general (2 fciones)-----------------------

-- | Pertenencia (Membership)
inQ :: Eq a => a -> QData a -> Bool
inQ x (Q s) = foldl (\a e -> a || (e == x)) False s

-- | Cardinalidad (cantidad de elementos en la estructura)
cardQ :: QData a -> Int
cardQ (Q s) = foldl (\a _ -> a + 1) 0 s

-- *** 5) Funciones de orden Superior sobre Colas ----------------------------

-- | Foldings
-- definir recursiva con o sin acumuladores
foldQr :: (a -> b -> b) -> b -> QData a -> b
foldQr f b (Q xs) = foldr f b xs

foldQl :: (a -> b -> a) -> a -> QData b -> a
foldQl f b (Q xs) = foldl f b xs

-- | Mapping
-- Definir usando 'foldQr' or 'foldQl'
mapQ :: Eq a => (t -> a) -> QData t -> QData a
mapQ f queue = Q $ foldQr (\e acc -> f e : acc) [] queue

-- | Filter
-- Definir usando 'foldQr' or 'foldQl'
filterQ :: Eq a => (a -> Bool) -> QData a -> QData a
filterQ p queue = Q $ foldQr (\e acc -> if p e then e : acc else acc) [] queue

-- | Zip
-- Definir usando 'foldQr' or 'foldQl'
zipQ :: QData a1 -> QData a2 -> QData (a1, a2)
zipQ q1 q2 = fst $ foldQl step (emptyQ, q2) q1
  where
    step (Q res, Q (y : ys)) e = (Q $ res ++ [(e, y)], Q ys)
    step (acc, Q []) _ = (acc, Q [])

mapQ1 :: (a -> b) -> QData a -> QData b
mapQ1 f (Q s) = Q (foldl (\a e -> f e : a) [] s)

veoQ :: Show a => QData a -> String
veoQ (Q []) = ""
veoQ (Q (x : xs)) = show x ++ "\n" ++ veoQ (Q xs)

-- Instancias de Functor

-- >>> (+1) <$> Q [1..3]
-- Cola (QData):
--  2
--  3
--  4
--
-- >>> (+1) <$> 1 :-: 2 :-: 3 :-: EmptyQOp
-- 2 :-: (3 :-: (4 :-: EmptyQOp))
--
-- >>> (+1) <$> foldr (:-:) EmptyQOp [1..3]
-- 2 :-: (3 :-: (4 :-: EmptyQOp))
--
-- >>> (+1) <$> QR 1 (QR 2 (QR 3 (EmptyQR)))
-- Cola (QDataR):
--  1
--  2
--  3
--
-- >>> (+1) <$> foldr QR EmptyQR [1..3]
-- Cola (QDataR):
--  2
--  3
--  4
--

instance Functor QData where
  fmap f (Q xs) = Q $ map f xs

instance Functor QDataOp where
  fmap _ EmptyQOp = EmptyQOp
  fmap f (a :-: next) = f a :-: fmap f next

instance Functor QDataR where
  fmap _ EmptyQR = EmptyQR
  fmap f (QR x xs) = QR (f x) (fmap f xs)

-- Instancias de Semigroup

instance Semigroup (QDataOp a) where
  EmptyQOp <> queue = queue
  queue <> EmptyQOp = queue
  (x :-: EmptyQOp) <> queue = x :-: queue
  (x :-: xs) <> queue = x :-: xs <> queue

-- Instancias de Applicative

instance Applicative QData where
  pure a = Q [a]
  (Q fs) <*> (Q xs) = Q $ fs <*> xs

instance Applicative QDataOp where
  pure a = a :-: EmptyQOp
  EmptyQOp <*> _ = EmptyQOp
  _ <*> EmptyQOp = EmptyQOp
  (f :-: fs) <*> (x :-: xs) = f x :-: (f <$> xs) <> (fs <*> xs)

-- ** 6.3 Definicion explicita alternativa de showsPrec sobre QDataR y/o QData

instance Show a => Show (QDataR a) where
  show EmptyQR = "Cola Vacia"
  show (QR a as) = "Cola (QDataR):\n┌ " <> show a <> showElems as
    where
      showElems (QR x xs) = "\n├ " <> show x <> showElems xs
      showElems EmptyQR = "\n▼"

instance Show a => Show (QData a) where
  show (Q []) = "Cola Vacia"
  show (Q (a : as)) = "Cola (QData):\n┌ " <> show a <> showElems as
    where
      showElems (x : xs) = "\n├ " <> show x <> showElems xs
      showElems [] = "\n▼"
