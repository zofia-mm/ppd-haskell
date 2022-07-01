
-- -------------- --
-- zadanie drugie --
-- -------------- --


-- -- zadanie samo w sobie

{-
zamiast imienia i nazwiska (bo dziwnie się czuję nazywając funkcję po sobie),
wzięłam moje dwa imiona (po angielsku, żeby pasowały do reszty kodu)
-}

class ( Eq a, Show a ) => Sophie a where
    (*?) :: a -> a -> a

data Margarett = Minus | Plus | Zero deriving (Show, Read)

instance Eq Margarett where
    Minus == Minus = True
    Plus == Plus   = True
    Zero == Zero   = True
    _ == _         = False

instance Sophie Margarett where
    Zero *? _      = Zero
    _ *? Zero      = Zero
    Minus *? Minus = Plus
    Plus *? Plus   = Plus
    _ *? _         = Minus

assignMargarett :: Int -> Margarett
assignMargarett number = if number == 0
    then Zero
    else if number > 0
        then Plus
        else Minus


-- -- dodatkowy typ

{-
zauważyłam, że /\ fajnie wygląda, więc napisałam kolejne duo typ / klasa,
żeby to wykorzystać
potem dodałam jeszcze Sentence, żeby urwać się od monotonii
jeden typ - jedna klasa oraz poćwiczyć bardziej to zagadnienie
-}

class ( Eq a, Show a ) => Logical a where
    (/\) ::  a -> a -> Logic
    (\/) ::  a -> a -> Logic
    (==>) :: a -> a -> Logic
    (<=>) :: a -> a -> Logic

data Logic = L0 | L1 deriving (Show, Read)
instance Eq Logic where
    L0 == L0 = True
    L1 == L1 = True
    _  == _  = False
instance Logical Logic where
    L1 /\ L1  = L1
    _  /\ _   = L0
    L0 \/ L0  = L0
    _  \/ _   = L1
    L1 ==> L0 = L0
    _  ==> _  = L1
    a  <=> b  = ( a ==> b ) /\ ( b ==> a )

data Sentence = Sen String Logic deriving (Show, Read)
instance Eq Sentence where
    (Sen a b) == (Sen c d) = b == d
instance Logical Sentence where
    (Sen a b) /\ (Sen c d) = b /\ d
    (Sen a b) \/ (Sen c d) = b \/ d
    (Sen a b) ==> (Sen c d) = b ==> d
    (Sen a b) <=> (Sen c d) = b <=> d

sampleSentence1 = Sen "Prawdziwe zdanie." L1
sampleSentence2 = Sen "Fałszywe zdanie." L0

{- funcja wyciągająca wartość logiczną z Sentence -}
logicalValue :: Sentence -> Logic
logicalValue (Sen string logic) = logic
