module Axioms where
import Parser

isAxiom x = detectAxiom x > 0


detectAxiom x
    | detectAxiom'1 x = 1
    | detectAxiom'2 x = 2
    | detectAxiom'3 x = 3
    | detectAxiom'4 x = 4
    | detectAxiom'5 x = 5
    | detectAxiom'6 x = 6
    | detectAxiom'7 x = 7
    | detectAxiom'8 x = 8
    | detectAxiom'9 x = 9
    | detectAxiom'10 x = 10
    | otherwise = -1

-- a -> b -> a
detectAxiom'1 :: Node -> Bool
detectAxiom'1 (NodeBinary '>' a (NodeBinary '>' b c)) = a == c
detectAxiom'1 x = False

-- (a -> b) -> (a -> b -> c) -> (a -> c)
detectAxiom'2 :: Node -> Bool
detectAxiom'2 (NodeBinary '>' 
                (NodeBinary '>' a0 b0)
                (NodeBinary '>' 
                    (NodeBinary '>' a1 (NodeBinary '>' b1 c0))
                    (NodeBinary '>' a2 c1)
                )
              ) = a0 == a1 && a1 == a2 && b0 == b1 && c0 == c1
detectAxiom'2 x = False

-- a -> b -> (a & b)
detectAxiom'3 :: Node -> Bool
detectAxiom'3 (NodeBinary '>'
                a0
                (NodeBinary '>'
                    b0 
                    (NodeBinary '&' a1 b1)
                )
              ) = a0 == a1 && b0 == b1
detectAxiom'3 x = False

-- a & b -> a
detectAxiom'4 :: Node -> Bool
detectAxiom'4 (NodeBinary '>'
                (NodeBinary '&' a0 b0)
                a1
              ) = a0 == a1
detectAxiom'4 x = False

-- a & b -> b
detectAxiom'5 :: Node -> Bool
detectAxiom'5 (NodeBinary '>'
                (NodeBinary '&' a0 b0)
                b1
              ) = b0 == b1
detectAxiom'5 x = False

-- a -> a | b
detectAxiom'6 :: Node -> Bool
detectAxiom'6 (NodeBinary '>'
                a0
                (NodeBinary '|' a1 b1)
              ) = a0 == a1
detectAxiom'6 x = False

-- b -> a | b
detectAxiom'7 :: Node -> Bool
detectAxiom'7 (NodeBinary '>'
                b0
                (NodeBinary '|' a1 b1)
              ) = b0 == b1
detectAxiom'7 x = False

-- (a -> c) -> (b -> c) -> ((a | b) -> c)
detectAxiom'8 :: Node -> Bool
detectAxiom'8 (NodeBinary '>'
                (NodeBinary '>' a0 c0)
                (NodeBinary '>' 
                    (NodeBinary '>' b0 c1)
                    (NodeBinary '>' 
                        (NodeBinary '|' a1 b1)
                        c2
                    )
                )
              ) = a0 == a1 && b0 == b1 && c0 == c1 && c1 == c2
detectAxiom'8 x = False

-- (a -> b) -> (a -> !b) -> !a
detectAxiom'9 :: Node -> Bool
detectAxiom'9 (NodeBinary '>'
                (NodeBinary '>' a0 b0)
                (NodeBinary '>' 
                    (NodeBinary '>' a1 (NodeNot b1))
                    (NodeNot a2)
                )
              ) = a0 == a1 && a1 == a2 && b0 == b1
detectAxiom'9 x = False

-- !!a -> a
detectAxiom'10 :: Node -> Bool
detectAxiom'10 (NodeBinary '>'
                    (NodeNot (NodeNot a0))
                    a1
               ) = a0 == a1
detectAxiom'10 x = False

