module Axioms where
import Parser

detect x
    |   (Impl a (Impl b c)) <- x, a == c = 1
    | 
        (Impl 
            (Impl a0 b0)
            (Impl 
                (Impl a1 (Impl b1 c0))
                (Impl a2 c1)
            )
        ) <- x,
        a0 == a1 && a1 == a2 && b0 == b1 && c0 == c1  = 2
    |
        (Impl
            a0
            (Impl
                b0 
                (And a1 b1)
            )
        ) <- x,
        a0 == a1 && b0 == b1 = 3
    |
        (Impl
            (And a0 b0)
            a1
        ) <- x,
        a0 == a1 = 4
    |
        (Impl
            (And a0 b0)
            b1
        ) <- x,
        b0 == b1 = 5
    |
        (Impl
            a0
            (Or a1 b1)
         ) <- x,
         a0 == a1 = 6
    |
        (Impl
            b0
            (Or a1 b1)
         ) <- x,
         b0 == b1 = 7
    |
        (Impl
            (Impl a0 c0)
            (Impl 
                (Impl b0 c1)
                (Impl 
                    (Or a1 b1)
                    c2
                )
            )
        ) <- x,
        a0 == a1 && b0 == b1 && c0 == c1 && c1 == c2 = 8
    |
        (Impl
            (Impl a0 b0)
            (Impl 
                (Impl a1 (Not b1))
                (Not a2)
            )
        ) <- x,
        a0 == a1 && a1 == a2 && b0 == b1 = 9
    |
        (Impl
            (Not (Not a0))
            a1
        ) <- x,
        a0 == a1 = 10
    | otherwise = 0


        


