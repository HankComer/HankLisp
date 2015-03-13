



module ScanLib where





data Phrase a = Phrase Int (Char -> Bool) (Action a) Int


type Status a = (String, [a])


type Action a = Status a -> Char -> Status a

type Reader a = String -> a

append :: Action a
append (buff, tokens) char = (buff ++ [char], tokens)

emit :: Reader a -> Action a
emit reader (buff, tokens) char = ([], tokens ++ [reader buff])

ignore :: Action a
ignore a char = a





