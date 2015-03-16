module Builtins where
import EvalLisp
import ParseLisp



addEnv :: Environment -> IO LValue -> IO (Environment, LValue)
addEnv env arg = fmap (\a -> (env, a)) arg


repl = repl' "stdin" stdEnv

repl' :: String -> Environment -> IO ()
repl' fname env = do
    putStr "hank-lisp> "
    line <- getLine
    handleCommands (env, fname) line (\str -> do
            let str' = (parseString str)
            (env', value) <- if null str' then return (env, nil) else lispEval(env, tok2Val $ head str')
            case value == nil of
                True -> repl' fname env'
                False -> do
                    print value
                    repl' fname $ ("it", value) : env')

loadBlocks :: String -> [LValue]
loadBlocks text = toks2Vals $ parseString text

execString env str = execBlocks env (loadBlocks str)

evalStr env str = case loadBlocks str of
    blocks -> (execBlocks env (init blocks)) >>= (\env -> lispEval(env, last blocks)) 


loadForRepl :: String -> IO ()
loadForRepl fname = do
    text <- readFile fname
    let trees = toks2Vals $ parseString text
    let env = execBlocks stdEnv trees
    env >>= (repl' fname)

execBlocks :: Environment -> [LValue] -> IO Environment
execBlocks env trees = foldl execExpr (return env) trees

handleCommands :: (Environment, String) -> String -> (String -> IO ()) -> IO ()
handleCommands (env, fname) str alt | null str = alt str
    | head str == ':' = case tail str of
        [] -> putStrLn "unrecognized command" >> alt ""
        "d" -> printDir env >> alt ""
        "dir" -> printDir env >> alt ""
        "reload" -> putStrLn "reloaded files" >> if (fname == "stdin") then repl else loadForRepl fname
        "r" -> putStrLn "reloaded files" >> if (fname == "stdin") then repl else loadForRepl fname
        "q" -> putStrLn "Leaving HankLisp."
        a -> putStrLn ("unknown command " ++ a) >> alt ""
    | otherwise = alt str

stdEnv :: Environment
stdEnv = [("+", LFunction lPlus "(+ x1 x2... xn) -> sum of x1 thru xn"),
    ("*", LFunction lTimes "(* x1 x2... xn) -> product of x1 thru xn"),
    ("eval", LFunction lEval "(eval something) -> unquoted 'something'"),
    ("car", LFunction lCar "(car (a b)) -> a"),
    ("cdr", LFunction lCdr "(cdr (a b)) -> (b)"),
    ("cons", LFunction lCons "(cons x1 x2) -> (x1 x2)"),
    ("get", LFunction lGetLine "(get) -> a single string from input"),
    ("putLn", LFunction lPutLine "(putLn x1 x2... xn) -> writes x1 x2.. xn to stdout, separated by spaces"),
    ("nil", Nil),
    ("=", LFunction lEq "(= x1 x2... xn) -> T if all are equal, else nil"),
    ("eq", LFunction lEq "(eq x1 x2... xn) -> T if all are equal, else nil"),
    ("write", LFunction lWriteFile "(write fname x1... xn) -> writes x1 thru xn joined by spaces to fname"),
    ("read", LFunction lReadFile "(read fname) -> reads from fname"),
    ("_", Str " "),
    ("concat", LFunction lConcat "(concat s1 s2..sn) -> s1 thru sn concatenated without spaces"),
    ("stringP", LFunction lStringP "(stringP arg) -> whether arg is a string"),
    ("toString", LFunction lToString "(toString arg) -> a string for arg. Not reversable."),
    ("flat", LFunction lFlat "(flat a1 a2... an) -> a list of all the arguments, with lists flattened"),
    ("null", LFunction lNull "(null arg) -> whether arg is null"),
    ("not", LFunction lNot "(not arg) -> logical negation of arg"),
    ("substr", LFunction lSubStr "(substr str start [end]) -> the string from index start onwards to end"),
    ("<=", LFunction lLessEq "(<= x1 x2 ... xn) -> whether x1 <= x2 <= ... xn"),
    ("-", LFunction lMinus "(- x1 x2 ... xn) -> foldr (-) 0 [x1 x2... xn]"),
    ("list", LFunction lList "(list x1 ... xn) -> (x1 ... xn)"),
    ("runString", LFunction lRunString "(runString text) -> evaluates text as lisp source code"),
    ("dir", LFunction lDispEnv "(dir) -> prints out the current scope")]



mapIOTuple :: Environment -> (LValue -> LValue) -> IO LValue -> IO (Environment, LValue)
mapIOTuple env f val = fmap (\x -> (env, x)) (fmap f val)

lPlus :: LFunctionT
lPlus env = mapIOTuple env (\args -> case haskList args of
    [] -> Number 0
    nums ->  foldr (\(Number x) (Number y) -> Number (x + y)) (Number 0) nums)

lMinus :: LFunctionT
lMinus env = mapIOTuple env (\args -> case haskList args of
    [] -> Number 0
    nums ->  foldr (\(Number x) (Number y) -> Number (x - y)) (Number 0) nums)
    
lTimes :: LFunctionT
lTimes env = mapIOTuple env (\args -> case haskList args of
    [] -> Number 1
    nums -> foldr (\(Number x) (Number y) -> Number (x * y)) (Number 1) nums)

lEval :: LFunctionT
lEval env arghs = arghs >>= (\a -> case a of
    thing:.Nil -> lispEval (env, thing))

lList :: LFunctionT
lList env args = fmap (\x -> (env, x)) args

lCar :: LFunctionT
lCar env = mapIOTuple env (\stuff -> case stuff of
    ((a:.b):.Nil) -> a
    (a:.b) -> a)

lCdr :: LFunctionT
lCdr env = mapIOTuple env (\stuff -> case stuff of
    ((a:.b):.Nil) -> b
    (a:.b) -> b)

lCons :: LFunctionT
lCons env = mapIOTuple env (\(a:.b:.Nil) -> (a:.b))

lGetLine :: LFunctionT
lGetLine env args = fmap (\x -> (env, x)) (fmap Str getLine)

lPutLine :: LFunctionT
lPutLine env args = (addEnv env) $ args >>= (\args' -> fmap (const nil) $ putStrLn (unwords $ lmapToList extract args'))

lEq :: LFunctionT
lEq env = mapIOTuple env (\args ->
    let
        blah (a:.b:.Nil) = a == b
        blah (a:.b:.xs) = (a == b) && (blah (b:.xs))
    in case blah args of
        True -> Atom "T"
        False -> Nil)

lConcat :: LFunctionT
lConcat env = mapIOTuple env (\args -> case args of
    _ -> Str $ concat (lmapToList extract args))

lNull :: LFunctionT
lNull env = mapIOTuple env (\args -> case args of
    Nil:.a -> Atom "T"
    a -> Nil)

lNot :: LFunctionT
lNot = lNull




lStringP :: LFunctionT
lStringP env = mapIOTuple env (\args -> case args of
    (Str a):.Nil -> Atom "T"
    other -> Nil)

lToString :: LFunctionT
lToString env = mapIOTuple env (\args -> case args of
    (Str s):.Nil -> Str s
    (Atom a):.Nil -> Str a
    a:.Nil -> Str $ show a)

lFlat :: LFunctionT
lFlat env = mapIOTuple env (\args -> lispList $ concat (lmapToList toList args)) where
    toList :: LValue -> [LValue]
    toList (a:.b) = haskList (a:.b)
    toList a = [a]
    
lSubStr :: LFunctionT
lSubStr env = mapIOTuple env (\args -> case args of
    (Str str):.(Number start):.Nil -> subStrSafe str (fromInteger start) (length str)
    (Str str):.(Number start):.(Number end):.Nil -> subStrSafe str (fromInteger start) (fromInteger end)
    a -> Str $ "Failure: bad args given: " ++ show a)

subStrSafe :: String -> Int -> Int -> LValue
subStrSafe str start end = if (start > -1) && (end >= start) && (end <= length str)
    then Str $ drop start (take end str)
    else Str $ "Failure: string indices out of bounds: " ++ (show start) ++ " " ++ (show end) ++ " " ++ (show $ length str)


printDir :: Environment -> IO ()
printDir env = putStrLn (unlines $ map (\(x, y) -> x ++ " " ++ show y) env)

lDispEnv :: LFunctionT
lDispEnv env _ = printDir env >> return (env, nil)

lLessEq :: LFunctionT
lLessEq env = mapIOTuple env (\args -> case isSorted (haskList args) of
    True -> Atom "T"
    False -> Nil)


lRunString :: LFunctionT
lRunString env args = args >>= (\arg -> case arg of
    (Str str):._ -> evalStr env str)

--Taken from Data.List.Ordered on Hackage
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (_:[]) = True
isSorted (x:y:zs) = (x <= y) && isSorted (y:zs)



lReadFile :: LFunctionT
lReadFile env args = (addEnv env) $ args >>= (\args' -> case args' of
    (Str fname):.Nil -> fmap Str (readFile fname)
    badArg:.Nil -> return $ Str ("Failure: " ++ show badArg ++ " is not a string or atom")
    Nil -> return $ Nil)

lWriteFile :: LFunctionT
lWriteFile env args = (addEnv env) $ args >>= (\args' -> case args' of
    (Str fname):.rest -> writeFile fname (unwords $ lmapToList extract rest) >> return Nil
    badArg:.blah -> return $ Str ("Failure: " ++ show badArg ++ " is not a string or atom")
    Nil -> return $ Str "Failure: no file specified to be written")
