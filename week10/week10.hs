Interface Observable<Input> {
	map<R>(transform: (_: Input) => R): Observable<R>;
	scan<V>(initialValue:V, fun: (a:V, el:Input) => V) : Observable<V>;
	flatMap<Output>(streamCreator: (_:Input) => Observable<Output>) : Observable<Output>;
}

map :: Observable a -> (a -> b) -> Observable b
map = undefined

scan :: Observable a -> b -> (b -> a -> b) -> Observable b 
scan = undefined

flatMap :: Observable a -> (a -> Observable b) -> Observable b
flatMap = undefined


-- sumFile :: FilePath -> IO Int
-- sumFile file = do 
-- 	x <- readFile file 
-- 	putStr x 

parseInt :: String -> Int
parseInt s = read s

sumFile :: FilePath -> IO Int 
sumFile path = do
    intList <- map (parseInt) <$> lines <$> (readFile path)
    return $ foldr (+) 0 intList