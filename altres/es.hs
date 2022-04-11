main = do
  putStrLn "Com et dius?"
  nom <- getLine -- binding o lligam. Retorna un IO::String, no un String
  putStr ("Veig que et dius " ++ nom ++ ". Quin any vas neixer?")
  any <- getLine
  putStr ("Tens " ++ show (2022 - read any) ++ " anys") -- el "read" infereix el tipus que necessitem