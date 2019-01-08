module LearnParsers where

import Text.Trifecta

stop :: Parser Char
stop = unexpected "stop"

one = char '1'
one' = one >> stop
one'' = one >> eof

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
oneTwo'' = oneTwo >> eof

stringOne = string "1"

stringOneTwo :: Parser String
stringOneTwo = string "12"

stringOneTwo' = stringOneTwo >> stop

stringOneTwoThree :: Parser String
stringOneTwoThree = string "123"

stringOneTwoThree' = string "123" >> stop

testParse :: (Show a) => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

string' (x : xs) = char x >> string' xs
string' [] = pure ()

stringOneTwoThree'' = string' "123"

main = do
	pNL "stop:"
	testParse stop
	pNL "one:"
	testParse one
	pNL "one':"
	testParse one'
	pNL "one'':"
	testParse one''
	pNL "oneTwo:"
	testParse oneTwo
	pNL "oneTwo':"
	testParse oneTwo'
	pNL "oneTwo'':"
	testParse oneTwo''
	pNL "stringOne:"
	testParse stringOne
	pNL "stringOneTwo:"
	testParse stringOneTwo
	pNL "stringOneTwo':"
	testParse stringOneTwo'
	pNL "stringOneTwoThree:"
	testParse stringOneTwoThree
	pNL "stringOneTwoThree':"
	testParse stringOneTwoThree'
	pNL "stringOneTwoThree'':"
	testParse stringOneTwoThree''


