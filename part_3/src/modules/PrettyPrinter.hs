-- Module for serialize a Abstract Sintax Tree

module PrettyPrinter where

-- from a happy file get a pretty printer
-- import AbsGrammar
-- import LexGrammar
-- import ParGrammar
import ErrM

fromOk (ErrM.Ok a) = a

-- import Data.List

-- pretty printer
pprint x = putStrLn $ show $ fromOk x
