module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Hashable (hash, hashEqual)

newtype MyBool = MyBool Boolean

data MyADTPets 
  = Cat String Int
  | Dog String Int

instance myShowBoolean :: Show MyBool where
  show (MyBool true) = "True oh yeah!"
  show (MyBool false) = "Flase oh No!"

instance myShowPets :: Show MyADTPets where
  show (Cat name age) = "I'm a cat, my name is " <> name <> "and my age" <> show age
  show (Dog name age) = "I'm a dog, my name is " <> name <> "and my age" <> show age

main :: Effect Unit
main = do
  logShow $ hash 123
  logShow (hash true)
  logShow (hash [1, 2, 3, 4])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")
  logShow (MyBool true)
  logShow (Cat "Anastasia" 3)

