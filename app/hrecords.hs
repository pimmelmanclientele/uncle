{-# LANGUAGE DataKinds #-}

module Main where

import Data.HList.CommonMain

name = Label :: Label "name"
age = Label :: Label "age"

record = name .=."Victoria"
         .*. age .=. 205
         .*. emptyRecord

main = do
    print record