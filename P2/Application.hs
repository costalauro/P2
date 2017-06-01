{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where
-- CADA TABELA VAI POSSUIR UM .hs
-- ESSE DEVE SER INCLUIDO NO .cabal
-- E IMPORTADO AQUI NO Application
import Foundation
import Yesod
import Funcionario
import Treinamento
import Profissao
import Add
import Home

mkYesodDispatch "App" resourcesApp