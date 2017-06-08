{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Time.Calendar

import Database.Persist.Postgresql

formFuncionario :: Form Funcionario
formFuncionario = renderDivs $ Funcionario <$>
             areq textField "Nome :" Nothing <*>
             areq intField "Idade :" Nothing <*>
             areq doubleField "Sal�rio :" Nothing <*>
             areq dayField "Data Nascimento :" Nothing <*>
             areq (selectField dptos) "Departamentos :" Nothing <*>
             areq (selectField profs) "Profiss�es :" Nothing

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

profs = do
       entidades <- runDB $ selectList [] [Asc ProfissaoNome] 
       optionsPairs $ fmap (\ent -> (profissaoSigla $ entityVal ent, entityKey ent)) entidades

             
