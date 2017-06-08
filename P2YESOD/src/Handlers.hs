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
             areq doubleField "Salário :" Nothing <*>
             areq dayField "Data Nascimento :" Nothing <*>
             areq (selectField dptos) "Departamentos :" Nothing <*>
             areq (selectField profs) "Profissões :" Nothing

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

profs = do
       entidades <- runDB $ selectList [] [Asc ProfissaoNome] 
       optionsPairs $ fmap (\ent -> (profissaoSigla $ entityVal ent, entityKey ent)) entidades

formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome :" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","3")]} Nothing

formProfissao :: Form Profissao
formProfissao = renderDivs $ Profissao <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Abreviação :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","10")]} Nothing <*>
            areq (selectField dptos) "Departamentos :" Nothing
            
formTreinamento :: Form Treinamento
formTreinamento = renderDivs $ Treinamento <$>
            areq textField "Nome :" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Abreviação :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","10")]} Nothing <*>
            areq textField "Responsavel :" Nothing <*>
            areq textField "Sala :" Nothing <*>
            areq intField "Qtd. Pessoas :" Nothing <*>
            areq (selectField profs) "Profissões :" Nothing <*>
            areq dayField "Data Aplicacao :" Nothing <*>
            areq dayField "Data Validade :" Nothing
