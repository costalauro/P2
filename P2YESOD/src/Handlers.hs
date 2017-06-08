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

getHelloR :: Handler Html
getHelloR = defaultLayout [whamlet|
     <h1> _{MsgHello}
|]

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formFuncionario
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroR enctype widget "Cadastrar Funcionário"
                 [whamlet|
                    <a href=@{HomeR}> 
                        Voltar
                 |]

getFuncionarioR :: FuncionarioId -> Handler Html
getFuncionarioR pid = do
             funcionario <- runDB $ get404 pid 
             dpto <- runDB $ get404 (funcionarioDeptoid funcionario)
             prof <- runDB $ get404 (funcionarioProfid funcionario)
             defaultLayout [whamlet| 
                 <h1> Funcionário: #{funcionarioNome funcionario}
                 <p> Salario: #{funcionarioSalario funcionario}
                 <p> Idade: #{funcionarioIdade funcionario}
                 <p> Departamento: #{departamentoNome dpto}
                 <p> Profissão: #{profissaoNome prof}
                    <a href=@{HomeR}> 
                        Voltar
             |]

postFuncionarioR :: FuncionarioId -> Handler Html
postFuncionarioR pid = do
     runDB $ delete pid
     redirect ListarR

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc FuncionarioNome]
             defaultLayout $ do 
             [whamlet|
                 <h1> Funcionários cadastradas:
                 $forall Entity pid funcionario <- listaP
                     <a href=@{FuncionarioR pid}> #{funcionarioNome funcionario} 
                     <form method=post action=@{FuncionarioR pid}> 
                         <input type="submit" value="Deletar"><br>
                     
             |] 
             [whamlet|
                 <a href=@{HomeR}> 
                        Voltar    
             |] 
             toWidget [lucius|
                form  { display:inline; }
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formFuncionario
                case result of
                    FormSuccess funcionario -> do
                       runDB $ insert funcionario 
                       defaultLayout [whamlet| 
                           <h1> #{funcionarioNome funcionario} Inserido com sucesso!
                           <a href=@{HomeR}> 
                                Voltar
                       |]
                    _ -> redirect HomeR
                    
getCadDeptoR :: Handler Html
getCadDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ do
             widgetForm CadDeptoR enctype widget "Departamentos"
             [whamlet|
                    <a href=@{HomeR}> 
                        Voltar
             |]
             
getDeptoR :: DepartamentoId -> Handler Html
getDeptoR did = do
             departamento <- runDB $ get404 did 
             defaultLayout [whamlet| 
                 <h1> Departamento: #{departamentoNome departamento}
                 <p> Sigla: #{departamentoSigla departamento}
                    <a href=@{HomeR}> 
                        Voltar
             |]
