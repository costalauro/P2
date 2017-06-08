{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Departamento
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show

Treinamento
   nome Text
   responsavel Text
   sala Text
   sigla Text sqltype=varchar(10)
   qtdPessoas Int
   profid ProfissaoId
   dataVal Day
   dataAplic Day
   deriving Show

Funcionario
   nome Text
   idade Int
   salario Double
   dataNasc Day
   deptoid DepartamentoId
   profid ProfissaoId
   deriving Show

Profissao
   nome Text
   sigla Text sqltype=varchar(10)
   deptoid DepartamentoId
   deriving Show
   
Usuario
   nome Text
   email Text
   senha Text
   UniqueEmail email
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

mkMessage "Sitio" "messages" "pt-BR"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Soh o admin acessa aqui!"

isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")
