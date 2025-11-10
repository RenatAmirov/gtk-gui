{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (set, on, AttrOp((:=)))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)
import System.Process (readProcess)
import Network.Socket (getAddrInfo, defaultHints, addrAddress, addrFlags, AddrInfo(..))
import Network.Socket (AddrInfoFlag(AI_CANONNAME), SocketType(Stream))

-- Упрощенное разрешение имени хоста
resolveHostname :: String -> IO (Either String [String])
resolveHostname hostname = do
    let hints = defaultHints { 
          addrFlags = [AI_CANONNAME],
          addrSocketType = Stream 
        }
    
    addrinfos <- getAddrInfo (Just hints) (Just hostname) Nothing
    
    let addresses = map (show . addrAddress) addrinfos
    if null addresses
        then return $ Left "Не удалось разрешить имя хоста"
        else return $ Right addresses

-- Получение локальных IP через системные команды
getLocalIPs :: IO [String]
getLocalIPs = do
    -- Попробуем разные системные команды для получения IP
    result1 <- tryCommand "hostname" ["-I"]  -- Linux
    result2 <- tryCommand "ipconfig" ["/all"]  -- Windows
    result3 <- tryCommand "ifconfig" ["-a"]   -- macOS/Unix
    
    let allResults = filter (not . null) [result1, result2, result3]
    if null allResults
        then return ["Не удалось определить локальные IP-адреса"]
        else return $ take 1 allResults  -- Возвращаем первый успешный результат
  where
    tryCommand cmd args = do
        result <- catchAny (readProcess cmd args "") (\_ -> return "")
        return $ if null result then "" else cmd ++ ": " ++ take 100 result  -- Ограничиваем длину

        
-- Универсальный обработчик исключений
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch


-- Основной интерфейс
createWindow :: IO ()
createWindow = do
  Gtk.initGUI
  
  -- Создание главного окна
  window <- Gtk.windowNew
  Gtk.set window 
    [ Gtk.windowTitle := (pack "DNS Resolver - Haskell/GTK+3")
    , Gtk.containerBorderWidth := 15
    , Gtk.windowDefaultWidth := 500
    , Gtk.windowDefaultHeight := 400
    ]
  
  -- Главный вертикальный контейнер
  mainBox <- Gtk.vBoxNew False 10
  Gtk.containerAdd window mainBox
  
  -- Заголовок
  titleLabel <- Gtk.labelNew (Just (pack "Разрешение имен в локальной сети"))
  Gtk.miscSetAlignment titleLabel 0 0.5
  Gtk.boxPackStart mainBox titleLabel Gtk.PackNatural 5
  
  -- Область ввода
  inputFrame <- Gtk.frameNew
  Gtk.frameSetLabel inputFrame (pack "Ввод имени хоста")
  Gtk.boxPackStart mainBox inputFrame Gtk.PackNatural 5
  
  inputBox <- Gtk.vBoxNew False 5
  Gtk.containerSetBorderWidth inputBox 10
  Gtk.containerAdd inputFrame inputBox
  
  -- Поле ввода имени хоста
  hostnameBox <- Gtk.hBoxNew False 5
  Gtk.boxPackStart inputBox hostnameBox Gtk.PackNatural 0
  
  hostnameLabel <- Gtk.labelNew (Just (pack "Имя хоста:"))
  Gtk.boxPackStart hostnameBox hostnameLabel Gtk.PackNatural 0
  
  hostnameEntry <- Gtk.entryNew
  Gtk.entrySetText hostnameEntry (pack "localhost")
  Gtk.boxPackStart hostnameBox hostnameEntry Gtk.PackGrow 0
  
  -- Кнопки
  buttonBox <- Gtk.hBoxNew True 10
  Gtk.boxPackStart inputBox buttonBox Gtk.PackNatural 5
  
  resolveButton <- Gtk.buttonNewWithLabel (pack "Разрешить имя")
  Gtk.boxPackStart buttonBox resolveButton Gtk.PackGrow 0
  
  localIPsButton <- Gtk.buttonNewWithLabel (pack "Мои IP-адреса")
  Gtk.boxPackStart buttonBox localIPsButton Gtk.PackGrow 0
  
  clearButton <- Gtk.buttonNewWithLabel (pack "Очистить")
  Gtk.boxPackStart buttonBox clearButton Gtk.PackGrow 0
  
  -- Область результатов
  resultsFrame <- Gtk.frameNew
  Gtk.frameSetLabel resultsFrame (pack "Результаты")
  Gtk.boxPackStart mainBox resultsFrame Gtk.PackGrow 0
  
  resultsBox <- Gtk.vBoxNew False 5
  Gtk.containerSetBorderWidth resultsBox 10
  Gtk.containerAdd resultsFrame resultsBox
  
  -- Текстовое поле для результатов
  resultsScrolled <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowSetShadowType resultsScrolled Gtk.ShadowIn
  Gtk.scrolledWindowSetPolicy resultsScrolled Gtk.PolicyAutomatic Gtk.PolicyAutomatic
  Gtk.boxPackStart resultsBox resultsScrolled Gtk.PackGrow 0
  
  resultsTextView <- Gtk.textViewNew
  Gtk.textViewSetEditable resultsTextView False
  Gtk.textViewSetCursorVisible resultsTextView False
  Gtk.textViewSetWrapMode resultsTextView Gtk.WrapWord
  Gtk.containerAdd resultsScrolled resultsTextView
  
  resultsBuffer <- Gtk.textViewGetBuffer resultsTextView
  
  -- Строка состояния
  statusbar <- Gtk.statusbarNew
  Gtk.boxPackStart mainBox statusbar Gtk.PackNatural 0
  
  statusContext <- Gtk.statusbarGetContextId statusbar (pack "main")
  
  -- Функция обновления результатов
  let updateResults text = do
        Gtk.textBufferSetText resultsBuffer (pack text)
        Gtk.statusbarPop statusbar statusContext
        Gtk.statusbarPush statusbar statusContext (pack "Готово")
  
  -- Обработчик кнопки разрешения имени
  _ <- Gtk.on resolveButton Gtk.buttonActivated $ do
    hostnameText <- Gtk.entryGetText hostnameEntry
    let hostname = unpack hostnameText
    
    Gtk.statusbarPop statusbar statusContext
    _ <- Gtk.statusbarPush statusbar statusContext (pack ("Разрешение " ++ hostname ++ "..."))
    
    result <- resolveHostname hostname
    case result of
      Right addrs -> do
        let resultText = "IP-адреса для '" ++ hostname ++ "':\n" ++ 
                        unlines (map ("  - " ++) addrs)
        updateResults resultText
      Left err -> do
        let resultText = "Ошибка разрешения '" ++ hostname ++ "':\n  " ++ err
        updateResults resultText
  
  -- Обработчик кнопки локальных IP
  _ <- Gtk.on localIPsButton Gtk.buttonActivated $ do
    Gtk.statusbarPop statusbar statusContext
    _ <- Gtk.statusbarPush statusbar statusContext (pack "Получение локальных IP-адресов...")
    
    localIPs <- getLocalIPs
    let resultText = (pack "Локальные IP-адреса:\n" ++ unlines (map ("  - " ++) localIPs))
    _ <- updateResults resultText
    Gtk.statusbarPop statusbar statusContext
    Gtk.statusbarPush statusbar statusContext (pack "Локальные IP-адреса получены")
  
  -- Обработчик кнопки очистки
  _ <- Gtk.on clearButton Gtk.buttonActivated $ do
    Gtk.textBufferSetText resultsBuffer ""
    Gtk.statusbarPop statusbar statusContext
    Gtk.statusbarPush statusbar statusContext (pack "Очищено")
  
  -- Обработчик закрытия окна
  _ <- Gtk.on window Gtk.objectDestroy Gtk.mainQuit
  
  -- Инициализация
  _ <- updateResults (pack "Добро пожаловать в DNS Resolver!\n\n" ++
                "Используйте:\n" ++
                "- 'Разрешить имя' для поиска IP по имени хоста\n" ++
                "- 'Мои IP-адреса' для просмотра локальных адресов\n" ++
                "- 'Очистить' для очистки результатов\n\n" ++
                "Примеры имен: localhost, google.com")
  
  -- Показ окна
  Gtk.widgetShowAll window
  Gtk.mainGUI

-- Главная функция
main :: IO ()
main = createWindow