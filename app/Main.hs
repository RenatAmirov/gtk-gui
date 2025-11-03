{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (set, on, AttrOp((:=)))
import qualified Graphics.Rendering.Cairo as Cairo
import Data.IORef
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import System.Random (randomRIO)

-- = Логика игры "Червячок" =

-- Направление движения червяка (переименовано чтобы избежать конфликта)
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)

-- Позиция на игровом поле
type Position = (Int, Int)

-- Состояние игры
data GameState = GameState
  { snake :: [Position]      -- Голова в начале списка
  , direction :: Direction
  , food :: Position
  , gameOver :: Bool
  , score :: Int
  } deriving (Show)

-- Размеры игрового поля
gridSize :: Int
gridSize = 20

cellSize :: Int
cellSize = 25

-- Начальное состояние игры
initialGameState :: IO GameState
initialGameState = do
  let startSnake = [(5, 5), (5, 4), (5, 3)]  -- Начальная позиция червяка
  startFood <- generateFood startSnake
  return $ GameState
    { snake = startSnake
    , direction = DirRight
    , food = startFood
    , gameOver = False
    , score = 0
    }

-- Генерация еды в случайной позиции, не занятой червяком
generateFood :: [Position] -> IO Position
generateFood snakePositions = do
  x <- randomRIO (0, gridSize - 1)
  y <- randomRIO (0, gridSize - 1)
  if (x, y) `elem` snakePositions
    then generateFood snakePositions
    else return (x, y)

-- Обновление состояния игры
updateGame :: GameState -> IO GameState
updateGame state
  | gameOver state = return state
  | otherwise = do
    let (headX, headY) = head (snake state)
        newHead = case direction state of
          DirUp    -> (headX, headY - 1)
          DirDown  -> (headX, headY + 1)
          DirLeft  -> (headX - 1, headY)
          DirRight -> (headX + 1, headY)
        
        -- Проверка столкновений
        collision = newHead `elem` tail (snake state) ||
                   fst newHead < 0 || fst newHead >= gridSize ||
                   snd newHead < 0 || snd newHead >= gridSize
        
        ateFood = newHead == food state
    
    if collision
      then return $ state { gameOver = True }
      else do
        let newSnakeBody = if ateFood 
              then snake state  -- Червяк растет, когда ест еду
              else init (snake state)
            
            newSnake = newHead : newSnakeBody
            
            newScore = if ateFood 
              then score state + 10 
              else score state
        
        newFood <- if ateFood 
          then generateFood newSnake 
          else return (food state)
        
        return $ state
          { snake = newSnake
          , food = newFood
          , score = newScore
          }

-- Изменение направления движения
changeDirection :: Direction -> Direction -> Direction
changeDirection currentDir newDir
  -- Запрещаем движение в противоположном направлении
  | (currentDir, newDir) `elem` [(DirUp, DirDown), (DirDown, DirUp), (DirLeft, DirRight), (DirRight, DirLeft)] 
    = currentDir
  | otherwise = newDir

-- = Графический интерфейс GTK =

-- Функция для рисования одной клетки с использованием Cairo
drawCell :: Cairo.Render () -> Position -> Cairo.Render ()
drawCell setColor (x, y) = do
  let xPix = fromIntegral (x * cellSize) + 0.5
      yPix = fromIntegral (y * cellSize) + 0.5
      size = fromIntegral (cellSize - 1)
  setColor
  Cairo.rectangle xPix yPix size size
  Cairo.fill

-- Создание главного окна
createWindow :: IORef GameState -> IO ()
createWindow gameStateRef = do
  Gtk.initGUI
  
  -- Создание главного окна
  window <- Gtk.windowNew
  Gtk.set window 
    [ Gtk.windowTitle := "Червячок (Snake) - Haskell/GTK+3"
    , Gtk.containerBorderWidth := 10
    , Gtk.windowDefaultWidth := gridSize * cellSize + 50
    , Gtk.windowDefaultHeight := gridSize * cellSize + 100
    ]
  
  -- Главный вертикальный контейнер
  mainBox <- Gtk.vBoxNew False 10
  Gtk.containerAdd window mainBox
  
  -- Метка для отображения счета
  scoreLabel <- Gtk.labelNew (Just "Счет: 0")
  Gtk.boxPackStart mainBox scoreLabel Gtk.PackNatural 0
  
  -- Область для рисования игры
  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest drawingArea (gridSize * cellSize) (gridSize * cellSize)
  Gtk.boxPackStart mainBox drawingArea Gtk.PackGrow 0
  
  -- Метка для отображения статуса игры
  statusLabel <- Gtk.labelNew (Just "Игра началась! Управление: стрелки")
  Gtk.boxPackStart mainBox statusLabel Gtk.PackNatural 0
  
  -- Кнопка для перезапуска игры
  restartButton <- Gtk.buttonNewWithLabel "Новая игра"
  Gtk.boxPackStart mainBox restartButton Gtk.PackNatural 0
  
  -- Обработчик отрисовки с использованием Cairo
  Gtk.on drawingArea Gtk.draw $ do
    state <- liftIO $ readIORef gameStateRef
    
    -- Устанавливаем черный фон
    Cairo.setSourceRGB 0 0 0
    Cairo.paint
    
    -- Рисуем червяка (зеленый)
    let drawGreenCell = Cairo.setSourceRGB 0 1 0
    mapM_ (drawCell drawGreenCell) (tail (snake state))
    
    -- Рисуем голову червяка (ярко-зеленый)
    let drawBrightGreenCell = Cairo.setSourceRGB 0.2 1 0.2
    drawCell drawBrightGreenCell (head (snake state))
    
    -- Рисуем еду (красный)
    let drawRedCell = Cairo.setSourceRGB 1 0 0
    drawCell drawRedCell (food state)
    
    return True
  
  -- Функция для обновления интерфейса
  let updateUI = do
        state <- readIORef gameStateRef
        Gtk.set scoreLabel [Gtk.labelLabel := pack ("Счет: " ++ show (score state))]
        Gtk.set statusLabel [Gtk.labelLabel := pack (
          if gameOver state 
            then "Игра окончена! Нажмите 'Новая игра'"
            else "Игра идет... Управление: стрелки"
          )]
        Gtk.widgetQueueDraw drawingArea
  
  -- Обработка нажатий клавиш
  Gtk.on window Gtk.keyPressEvent $ do
    keyVal <- Gtk.eventKeyVal
    state <- liftIO $ readIORef gameStateRef
    let newDir = case keyVal of
          65362 -> Just DirUp      -- Код клавиши Up
          65364 -> Just DirDown    -- Код клавиши Down  
          65361 -> Just DirLeft    -- Код клавиши Left
          65363 -> Just DirRight   -- Код клавиши Right
          _ -> Nothing
        
    case newDir of
      Just dir -> do
        let currentDir = direction state
            actualDir = changeDirection currentDir dir
        liftIO $ modifyIORef gameStateRef (\s -> s { direction = actualDir })
        return True
      Nothing -> return False
  
  -- Обработка кнопки перезапуска
  Gtk.on restartButton Gtk.buttonActivated $ do
    newState <- initialGameState
    writeIORef gameStateRef newState
    updateUI
  
  -- Игровой цикл (обновление каждые 150 мс)
  let gameLoop :: IO Bool
      gameLoop = do
        state <- readIORef gameStateRef
        newState <- updateGame state
        writeIORef gameStateRef newState
        updateUI
        return (not (gameOver newState))
  
  -- Запуск игрового цикла
  void $ Gtk.timeoutAdd gameLoop 150
  
  -- Обработка закрытия окна
  Gtk.on window Gtk.objectDestroy Gtk.mainQuit
  
  -- Отображение окна и запуск главного цикла
  Gtk.widgetShowAll window
  Gtk.mainGUI

-- Главная функция
main :: IO ()
main = do
  initialState <- initialGameState
  gameStateRef <- newIORef initialState
  createWindow gameStateRef