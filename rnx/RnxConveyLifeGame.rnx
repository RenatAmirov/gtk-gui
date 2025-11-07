{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (set, on, AttrOp((:=)))
import qualified Graphics.Rendering.Cairo as Cairo
import Data.IORef
import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import System.Random (randomRIO)

-- = Логика игры "Жизнь" Конвея =

-- Размеры игрового поля
gridWidth :: Int
gridWidth = 50

gridHeight :: Int
gridHeight = 50

-- Размер клетки в пикселях
cellSize :: Double
cellSize = 10.0

-- Состояние игры
data GameState = GameState
  { cells :: [[Bool]]           -- Двумерный массив клеток (True - живая, False - мертвая)
  , gameRunning :: Bool         -- Запущена ли симуляция
  , generation :: Int           -- Номер поколения
  } deriving (Show)

-- Начальное состояние игры (все клетки мертвые)
initialGameState :: IO GameState
initialGameState = do
  let emptyCells = replicate gridHeight (replicate gridWidth False)
  return $ GameState
    { cells = emptyCells
    , gameRunning = False
    , generation = 0
    }

-- Получить состояние клетки (с обработкой границ)
getCell :: [[Bool]] -> Int -> Int -> Bool
getCell grid x y
  | x < 0 || x >= gridWidth || y < 0 || y >= gridHeight = False
  | otherwise = grid !! y !! x

-- Подсчет живых соседей
countLiveNeighbors :: [[Bool]] -> Int -> Int -> Int
countLiveNeighbors grid x y = length
  [ (dx, dy)
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , not (dx == 0 && dy == 0)
  , getCell grid (x + dx) (y + dy)
  ]

-- Применить правила игры "Жизнь" к одной клетке
applyRules :: [[Bool]] -> Int -> Int -> Bool
applyRules grid x y = 
  let currentState = getCell grid x y
      liveNeighbors = countLiveNeighbors grid x y
  in case currentState of
    True -> liveNeighbors == 2 || liveNeighbors == 3  -- Выживание
    False -> liveNeighbors == 3                       -- Рождение

-- Вычислить следующее поколение
nextGeneration :: GameState -> GameState
nextGeneration state =
  let oldGrid = cells state
      newGrid = [ [ applyRules oldGrid x y | x <- [0..gridWidth-1] ] | y <- [0..gridHeight-1] ]
  in state
    { cells = newGrid
    , generation = generation state + 1
    }

-- Переключить состояние клетки
toggleCell :: GameState -> Int -> Int -> GameState
toggleCell state x y =
  let oldGrid = cells state
      newGrid = [ [ if rowIndex == y && colIndex == x then not cell else cell
                  | (colIndex, cell) <- zip [0..] row ]
                | (rowIndex, row) <- zip [0..] oldGrid ]
  in state { cells = newGrid }

-- Случайным образом заполнить поле
randomizeGrid :: GameState -> IO GameState
randomizeGrid state = do
  newGrid <- mapM (\_ -> 
    mapM (\_ -> randomRIO (False, True)) [1..gridWidth]) [1..gridHeight]
  return $ state { cells = newGrid }

-- Очистить поле (все клетки мертвые)
clearGrid :: GameState -> GameState
clearGrid state =
  state { cells = replicate gridHeight (replicate gridWidth False), generation = 0 }

-- = Графический интерфейс GTK =

-- Создание главного окна
createWindow :: IORef GameState -> IO ()
createWindow gameStateRef = do
  Gtk.initGUI
  
  -- Создание главного окна
  window <- Gtk.windowNew
  Gtk.set window 
    [ Gtk.windowTitle := (pack "Игра Жизнь Конвея - Haskell/GTK+3")
    , Gtk.containerBorderWidth := 10
    , Gtk.windowDefaultWidth := 800
    , Gtk.windowDefaultHeight := 600
    ]
  
  -- Главный вертикальный  контейнер
  mainBox <- Gtk.vBoxNew False 10
  Gtk.containerAdd window mainBox
  
  -- Метка для отображения информации
  infoLabel <- Gtk.labelNew (Just (pack "Поколение: 0 | Игра приостановлена"))
  Gtk.boxPackStart mainBox infoLabel Gtk.PackNatural 0
  
  -- Область для рисования игры
  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest drawingArea (floor (fromIntegral gridWidth * cellSize)) 
                                   (floor (fromIntegral gridHeight * cellSize))
  Gtk.boxPackStart mainBox drawingArea Gtk.PackGrow 0
  
  -- Горизонтальный контейнер для кнопок
  buttonBox <- Gtk.hBoxNew True 10
  Gtk.boxPackStart mainBox buttonBox Gtk.PackNatural 0
  
  -- Кнопки управления
  startButton <- Gtk.buttonNewWithLabel (pack "Старт")
  Gtk.boxPackStart buttonBox startButton Gtk.PackGrow 0
  
  pauseButton <- Gtk.buttonNewWithLabel (pack "Пауза")
  Gtk.boxPackStart buttonBox pauseButton Gtk.PackGrow 0
  
  stepButton <- Gtk.buttonNewWithLabel (pack "Шаг")
  Gtk.boxPackStart buttonBox stepButton Gtk.PackGrow 0
  
  clearButton <- Gtk.buttonNewWithLabel (pack "Очистить")
  Gtk.boxPackStart buttonBox clearButton Gtk.PackGrow 0
  
  randomButton <- Gtk.buttonNewWithLabel (pack "Случайно")
  Gtk.boxPackStart buttonBox randomButton Gtk.PackGrow 0
  
  -- Обработчик отрисовки с использованием Cairo
  Gtk.on drawingArea Gtk.draw $ do
    state <- liftIO $ readIORef gameStateRef
    let cellGrid = cells state
    
    -- Устанавливаем белый фон
    Cairo.setSourceRGB 1 1 1
    Cairo.paint
    
    -- Рисуем сетку и клетки
    Cairo.setSourceRGB 0.8 0.8 0.8  -- Светло-серый для сетки
    
    -- Вертикальные линии
    forM_ [0..gridWidth] $ \x -> do
      let xPos = fromIntegral x * cellSize
      Cairo.moveTo xPos 0
      Cairo.lineTo xPos (fromIntegral gridHeight * cellSize)
    
    -- Горизонтальные линии
    forM_ [0..gridHeight] $ \y -> do
      let yPos = fromIntegral y * cellSize
      Cairo.moveTo 0 yPos
      Cairo.lineTo (fromIntegral gridWidth * cellSize) yPos
    
    Cairo.setLineWidth 0.5
    Cairo.stroke
    
    -- Рисуем живые клетки
    Cairo.setSourceRGB 0 0 0  -- Черный для живых клеток
    
    forM_ [0..gridHeight-1] $ \y -> do
      forM_ [0..gridWidth-1] $ \x -> do
        when (cellGrid !! y !! x) $ do
          let xPos = fromIntegral x * cellSize + 0.5
          let yPos = fromIntegral y * cellSize + 0.5
          Cairo.rectangle xPos yPos (cellSize - 1) (cellSize - 1)
          Cairo.fill
  
  -- Функция для обновления интерфейса
  let updateUI = do
        state <- readIORef gameStateRef
        let status = if gameRunning state 
                     then "Поколение: " ++ show (generation state) ++ " | Игра запущена"
                     else "Поколение: " ++ show (generation state) ++ " | Игра приостановлена"
        Gtk.set infoLabel [Gtk.labelLabel := pack status]
        Gtk.widgetQueueDraw drawingArea
  
  -- Обработка кликов мыши для переключения клеток
  Gtk.on drawingArea Gtk.buttonPressEvent $ do
    (x, y) <- Gtk.eventCoordinates
    state <- liftIO $ readIORef gameStateRef
    
    -- Преобразуем координаты мыши в координаты сетки
    let cellX = floor (x / cellSize)
    let cellY = floor (y / cellSize)
    
    -- Переключаем клетку только если игра на паузе
    when (not (gameRunning state) && 
          cellX >= 0 && cellX < gridWidth && 
          cellY >= 0 && cellY < gridHeight) $ do
      liftIO $ do
        modifyIORef gameStateRef (\s -> toggleCell s cellX cellY)
        updateUI
    
    return True
  
  -- Обработка кнопки старта
  Gtk.on startButton Gtk.buttonActivated $ do
    modifyIORef gameStateRef (\s -> s { gameRunning = True })
    updateUI
  
  -- Обработка кнопки паузы
  Gtk.on pauseButton Gtk.buttonActivated $ do
    modifyIORef gameStateRef (\s -> s { gameRunning = False })
    updateUI
  
  -- Обработка кнопки шага
  Gtk.on stepButton Gtk.buttonActivated $ do
    modifyIORef gameStateRef nextGeneration
    updateUI
  
  -- Обработка кнопки очистки
  Gtk.on clearButton Gtk.buttonActivated $ do
    modifyIORef gameStateRef clearGrid
    updateUI
  
  -- Обработка кнопки случайного заполнения
  Gtk.on randomButton Gtk.buttonActivated $ do
    state <- readIORef gameStateRef
    newState <- randomizeGrid state
    writeIORef gameStateRef newState
    updateUI
  
  -- Обработка нажатий клавиш
  Gtk.on window Gtk.keyPressEvent $ do
    keyVal <- Gtk.eventKeyVal
    case keyVal of
      32 -> do -- Пробел (переключение паузы)
        liftIO $ do
          modifyIORef gameStateRef (\s -> s { gameRunning = not (gameRunning s) })
          updateUI
        return True
      
      114 -> do -- 'R' (случайное заполнение)
        liftIO $ do
          state <- readIORef gameStateRef
          newState <- randomizeGrid state
          writeIORef gameStateRef newState
          updateUI
        return True
      
      99 -> do -- 'C' (очистка)
        liftIO $ do
          modifyIORef gameStateRef clearGrid
          updateUI
        return True
      
      _ -> return False
  
  -- Игровой цикл (обновление каждые 100 мс)
  let gameLoop :: IO Bool
      gameLoop = do
        state <- readIORef gameStateRef
        when (gameRunning state) $ do
          modifyIORef gameStateRef nextGeneration
          updateUI
        return True  -- Продолжаем цикл независимо от состояния
  
  -- Запуск игрового цикла
  void $ Gtk.timeoutAdd gameLoop 100
  
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