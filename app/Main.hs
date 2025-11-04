{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (set, on, AttrOp((:=)))
import qualified Graphics.Rendering.Cairo as Cairo
import Data.IORef
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.List (find)
import Data.Maybe (isJust, fromJust)

-- = Логика игры "Шашки" =

-- Тип шашки
data PieceType = Regular | King deriving (Eq, Show)

-- Цвет игрока
data Player = Black | White deriving (Eq, Show)

-- Шашка
data Piece = Piece 
  { piecePlayer :: Player
  , pieceType :: PieceType
  } deriving (Eq, Show)

-- Клетка доски
type Cell = Maybe Piece

-- Позиция на доске
type Position = (Int, Int)

-- Состояние игры
data GameState = GameState
  { board :: [[Cell]]           -- Доска 8x8
  , currentPlayer :: Player     -- Текущий игрок
  , selectedPiece :: Maybe Position  -- Выбранная шашка
  , validMoves :: [Position]    -- Возможные ходы для выбранной шашки
  , gameOver :: Bool
  , winner :: Maybe Player
  } deriving (Show)

-- Размер доски
boardSize :: Int
boardSize = 8

-- Размер клетки
cellSize :: Double
cellSize = 60

-- Начальное состояние игры
initialGameState :: GameState
initialGameState = GameState
  { board = initialBoard
  , currentPlayer = Black
  , selectedPiece = Nothing
  , validMoves = []
  , gameOver = False
  , winner = Nothing
  }

-- Создание начальной доски
initialBoard :: [[Cell]]
initialBoard = 
  [ [ if (i + j) `mod` 2 == 1 && i < 3 then Just (Piece White Regular)
      else if (i + j) `mod` 2 == 1 && i > 4 then Just (Piece Black Regular)
      else Nothing
    | j <- [0..7]
    ]
  | i <- [0..7]
  ]

-- Получить шашку на позиции
getPiece :: Position -> [[Cell]] -> Cell
getPiece (row, col) board = 
  if row >= 0 && row < boardSize && col >= 0 && col < boardSize
  then board !! row !! col
  else Nothing

-- Проверка, является ли позиция на доске
isValidPosition :: Position -> Bool
isValidPosition (row, col) = 
  row >= 0 && row < boardSize && col >= 0 && col < boardSize

-- Получить все возможные ходы для шашки
getValidMoves :: Position -> [[Cell]] -> Player -> [Position]
getValidMoves pos@(row, col) board player = 
  let piece = getPiece pos board
      moves = case piece of
        Just (Piece p Regular) -> 
          if p == player
          then getRegularMoves pos board player
          else []
        Just (Piece p King) -> 
          if p == player
          then getKingMoves pos board player
          else []
        _ -> []
  in moves

-- Ходы для обычной шашки
getRegularMoves :: Position -> [[Cell]] -> Player -> [Position]
getRegularMoves (row, col) board player =
  let directions = case player of
        Black -> [(-1, -1), (-1, 1)]  -- Черные ходят вверх
        White -> [(1, -1), (1, 1)]    -- Белые ходят вниз
      simpleMoves = 
        [ (row + dr, col + dc) 
        | (dr, dc) <- directions
        , isValidPosition (row + dr, col + dc)
        , getPiece (row + dr, col + dc) board == Nothing
        ]
      jumpMoves = 
        [ (row + 2*dr, col + 2*dc)
        | (dr, dc) <- directions
        , isValidPosition (row + dr, col + dc)
        , isValidPosition (row + 2*dr, col + 2*dc)
        , let middle = getPiece (row + dr, col + dc) board
        , let target = getPiece (row + 2*dr, col + 2*dc) board
        , isJust middle && piecePlayer (fromJust middle) /= player
        , target == Nothing
        ]
  in simpleMoves ++ jumpMoves

-- Ходы для дамки
getKingMoves :: Position -> [[Cell]] -> Player -> [Position]
getKingMoves (row, col) board player =
  let directions = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
      getMovesInDirection dr dc = 
        takeWhile (\(r, c) -> isValidPosition (r, c) && getPiece (r, c) board == Nothing) $
        take 7 $ iterate (\(r, c) -> (r + dr, c + dc)) (row + dr, col + dc)
  in concatMap (\(dr, dc) -> getMovesInDirection dr dc) directions

-- Проверка возможности хода для игрока
hasValidMoves :: Player -> [[Cell]] -> Bool
hasValidMoves player board =
  any (\(i, j) -> 
    case getPiece (i, j) board of
      Just (Piece p _) | p == player -> 
        not $ null $ getValidMoves (i, j) board player
      _ -> False
  ) [(i, j) | i <- [0..7], j <- [0..7]]

-- Проверка окончания игры
checkGameOver :: [[Cell]] -> Player -> (Bool, Maybe Player)
checkGameOver board currentPlayer =
  let blackPieces = countPieces Black board
      whitePieces = countPieces White board
      blackHasMoves = hasValidMoves Black board
      whiteHasMoves = hasValidMoves White board
  in if blackPieces == 0 
     then (True, Just White)
     else if whitePieces == 0
          then (True, Just Black)
          else if currentPlayer == Black && not blackHasMoves
               then (True, Just White)
               else if currentPlayer == White && not whiteHasMoves
                    then (True, Just Black)
                    else (False, Nothing)

-- Подсчет шашек игрока
countPieces :: Player -> [[Cell]] -> Int
countPieces player board =
  length [() | 
    i <- [0..7], 
    j <- [0..7],
    case getPiece (i, j) board of
      Just (Piece p _) -> p == player
      _ -> False
  ]

-- Выполнение хода
makeMove :: GameState -> Position -> Position -> GameState
makeMove state from to =
  let (fromRow, fromCol) = from
      (toRow, toCol) = to
      piece = fromJust $ getPiece from (board state)
      newBoard = updateBoard (board state) from to
      captured = if abs (toRow - fromRow) == 2  -- Это взятие
                then Just ((fromRow + toRow) `div` 2, (fromCol + toCol) `div` 2)
                else Nothing
      
      -- Обновляем доску после взятия
      finalBoard = case captured of
        Just capPos -> setCell newBoard capPos Nothing
        Nothing -> newBoard
      
      -- Проверяем превращение в дамку
      promotedBoard = if pieceType piece == Regular && 
                         ((piecePlayer piece == Black && toRow == 0) || 
                          (piecePlayer piece == White && toRow == 7))
                      then setCell finalBoard to (Just (Piece (piecePlayer piece) King))
                      else finalBoard
      
      -- Проверяем окончание игры
      (gameOver', winner') = checkGameOver promotedBoard (currentPlayer state)
      nextPlayer = if isJust captured && hasMoreJumps to promotedBoard (currentPlayer state)
                  then currentPlayer state  -- Продолжаем ход тем же игроком при множественном взятии
                  else case currentPlayer state of
                        Black -> White
                        White -> Black
  in state
     { board = promotedBoard
     , currentPlayer = nextPlayer
     , selectedPiece = Nothing
     , validMoves = []
     , gameOver = gameOver'
     , winner = winner'
     }

-- Проверка возможности дальнейших взятий
hasMoreJumps :: Position -> [[Cell]] -> Player -> Bool
hasMoreJumps pos board player =
  let jumps = [ (row + 2*dr, col + 2*dc)
              | (dr, dc) <- [(-1, -1), (-1, 1), (1, -1), (1, 1)]
              , isValidPosition (row + dr, col + dc)
              , isValidPosition (row + 2*dr, col + 2*dc)
              , let middle = getPiece (row + dr, col + dc) board
              , let target = getPiece (row + 2*dr, col + 2*dc) board
              , isJust middle && piecePlayer (fromJust middle) /= player
              , target == Nothing
              ]
      (row, col) = pos
  in not (null jumps)

-- Обновление доски при перемещении
updateBoard :: [[Cell]] -> Position -> Position -> [[Cell]]
updateBoard board from to =
  let piece = fromJust $ getPiece from board
  in setCell (setCell board from Nothing) to (Just piece)

-- Установка значения клетки
setCell :: [[Cell]] -> Position -> Cell -> [[Cell]]
setCell board (row, col) value =
  take row board ++
  [take col (board !! row) ++ [value] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board

-- = Графический интерфейс GTK =

-- Обработка клика по клетке
handleCellClick :: IORef GameState -> (IO ()) -> GameState -> Position -> IO ()
handleCellClick gameStateRef updateUI state pos = do
  let (row, col) = pos
      cell = getPiece pos (board state)
  
  case selectedPiece state of
    Nothing ->  -- Ничего не выбрано
      case cell of
        Just (Piece player _) | player == currentPlayer state -> 
          -- Выбираем шашку текущего игрока
          let moves = getValidMoves pos (board state) player
          in writeIORef gameStateRef $ state 
             { selectedPiece = Just pos
             , validMoves = moves
             }
        _ -> return ()  -- Клик по пустой клетке или чужой шашке
    
    Just selected ->  -- Уже есть выбранная шашка
      if pos `elem` validMoves state
      then do
        -- Выполняем ход
        modifyIORef gameStateRef (\s -> makeMove s selected pos)
      else
        -- Выбираем другую шашку или снимаем выделение
        case cell of
          Just (Piece player _) | player == currentPlayer state -> 
            let moves = getValidMoves pos (board state) player
            in writeIORef gameStateRef $ state 
               { selectedPiece = Just pos
               , validMoves = moves
               }
          _ -> 
            writeIORef gameStateRef $ state 
              { selectedPiece = Nothing
              , validMoves = []
              }
  
  updateUI

-- Рисование доски
drawBoard :: GameState -> Cairo.Render ()
drawBoard state = do
  -- Рисуем клетки доски
  sequence_ 
    [ drawCell i j (board state !! i !! j) 
    | i <- [0..7], j <- [0..7]
    ]
  
  -- Подсвечиваем выбранную шашку и возможные ходы
  case selectedPiece state of
    Just (selRow, selCol) -> do
      -- Подсветка выбранной шашки
      Cairo.setSourceRGB 0 1 0  -- Зеленый
      Cairo.rectangle 
        (fromIntegral selCol * cellSize) 
        (fromIntegral selRow * cellSize) 
        cellSize cellSize
      Cairo.setLineWidth 3
      Cairo.stroke
      
      -- Подсветка возможных ходов
      Cairo.setSourceRGB 1 0.5 0  -- Оранжевый
      mapM_ (\(row, col) -> do
        Cairo.rectangle 
          (fromIntegral col * cellSize + 5) 
          (fromIntegral row * cellSize + 5) 
          (cellSize - 10) (cellSize - 10)
        Cairo.setLineWidth 2
        Cairo.stroke
        ) (validMoves state)
    Nothing -> return ()

-- Рисование клетки и шашки
drawCell :: Int -> Int -> Cell -> Cairo.Render ()
drawCell row col cell = do
  -- Рисуем клетку
  let color = if (row + col) `mod` 2 == 0 
              then (0.8, 0.8, 0.8)  -- Светлая
              else (0.4, 0.2, 0.0)  -- Темная (коричневая)
  Cairo.setSourceRGB (fst3 color) (snd3 color) (thd3 color)
  Cairo.rectangle 
    (fromIntegral col * cellSize) 
    (fromIntegral row * cellSize) 
    cellSize cellSize
  Cairo.fill
  
  -- Рисуем шашку если есть
  case cell of
    Just piece -> drawPiece row col piece
    Nothing -> return ()
  where
    fst3 (x, _, _) = x
    snd3 (_, y, _) = y
    thd3 (_, _, z) = z

-- Рисование шашки
drawPiece :: Int -> Int -> Piece -> Cairo.Render ()
drawPiece row col piece = do
  let (color, borderColor) = case piecePlayer piece of
        Black -> ((0.2, 0.2, 0.2), (0.1, 0.1, 0.1))  -- Черная
        White -> ((0.9, 0.9, 0.9), (0.7, 0.7, 0.7))  -- Белая
      
      centerX = fromIntegral col * cellSize + cellSize / 2
      centerY = fromIntegral row * cellSize + cellSize / 2
      radius = cellSize * 0.4
  
  -- Рисуем основу шашки
  Cairo.setSourceRGB (fst3 color) (snd3 color) (thd3 color)
  Cairo.arc centerX centerY radius 0 (2 * pi)
  Cairo.fill
  
  -- Рисуем границу
  Cairo.setSourceRGB (fst3 borderColor) (snd3 borderColor) (thd3 borderColor)
  Cairo.arc centerX centerY radius 0 (2 * pi)
  Cairo.setLineWidth 2
  Cairo.stroke
  
  -- Если это дамка, рисуем корону
  when (pieceType piece == King) $ do
    Cairo.setSourceRGB 1 0.84 0  -- Золотой
    Cairo.arc centerX centerY (radius * 0.6) 0 (2 * pi)
    Cairo.fill
    
    Cairo.setSourceRGB 0.8 0.6 0  -- Темно-золотой
    Cairo.arc centerX centerY (radius * 0.6) 0 (2 * pi)
    Cairo.setLineWidth 1.5
    Cairo.stroke
  where
    fst3 (x, _, _) = x
    snd3 (_, y, _) = y
    thd3 (_, _, z) = z

-- Создание главного окна
createWindow :: IORef GameState -> IO ()
createWindow gameStateRef = do
  Gtk.initGUI
  
  -- Создание главного окна
  window <- Gtk.windowNew
  Gtk.set window 
    [ Gtk.windowTitle := (pack "Шашки - Haskell/GTK+3")
    , Gtk.containerBorderWidth := 10
    , Gtk.windowDefaultWidth := 500
    , Gtk.windowDefaultHeight := 600
    ]
  
  -- Главный вертикальный контейнер
  mainBox <- Gtk.vBoxNew False 10
  Gtk.containerAdd window mainBox
  
  -- Метка для отображения текущего игрока
  playerLabel <- Gtk.labelNew (Just (pack "Ход черных"))
  Gtk.boxPackStart mainBox playerLabel Gtk.PackNatural 0
  
  -- Область для рисования доски
  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest drawingArea (floor (cellSize * 8)) (floor (cellSize * 8))
  Gtk.boxPackStart mainBox drawingArea Gtk.PackGrow 0
  
  -- Метка для отображения статуса игры
  statusLabel <- Gtk.labelNew (Just (pack "Выберите шашку для хода"))
  Gtk.boxPackStart mainBox statusLabel Gtk.PackNatural 0
  
  -- Горизонтальный контейнер для кнопок
  buttonBox <- Gtk.hBoxNew True 10
  Gtk.boxPackStart mainBox buttonBox Gtk.PackNatural 0
  
  -- Кнопка для перезапуска игры
  restartButton <- Gtk.buttonNewWithLabel (pack "Новая игра")
  Gtk.boxPackStart buttonBox restartButton Gtk.PackGrow 0
  
  -- Обработчик отрисовки с использованием Cairo
  Gtk.on drawingArea Gtk.draw $ do
    state <- liftIO $ readIORef gameStateRef
    drawBoard state
  
  -- Функция для обновления интерфейса
  let updateUI = do
        state <- readIORef gameStateRef
        let playerText = case currentPlayer state of
              Black -> "Ход черных"
              White -> "Ход белых"
            statusText = if gameOver state
              then case winner state of
                Just Black -> "Игра окончена! Победили черные!"
                Just White -> "Игра окончена! Победили белые!"
                Nothing -> "Игра окончена!"
              else "Выберите шашку для хода"
        
        Gtk.set playerLabel [Gtk.labelLabel := pack playerText]
        Gtk.set statusLabel [Gtk.labelLabel := pack statusText]
        Gtk.widgetQueueDraw drawingArea
  
  -- Обработка кликов мыши
  Gtk.on drawingArea Gtk.buttonPressEvent $ do
    (x, y) <- Gtk.eventCoordinates
    button <- Gtk.eventButton
    state <- liftIO $ readIORef gameStateRef
    
    when (button == 1 && not (gameOver state)) $ do  -- Левая кнопка мыши
      let col = floor (x / cellSize)
          row = floor (y / cellSize)
          pos = (row, col)
      
      liftIO $ handleCellClick gameStateRef updateUI state pos
      
    return True
  
  -- Обработка кнопки перезапуска
  Gtk.on restartButton Gtk.buttonActivated $ do
    writeIORef gameStateRef initialGameState
    updateUI
  
  -- Обработка закрытия окна
  Gtk.on window Gtk.objectDestroy Gtk.mainQuit
  
  -- Отображение окна и запуск главного цикла
  Gtk.widgetShowAll window
  Gtk.mainGUI

-- Главная функция
main :: IO ()
main = do
  let initialState = initialGameState
  gameStateRef <- newIORef initialState
  createWindow gameStateRef