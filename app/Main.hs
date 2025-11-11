{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import Data.IORef
import qualified Data.Vector as V
import Data.ByteString (ByteString)

-- Database imports
import Hasql.Connection (Connection, acquire, release)
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import qualified Hasql.TH as TH

-- Product entity definition
data Product = Product
  { productId :: Int
  , productName :: Text
  , productDescription :: Text
  , productPrice :: Double
  } deriving (Show)

-- Database connection settings
connectionString :: ByteString
connectionString = "host=localhost port=5432 user=myuser password=AStrongPassword dbname=mydatabase"

-- SQL statements using Template Haskell for type safety
insertProductStatement :: Statement.Statement (Text, Text, Double) Int64
insertProductStatement =
  [TH.singletonStatement|
    insert into product (name, description, price) 
    values ($1 :: text, $2 :: text, $3 :: float8)
    returning id :: int8
  |]

selectAllProductsStatement :: Statement.Statement () [Product]
selectAllProductsStatement =
  Statement.sql query encoder decoder True
  where
    query = "SELECT id, name, description, price FROM product ORDER BY id"
    encoder = Encoders.noParams
    decoder = Decoders.rowList productRow
    productRow = Product
      <$> Decoders.column (Decoders.nonNullable Decoders.int4)
      <*> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.float8)

updateProductStatement :: Statement.Statement (Int, Text, Text, Double) Int64
updateProductStatement =
  [TH.singletonStatement|
    update product 
    set name = $2 :: text, description = $3 :: text, price = $4 :: float8
    where id = $1 :: int4
    returning id :: int8
  |]

deleteProductStatement :: Statement.Statement Int Int64
deleteProductStatement =
  [TH.singletonStatement|
    delete from product where id = $1 :: int4
    returning id :: int8
  |]

-- Database operations
connectDB :: IO (Either String Connection)
connectDB = acquire connectionString

insertProduct :: Connection -> (Text, Text, Double) -> IO (Either String Int64)
insertProduct conn product = Session.run (Session.statement product insertProductStatement) conn

getAllProducts :: Connection -> IO (Either String [Product])
getAllProducts conn = Session.run (Session.statement () selectAllProductsStatement) conn

updateProduct :: Connection -> (Int, Text, Text, Double) -> IO (Either String Int64)
updateProduct conn product = Session.run (Session.statement product updateProductStatement) conn

deleteProduct :: Connection -> Int -> IO (Either String Int64)
deleteProduct conn productId = Session.run (Session.statement productId deleteProductStatement) conn

-- GTK Application using only GI.Gtk (newer binding)
createMainWindow :: Connection -> IORef [Product] -> IO ()
createMainWindow conn productsRef = do
  Gtk.init Nothing

  -- Create main window
  window <- new Gtk.Window [ #title := "Product Management" ]
  #setDefaultSize window 600 400

  -- Main vertical box
  mainBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10 ]
  #setMarginAll mainBox 10
  #add window mainBox

  -- Input fields
  inputGrid <- new Gtk.Grid [ #columnSpacing := 10, #rowSpacing := 10 ]
  #add mainBox inputGrid

  nameLabel <- new Gtk.Label [ #label := "Name:", #xalign := 0 ]
  nameEntry <- new Gtk.Entry []
  #attach inputGrid nameLabel 0 0 1 1
  #attach inputGrid nameEntry 1 0 1 1

  descLabel <- new Gtk.Label [ #label := "Description:", #xalign := 0 ]
  descEntry <- new Gtk.Entry []
  #attach inputGrid descLabel 0 1 1 1
  #attach inputGrid descEntry 1 1 1 1

  priceLabel <- new Gtk.Label [ #label := "Price:", #xalign := 0 ]
  priceEntry <- new Gtk.Entry []
  #attach inputGrid priceLabel 0 2 1 1
  #attach inputGrid priceEntry 1 2 1 1

  -- Buttons
  buttonBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 5 ]
  #add mainBox buttonBox

  addButton <- new Gtk.Button [ #label := "Add Product" ]
  updateButton <- new Gtk.Button [ #label := "Update Product" ]
  deleteButton <- new Gtk.Button [ #label := "Delete Product" ]
  refreshButton <- new Gtk.Button [ #label := "Refresh" ]

  #packStart buttonBox addButton True True 0
  #packStart buttonBox updateButton True True 0
  #packStart buttonBox deleteButton True True 0
  #packStart buttonBox refreshButton True True 0

  -- Products list using TreeView with ListStore
  listStore <- Gtk.listStoreNew []
  treeView <- new Gtk.TreeView [ #model := listStore ]

  -- Create columns
  idColumn <- new Gtk.TreeViewColumn []
  nameColumn <- new Gtk.TreeViewColumn []
  descColumn <- new Gtk.TreeViewColumn []
  priceColumn <- new Gtk.TreeViewColumn []

  #setTitle idColumn "ID"
  #setTitle nameColumn "Name"
  #setTitle descColumn "Description"
  #setTitle priceColumn "Price"

  -- Cell renderers
  idRenderer <- new Gtk.CellRendererText []
  nameRenderer <- new Gtk.CellRendererText []
  descRenderer <- new Gtk.CellRendererText []
  priceRenderer <- new Gtk.CellRendererText []

  #packStart idColumn idRenderer True
  #packStart nameColumn nameRenderer True
  #packStart descColumn descRenderer True
  #packStart priceColumn priceRenderer True

  -- Set up cell data functions
  #setCellDataFunc idColumn idRenderer $ \cellLayout cellRenderer treeModel iter -> do
    value <- Gtk.treeModelGetValue treeModel iter 0
    let product = value
    #set cellRenderer [ #text := show (productId product) ]

  #setCellDataFunc nameColumn nameRenderer $ \cellLayout cellRenderer treeModel iter -> do
    value <- Gtk.treeModelGetValue treeModel iter 0
    let product = value
    #set cellRenderer [ #text := unpack (productName product) ]

  #setCellDataFunc descColumn descRenderer $ \cellLayout cellRenderer treeModel iter -> do
    value <- Gtk.treeModelGetValue treeModel iter 0
    let product = value
    #set cellRenderer [ #text := unpack (productDescription product) ]

  #setCellDataFunc priceColumn priceRenderer $ \cellLayout cellRenderer treeModel iter -> do
    value <- Gtk.treeModelGetValue treeModel iter 0
    let product = value
    #set cellRenderer [ #text := show (productPrice product) ]

  #appendColumn treeView idColumn
  #appendColumn treeView nameColumn
  #appendColumn treeView descColumn
  #appendColumn treeView priceColumn

  -- Add scrolled window for tree view
  scrolledWindow <- new Gtk.ScrolledWindow []
  #add scrolledWindow treeView
  #add mainBox scrolledWindow

  -- Selection handling
  selection <- #getSelection treeView
  #setMode selection Gtk.SelectionSingle

  -- Function to refresh products list
  let refreshProductsList = do
        result <- getAllProducts conn
        case result of
          Right products -> do
            writeIORef productsRef products
            Gtk.listStoreClear listStore
            mapM_ (Gtk.listStoreAppend listStore) products
          Left err -> putStrLn $ "Error fetching products: " ++ show err

  -- Function to get selected product
  let getSelectedProduct = do
        selected <- #getSelectedRows selection
        case selected of
          [] -> return Nothing
          (path:_) -> do
            iter <- Gtk.treeModelGetIter listStore path
            Gtk.treeModelGetValue listStore iter 0

  -- Function to clear input fields
  let clearInputFields = do
        #setText nameEntry ""
        #setText descEntry ""
        #setText priceEntry ""

  -- Button click handlers
  on addButton #clicked $ do
    name <- #getText nameEntry
    desc <- #getText descEntry
    priceText <- #getText priceEntry
    
    case reads priceText of
      [(price, "")] -> do
        result <- insertProduct conn (pack name, pack desc, price)
        case result of
          Right _ -> do
            refreshProductsList
            clearInputFields
          Left err -> putStrLn $ "Error inserting product: " ++ show err
      _ -> putStrLn "Invalid price format"

  on updateButton #clicked $ do
    selected <- getSelectedProduct
    case selected of
      Just product -> do
        name <- #getText nameEntry
        desc <- #getText descEntry
        priceText <- #getText priceEntry
        
        case reads priceText of
          [(price, "")] -> do
            result <- updateProduct conn (productId product, pack name, pack desc, price)
            case result of
              Right _ -> refreshProductsList
              Left err -> putStrLn $ "Error updating product: " ++ show err
          _ -> putStrLn "Invalid price format"
      Nothing -> putStrLn "No product selected"

  on deleteButton #clicked $ do
    selected <- getSelectedProduct
    case selected of
      Just product -> do
        result <- deleteProduct conn (productId product)
        case result of
          Right _ -> do
            refreshProductsList
            clearInputFields
          Left err -> putStrLn $ "Error deleting product: " ++ show err
      Nothing -> putStrLn "No product selected"

  on refreshButton #clicked $ refreshProductsList

  -- Handle row selection
  on selection #changed $ do
    selected <- getSelectedProduct
    case selected of
      Just product -> do
        #setText nameEntry (unpack $ productName product)
        #setText descEntry (unpack $ productDescription product)
        #setText priceEntry (show $ productPrice product)
      Nothing -> return ()

  -- Handle window close
  on window #destroy Gtk.mainQuit

  -- Initial data load
  refreshProductsList

  -- Show window and start main loop
  #showAll window
  Gtk.main

-- Main function
main :: IO ()
main = do
  -- Connect to database
  connResult <- connectDB
  case connResult of
    Left err -> putStrLn $ "Database connection error: " ++ show err
    Right conn -> do
      productsRef <- newIORef []
      createMainWindow conn productsRef
      release conn