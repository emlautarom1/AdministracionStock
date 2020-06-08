{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad ((>=>), void)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Forms
import GI.Gtk
  ( Box (..),
    Button (..),
    Entry (..),
    Label (..),
    Orientation (..),
    ShadowType (..),
    Viewport (..),
    Window (..),
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Objects.Entry (entryGetText)
import Layouts
  ( defaultBoxChild,
    displayInfoLayout,
    expandBoxChild,
    simpleListView,
    subPageLayout,
  )
import Lens.Micro.Platform
import ModelStock
import QueueData (QData (..), filterQ, insertQ, qtoL)

data Event
  = ToScreen Screen
  | UpdateForms Forms
  | CRUDItem (CRUD ItemStock Int)
  | CRUDProveedor (CRUD Proveedor Int)
  | StoreToFile
  | ReadFromFile
  | UpdateTables (TablaStock, TablaProveedor)
  | Close

data CRUD a k = Create (Maybe a) | Delete (Maybe k)

data Screen
  = MainMenu
  | CreateProveedor
  | CreateItemStock
  | DeleteProveedor
  | DeleteItem
  | ListProveedores
  | ListItemStock
  | ListItemStockReponer
  deriving (Show)

data State = State
  { _currentScreen :: Screen,
    _tablaStock :: TablaStock,
    _tablaProveedor :: TablaProveedor,
    _forms :: Forms
  }

$(makeLenses ''State)

initialState' :: State
initialState' =
  State
    { _currentScreen = MainMenu,
      _tablaStock = tabla1S,
      _tablaProveedor = tabla1P,
      _forms = appForms
    }

view' :: State -> AppView Window Event
view' s =
  bin
    Window
    [ #title := "Sistema Administración de Stock",
      on #deleteEvent (const (True, Close)),
      #widthRequest := 500,
      #heightRequest := 700
    ]
    $ bin
      Viewport
      [ #margin := 10,
        #shadowType := ShadowTypeNone
      ]
    $ case s ^. currentScreen of
      MainMenu -> mainMenuLayout
      CreateItemStock -> createItemStockLayout (s ^. forms)
      CreateProveedor -> createProveedorLayout (s ^. forms)
      DeleteItem -> deleteItemStockLayout (s ^. forms)
      DeleteProveedor -> deleteProveedorLayout (s ^. forms)
      ListItemStock -> listItemStockLayout (s ^. tablaStock)
      ListProveedores -> listProveedoresLayout (s ^. tablaProveedor)
      ListItemStockReponer -> listItemStockReponerLayout (s ^. tablaStock)

update' :: State -> Event -> Transition State Event
-- Actualizacion de Formularios
update' s (UpdateForms f) = Transition (s & forms .~ f) (return Nothing)
-- CRUD ItemStock
update' s (CRUDItem action) = case action of
  Create (Just item) ->
    let tablaStock' = insertQ item $ s ^. tablaStock
     in Transition (s & tablaStock .~ tablaStock') (return $ Just $ ToScreen MainMenu)
  Delete (Just codigo) ->
    let tablaStock' = filterQ ((/=) codigo . itemCodigo) $ s ^. tablaStock
     in Transition (s & tablaStock .~ tablaStock') (return $ Just $ ToScreen MainMenu)
  _ -> Transition s (return Nothing)
-- CRUD Proveedor
update' s (CRUDProveedor action) = case action of
  Create (Just proveedor) ->
    let tablaProveedor' = insertQ proveedor $ s ^. tablaProveedor
     in Transition (s & tablaProveedor .~ tablaProveedor') (return $ Just $ ToScreen MainMenu)
  Delete (Just codigo) ->
    let tablaProveedor' = filterQ ((/=) codigo . proveedorCodigo) $ s ^. tablaProveedor
     in Transition (s & tablaProveedor .~ tablaProveedor') (return $ Just $ ToScreen MainMenu)
  _ -> Transition s (return Nothing)
-- Navegacion entre pantallas
update' s (ToScreen screen) = Transition (s & currentScreen .~ screen) (return Nothing)
-- Guardado y Carga de datos
update' s StoreToFile = Transition s $ do
  writeFile "items.txt" (show . qtoL $ s ^. tablaStock)
  writeFile "proveedores.txt" (show . qtoL $ s ^. tablaProveedor)
  putStrLn "Datos guardados con éxito"
  return Nothing
update' s ReadFromFile = Transition s $ do
  stock' <- Q . read <$> readFile "items.txt"
  proveedores' <- Q . read <$> readFile "proveedores.txt"
  putStrLn "Datos cargados con éxito"
  return . Just $ UpdateTables (stock', proveedores')
update' s (UpdateTables (ts, tp)) = Transition ((s & tablaStock .~ ts) & tablaProveedor .~ tp) (return Nothing)
-- Fin de programa
update' _ Close = Exit

main :: IO ()
main =
  void $
    run
      App
        { view = view',
          update = update',
          inputs = [],
          initialState = initialState'
        }

-- Layouts
mainMenuLayout :: Widget Event
mainMenuLayout =
  container Box [#spacing := 20, #orientation := OrientationVertical] $
    [defaultBoxChild $ widget Label [#label := "Menu Principal"]]
      <> [ expandBoxChild . simpleListView $
             (\(label', action) -> widget Button [#label := label', on #clicked action]) <$> menuButtons
         ]
      <> [defaultBoxChild $ widget Button [#label := "Salir", on #clicked Close]]
  where
    menuButtons =
      [ ("Insertar Item", ToScreen CreateItemStock),
        ("Insertar Proveedor", ToScreen CreateProveedor),
        ("Eliminar Item", ToScreen DeleteItem),
        ("Eliminar Proveedor", ToScreen DeleteProveedor),
        ("Listar Items", ToScreen ListItemStock),
        ("Listar Proveedores", ToScreen ListProveedores),
        ("Productos a Reponer", ToScreen ListItemStockReponer),
        ("Cargar Datos", ReadFromFile),
        ("Guardar Datos", StoreToFile)
      ]

createItemStockLayout :: Forms -> Widget Event
createItemStockLayout form =
  subPageLayout "Nuevo Item" (ToScreen MainMenu)
    $ container Box [#spacing := 20, #orientation := OrientationVertical]
    $ defaultBoxChild
      <$> [simpleListView $ entryBuilder <$> entries]
      <> [widget Button [#label := "Registrar", on #clicked submit]]
  where
    submit = CRUDItem . Create $ formToValue (form ^. itemForm)
    entryBuilder (label', _, setter') =
      widget
        Entry
        [ #placeholderText := label',
          onM #changed $ entryGetText >=> return . UpdateForms . setter' form
        ]
    entries =
      [ formFieldToTuple $ form ^. itemForm . codigoIField,
        formFieldToTuple $ form ^. itemForm . descripcionField,
        formFieldToTuple $ form ^. itemForm . marcaField,
        formFieldToTuple $ form ^. itemForm . rubroField,
        formFieldToTuple $ form ^. itemForm . codigoProveedorField,
        formFieldToTuple $ form ^. itemForm . unidadMedidadField,
        formFieldToTuple $ form ^. itemForm . cantidadExistenteField,
        formFieldToTuple $ form ^. itemForm . cantidadMinimaField,
        formFieldToTuple $ form ^. itemForm . cantidadMaximaField,
        formFieldToTuple $ form ^. itemForm . precioCompraField,
        formFieldToTuple $ form ^. itemForm . porcentajeGananciaField
      ]

createProveedorLayout :: Forms -> Widget Event
createProveedorLayout form =
  subPageLayout "Nuevo Proveedor" (ToScreen MainMenu)
    $ container Box [#spacing := 20, #orientation := OrientationVertical]
    $ defaultBoxChild
      <$> [simpleListView $ entryBuilder <$> entries]
      <> [widget Button [#label := "Registrar", on #clicked submit]]
  where
    submit = CRUDProveedor . Create $ formToValue (form ^. proveedorForm)
    entryBuilder (label', _, setter') =
      widget
        Entry
        [ #placeholderText := label',
          on #activate submit,
          onM #changed $ entryGetText >=> return . UpdateForms . setter' form
        ]
    entries =
      [ formFieldToTuple $ form ^. proveedorForm . codigoPField,
        formFieldToTuple $ form ^. proveedorForm . nombreField,
        formFieldToTuple $ form ^. proveedorForm . direccionField,
        formFieldToTuple $ form ^. proveedorForm . telefonoField
      ]

listItemStockLayout :: TablaStock -> Widget Event
listItemStockLayout stock =
  subPageLayout "Items" (ToScreen MainMenu)
    $ displayInfoLayout
    $ Vector.fromList
    $ Text.pack . itemDescripcion <$> qtoL stock

listProveedoresLayout :: TablaProveedor -> Widget Event
listProveedoresLayout proveedores =
  subPageLayout "Proveedores" (ToScreen MainMenu)
    $ displayInfoLayout
    $ Vector.fromList
    $ Text.pack . proveedorNombre <$> qtoL proveedores

listItemStockReponerLayout :: TablaStock -> Widget Event
listItemStockReponerLayout stock =
  subPageLayout "Items a Reponer" (ToScreen MainMenu)
    $ displayInfoLayout
    $ Vector.fromList
    $ Text.pack . itemDescripcion <$> fReponer stock

deleteItemStockLayout :: Forms -> Widget Event
deleteItemStockLayout form =
  let changed = entryGetText >=> \text -> return . UpdateForms $ set (eliminarItemID . value) text form
      submit _ = return $ CRUDItem . Delete $ fieldToValue $ form ^. eliminarItemID
   in subPageLayout "Eliminar Item" (ToScreen MainMenu)
        $ simpleListView
        $ [widget Entry [#placeholderText := "ID Item", onM #changed changed]]
          <> [widget Button [#label := "Eliminar", onM #clicked submit]]

deleteProveedorLayout :: Forms -> Widget Event
deleteProveedorLayout form =
  let changed = entryGetText >=> \text -> return . UpdateForms $ set (eliminarProveedorID . value) text form
      submit _ = return $ CRUDProveedor . Delete $ fieldToValue $ form ^. eliminarProveedorID
   in subPageLayout "Eliminar Proveedor" (ToScreen MainMenu)
        $ simpleListView
        $ [widget Entry [#placeholderText := "ID Proveedor", onM #changed changed]]
          <> [widget Button [#label := "Eliminar", onM #clicked submit]]
