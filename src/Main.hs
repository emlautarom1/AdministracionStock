{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

{-
  Averiguar sobre:
  - Estilos (css) para GTK
  - Iconos
  - Box vs VBox vs ListRow
  TODO:
    - Mover layouts a otro módulo
-}

import Control.Monad (void)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Forms
  ( FormToObject (..),
    Forms (..),
    ItemForm (..),
    ProveedorForm (..),
    bajaItemID,
    bajaProveedorID,
    emptyForms,
    simpleInputError,
  )
import GI.Gtk
  ( Box (..),
    Button (..),
    Entry (..),
    Label (..),
    Orientation (..),
    ScrolledWindow (..),
    ShadowType (..),
    Viewport (..),
    Window (..),
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Objects.Entry (entryGetText)
import ModelStock
import QueueData (filterQ, insertQ, qtoL)

data Event
  = ToScreen Screen
  | UpdateForms Forms
  | CRUDItem (CRUD ItemStock Int)
  | CRUDProveedor (CRUD Proveedor Int)
  | Close

data CRUD a k = Alta (Maybe a) | Baja (Maybe k)

data Screen
  = MainMenu
  | NuevoProveedor
  | NuevoItem
  | EliminarProveedor
  | EliminarItem
  | MostrarProveedores
  | MostarItems
  | MostarItemsAReponer
  | CargarDatos
  | GuardarDatos
  deriving (Show)

data State = State
  { currentScreen :: Screen,
    tablaStock :: TablaStock,
    tablaProveedor :: TablaProveedor,
    forms :: Forms
  }

initialState' :: State
initialState' =
  State
    { currentScreen = MainMenu,
      tablaStock = tabla1S,
      tablaProveedor = tabla1P,
      forms = emptyForms
    }

view' :: State -> AppView Window Event
view' State {..} =
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
    $ case currentScreen of
      MainMenu -> mainMenuLayout
      NuevoItem -> nuevoItemLayout forms
      NuevoProveedor -> nuevoProveedorLayout forms
      EliminarItem -> eliminarItemLayout forms
      EliminarProveedor -> eliminarProveedorLayout forms
      MostarItems -> listarItemsLayout tablaStock
      MostrarProveedores -> listarProveedoresLayout tablaProveedor
      MostarItemsAReponer -> listarItemsReponerLayour tablaStock
      _ -> mainMenuLayout

update' :: State -> Event -> Transition State Event
-- Actualizacion de Formularioss
update' s (UpdateForms forms') = Transition s {forms = forms'} (return Nothing)
-- CRUD ItemStock
update' s (CRUDItem action) = case action of
  Alta (Just item) ->
    let tablaStock' = insertQ item $ tablaStock s
     in Transition (s {tablaStock = tablaStock'}) (return $ Just $ ToScreen MainMenu)
  Alta Nothing -> Transition s simpleInputError
  Baja (Just codigo) ->
    let tablaStock' = filterQ ((/=) codigo . itemCodigo) $ tablaStock s
     in Transition (s {tablaStock = tablaStock'}) (return $ Just $ ToScreen MainMenu)
  Baja Nothing -> Transition s simpleInputError
-- CRUD Proveedor
update' s (CRUDProveedor action) = case action of
  Alta (Just proveedor) ->
    let tablaProveedor' = insertQ proveedor $ tablaProveedor s
     in Transition (s {tablaProveedor = tablaProveedor'}) (return $ Just $ ToScreen MainMenu)
  Alta Nothing -> Transition s simpleInputError
  Baja (Just codigo) ->
    let tablaProveedor' = filterQ ((/=) codigo . proveedorCodigo) $ tablaProveedor s
     in Transition (s {tablaProveedor = tablaProveedor'}) (return $ Just $ ToScreen MainMenu)
  Baja Nothing -> Transition s simpleInputError
-- Navegacion entre pantallas
update' s (ToScreen screen) = Transition (s {currentScreen = screen}) logScreen
  where
    logScreen = do
      putStrLn $ "Moviendose a la pantalla '" <> show screen <> "'"
      return Nothing
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
             (\(label, toScreen) -> widget Button [#label := label, on #clicked toScreen]) <$> menuLinks
         ]
      <> [defaultBoxChild $ widget Button [#label := "Salir", on #clicked Close]]
  where
    menuLinks =
      Bifunctor.second ToScreen
        <$> [ ("Insertar Item", NuevoItem),
              ("Insertar Proveedor", NuevoProveedor),
              ("Eliminar Item", EliminarItem),
              ("Eliminar Proveedor", EliminarProveedor),
              ("Listar Items", MostarItems),
              ("Listar Proveedores", MostrarProveedores),
              ("Productos a Reponer", MostarItemsAReponer),
              ("Cargar Datos", CargarDatos),
              ("Guardar Datos", GuardarDatos)
            ]

nuevoItemLayout :: Forms -> Widget Event
nuevoItemLayout forms = subPageLayout "Nuevo Item" $ formEntryLayout entries submitItem
  where
    submitItem = CRUDItem . Alta $ formToObject (itemForm forms)
    updateItemForm update value = forms {itemForm = update (itemForm forms) value}
    entries =
      [ ("Código", updateItemForm $ \f v -> f {itemFormCodigo = v}),
        ("Descripción", updateItemForm $ \f v -> f {itemFormDescripcion = v}),
        ("Marca", updateItemForm $ \f v -> f {itemFormMarca = v}),
        ("Rubro", updateItemForm $ \f v -> f {itemFormRubro = v}),
        ("Código del Proveedor", updateItemForm $ \f v -> f {itemFormCodigoProveedor = v}),
        ("Unidad de Medida", updateItemForm $ \f v -> f {itemFormUnidadMedidad = v}),
        ("Cantidad Existente", updateItemForm $ \f v -> f {itemFormCantidadExistente = v}),
        ("Cantidad Mínima", updateItemForm $ \f v -> f {itemFormCantidadMinima = v}),
        ("Cantidad Máxima", updateItemForm $ \f v -> f {itemFormCantidadMaxima = v}),
        ("Precio de Compra", updateItemForm $ \f v -> f {itemFormPrecioCompra = v}),
        ("Porcentaje de Ganancia", updateItemForm $ \f v -> f {itemFormPorcentajeGanancia = v})
      ]

nuevoProveedorLayout :: Forms -> Widget Event
nuevoProveedorLayout forms = subPageLayout "Nuevo Proveedor" $ formEntryLayout entries submitItem
  where
    submitItem = CRUDProveedor . Alta $ formToObject (proveedorForm forms)
    updateProveedorForm update value = forms {proveedorForm = update (proveedorForm forms) value}
    entries =
      [ ("Código", updateProveedorForm $ \f v -> f {proveedorFormCodigo = v}),
        ("Nombre", updateProveedorForm $ \f v -> f {proveedorFormNombre = v}),
        ("Dirección", updateProveedorForm $ \f v -> f {proveedorFormDireccion = v}),
        ("Número de Teléfono", updateProveedorForm $ \f v -> f {proveedorFormTelefono = v})
      ]

listarItemsLayout :: TablaStock -> Widget Event
listarItemsLayout stock =
  subPageLayout "Items" $ displayInfoLayout
    $ Vector.fromList
    $ Text.pack . itemDescripcion <$> qtoL stock

listarProveedoresLayout :: TablaProveedor -> Widget Event
listarProveedoresLayout proveedores =
  subPageLayout "Proveedores" $ displayInfoLayout
    $ Vector.fromList
    $ Text.pack . proveedorNombre <$> qtoL proveedores

listarItemsReponerLayour :: TablaStock -> Widget Event
listarItemsReponerLayour stock =
  subPageLayout "Items a Reponer" $ displayInfoLayout
    $ Vector.fromList
    $ Text.pack . itemDescripcion <$> fReponer stock

eliminarItemLayout :: Forms -> Widget Event
eliminarItemLayout forms =
  deleteLayout "Eliminar Item" (CRUDItem . Baja $ bajaItemID forms) (\i -> forms {formBajaItemID = i})

eliminarProveedorLayout :: Forms -> Widget Event
eliminarProveedorLayout forms =
  deleteLayout "Eliminar Proveedor" (CRUDItem . Baja $ bajaProveedorID forms) (\i -> forms {formBajaProveedorID = i})

-- Generic Layouts
subPageLayout :: Text -> Widget Event -> Widget Event
subPageLayout title body =
  container Box [#spacing := 20, #orientation := OrientationVertical] $
    [defaultBoxChild $ widget Label [#label := title]]
      <> [expandBoxChild body]
      <> [defaultBoxChild $ widget Button [#label := "Atrás", on #clicked $ ToScreen MainMenu]]

formEntryLayout :: Vector (Text, Text -> Forms) -> Event -> Widget Event
formEntryLayout entries submit =
  container Box [#spacing := 20, #orientation := OrientationVertical] $
    defaultBoxChild
      <$> [simpleListView $ entryBuilder <$> entries]
      <> [widget Button [#label := "Registrar", on #clicked submit]]
  where
    entryBuilder (label, updater) =
      widget
        Entry
        [ #placeholderText := label,
          onM #changed $ updateFormsFromEntry updater
        ]

displayInfoLayout :: Vector Text -> Widget Event
displayInfoLayout info =
  bin ScrolledWindow []
    $ bin Viewport []
    $ simpleListView
    $ (\i -> widget Label [#label := i, #xalign := 0]) <$> info

deleteLayout :: Text -> Event -> (Text -> Forms) -> Widget Event
deleteLayout title submit updater =
  subPageLayout title
    $ simpleListView
    $ [widget Entry [#placeholderText := "ID", onM #changed $ updateFormsFromEntry updater]]
      <> [widget Button [#label := "Eliminar", onM #clicked $ const . return $ submit]]

-- Layout Utils
defaultBoxChild :: Widget e -> BoxChild e
defaultBoxChild = BoxChild defaultBoxChildProperties

expandBoxChild :: Widget e -> BoxChild e
expandBoxChild = BoxChild BoxChildProperties {expand = True, fill = True, padding = 0}

simpleListView :: Vector (Widget e) -> Widget e
simpleListView children =
  container Box [#spacing := 10, #orientation := OrientationVertical] $ defaultBoxChild <$> children

-- Forms Utils
updateFormsFromEntry :: (Text -> Forms) -> Entry -> IO Event
updateFormsFromEntry updater entry = do
  entryText <- entryGetText entry
  return . UpdateForms $ updater entryText
