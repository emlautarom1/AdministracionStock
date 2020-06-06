{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Forms
  ( Forms (..),
    ItemForm (..),
    ProveedorForm (..),
    FormToObject (..),
    simpleInputError,
    emptyForms,
    bajaProveedorID,
    bajaItemID,
  )
where

import qualified Data.Text as Text
import Data.Text (Text)
import ModelStock
import Text.Read (readMaybe)

-- Forms

data Forms = Forms
  { itemForm :: ItemForm,
    proveedorForm :: ProveedorForm,
    formBajaProveedorID :: Text,
    formBajaItemID :: Text
  }
  deriving (Show)

data ItemForm = ItemForm
  { itemFormCodigo :: Text,
    itemFormDescripcion :: Text,
    itemFormMarca :: Text,
    itemFormRubro :: Text,
    itemFormCodigoProveedor :: Text,
    itemFormUnidadMedidad :: Text,
    itemFormCantidadExistente :: Text,
    itemFormCantidadMinima :: Text,
    itemFormCantidadMaxima :: Text,
    itemFormPrecioCompra :: Text,
    itemFormPorcentajeGanancia :: Text
  }
  deriving (Show)

data ProveedorForm = ProveedorForm
  { proveedorFormCodigo :: Text,
    proveedorFormNombre :: Text,
    proveedorFormDireccion :: Text,
    proveedorFormTelefono :: Text
  }
  deriving (Show)

class FormToObject f o where
  formToObject :: f -> Maybe o

instance FormToObject ItemForm ItemStock where
  formToObject ItemForm {..} =
    ItemStock
      <$> (readMaybe . Text.unpack $ itemFormCodigo)
        <*> (Just . Text.unpack $ itemFormDescripcion)
        <*> (Just . Text.unpack $ itemFormMarca)
        <*> (Just . Text.unpack $ itemFormRubro)
        <*> (readMaybe . Text.unpack $ itemFormCodigoProveedor)
        <*> (Just . Text.unpack $ itemFormUnidadMedidad)
        <*> (readMaybe . Text.unpack $ itemFormCantidadExistente)
        <*> (readMaybe . Text.unpack $ itemFormCantidadMinima)
        <*> (readMaybe . Text.unpack $ itemFormCantidadMaxima)
        <*> (readMaybe . Text.unpack $ itemFormPrecioCompra)
        <*> (readMaybe . Text.unpack $itemFormPorcentajeGanancia)

instance FormToObject ProveedorForm Proveedor where
  formToObject ProveedorForm {..} =
    Proveedor
      <$> (readMaybe . Text.unpack $ proveedorFormCodigo)
      <*> (Just . Text.unpack $ proveedorFormNombre)
      <*> (Just . Text.unpack $ proveedorFormDireccion)
      <*> (Just . Text.unpack $ proveedorFormTelefono)

bajaItemID :: Forms -> Maybe Int
bajaItemID forms = readMaybe . Text.unpack $ formBajaItemID forms

bajaProveedorID :: Forms -> Maybe Int
bajaProveedorID forms = readMaybe . Text.unpack $ formBajaProveedorID forms

-- Forms utils

simpleInputError :: IO (Maybe a)
simpleInputError = do
  putStrLn "Entrada inválida"
  return Nothing

emptyForms :: Forms
emptyForms =
  Forms
    { itemForm = ItemForm "" "" "" "" "" "" "" "" "" "" "",
      proveedorForm = ProveedorForm "" "" "" "",
      formBajaItemID = "",
      formBajaProveedorID = ""
    }
