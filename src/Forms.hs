{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms where

import qualified Data.Text as Text
import Data.Text (Text)
import Lens.Micro.Platform
import ModelStock (ItemStock (..), Proveedor (..))
import Text.Read (readMaybe)

data FormField src a v = FormField
  { _label :: Text,
    _value :: a,
    _setter :: src -> a -> src
  }

$(makeLenses ''FormField)

class FieldToValue (f :: * -> * -> * -> *) a v where
  fieldToValue :: f src a v -> Maybe v

instance FieldToValue FormField Text String where
  fieldToValue field = Just . Text.unpack $ field ^. value

instance FieldToValue FormField Text Int where
  fieldToValue field = readMaybe . Text.unpack $ field ^. value

instance FieldToValue FormField Text Double where
  fieldToValue field = readMaybe . Text.unpack $ field ^. value

class FormToValue f v where
  formToValue :: f -> Maybe v

formFieldToTuple :: FormField src a v -> (Text, a, src -> a -> src)
formFieldToTuple FormField {..} = (_label, _value, _setter)

data ProveedorForm = ProveedorForm
  { _codigoPField :: FormField Forms Text Int,
    _nombreField :: FormField Forms Text String,
    _direccionField :: FormField Forms Text String,
    _telefonoField :: FormField Forms Text String
  }

data ItemStockForm = ItemStockForm
  { _codigoIField :: FormField Forms Text Int,
    _descripcionField :: FormField Forms Text String,
    _marcaField :: FormField Forms Text String,
    _rubroField :: FormField Forms Text String,
    _codigoProveedorField :: FormField Forms Text Int,
    _unidadMedidadField :: FormField Forms Text String,
    _cantidadExistenteField :: FormField Forms Text Int,
    _cantidadMinimaField :: FormField Forms Text Int,
    _cantidadMaximaField :: FormField Forms Text Int,
    _precioCompraField :: FormField Forms Text Double,
    _porcentajeGananciaField :: FormField Forms Text Int
  }

data Forms = Forms
  { _itemForm :: ItemStockForm,
    _proveedorForm :: ProveedorForm,
    _eliminarItemID :: FormField Forms Text Int,
    _eliminarProveedorID :: FormField Forms Text Int
  }

-- Magia!
$(makeLenses ''ProveedorForm)
$(makeLenses ''ItemStockForm)
$(makeLenses ''Forms)

instance FormToValue ProveedorForm Proveedor where
  formToValue form =
    Proveedor
      <$> fieldToValue (form ^. codigoPField)
      <*> fieldToValue (form ^. nombreField)
      <*> fieldToValue (form ^. direccionField)
      <*> fieldToValue (form ^. telefonoField)

instance FormToValue ItemStockForm ItemStock where
  formToValue form =
    ItemStock
      <$> fieldToValue (form ^. codigoIField)
      <*> fieldToValue (form ^. descripcionField)
      <*> fieldToValue (form ^. marcaField)
      <*> fieldToValue (form ^. rubroField)
      <*> fieldToValue (form ^. codigoProveedorField)
      <*> fieldToValue (form ^. unidadMedidadField)
      <*> fieldToValue (form ^. cantidadExistenteField)
      <*> fieldToValue (form ^. cantidadMinimaField)
      <*> fieldToValue (form ^. cantidadMaximaField)
      <*> fieldToValue (form ^. precioCompraField)
      <*> fieldToValue (form ^. porcentajeGananciaField)

appForms :: Forms
appForms =
  let update field form v = form & field . value .~ v
   in Forms
        { _itemForm =
            let update' field form v = form & itemForm . field . value .~ v
             in ItemStockForm
                  { _codigoIField = FormField "Código" "" $ update' codigoIField,
                    _descripcionField = FormField "Descripción" "" $ update' descripcionField,
                    _marcaField = FormField "Marca" "" $ update' marcaField,
                    _rubroField = FormField "Rubro" "" $ update' rubroField,
                    _codigoProveedorField = FormField "Código del Proveedor" "" $ update' codigoProveedorField,
                    _unidadMedidadField = FormField "Unidad de Medida" "" $ update' unidadMedidadField,
                    _cantidadExistenteField = FormField "Cantidad Existente" "" $ update' cantidadExistenteField,
                    _cantidadMinimaField = FormField "Cantidad Mínima" "" $ update' cantidadMinimaField,
                    _cantidadMaximaField = FormField "Cantidad Máxima" "" $ update' cantidadMaximaField,
                    _precioCompraField = FormField "Precio de Compra" "" $ update' precioCompraField,
                    _porcentajeGananciaField = FormField "Porcentaje de Ganancia" "" $ update' porcentajeGananciaField
                  },
          _proveedorForm =
            let update' field form v = form & proveedorForm . field . value .~ v
             in ProveedorForm
                  { _codigoPField = FormField "Código" "" $ update' codigoPField,
                    _nombreField = FormField "Nombre" "" $ update' nombreField,
                    _direccionField = FormField "Dirección" "" $ update' direccionField,
                    _telefonoField = FormField "Teléfono" "" $ update' telefonoField
                  },
          _eliminarItemID = FormField "ID Item" "" $ update eliminarItemID,
          _eliminarProveedorID = FormField "ID Proveedor" "" $ update eliminarProveedorID
        }
