------------------------------------------------------------------------------
-- Modulo      :  ModelStock
-- Programador :
-- Estabilidad :  experimental
-- Portabilidad:  experimental
--
-- Optativa: Programacion Funcional en Haskell, 2020
-- Este Modulo contiene un modelo simplificado de un Sistema de Stock,
-- modelado con sinónimos de tipo (type) y
-- el tipo QData importado del módulo QueueData
-----------------------------------------------------------------------------
module ModelStock where

import Data.List (intercalate)
import QueueData

---------------------------------------
-- MODELO SISTEMA DE STOCK DE ALMACEN -
---------------------------------------

data ItemStock = ItemStock
  { -- | Codigo interno del producto
    itemCodigo :: Int,
    -- | Descripcion del producto
    itemDescripcion :: String,
    -- | Marca del producto
    itemMarca :: String,
    -- | Rubro del producto
    itemRubro :: String,
    -- | Codigo interno del proveedor
    itemCodigoProveedor :: Int,
    -- |  Unidad de Medida: 1LT,800GRM, 1500CM3, etc
    itemUnidadMedidad :: String,
    -- | Cantidad de productos en deposito (E)
    itemCantidadExistente :: Int,
    -- | Valor en existencia recomendado para reposicion (EMin)
    itemCantidadMinima :: Int,
    -- | Valor maximo de acopio en deposito  (EMax)
    itemCantidadMaxima :: Int,
    -- | Precio o valor de compra unitario
    itemPrecioCompra :: Double,
    -- | Porcentaje de ganancia sobre el precio de compra
    itemPorcentajeGanancia :: Int
  }
  deriving (Eq, Show, Read)

data Proveedor = Proveedor
  { -- | Codigo interno del proveedor
    proveedorCodigo :: Int,
    -- | Nombre del proveedor
    proveedorNombre :: String,
    -- | Dirección del proveedor
    proveedorDireccion :: String,
    -- | Teléfono del proveedor
    proveedorTelefono :: String
  }
  deriving (Eq, Show, Read)

-- | Tabla con el Stock de un comercio.
type TablaStock = QData ItemStock

-- | Tabla con los proveedores de un comercio
type TablaProveedor = QData Proveedor

-- | Agrego supresion por contenido en Tablas de Stock y proveedores
supressIs :: Int -> TablaStock -> [ItemStock]
supressIs _ (Q []) = []
supressIs n (Q (x : xs)) = if n == itemCodigo x then supressIs n (Q xs) else x : supressIs n (Q xs)

supressPr :: Int -> TablaProveedor -> [Proveedor]
supressPr _ (Q []) = []
supressPr n (Q (x : xs)) =
  if n == proveedorCodigo x
    then supressPr n (Q xs)
    else x : supressPr n (Q xs)

------------------------------------------------------------------------------------
-- TAREA 2: Implementar funciones

-- | 'fReponer': Devuelve lista con todos los articulos que hacen falta reponer (La cantidad es menor al valor minimo)
fReponer :: TablaStock -> [ItemStock]
fReponer tablaStock = qtoL $ filterQ (\item -> itemCantidadExistente item < itemCantidadMinima item) tablaStock

-- Funciones auxiliares de visualizacion en modo texto

-- | Convierte tupla de 'ItemStock' en un 'String'
showItemStock :: ItemStock -> String
showItemStock itemStock = concat ["(", intercalate "," showFields, ")"]
  where
    fields =
      [ show . itemCodigo,
        itemDescripcion,
        itemMarca,
        itemRubro,
        show . itemCodigoProveedor,
        itemUnidadMedidad,
        show . itemCantidadExistente,
        show . itemCantidadMinima,
        show . itemCantidadMaxima,
        show . itemPrecioCompra,
        show . itemPorcentajeGanancia
      ]
    showFields = [field itemStock | field <- fields]

-- | Convierte cola de tuplas de 'ItemStock' en una cola de 'String' con la funcion de arriba
showTablaStock :: TablaStock -> QData String
showTablaStock = mapQ showItemStock

-----------------------------------------------------------------------------------

-- | Datos iniciales
tabla1S :: TablaStock
tabla1S =
  Q
    [ ItemStock 100 "ARROZ GRANO GRANDE" "CONDOR" "Alimentos" 20 "1LT" 8000 500 10000 20 30,
      ItemStock 107 "ARROZ GRANO GRANDE" "GALLO" "Alimentos" 20 "1KG" 6000 200 8000 25 30,
      ItemStock 200 "ACEITE DE GIRASOL" "NATURA" "Alimentos" 30 "1LT" 9800 600 10000 40 30,
      ItemStock 200 "ACEITE DE GIRASOL" "COCINERO" "Alimentos" 32 "1LT" 900 500 10000 30 30,
      ItemStock 410 "AGUA MINERAL S/GAS BAJO SODIO" "SER" "Alimentos" 300 "1.5LT" 20 50 3000 10 35,
      ItemStock 412 "AGUA SABORIZADA LIMA LIMON CON GAS" "SER" "Alimentos" 300 "2LT" 1570 50 3000 12 35,
      ItemStock 478 "ALFAJOR CHOCOLATE TITA" "TERRABUSI" "Alimentos" 579 "36GR" 2000 200 5000 3 30,
      ItemStock 479 "ALFAJOR CHOCOLATE RODESIA" "TERRABUSI" "Alimentos" 579 "40GR" 1290 200 3500 4 30,
      ItemStock 1208 "LECHE DESCREMADA PASTEURIZADA DESLACTOSADA" "LA SERENISIMA" "Alimentos" 231 " 1TL" 2300 1000 12000 230 30,
      ItemStock 1209 "LECHE DESCREMADA PASTEURIZADA" "LA SERENISIMA" "Alimentos" 231 "1TL" 5300 2000 14000 130 30,
      ItemStock 2301 "ANTITRANSPIRANTE ROLL ON CLASICO" "ETIQUET" "PERFUMERIA" 204 "60gr" 300 450 2000 25 30,
      ItemStock 2301 "ANTITRANSPIRANTE ROLL ON CLASICO" "DOVE" "PERFUMERIA" 204 "60gr" 460 300 2000 35 30,
      ItemStock 667 "ARVEJAS SECAS REMOJADAS" "NOEL" "Alimentos" 20 "300GR" 1203 500 3000 10 30
    ]

tabla1P :: TablaProveedor
tabla1P =
  Q
    [ Proveedor 100 "Juan Perez" "Belgrano 1827 San Luis 5700 Argentina" "2664-786543",
      Proveedor 2301 "Jose Lopez" "Junin 444 Mendoza 5500 Argentina" "261-3452677"
    ]
