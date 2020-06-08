# Administración de Stock

Aplicación de Escritorio (GUI) multiplataforma para administrar Ítems y Proveedores, escrito totalmente en **Haskell**, parte de la materia optativa **Aplicaciones en Haskell**.

Esta aplicación utiliza [gi-gtk-declarative](https://github.com/owickstrom/gi-gtk-declarative/) para su interfaz gráfica y [micro-lens](https://hackage.haskell.org/package/microlens) para simplificar parte del código.

## Compilar

Se requiere de `ghc >= 8.8.3` y `cabal >= 3.0.0.0`.

Para poder utilizar **gi-gtk-declarative** visite la [documentación oficial](https://owickstrom.github.io/gi-gtk-declarative/)

```bash
$ git clone https://github.com/emlautarom1/AdministracionStock
$ cd AdministracionStock
$ cabal build
$ cabal run
```
