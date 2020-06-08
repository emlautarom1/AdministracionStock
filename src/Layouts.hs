{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Layouts
  ( subPageLayout,
    displayInfoLayout,
    defaultBoxChild,
    expandBoxChild,
    simpleListView,
  )
where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Vector (Vector)
import GI.Gtk
  ( Box (..),
    Button (..),
    Label (..),
    Orientation (..),
    ScrolledWindow (..),
    Viewport (..),
  )
import GI.Gtk.Declarative

subPageLayout :: Text -> e -> Widget e -> Widget e
subPageLayout title onBack body =
  container Box [#spacing := 20, #orientation := OrientationVertical] $
    [defaultBoxChild $ widget Label [#label := title]]
      <> [expandBoxChild body]
      <> [defaultBoxChild $ widget Button [#label := "Atrás", on #clicked onBack]]

displayInfoLayout :: Vector Text -> Widget e
displayInfoLayout info =
  bin ScrolledWindow []
    $ bin Viewport []
    $ simpleListView
    $ (\label' -> widget Label [#label := Text.toUpper label', #xalign := 0]) <$> info

defaultBoxChild :: Widget e -> BoxChild e
defaultBoxChild = BoxChild defaultBoxChildProperties

expandBoxChild :: Widget e -> BoxChild e
expandBoxChild = BoxChild BoxChildProperties {expand = True, fill = True, padding = 0}

simpleListView :: Vector (Widget e) -> Widget e
simpleListView children =
  container Box [#spacing := 10, #orientation := OrientationVertical] $ defaultBoxChild <$> children
