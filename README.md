An experimental library of material design components, not for production use.

Currently it is work in progress.

Implementation is based on [mdgriffith/elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) with inspiration of [Material Design](https://material.io)

Next components are available so far:
 - AppBar
 - Button
 - Checkbox
 - DataTable
 - DropDown
 - IconButton
 - List
 - Paginator
 - SidePanel
 - TextField


Execute `Examples.elm`, or try it yourself:
```
import Hatchinq.AppBar as AppBar
import Hatchinq.Attribute exposing (Attribute, height, width, withAttributes)
import Hatchinq.Button as Button exposing (..)
import Hatchinq.Checkbox as Checkbox exposing (..)
import Hatchinq.DataTable as DataTable exposing (..)
import Hatchinq.DropDown as DropDown exposing (..)
import Hatchinq.IconButton as IconButton (..)
import Hatchinq.List as MaterialList exposing (..)
import Hatchinq.SidePanel as SidePanel exposing (..)
import Hatchinq.TextField as TextField exposing (..)
import Hatchinq.Theme as Theme exposing (..)

theme =
    Theme.default


appBar =
    AppBar.configure { theme = theme }


checkbox =
    Checkbox.configure { theme = theme }


button =
    Button.configure { theme = theme }

{--
etc.
--}
```


