An experimental library for easier UI elements creation. Not for production use.

Currently it is work in progress.

Implementation is based on [mdgriffith/elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/).

Next UI elements are available so far:
 - AppBar
 - Button
 - Card
 - Checkbox
 - Chip
 - DataTable
 - DropDown
 - IconButton
 - List
 - Paginator
 - ProgressIndicator
 - RadioButton
 - SidePanel
 - Snackbar
 - TextField
 - Tree


Execute `Examples.elm`, or try it yourself:
```
import Hatchinq.AppBar as AppBar
import Hatchinq.Attribute exposing (Attribute, height, width, withAttributes)
import Hatchinq.Button as Button exposing (..)
import Hatchinq.Checkbox as Checkbox exposing (..)
import Hatchinq.Chip as Checkbox exposing (..)
import Hatchinq.DataTable as DataTable exposing (..)
import Hatchinq.DropDown as DropDown exposing (..)
import Hatchinq.IconButton as IconButton (..)
import Hatchinq.List as MaterialList exposing (..)
import Hatchinq.Paginator as Paginator exposing (..)
import Hatchinq.ProgressIndicator as ProgressIndicator exposing (..)
import Hatchinq.RadioButton as RadioButton exposing (..)
import Hatchinq.SidePanel as SidePanel exposing (..)
import Hatchinq.Snackbar as Snackbar exposing (..)
import Hatchinq.TextField as TextField exposing (..)
import Hatchinq.Theme as Theme exposing (..)
import Hatchinq.Tree as Tree exposing (..)

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


