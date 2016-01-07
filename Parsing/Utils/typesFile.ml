open Types

type package = string option

type isStatic = 
|Static
|NonStatic

type import = Import of (isStatic * string)
type importsList = import list option


type fileTreeMap = {pack:package; imports : importsList;} 

type classTreeList = objectTree list option 

type fileTree = 
|FileTree of (fileTreeMap * objectTree)
|Empty

