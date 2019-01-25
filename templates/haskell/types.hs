module $name.pascal$.Types where

$for(type)$
data $type.name.pascal$ = 
$for(type.constructor)$
  $type.constructor.name.pascal$$sep$ |
$endfor$$sep$ 


$endfor$