--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module $for(options.module)$$options.module$.$endfor$$api.name.pascal$ (
$for(api.method)$
  $api.method.name.camel$,  
$endfor$
  $api.name.pascal$ ($api.name.pascal$)
) where 

import $for(options.module)$$options.module$.$endfor$$name.pascal$

data $api.name.pascal$ = $api.name.pascal$ {
$for(api.method)$
    $api.method.name.camel$ :: $for(api.method.parameter)$$api.method.parameter.type$ -> $endfor$IO $if(api.method.return)$$api.method.return$$else$()$endif$$sep$,
$endfor$

  }