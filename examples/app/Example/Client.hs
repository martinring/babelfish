--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module Example.Client (
  message,  
  Client (Client)
) where 

import Example.Messages

data Client = Client {
    message :: Person -> String -> IO Int
  }