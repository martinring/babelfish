--
-- THIS MODULE IS AUTOGENERATED BY BABELFISH. DO NOT MODIFY
--

module Example.Server (
  register,  
  chat,  
  Server (Server)
) where 

import Example.Messages

data Server = Server {
    register :: Person -> IO Bool,
    chat :: String -> IO ()
  }