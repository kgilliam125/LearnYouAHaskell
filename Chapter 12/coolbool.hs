data DataCoolBool = DataCoolBool { getDataCoolBool :: Bool }

newtype NewTypeCoolBool = NewTypeCoolBool { getNewTypeCoolBool :: Bool }

helloMe :: DataCoolBool -> String
helloMe (DataCoolBool _) = "hello"

helloMe' :: NewTypeCoolBool -> String
helloMe' (NewTypeCoolBool _) = "hello"