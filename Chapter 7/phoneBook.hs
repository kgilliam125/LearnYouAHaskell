type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = 
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-2928")
    ,("penny", "853-2928")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook