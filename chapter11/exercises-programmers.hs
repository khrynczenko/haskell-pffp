
data OperatingSystem = GnuPlusLinux
                     | OpenBSD
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgLang = Haskell
              | Agda
              | Idris
              | PureScript
              deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang }
                             deriving (Eq, Show)


-- Write a function that generates all possible values of Programmer.
--
-- Cardinality of the Programmer type is 16 (4 * 4)

allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = concatMap (\f -> map f allLanguages) (map Programmer allOperatingSystems)
