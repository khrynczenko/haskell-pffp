data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) (Only lhs) (Only rhs) = Only (lhs <> rhs)
    (<>) (Only lhs) Nada = Only lhs
    (<>) Nada (Only rhs) = Only rhs
    (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = mempty
    mappend = (<>)
