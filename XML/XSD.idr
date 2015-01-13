module XSD

data BaseTy = StringTy | DecimalTy | IntTy | BoolTy | DateTy | TimeTy | DateTimeTy

interpTy : BaseTy -> Type
interpTy StringTy = String
interpTy DecimalTy = Float
interpTy IntTy     = Int
interpTy BoolTy    = Bool
interpTy DateTy    = String
interpTy TimeTy    = String
interpTy DateTimeTy = String
data ElemTy = TYPE | ELEM | ATTR | SCHEMA


-- TODO how to model restrictions?
data Order = All | Sequence | Choice
data Occurence = Bounded Int Int | Unbounded | Default

data Schema : ElemTy -> Type where
  MkSchema : Schema ELEM -> Schema SCHEMA

  Simple : String -> Maybe String -> List (Schema ATTR) -> BaseTy -> Maybe (Either BaseTy BaseTy) -> Schema ELEM
  Complex : String -> Maybe String -> Occurence -> Bool -> Order -> List (Schema ATTR) -> List (Schema ELEM) -> Schema ELEM
  -- Cited : String -> String -> Schema ELEM
  -- Group : String -> Order -> List (Schema ELEM) -> Schema ELEM

  Attr : String -> Maybe String -> Bool -> Maybe (Either String String) -> Schema ATTR
  -- AttrGroup : String -> List (Schema ATTR) -> Schema ATTR
  -- CitedAttr : String -> Schema ATTR


pattern : Schema SCHEMA
pattern = MkSchema $ Complex "pattern" Nothing Default False Sequence
                             [Attr "id" Nothing True Nothing]
                             [head, context]
  where
    head : Schema ELEM
    head = Complex "mdata" Nothing Default False Sequence Nil [
      Simple "name" Nothing Nil StringTy Nothing,
      Complex "aliases" Nothing Default False Sequence Nil [Simple "alias" Nothing Nil StringTy Nothing],
      Complex "classification" Nothing Default False Sequence Nil [Simple "class" Nothing Nil StringTy Nothing],
      Simple "abstract" Nothing Nil StringTy Nothing,
      Simple "created" Nothing Nil DateTimeTy Nothing,
      Simple "modified" Nothing Nil DateTimeTy Nothing,
      Simple "author" Nothing Nil DateTimeTy Nothing
    ]
    context : Schema ELEM
    context = Simple "context" Nothing Nil StringTy Nothing
