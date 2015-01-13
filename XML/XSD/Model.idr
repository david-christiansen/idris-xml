-- --------------------------------------------------------------- [ Model.idr ]
-- Module    : Model.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module XML.XSD.Model

data BaseTy = StringTy
            | DecimalTy
            | IntTy
            | BoolTy
            | DateTy
            | TimeTy
            | DateTimeTy

interpTy : BaseTy -> Type
interpTy StringTy = String
interpTy DecimalTy = Float
interpTy IntTy     = Int
interpTy BoolTy    = Bool
interpTy DateTy    = String
interpTy TimeTy    = String
interpTy DateTimeTy = String

data ElemTy = TYPE
            | ELEM
            | ATTR
            | SCHEMA


-- TODO how to model restrictions?
data Order = All | Sequence | Choice

data Occurrence = Bounded Int Int
               | Unbounded
               | Default

data Schema : ElemTy -> Type where
  MkSchema : Schema ELEM -> Schema SCHEMA

  ||| Make a simple element.
  |||
  ||| @name Element's name.
  ||| @id   Element's ID.
  ||| @as   Attributes
  ||| @ty   Type of value
  ||| @values Might have a default value *or* a fixed value.
  Simple : (name : String)
         -> (ty : BaseTy)
         -> (id : Maybe String)
         -> (as : List (Schema ATTR))
         -> (values : Maybe (Either BaseTy BaseTy)) -> Schema ELEM

  ||| Make a Complex element.
  |||
  ||| @name  Element's name
  ||| @id    Element's ID.
  ||| @occ   Occurrence Indicators.
  ||| @mixed Can elements be mixed with text nodes?
  ||| @ord   Order Indicators.
  ||| @as    Attributes
  ||| @es    Child elements.
  Complex : (name : String)
          -> (id : Maybe String)
          -> (occ : Occurrence)
          -> (mixed : Bool)
          -> (ord : Order)
          -> (as : List (Schema ATTR))
          -> (es : List (Schema ELEM)) -> Schema ELEM
  -- Cited : String -> String -> Schema ELEM
  -- Group : String -> Order -> List (Schema ELEM) -> Schema ELEM

  ||| Make an attribute.
  |||
  ||| @name   Name of attribute
  ||| @ty     Base type.
  ||| @req    Required?
  ||| @values Might have a default value *or* a fixed value.
  Attr : (name : String)
       -> (ty : BaseTy)
       -> (req : Bool)
       -> (values : Maybe (Either String String)) -> Schema ATTR
  -- AttrGroup : String -> List (Schema ATTR) -> Schema ATTR
  -- CitedAttr : String -> Schema ATTR

-- --------------------------------------------------------------------- [ EOF ]
