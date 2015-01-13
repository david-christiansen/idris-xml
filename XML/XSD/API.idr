module XML.XSD.API

import XML.XSD.Model

mkElement : String -> BaseTy -> Schema ELEM
mkElement n ty = Simple n ty Nothing Nil Nothing

mkComplex : String -> Schema ELEM
mkComplex n = Complex n Nothing Default False Sequence Nil Nil

mkAttribute : String -> BaseTy -> Schema ATTR
mkAttribute n ty = Attr n ty False Nothing

addElement : Schema ELEM -> Schema ELEM -> Schema ELEM
addElement (Complex n id o m ord as es) e = Complex n id o m ord as (e::es)
addElement e                            _ = e

addAttribute : Schema ATTR -> Schema ELEM -> Schema ELEM
addAttribute a (Simple n ty id as v) = Simple n ty id (a::as) v
addAttribute a (Complex n id o m ord as es) = (Complex n id o m ord (a::as) es)

infixl 2 <++>
infixl 2 <+@>

(<++>) : Schema ELEM -> Schema ELEM -> Schema ELEM
(<++>) p c = addElement p c

(<+@>) : Schema ELEM -> Schema ATTR -> Schema ELEM
(<+@>) e a = addAttribute a e


pattern : Schema SCHEMA
pattern = MkSchema $ mkComplex "pattern" <+@> mkAttribute "id" StringTy <++> head <++> context
  where
    head : Schema ELEM
    head = mkComplex "mdata"
      <++> (mkComplex "aliases"  <++> (mkElement "alias" StringTy))
      <++> (mkComplex "classification" <++> (mkElement "class" StringTy))
      <++> (mkElement "created"  DateTimeTy)
      <++> (mkElement "modified" DateTimeTy)
      <++> (mkElement "author"   DateTimeTy)

    context : Schema ELEM
    context = mkElement "context" StringTy

    abstrac : Schema ELEM
    abstrac = mkElement "abstract" StringTy

    name : Schema ELEM
    name = mkElement "name" StringTy
