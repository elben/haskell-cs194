
Template Haskell
================

CIS 194 Week 11
13 November 2014

This file is the companion module to the main lecture notes for this week,
defining the functions used in splices.

The main narrative is in the other file.

> {-# LANGUAGE TemplateHaskell #-}
>
> module SpliceFunctions where
>
> import Control.Monad ( replicateM, liftM )
> import Data.Maybe ( maybeToList )
> import Language.Haskell.TH
>
> add5 :: Integer -> Q Exp
> add5 n = return (AppE (AppE (VarE (mkName "+")) (LitE (IntegerL n)))
>                       (LitE (IntegerL 5)))
>
> compileTimeAdd5 :: Integer -> Q Exp
> compileTimeAdd5 n = return (LitE (IntegerL (n + 5)))
>
> listOfAs :: Q Exp
> listOfAs = return (ListE (map VarE [ mkName ('a' : show n) | n <- [1..100] ]))

> -- | Produce the body of a @liftM@ implementation. The parameter is the
> -- number of the @liftM@.
> liftMBody :: Int -> Q Exp
> liftMBody n = let m_names = take n [ mkName ('m' : [x]) | x <- ['a'..] ]
>                   names   = take n [ mkName [x]         | x <- ['a'..] ]
>                   binds   = zipWith mk_bind m_names names
>                   ret     = NoBindS (AppE (VarE (mkName "return"))
>                                           (mk_apps (VarE (mkName "f"))
>                                                    (map VarE names)))
>
>                   mk_bind :: Name -> Name -> Stmt
>                   mk_bind m_name name = BindS (VarP name) (VarE m_name)
>
>                   -- apply one expression to a list
>                   mk_apps :: Exp -> [Exp] -> Exp
>                   mk_apps f []     = f
>                   mk_apps f (x:xs) = mk_apps (AppE f x) xs
>
>                   -- or
>                   -- mk_apps = foldl AppE
>               in
>               return $ LamE (map VarP m_names) (DoE (binds ++ [ret]))

> liftMType :: Int -> Q Type
> liftMType n = let names = take n [ mkName [x] | x <- ['a'..] ]
>                   types = map VarT names
>                   m     = mkName "m"
>                   res   = mkName "r"
>                   resty = VarT res
>
>                   -- make nested arrows
>                   mk_arrs :: [Type] -> Type -> Type
>                   mk_arrs [] result = result
>                   mk_arrs (x:xs) result = AppT (AppT ArrowT x)
>                                                (mk_arrs xs result)
>
>                   -- apply "m" to a type
>                   app_m :: Type -> Type
>                   app_m ty = AppT (VarT m) ty
>               in
>               return $ ForallT (PlainTV m : PlainTV res :
>                                 map PlainTV names)
>                                [ClassP ''Monad [VarT m]]
>                                (mk_arrs (mk_arrs types resty : map app_m types)
>                                         (app_m resty))

> -- | Produce the body of a @liftM@ implementation. The parameter is the
> -- number of the @liftM@.
> liftMBody' :: Int -> Q Exp
> liftMBody' n = do
>   m_names <- replicateM n (newName "m")
>   names   <- replicateM n (newName "a")
>   let binds   = zipWith mk_bind m_names names
>       ret     = NoBindS (AppE (VarE (mkName "return"))
>                               (mk_apps (VarE (mkName "f"))
>                                        (map VarE names)))
>
>       mk_bind :: Name -> Name -> Stmt
>       mk_bind m_name name = BindS (VarP name) (VarE m_name)
>
>       mk_apps :: Exp -> [Exp] -> Exp
>       mk_apps = foldl AppE
>   return $ LamE (map VarP m_names) (DoE (binds ++ [ret]))

> liftMType' :: Int -> Q Type
> liftMType' n = do
>   names <- replicateM n (newName "a")
>   m     <- newName "m"
>   res   <- newName "r"
>
>   let types = map (return . VarT) names
>       mty   = (return . VarT) m
>       resty = (return . VarT) res
>
>       -- make nested arrows
>       mk_arrs :: [Q Type] -> Q Type -> Q Type
>       mk_arrs []     result = result
>       mk_arrs (x:xs) result = [t| $x -> $(mk_arrs xs result) |]
>
>       -- apply "m" to a type
>       app_m :: Q Type -> Q Type
>       app_m ty = [t| $mty $ty |]
>
>   forallT (map PlainTV (m : res : names)) (cxt [])
>           [t| Monad $mty => $(mk_arrs types resty) ->
>                             $(mk_arrs (map app_m types) (app_m resty)) |]


> class Sizable a where
>   size :: a -> Int
>   size _ = 1

Any member of this class has a known size, with a default of 1. We can define instances
for some basic types, all of size 1:

> instance Sizable Int
> instance Sizable Integer
> instance Sizable Bool
> instance Sizable Char

We could go further and start writing `Sizable` instances for other types, but the code
would be *very* boring. Instead, let's write TH functions to do the work for us.

> mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
> mapMaybeM _ [] = return []
> mapMaybeM f (x:xs) = do
>   maybe_b <- f x
>   bs      <- mapMaybeM f xs
>   return $ maybeToList maybe_b ++ bs
>   
> deriveSizable :: [Name] -> Q [Dec]  -- type is suitable for declaration splice
> deriveSizable = mapMaybeM deriveSizable1
>
> deriveSizable1 :: Name -> Q (Maybe Dec)
> deriveSizable1 name = do
>   info <- reify name
>   case info of
>     TyConI (DataD _ _name tvbs cons _)   ->
>       Just `liftM` deriveSizableData name tvbs cons
>     TyConI (NewtypeD _ _name tvbs con _) ->
>       Just `liftM` deriveSizableData name tvbs [con]
>     _                                    -> do
>       reportError $ show name ++ " is not a datatype"
>       return Nothing
>
> deriveSizableData :: Name -> [TyVarBndr] -> [Con] -> Q Dec
> deriveSizableData name tvbs cons = do
>   clauses <- mapM deriveSizableClause cons
>   return $ InstanceD context
>                      (AppT (ConT ''Sizable)
>                       (foldl AppT (ConT name) tvb_tys))
>                      [FunD 'size clauses]
>   where
>     tvb_names = map getTvbName tvbs
>     tvb_tys   = map VarT tvb_names
>     context   = map (ClassP ''Sizable . (:[]) . VarT) tvb_names
>
>     getTvbName (PlainTV n)    = n
>     getTvbName (KindedTV n _) = n
>
> deriveSizableClause :: Con -> Q Clause
> deriveSizableClause (NormalC con_name stys) = do
>   field_names <- mapM (const $ newName "x") stys
>   clause [conP con_name (map varP field_names)]
>          (normalB [| 1 + sum $(
>            listE (map (\x -> [| size $(varE x) |]) field_names)
>          ) |])
>          []
> deriveSizableClause (RecC con_name vstys)
>   = let stys = map strip_fst vstys in
>     deriveSizableClause (NormalC con_name stys)
>   where
>     strip_fst (_,b,c) = (b,c)
> deriveSizableClause (InfixC sty1 con_name sty2)
>   = deriveSizableClause (NormalC con_name [sty1, sty2])
> deriveSizableClause (ForallC _ _ con) = deriveSizableClause con
