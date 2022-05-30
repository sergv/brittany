{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern
  ( layoutPat
  , colsWrapPat
  ) where

import Data.Bifunctor
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Traversable
import GHC (GenLocated(L), ol_val)
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Parser.Annotation.Ext
import GHC.Types.Basic
import Language.Haskell.Brittany.Internal.LayouterBasics
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types

-- | Layouts patterns (inside function bindings, case alternatives, let
-- bindings or do notation). E.g. for input
--        > case computation of
--        >   (warnings, Success a b) -> ..
-- This part  ^^^^^^^^^^^^^^^^^^^^^^^ of the syntax tree is layouted by
-- 'layoutPat'. Similarly for
-- > func abc True 0 = []
--        ^^^^^^^^^^ this part
-- We will use `case .. of` as the imagined prefix to the examples used in
-- the different cases below.
layoutPat :: LPat GhcPs -> ToBriDocM (Seq BriDocNumbered)
layoutPat lpat@(L _ pat) = docWrapNodeAround lpat $ case pat of
  WildPat _ -> fmap Seq.singleton $ docLitS "_"
    -- _ -> expr
  VarPat _ n -> fmap Seq.singleton $ docLit $ lrdrNameToText n
    -- abc -> expr
  LitPat _ lit -> fmap Seq.singleton $ allocateNode $ litBriDoc lit
    -- 0 -> expr
  ParPat _ inner -> do
    -- (nestedpat) -> expr
    left  <- docParenL
    right <- docParenR
    innerDocs <- colsWrapPat =<< layoutPat inner
    return $ Seq.empty Seq.|> left Seq.|> innerDocs Seq.|> right
    -- return $ (left Seq.<| innerDocs) Seq.|> right
    -- case Seq.viewl innerDocs of
    --   Seq.EmptyL -> fmap return $ docLit $ Text.pack "()" -- this should never occur..
    --   x1 Seq.:< rest -> case Seq.viewr rest of
    --     Seq.EmptyR ->
    --       fmap return $ docSeq
    --       [ docLit $ Text.pack "("
    --       , return x1
    --       , docLit $ Text.pack ")"
    --       ]
    --     middle Seq.:> xN -> do
    --       x1' <- docSeq [docLit $ Text.pack "(", return x1]
    --       xN' <- docSeq [return xN, docLit $ Text.pack ")"]
    --       return $ (x1' Seq.<| middle) Seq.|> xN'
  ConPat _ lname (PrefixCon tyargs args) -> do
    -- Abc @x @y @z a b c -> expr
    let nameDoc = lrdrNameToTextAnn lname
    -- HsPatSigType
    tyargs' <- traverse layoutHsPatSigType tyargs
    args'   <- traverse layoutPat args
    case tyargs' ++ args' of
      [] -> pure <$> docLit nameDoc
      xs -> do
        x1 <- appSep (docLit nameDoc)
        xR <- fmap Seq.fromList $ sequence $ spacifyDocs $ fmap colsWrapPat xs
        pure $ x1 Seq.<| xR

    -- if null args'
    --   then return <$> docLit nameDoc
    --   else do
    --     x1 <- appSep (docLit nameDoc)
    --     xR <- fmap Seq.fromList $ sequence $ spacifyDocs $ fmap colsWrapPat args'
    --     return $ x1 Seq.<| xR
  ConPat _ lname (InfixCon left right) -> do
    -- a :< b -> expr
    let nameDoc = lrdrNameToTextAnn lname
    leftDoc  <- appSep . colsWrapPat =<< layoutPat left
    rightDoc <- colsWrapPat =<< layoutPat right
    middle   <- appSep $ docLit nameDoc
    return $ Seq.empty Seq.|> leftDoc Seq.|> middle Seq.|> rightDoc
  ConPat _ lname (RecCon (HsRecFields [] Nothing)) -> do
    -- Abc{} -> expr
    let t = lrdrNameToText lname
    fmap Seq.singleton $ docLit $ t <> Text.pack "{}"
  ConPat _ lname (RecCon (HsRecFields fs@(_ : _) Nothing)) -> do
    -- Abc { a = locA, b = locB, c = locC } -> expr1
    -- Abc { a, b, c } -> expr2
    let t = lrdrNameToText lname
    fds <- fs `forM` \(L _ (HsRecField _ann (L _ fieldOcc) fPat pun)) -> do
      let FieldOcc _ lnameF = fieldOcc
      fExpDoc <- if pun
        then return Nothing
        else Just <$> docSharedWrapper layoutPat fPat
      return (lrdrNameToText lnameF, fExpDoc)
    Seq.singleton <$> docSeq
      [ appSep $ docLit t
      , appSep $ docLitS "{"
      , docSeq $ List.intersperse docCommaSep $ fds <&> \case
        (fieldName, Just fieldDoc) -> docSeq
          [ appSep $ docLit fieldName
          , appSep $ docLitS "="
          , fieldDoc >>= colsWrapPat
          ]
        (fieldName, Nothing) -> docLit fieldName
      , docSeparator
      , docLitS "}"
      ]
  ConPat _ lname (RecCon (HsRecFields [] (Just (L _ 0)))) -> do
    -- Abc { .. } -> expr
    let t = lrdrNameToText lname
    Seq.singleton <$> docSeq [appSep $ docLit t, docLitS "{..}"]
  ConPat _ lname (RecCon (HsRecFields fs@(_ : _) (Just (L _ dotdoti))))
    | dotdoti == length fs -> do
    -- Abc { a = locA, .. }
      let t = lrdrNameToText lname
      fds <- fs `forM` \(L _ (HsRecField _ann (L _ fieldOcc) fPat pun)) -> do
        let FieldOcc _ lnameF = fieldOcc
        fExpDoc <- if pun
          then return Nothing
          else Just <$> docSharedWrapper layoutPat fPat
        return (lrdrNameToText lnameF, fExpDoc)
      Seq.singleton <$> docSeq
        [ appSep $ docLit t
        , appSep $ docLitS "{"
        , docSeq $ fds >>= \case
          (fieldName, Just fieldDoc) ->
            [ appSep $ docLit fieldName
            , appSep $ docLitS "="
            , fieldDoc >>= colsWrapPat
            , docCommaSep
            ]
          (fieldName, Nothing) -> [docLit fieldName, docCommaSep]
        , docLitS "..}"
        ]
  TuplePat _ args boxity -> do
    -- (nestedpat1, nestedpat2, nestedpat3) -> expr
    -- (#nestedpat1, nestedpat2, nestedpat3#) -> expr
    case boxity of
      Boxed -> wrapPatListy args "()" docParenL docParenR
      Unboxed -> wrapPatListy args "(##)" docParenHashLSep docParenHashRSep
  AsPat _ asName asPat -> do
    -- bind@nestedpat -> expr
    wrapPatPrepend asPat (docLit $ Text.snoc (lrdrNameToText asName) '@')
  SigPat _ pat1 (HsPS _ ty1) -> do
    -- i :: Int -> expr
    patDocs <- layoutPat pat1
    tyDoc <- docSharedWrapper layoutType ty1
    case Seq.viewr patDocs of
      Seq.EmptyR -> error "cannot happen"
      xR Seq.:> xN -> do
        xN' <- -- at the moment, we don't support splitting patterns into
               -- multiple lines. but we cannot enforce pasting everything
               -- into one line either, because the type signature will ignore
               -- this if we overflow sufficiently.
               -- In order to prevent syntactically invalid results in such
               -- cases, we need the AddBaseY here.
               -- This can all change when patterns get multiline support.
               docAddBaseY BrIndentRegular $ docSeq
          [ appSep $ return xN
          , appSep $ docLitS "::"
          , docForceSingleline tyDoc
          ]
        return $ xR Seq.|> xN'
  ListPat _ elems ->
    -- [] -> expr1
    -- [nestedpat1, nestedpat2, nestedpat3] -> expr2
    wrapPatListy elems "[]" docBracketL docBracketR
  BangPat _ pat1 -> do
    -- !nestedpat -> expr
    wrapPatPrepend pat1 (docLitS "!")
  LazyPat _ pat1 -> do
    -- ~nestedpat -> expr
    wrapPatPrepend pat1 (docLitS "~")
  NPat ann llit@(L _ ol) mNegative _ -> do
    -- -13 -> expr
    let llit' = first (SrcSpanAnn ann) llit
    litDoc <- docWrapNodeAround llit' $ allocateNode $ overLitValBriDoc $ GHC.ol_val ol
    negDoc <- docLitS "-"
    pure $ case mNegative of
      Just{} -> Seq.fromList [negDoc, litDoc]
      Nothing -> Seq.singleton litDoc

  _ -> return <$> briDocByExactInlineOnly "some unknown pattern" lpat

colsWrapPat :: Seq BriDocNumbered -> ToBriDocM BriDocNumbered
colsWrapPat = docCols ColPatterns . fmap return . Foldable.toList

wrapPatPrepend
  :: LPat GhcPs -> ToBriDocM BriDocNumbered -> ToBriDocM (Seq BriDocNumbered)
wrapPatPrepend pat prepElem = do
  patDocs <- layoutPat pat
  case Seq.viewl patDocs of
    Seq.EmptyL -> return Seq.empty
    x1 Seq.:< xR -> do
      x1' <- docSeq [prepElem, return x1]
      return $ x1' Seq.<| xR

wrapPatListy
  :: [LPat GhcPs]
  -> String
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM (Seq BriDocNumbered)
wrapPatListy elems both start end = do
  elemDocs <- Seq.fromList elems `forM` (layoutPat >=> colsWrapPat)
  case Seq.viewl elemDocs of
    Seq.EmptyL     -> fmap Seq.singleton $ docLitS both
    x1 Seq.:< rest -> do
      sDoc  <- start
      eDoc  <- end
      rest' <- rest `forM` \bd -> docSeq [docCommaSep, return bd]
      return $ (sDoc Seq.<| x1 Seq.<| rest') Seq.|> eDoc

layoutHsPatSigType :: HsPatSigType GhcPs -> ToBriDocM (Seq BriDocNumbered)
layoutHsPatSigType HsPS{hsps_ext = ann, hsps_body = typ} =
  Seq.singleton <$> docSeq [docLitS "@", layoutType typ]
