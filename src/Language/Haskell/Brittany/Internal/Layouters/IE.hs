{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Haskell.Brittany.Internal.Layouters.IE
  ( layoutLLIEs
  , layoutAnnAndSepLLIEs
  , SortItemsFlag(..)
  ) where

import Data.Data (Data)
import Data.List.Extra qualified
import Data.Text (Text)
import Data.Text qualified as T
import Data.List qualified as L

import GHC (GenLocated(L), unLoc)
import GHC.Hs
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils

layoutIE :: LIE GhcPs -> ToBriDocM BriDocNumbered
layoutIE lie@(L _ ie) = docWrapNodeAround lie $ case ie of
  IEVar      _ x -> layoutWrapped lie x
  IEThingAbs _ x -> layoutWrapped lie x
  IEThingAll _ x -> docSeq [layoutWrapped lie x, docLitS "(..)"]
  IEThingWith _ x (IEWildcard _) _ ->
    docSeq [layoutWrapped lie x, docLitS "(..)"]
  IEThingWith _ x _ ns -> do
    let hasComments =
          hasCommentsBetween lie AnnOpenP AnnCloseP || hasAnyCommentsBelow x || or (map hasAnyCommentsBelow ns)
    let sortedNs = L.sortOn wrappedNameToText ns
    runFilteredAlternative $ do
      addAlternativeCond (not hasComments)
        $ docSeq
        $ [layoutWrapped lie x, docParenL]
        ++ L.intersperse docCommaSep (map nameDoc sortedNs)
        ++ [docParenR]
      addAlternative
        $ docWrapNodeAfter lie
        $ docAddBaseY BrIndentRegular
        $ docPar (layoutWrapped lie x) (layoutItems (splitFirstLast sortedNs))
     where
       nameDoc :: LIEWrappedName GhcPs -> ToBriDocM BriDocNumbered
       nameDoc = docLit . lrdrNameToTextAnn . ieLWrappedName
       layoutItem n = docSeq [docCommaSep, docWrapNodeAround n $ nameDoc n]
       layoutItems FirstLastEmpty = docSetBaseY $ docLines
         [ docSeq [docParenLSep, docNodeAnnKW lie (Just AnnOpenP) docEmpty]
         , docParenR
         ]
       layoutItems (FirstLastSingleton n) = docSetBaseY $ docLines
         [ docSeq [docParenLSep, docNodeAnnKW lie (Just AnnOpenP) $ nameDoc n]
         , docParenR
         ]
       layoutItems (FirstLast n1 nMs nN) =
         docSetBaseY
           $ docLines
           $ [docSeq [docParenLSep, docWrapNodeAround n1 $ nameDoc n1]]
           ++ map layoutItem nMs
           ++ [ docSeq [docCommaSep, docNodeAnnKW lie (Just AnnOpenP) $ nameDoc nN]
              , docParenR
              ]
  IEModuleContents _ n -> docSeq
    [ docLitS "module"
    , docSeparator
    , docLitS . moduleNameString $ unLoc n
    ]
  _ -> docEmpty
  where
    layoutWrapped :: LIE GhcPs -> LIEWrappedName GhcPs -> ToBriDocM BriDocNumbered
    layoutWrapped _ = \case
      L _ (IEName    _ n) -> docLit $ lrdrNameToTextAnn n
      L _ (IEPattern _ n) -> docLit $ T.pack "pattern " <> lrdrNameToTextAnn n
      L _ (IEType    _ n) -> docLit $ T.pack "type " <> lrdrNameToTextAnn n

data SortItemsFlag = ShouldSortItems | KeepItemsUnsorted
-- Helper function to deal with Located lists of LIEs.
-- In particular this will also associate documentation
-- from the located list that actually belongs to the last IE.
-- It also adds docCommaSep to all but the first element
-- This configuration allows both vertical and horizontal
-- handling of the resulting list. Adding parens is
-- left to the caller since that is context sensitive
layoutAnnAndSepLLIEs
  :: SortItemsFlag
  -> LocatedAn ann [LIE GhcPs]
  -> ToBriDocM [ToBriDocM BriDocNumbered]
layoutAnnAndSepLLIEs shouldSort llies@(L _ lies) = do
  let makeIENode ie = docSeq [docCommaSep, ie]
  let
    sortedLies =
      [ items
      | group <- Data.List.Extra.groupOn lieToText $ L.sortOn lieToText lies
      , items <- mergeGroup group
      ]
  let
    ieDocs = fmap layoutIE $ case shouldSort of
      ShouldSortItems -> sortedLies
      KeepItemsUnsorted -> lies
  ieCommaDocs <-
    docWrapNodeAfter llies $ sequence $ case splitFirstLast ieDocs of
      FirstLastEmpty -> []
      FirstLastSingleton ie -> [ie]
      FirstLast ie1 ieMs ieN ->
        [ie1] ++ map makeIENode ieMs ++ [makeIENode ieN]
  pure $ fmap pure ieCommaDocs -- returned shared nodes
 where
  mergeGroup :: [LIE GhcPs] -> [LIE GhcPs]
  mergeGroup [] = []
  mergeGroup items@[_] = items
  mergeGroup items = if
    | all isProperIEThing items -> [L.foldl1' thingFolder items]
    | all isIEVar items -> [L.foldl1' thingFolder items]
    | otherwise -> items
  -- proper means that if it is a ThingWith, it does not contain a wildcard
  -- (because I don't know what a wildcard means if it is not already a
  -- IEThingAll).
  isProperIEThing :: LIE GhcPs -> Bool
  isProperIEThing = \case
    L _ (IEThingAbs _ _wn) -> True
    L _ (IEThingAll _ _wn) -> True
    L _ (IEThingWith _ _wn NoIEWildcard _) -> True
    _ -> False
  isIEVar :: LIE GhcPs -> Bool
  isIEVar = \case
    L _ IEVar{} -> True
    _ -> False
  thingFolder :: LIE GhcPs -> LIE GhcPs -> LIE GhcPs
  thingFolder l1@(L _ IEVar{}) _ = l1
  thingFolder l1@(L _ IEThingAll{}) _ = l1
  thingFolder _ l2@(L _ IEThingAll{}) = l2
  thingFolder l1 (L _ IEThingAbs{}) = l1
  thingFolder (L _ IEThingAbs{}) l2 = l2
  thingFolder (L l (IEThingWith x wn _ consItems1)) (L _ (IEThingWith _ _ _ consItems2))
    = L l (IEThingWith x wn NoIEWildcard (consItems1 ++ consItems2))
  thingFolder _ _ =
    error "thingFolder should be exhaustive because we have a guard above"


-- Builds a complete layout for the given located
-- list of LIEs. The layout provides two alternatives:
-- (item, item, ..., item)
-- ( item
-- , item
-- ...
-- , item
-- )
-- If the llies contains comments the list will
-- always expand over multiple lines, even when empty:
-- () -- no comments
-- ( -- a comment
-- )
layoutLLIEs
  :: Data ann
  => Bool -> SortItemsFlag -> LocatedAn ann [LIE GhcPs] -> ToBriDocM BriDocNumbered
layoutLLIEs enableSingleline shouldSort llies = do
  ieDs <- layoutAnnAndSepLLIEs shouldSort llies
  let hasComments = hasAnyCommentsBelow llies
  runFilteredAlternative $ case ieDs of
    [] -> do
      addAlternativeCond (not hasComments) $ docLitS "()"
      addAlternativeCond hasComments $ docPar
        (docSeq [docParenLSep, docWrapNodeAfter llies docEmpty])
        docParenR
    (ieDsH : ieDsT) -> do
      addAlternativeCond (not hasComments && enableSingleline)
        $ docSeq
        $ [docParenL]
        ++ (docForceSingleline <$> ieDs)
        ++ [docParenR]
      addAlternative
        $ docPar (docSetBaseY $ docSeq [docParenLSep, ieDsH])
        $ docLines
        $ ieDsT
        ++ [docParenR]

-- | Returns a "fingerprint string", not a full text representation, nor even
-- a source code representation of this syntax node.
-- Used for sorting, not for printing the formatter's output source code.
wrappedNameToText :: LIEWrappedName GhcPs -> Text
wrappedNameToText = \case
  L _ (IEName    _ n) -> lrdrNameToText n
  L _ (IEPattern _ n) -> lrdrNameToText n
  L _ (IEType    _ n) -> lrdrNameToText n

-- | Returns a "fingerprint string", not a full text representation, nor even
-- a source code representation of this syntax node.
-- Used for sorting, not for printing the formatter's output source code.
lieToText :: LIE GhcPs -> Text
lieToText = \case
  L _ (IEVar       _ wn)     -> wrappedNameToText wn
  L _ (IEThingAbs  _ wn)     -> wrappedNameToText wn
  L _ (IEThingAll  _ wn)     -> wrappedNameToText wn
  L _ (IEThingWith _ wn _ _) -> wrappedNameToText wn
  -- TODO: These _may_ appear in exports!
  -- Need to check, and either put them at the top (for module) or do some
  -- other clever thing.
  L _ (IEModuleContents _ n) -> moduleNameToText n
  L _ IEGroup{}              -> T.pack "@IEGroup"
  L _ IEDoc{}                -> T.pack "@IEDoc"
  L _ IEDocNamed{}           -> T.pack "@IEDocNamed"
 where
  moduleNameToText :: LocatedAn ann ModuleName -> Text
  moduleNameToText (L _ name) =
    T.pack ("@IEModuleContents" ++ moduleNameString name)
