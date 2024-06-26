module Language.Haskell.Brittany.Internal.Layouters.Import (layoutImport) where

import Control.Monad.Trans.MultiRWS (MonadMultiReader(..))
import Data.Semigroup
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Ext qualified as T

import GHC (GenLocated(L), unLoc)
import GHC.Hs
import GHC.Types.PkgQual
import GHC.Types.SourceText
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE
import Language.Haskell.Brittany.Internal.Types

prepPkg :: SourceText -> Text
prepPkg rawN = case rawN of
  SourceText n -> T.fromFastString n
  -- This would be odd to encounter and the
  -- result will most certainly be wrong
  NoSourceText -> T.empty

layoutImport :: ImportDecl GhcPs -> ToBriDocM BriDocNumbered
layoutImport importD = case importD of
  ImportDecl _ (L _ modName) pkg src safe q mas impList -> do
    importCol    <- confUnpack . _lconfig_importColumn   . _conf_layout <$> mAsk
    importAsCol  <- confUnpack . _lconfig_importAsColumn . _conf_layout <$> mAsk
    indentPolicy <- confUnpack . _lconfig_indentPolicy   . _conf_layout <$> mAsk
    let compact     = indentPolicy /= IndentPolicyFree
        modNameT    = T.pack $ moduleNameString modName
        pkgNameT :: Maybe Text
        pkgNameT    = case pkg of
          NoRawPkgQual -> Nothing
          RawPkgQual s -> Just $ prepPkg $ sl_st s
        masT        = T.pack . moduleNameString . unLoc <$> mas
        hiding      = case impList of
          Just (EverythingBut, _) -> True
          _                       -> False
        minQLength  = length "import qualified"
        qLengthReal =
          let qualifiedPart = if q /= NotQualified then length "qualified " else 0
              safePart = if safe then length "safe " else 0
              pkgPart  = maybe 0 ((+ 1) . T.length) pkgNameT
              srcPart  = case src of
                IsBoot  -> length "{-# SOURCE #-} "
                NotBoot -> 0
          in length "import " + srcPart + safePart + qualifiedPart + pkgPart
        qLength          = max minQLength qLengthReal
        -- Cost in columns of importColumn
        asCost           = length "as "
        hidingParenCost  = if hiding then length "hiding ( " else length "( "
        nameCost         = T.length modNameT + qLength
        importQualifiers = docSeq
          [ appSep $ docLitS "import"
          , case src of
            IsBoot  -> appSep $ docLitS "{-# SOURCE #-}"
            NotBoot -> docEmpty
          , if safe then appSep $ docLitS "safe" else docEmpty
          , if q /= NotQualified
            then appSep $ docLitS "qualified"
            else docEmpty
          , maybe docEmpty (appSep . docLit) pkgNameT
          ]
        indentName    =
          if compact then id else docEnsureIndent (BrIndentSpecial qLength)
        modNameD      = indentName $ appSep $ docLit modNameT
        hidDocCol     = if hiding then importCol - hidingParenCost else importCol - 2
        hidDocColDiff = importCol - 2 - hidDocCol
        hidDoc        =
          if hiding then appSep $ docLitS "hiding" else docEmpty
        importHead    = docSeq [importQualifiers, modNameD]
        bindingsD     = case impList of
          Nothing -> docEmpty
          Just (_, llies) -> do
            let hasComments = hasAnyCommentsBelow llies
            if compact
              then docAlt
                [ docSeq
                  [ hidDoc
                  , docForceSingleline $ layoutLLIEs True ShouldSortItems llies
                  ]
                , let
                    makeParIfHiding = if hiding
                      then docAddBaseY BrIndentRegular . docPar hidDoc
                      else id
                  in makeParIfHiding (layoutLLIEs True ShouldSortItems llies)
                ]
              else do
                ieDs <- layoutAnnAndSepLLIEs ShouldSortItems llies
                docWrapNodeAfter llies
                  $ docEnsureIndent (BrIndentSpecial hidDocCol)
                  $ case ieDs of
                    -- ..[hiding].( )
                      [] -> if hasComments
                        then docPar
                          (docSeq
                            [hidDoc, docParenLSep, docWrapNodeAround llies docEmpty]
                          )
                          (docEnsureIndent
                            (BrIndentSpecial hidDocColDiff)
                            docParenR
                          )
                        else docSeq
                          [hidDoc, docParenLSep, docSeparator, docParenR]
                      -- ..[hiding].( b )
                      [ieD] -> runFilteredAlternative $ do
                        addAlternativeCond (not hasComments)
                          $ docSeq
                              [ hidDoc
                              , docParenLSep
                              , docForceSingleline ieD
                              , docSeparator
                              , docParenR
                              ]
                        addAlternative $ docPar
                          (docSeq [hidDoc, docParenLSep, docNonBottomSpacing ieD])
                          (docEnsureIndent
                            (BrIndentSpecial hidDocColDiff)
                            docParenR
                          )
                      -- ..[hiding].( b
                      --            , b'
                      --            )
                      (ieD : ieDs') -> docPar
                        (docSeq [hidDoc, docSetBaseY $ docSeq [docParenLSep, ieD]]
                        )
                        (docEnsureIndent (BrIndentSpecial hidDocColDiff)
                        $ docLines
                        $ ieDs'
                        ++ [docParenR]
                        )
        makeAsDoc asT =
          docSeq [docLitS "as", docSeparator, docLit asT, docSeparator]
    if compact
      then
        let asDoc = maybe docEmpty makeAsDoc masT
        in
          docAlt
            [ docForceSingleline $ docSeq [importHead, asDoc, bindingsD]
            , docAddBaseY BrIndentRegular
              $ docPar (docSeq [importHead, asDoc]) bindingsD
            ]
      else case masT of
        Just n -> if enoughRoom
          then docLines [docSeq [importHead, asDoc], bindingsD]
          else docLines [importHead, asDoc, bindingsD]
          where
            enoughRoom = nameCost < importAsCol - asCost
            asDoc      = docEnsureIndent (BrIndentSpecial (importAsCol - asCost)) $ makeAsDoc n
        Nothing -> if enoughRoom
          then docSeq [importHead, bindingsD]
          else docLines [importHead, bindingsD]
          where
            enoughRoom = nameCost < importCol - hidingParenCost
