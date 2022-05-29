{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Type
  ( layoutSigType
  , layoutType
  , layoutTyVarBndrs
  , processTyVarBndrsSingleline
  ) where

import qualified Data.Text as Text
import GHC (AnnKeywordId(..), GenLocated(L))
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (ftext, showSDocUnsafe)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils (FirstLastView(..), splitFirstLast)

forgetTyVarBndrFlag :: LHsTyVarBndr a (NoGhcTc GhcPs) -> LHsTyVarBndr () (NoGhcTc GhcPs)
forgetTyVarBndrFlag (L a bndr) = L a $ case bndr of
  UserTyVar   ext flag x   -> UserTyVar ext () x
  KindedTyVar ext flag x y -> KindedTyVar ext () x y

layoutSigType :: LHsSigType GhcPs -> ToBriDocM BriDocNumbered
layoutSigType (L _ HsSig{sig_bndrs, sig_body}) =
  case sig_bndrs of
    HsOuterImplicit{}          -> layoutType sig_body
    HsOuterExplicit{hso_bndrs} -> layoutForallType (map forgetTyVarBndrFlag hso_bndrs) sig_body

layoutContext :: LHsContext GhcPs -> ToBriDocM BriDocNumbered
layoutContext lcs@(L _ cs) = do
  cs' <- traverse (docSharedWrapper layoutType) cs
  docWrapNodeAround lcs $ case cs' of
    []  -> docLitS "()"
    [x] -> x
    _   -> runFilteredAlternative $ do
      addAlternative $ do
        let list = List.intersperse docCommaSep $ docForceSingleline <$> cs'
        docSeq $ [docParenL] ++ list ++ [docParenR]
      addAlternative $ do
        let open = docCols
              ColTyOpPrefix
              [docParenLSep, docAddBaseY (BrIndentSpecial 2) $ head cs']
            list = List.tail cs' <&> \cntxtDoc -> docCols
              ColTyOpPrefix
              [docCommaSep, docAddBaseY (BrIndentSpecial 2) $ cntxtDoc]
        docPar open $ docLines $ list ++ [docParenR]

layoutForallType :: [LHsTyVarBndr () (NoGhcTc GhcPs)] -> LHsType GhcPs -> ToBriDocM BriDocNumbered
layoutForallType bndrs ltype = do
  forallDoc <- docSharedWrapper docLitS "forall"
  tyVarDocs <- layoutTyVarBndrs bndrs
  case ltype of
    L _ HsQualTy{hst_ctxt = Just context, hst_body} -> do
      typeDoc    <- docSharedWrapper layoutType hst_body
      contextDoc <- docSharedWrapper layoutContext context
      let maybeForceML = case hst_body of
            L _ HsFunTy{} -> docForceMultiline
            _             -> id
          tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
          forallHeader     = runFilteredAlternative $ do
            addAlternative $
              docSeq $ [forallDoc] ++ tyVarDocLineList
            addAlternative $
              docPar
                forallDoc
                (docLines $ tyVarDocs <&> \case
                  (tname, Nothing)  -> docEnsureIndent BrIndentRegular $ docLit tname
                  (tname, Just doc) -> docEnsureIndent BrIndentRegular $ docLines
                    [ docCols ColTyOpPrefix [docParenLSep, docLit tname]
                    , docCols ColTyOpPrefix [docLitS ":: ", doc]
                    , docParenR
                    ])
      runFilteredAlternative $ do
        -- :: forall a b c . (Foo a b c) => a b -> c
        addAlternative $
          docSeq
            [ if null bndrs
              then docEmpty
              else docSeq ([forallHeader, docSeparator] ++ tyVarDocLineList ++ [docLitS " . "])
            , docForceSingleline contextDoc
            , docLitS " => "
            , docForceSingleline typeDoc
            ]
        -- :: forall a b c
        --  . (Foo a b c)
        -- => a b
        -- -> c
        addAlternative $
          docPar
            forallHeader
            (docLines
              [ docCols
                ColTyOpPrefix
                [ docWrapNodeAfter ltype $ docLitS " . "
                , docAddBaseY (BrIndentSpecial 3) contextDoc
                ]
              , docCols
                ColTyOpPrefix
                [ docLitS "=> "
                , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc
                ]
              ])
    typ -> do
      -- typeDoc   <- layoutType typ
      typeDoc <- docSharedWrapper layoutType typ
      let maybeForceML = case typ of
            L _ HsFunTy{} -> docForceMultiline
            _             -> id
      let tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
      runFilteredAlternative $ do
        -- forall x . x
        addAlternative $
          docSeq
          [ if null bndrs
            then docEmpty
            else docSeq ([forallDoc] ++ tyVarDocLineList ++ [docLitS " . "])
          , docForceSingleline typeDoc
          ]
        -- :: forall x
        --  . x
        addAlternative $
          docPar
            (docSeq $ forallDoc : tyVarDocLineList)
            (docCols
              ColTyOpPrefix
              [ docWrapNodeAfter typ $ docLitS " . "
              , maybeForceML typeDoc
              ])
        -- :: forall
        --      (x :: *)
        --  . x
        addAlternative $
          docPar
            forallDoc
            (docLines
            $ (tyVarDocs <&> \case
                (tname, Nothing)  ->
                  docEnsureIndent BrIndentRegular $ docLit tname
                (tname, Just doc) -> docEnsureIndent BrIndentRegular $ docLines
                  [ docCols ColTyOpPrefix [docParenLSep, docLit tname]
                  , docCols ColTyOpPrefix [docLitS ":: ", doc]
                  , docParenR
                  ])
            ++ [ docCols
                   ColTyOpPrefix
                   [ docWrapNodeAfter typ $ docLitS " . "
                   , maybeForceML typeDoc
                   ]
               ])

layoutType :: LHsType GhcPs -> ToBriDocM BriDocNumbered
layoutType ltype@(L _ typ) = docWrapNodeAround ltype $ case typ of
  HsTyVar _ promoted name -> do
    let t = lrdrNameToTextAnnTypeEqualityIsSpecial name
    case promoted of
      IsPromoted  -> docSeq [docSeparator, docTick, docWrapNodeAround name $ docLit t]
      NotPromoted -> docWrapNodeAround name $ docLit t

  HsForAllTy _ hsf typ -> do
    let bndrs = getBinders hsf
    layoutForallType bndrs typ

  -- TODO: confirm this implementation makes sense, maybe unify with
  -- the following case.
  HsQualTy _ Nothing typ1 -> do
    typeDoc <- docSharedWrapper layoutType typ1
    let maybeForceML = case typ1 of
          L _ HsFunTy{} -> docForceMultiline
          _             -> id
    runFilteredAlternative $ do
      -- a b -> c
      addAlternative $
        docForceSingleline typeDoc
      --    a b
      -- -> c
      addAlternative $
        docCols
          ColTyOpPrefix
          [ docLitS "   "
          , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc
          ]
  HsQualTy _ (Just context) typ1 -> do
    typeDoc    <- docSharedWrapper layoutType typ1
    contextDoc <- docSharedWrapper layoutContext context
    let maybeForceML = case typ1 of
          L _ HsFunTy{} -> docForceMultiline
          _             -> id
    runFilteredAlternative $ do
      -- (Foo a b c) => a b -> c
      addAlternative $
        docSeq
        [ docForceSingleline contextDoc
        , docLitS " => "
        , docForceSingleline typeDoc
        ]
      --    (Foo a b c)
      -- => a b
      -- -> c
      addAlternative $ docPar
        (docForceSingleline contextDoc)
        (docCols
          ColTyOpPrefix
          [ docLitS "=> "
          , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc
          ])
  HsFunTy _ _ typ1 typ2 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc2 <- docSharedWrapper layoutType typ2
    let maybeForceML = case typ2 of
          L _ HsFunTy{} -> docForceMultiline
          _             -> id
        hasComments  = hasAnyCommentsBelow ltype
    runFilteredAlternative $ do
      addAlternativeCond (not hasComments) $
        docSeq
          [ appSep $ docForceSingleline typeDoc1
          , appSep $ docLitS "->"
          , docForceSingleline typeDoc2
          ]
      addAlternative $
        docPar
          (docNodeAnnKW ltype Nothing typeDoc1)
          (docCols
            ColTyOpPrefix
            [ docWrapNodeAfter ltype $ appSep $ docLitS "->"
            , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc2
            ])
  HsParTy _ typ1 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    runFilteredAlternative $ do
      addAlternative $
        docSeq
        [ docWrapNodeAfter ltype docParenL
        , docForceSingleline typeDoc1
        , docParenR
        ]
      addAlternative $
        docPar
        (docCols
          ColTyOpPrefix
          [ docWrapNodeAfter ltype $ docParenLSep
          , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
          ])
        docParenR
  HsAppTy _ typ1@(L _ HsAppTy{}) typ2 -> do
    let
      gather
        :: [LHsType GhcPs] -> LHsType GhcPs -> (LHsType GhcPs, [LHsType GhcPs])
      gather list = \case
        L _ (HsAppTy _ ty1 ty2) -> gather (ty2 : list) ty1
        final -> (final, list)
    let (typHead, typRest) = gather [typ2] typ1
    docHead <- docSharedWrapper layoutType typHead
    docRest <- docSharedWrapper layoutType `mapM` typRest
    runFilteredAlternative $ do
      addAlternative $
        docSeq
          $ docForceSingleline docHead
          : (docRest >>= \d -> [docSeparator, docForceSingleline d])
      addAlternative $
        docPar docHead (docLines $ docEnsureIndent BrIndentRegular <$> docRest)
  HsAppTy _ typ1 typ2 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc2 <- docSharedWrapper layoutType typ2
    runFilteredAlternative $ do
      addAlternative $
        docSeq
          [docForceSingleline typeDoc1, docSeparator, docForceSingleline typeDoc2]
      addAlternative $
        docPar typeDoc1 (docEnsureIndent BrIndentRegular typeDoc2)
  HsListTy _ typ1 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    runFilteredAlternative $ do
      addAlternative $
        docSeq
        [ docWrapNodeAfter ltype $ docLitS "["
        , docForceSingleline typeDoc1
        , docLitS "]"
        ]
      addAlternative $
        docPar
          (docCols
            ColTyOpPrefix
            [ docWrapNodeAfter ltype $ docLitS "[ "
            , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (docLitS "]")
  HsTupleTy _ tupleSort typs -> case tupleSort of
    HsUnboxedTuple           -> unboxed
    HsBoxedOrConstraintTuple -> simple
   where
    unboxed = if null typs
      then error "brittany internal error: unboxed unit"
      else unboxedL
    simple = if null typs then unitL else simpleL
    unitL  = docLitS "()"
    simpleL = do
      docs <- docSharedWrapper layoutType `mapM` typs
      let lines =
            List.tail docs
              <&> \d -> docAddBaseY (BrIndentSpecial 2)
                    $ docCols ColTyOpPrefix [docCommaSep, d]
          commaDocs = List.intersperse docCommaSep (docForceSingleline <$> docs)
      runFilteredAlternative $ do
        addAlternative $
          docSeq
            $ [docParenL]
            ++ docWrapNodeAfter ltype commaDocs
            ++ [docParenR]
        addAlternative $ do
          let line1 = docCols ColTyOpPrefix [docParenLSep, head docs]
          docPar
            (docAddBaseY (BrIndentSpecial 2) line1)
            (docLines $ docWrapNodeAfter ltype lines ++ [docParenR])
    unboxedL = do
      docs <- docSharedWrapper layoutType `mapM` typs
      let start = docParenHashLSep
          end   = docParenHashRSep
      runFilteredAlternative $ do
        addAlternative $
          docSeq
            $ [start]
            ++ docWrapNodeAfter ltype (List.intersperse docCommaSep docs)
            ++ [end]
        addAlternative $ do
          let line1 = docCols ColTyOpPrefix [start, head docs]
              lines =
                List.tail docs
                  <&> \d -> docAddBaseY (BrIndentSpecial 2)
                        $ docCols ColTyOpPrefix [docCommaSep, d]
          docPar
            (docAddBaseY (BrIndentSpecial 2) line1)
            (docLines $ lines ++ [end])
  HsOpTy{} -> -- TODO
    briDocByExactInlineOnly "HsOpTy{}" ltype
  -- HsOpTy typ1 opName typ2 -> do
  --   -- TODO: these need some proper fixing. precedences don't add up.
  --   --       maybe the parser just returns some trivial right recursion
  --   --       parse result for any type level operators.
  --   --       need to check how things are handled on the expression level.
  --   let opStr = lrdrNameToText opName
  --   let opLen = Text.length opStr
  --   layouter1@(Layouter desc1 _ _) <- layoutType typ1
  --   layouter2@(Layouter desc2 _ _) <- layoutType typ2
  --   let line = do -- Maybe
  --         l1 <- _ldesc_line desc1
  --         l2 <- _ldesc_line desc2
  --         let len1 = _lColumns_min l1
  --         let len2 = _lColumns_min l2
  --         let len = 2 + opLen + len1 + len2
  --         return $ LayoutColumns
  --           { _lColumns_key = ColumnKeyUnique
  --           , _lColumns_lengths = [len]
  --           , _lColumns_min = len
  --           }
  --   let block = do -- Maybe
  --         rol1 <- descToBlockStart desc1
  --         (min2, max2) <- descToMinMax (1+opLen) desc2
  --         let (minR, maxR) = case descToBlockMinMax desc1 of
  --               Nothing -> (min2, max2)
  --               Just (min1, max1) -> (max min1 min2, max max1 max2)
  --         return $ BlockDesc
  --           { _bdesc_blockStart = rol1
  --           , _bdesc_min = minR
  --           , _bdesc_max = maxR
  --           , _bdesc_opIndentFloatUp = Just (1+opLen)
  --           }
  --   return $ Layouter
  --     { _layouter_desc = LayoutDesc
  --       { _ldesc_line = line
  --       , _ldesc_block = block
  --       }
  --     , _layouter_func = \params -> do
  --         remaining <- getCurRemaining
  --         let allowSameLine = _params_sepLines params /= SepLineTypeOp
  --         case line of
  --           Just (LayoutColumns _ _ m) | m <= remaining && allowSameLine -> do
  --             applyLayouterRestore layouter1 defaultParams
  --             layoutWriteAppend $ Text.pack " " <> opStr <> Text.pack " "
  --             applyLayouterRestore layouter2 defaultParams
  --           _ -> do
  --             let upIndent   = maybe (1+opLen) (max (1+opLen)) $ _params_opIndent params
  --             let downIndent = maybe upIndent (max upIndent) $ _bdesc_opIndentFloatUp =<< _ldesc_block desc2
  --             layoutWithAddIndentN downIndent $ applyLayouterRestore layouter1 defaultParams
  --             layoutWriteNewline
  --             layoutWriteAppend $ opStr <> Text.pack " "
  --             layoutWriteEnsureBlockPlusN downIndent
  --             applyLayouterRestore layouter2 defaultParams
  --               { _params_sepLines = SepLineTypeOp
  --               , _params_opIndent = Just downIndent
  --               }
  --     , _layouter_ast = ltype
  --     }
  HsIParamTy _ (L _ (HsIPName ipName)) typ1 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    runFilteredAlternative $ do
      addAlternative $
        docSeq
          [ docWrapNodeAfter ltype $ docLitS $ "?" ++ showSDocUnsafe (ftext ipName) ++ "::"
          , docForceSingleline typeDoc1
          ]
      addAlternative $
        docPar
          (docLitS $ "?" ++ showSDocUnsafe (ftext ipName))
          (docCols
            ColTyOpPrefix
            [ docWrapNodeAfter ltype $ docLitS ":: "
            , docAddBaseY (BrIndentSpecial 2) typeDoc1
            ])
  -- TODO: test KindSig
  HsKindSig _ typ1 kind1 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    kindDoc1 <- docSharedWrapper layoutType kind1
    let hasParens = hasAnnKeyword ltype AnnOpenP
    runFilteredAlternative $ do
      addAlternative $
        if hasParens
        then docSeq
          [ docParenL
          , docForceSingleline typeDoc1
          , docSeparator
          , docLitS "::"
          , docSeparator
          , docForceSingleline kindDoc1
          , docParenR
          ]
        else docSeq
          [ docForceSingleline typeDoc1
          , docSeparator
          , docLitS "::"
          , docSeparator
          , docForceSingleline kindDoc1
          ]
      addAlternative $
        if hasParens
        then docLines
          [ docCols
            ColTyOpPrefix
            [ docWrapNodeAfter ltype $ docParenLSep
            , docAddBaseY (BrIndentSpecial 3) $ typeDoc1
            ]
          , docCols
            ColTyOpPrefix
            [ docWrapNodeAfter ltype $ docLitS ":: "
            , docAddBaseY (BrIndentSpecial 3) kindDoc1
            ]
          , (docParenR)
          ]
        else docPar
          typeDoc1
          (docCols
            ColTyOpPrefix
            [ docWrapNodeAfter ltype $ docLitS ":: "
            , docAddBaseY (BrIndentSpecial 3) kindDoc1
            ])
  HsBangTy{} -> -- TODO
    briDocByExactInlineOnly "HsBangTy{}" ltype
  -- HsBangTy bang typ1 -> do
  --   let bangStr = case bang of
  --         HsSrcBang _ unpackness strictness ->
  --           (++)
  --             (case unpackness of
  --               SrcUnpack   -> "{-# UNPACK -#} "
  --               SrcNoUnpack -> "{-# NOUNPACK -#} "
  --               NoSrcUnpack -> ""
  --             )
  --             (case strictness of
  --               SrcLazy     -> "~"
  --               SrcStrict   -> "!"
  --               NoSrcStrict -> ""
  --             )
  --   let bangLen = length bangStr
  --   layouter@(Layouter desc _ _) <- layoutType typ1
  --   let line = do -- Maybe
  --         l <- _ldesc_line desc
  --         let len = bangLen + _lColumns_min l
  --         return $ LayoutColumns
  --           { _lColumns_key = ColumnKeyUnique
  --           , _lColumns_lengths = [len]
  --           , _lColumns_min = len
  --           }
  --   let block = do -- Maybe
  --         rol <- descToBlockStart desc
  --         (minR,maxR) <- descToBlockMinMax desc
  --         return $ BlockDesc
  --           { _bdesc_blockStart = rol
  --           , _bdesc_min = minR
  --           , _bdesc_max = maxR
  --           , _bdesc_opIndentFloatUp = Nothing
  --           }
  --   return $ Layouter
  --     { _layouter_desc = LayoutDesc
  --       { _ldesc_line = line
  --       , _ldesc_block = block
  --       }
  --     , _layouter_func = \_params -> do
  --         remaining <- getCurRemaining
  --         case line of
  --           Just (LayoutColumns _ _ m) | m <= remaining -> do
  --             layoutWriteAppend $ Text.pack $ bangStr
  --             applyLayouterRestore layouter defaultParams
  --           _ -> do
  --             layoutWriteAppend $ Text.pack $ bangStr
  --             layoutWritePostCommentsRestore ltype
  --             applyLayouterRestore layouter defaultParams
  --     , _layouter_ast = ltype
  --     }
  HsSpliceTy{} -> -- TODO
    briDocByExactInlineOnly "HsSpliceTy{}" ltype
  HsDocTy{} -> -- TODO
    briDocByExactInlineOnly "HsDocTy{}" ltype
  HsRecTy{} -> -- TODO
    briDocByExactInlineOnly "HsRecTy{}" ltype
  HsExplicitListTy _ _ typs -> do
    typDocs <- docSharedWrapper layoutType `mapM` typs
    let hasComments     = hasAnyCommentsBelow ltype
        specialCommaSep = appSep $ docLitS " ,"
    runFilteredAlternative $ do
      addAlternative $
        docSeq
          $ [docLitS "'["]
          ++ List.intersperse specialCommaSep (docForceSingleline <$> typDocs)
          ++ [docLitS "]"]
      addAlternative $
        case splitFirstLast typDocs of
          FirstLastEmpty -> docSeq
            [ docLitS "'["
            , docNodeAnnKW ltype (Just AnnOpenS) $ docLitS "]"
            ]
          FirstLastSingleton e -> runFilteredAlternative $ do
            addAlternative $
              docSeq
                [ docLitS "'["
                , docNodeAnnKW ltype (Just AnnOpenS) $ docForceSingleline e
                , docLitS "]"
                ]
            addAlternative $
              docSetBaseY $ docLines
                [ docSeq
                  [ docLitS "'["
                  , docSeparator
                  , docSetBaseY $ docNodeAnnKW ltype (Just AnnOpenS) e
                  ]
                , docLitS " ]"
                ]
          FirstLast e1 ems eN -> runFilteredAlternative $ do
            addAlternativeCond (not hasComments)
              $ docSeq
              $ [docLitS "'["]
              ++ List.intersperse
                   specialCommaSep
                   (docForceSingleline <$> (e1 : ems ++ [docNodeAnnKW ltype (Just AnnOpenS) eN]))
              ++ [docLitS " ]"]
            addAlternative
              $ let
                  start = docCols ColList [appSep $ docLitS "'[", e1]
                  linesM = ems <&> \d -> docCols ColList [specialCommaSep, d]
                  lineN = docCols
                    ColList
                    [specialCommaSep, docNodeAnnKW ltype (Just AnnOpenS) eN]
                  end = docLitS " ]"
                in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [end]
  HsExplicitTupleTy{} -> -- TODO
    briDocByExactInlineOnly "HsExplicitTupleTy{}" ltype
  HsTyLit _ lit -> do
    let srctext = case lit of
          HsNumTy x _ -> x
          HsStrTy x _ -> x
          HsCharTy x _ -> x
    case srctext of
      SourceText x -> docLitS x
      NoSourceText ->
        error "overLitValBriDoc: literal without SourceText"
  HsWildCardTy _ -> docLitS "_"
  HsSumTy{} -> -- TODO
    briDocByExactInlineOnly "HsSumTy{}" ltype
  HsStarTy _ isUnicode -> do
    if isUnicode
      then docLitS "\x2605" -- Unicode star
      else docLitS "*"
  XHsType{} -> error "brittany internal error: XHsType"
  HsAppKindTy _ ty kind -> do
    t <- docSharedWrapper layoutType ty
    k <- docSharedWrapper layoutType kind
    runFilteredAlternative $ do
      addAlternative $
        docSeq
          [ docForceSingleline t
          , docSeparator
          , docLitS "@"
          , docForceSingleline k
          ]
      addAlternative $
        docPar t (docSeq [docLitS "@", k])

layoutTyVarBndrs
  :: [LHsTyVarBndr () GhcPs]
  -> ToBriDocM [(Text, Maybe (ToBriDocM BriDocNumbered))]
layoutTyVarBndrs = mapM $ \case
  (L _ (UserTyVar _ _ name)) -> return $ (lrdrNameToText name, Nothing)
  (L _ (KindedTyVar _ _ lrdrName kind)) -> do
    d <- docSharedWrapper layoutType kind
    return $ (lrdrNameToText lrdrName, Just $ d)

-- there is no specific reason this returns a list instead of a single
-- BriDoc node.
processTyVarBndrsSingleline
  :: [(Text, Maybe (ToBriDocM BriDocNumbered))] -> [ToBriDocM BriDocNumbered]
processTyVarBndrsSingleline bndrDocs = bndrDocs >>= \case
  (tname, Nothing) -> [docSeparator, docLit tname]
  (tname, Just doc) ->
    [ docSeparator
    , docLit $ Text.pack "(" <> tname <> Text.pack " :: "
    , docForceSingleline $ doc
    , docParenR
    ]

getBinders :: HsForAllTelescope GhcPs -> [LHsTyVarBndr () GhcPs]
getBinders x = case x of
  HsForAllVis _ b   -> b
  HsForAllInvis _ b -> fmap withoutSpecificity b

withoutSpecificity :: LHsTyVarBndr flag GhcPs -> LHsTyVarBndr () GhcPs
withoutSpecificity = fmap $ \case
  UserTyVar a _ c     -> UserTyVar a () c
  KindedTyVar a _ c d -> KindedTyVar a () c d
