----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PrettyInstances
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.PrettyInstances () where

import Data.IORef
import Prettyprinter.Combinators
import Prettyprinter.Generics
import Prettyprinter.Show

import GHC hiding (parseModule)

import GHC.Arr
import GHC.Core.Coercion.Axiom
import GHC.Core.TyCo.Rep
import GHC.Data.Bag
import GHC.Data.BooleanFormula
import GHC.Data.FastString
import GHC.Data.Strict qualified as Strict
import GHC.Types.Basic
import GHC.Types.FieldLabel
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.GREInfo
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.PkgQual
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set
import GHC.Types.Var
import GHC.Unit.Module.Warnings
import GHC.Unit.Types
import GHC.Utils.Outputable
import Language.Haskell.Syntax.Basic

deriving instance Generic (AmbiguousFieldOcc GhcPs)
deriving instance Generic (AmbiguousFieldOcc GhcRn)
deriving instance Generic (AnnDecl GhcPs)
deriving instance Generic (AnnDecl GhcRn)
deriving instance Generic (AnnProvenance GhcPs)
deriving instance Generic (AnnProvenance GhcRn)
deriving instance Generic (ApplicativeArg GhcPs)
deriving instance Generic (ApplicativeArg GhcRn)
deriving instance Generic (ArithSeqInfo GhcPs)
deriving instance Generic (ArithSeqInfo GhcRn)
deriving instance Generic (BooleanFormula a)
deriving instance Generic (Branches a)
deriving instance Generic (ClsInstDecl GhcPs)
deriving instance Generic (ClsInstDecl GhcRn)
deriving instance Generic (CoAxiom a)
deriving instance Generic (ConDecl GhcPs)
deriving instance Generic (ConDecl GhcRn)
deriving instance Generic (ConDeclField GhcPs)
deriving instance Generic (ConDeclField GhcRn)
deriving instance Generic (DataDefnCons a)
deriving instance Generic (DataFamInstDecl GhcPs)
deriving instance Generic (DataFamInstDecl GhcRn)
deriving instance Generic (DefaultDecl GhcPs)
deriving instance Generic (DefaultDecl GhcRn)
deriving instance Generic (Definite UnitId)
deriving instance Generic (DerivClauseTys GhcPs)
deriving instance Generic (DerivClauseTys GhcRn)
deriving instance Generic (DerivDecl GhcPs)
deriving instance Generic (DerivDecl GhcRn)
deriving instance Generic (DerivStrategy GhcPs)
deriving instance Generic (DerivStrategy GhcRn)
deriving instance Generic (DocDecl GhcPs)
deriving instance Generic (DocDecl GhcRn)
deriving instance Generic (DotFieldOcc GhcPs)
deriving instance Generic (DotFieldOcc GhcRn)
deriving instance Generic (EpAnn ann)
deriving instance Generic (FamEqn GhcPs a)
deriving instance Generic (FamEqn GhcRn a)
deriving instance Generic (FamilyDecl GhcPs)
deriving instance Generic (FamilyDecl GhcRn)
deriving instance Generic (FamilyInfo GhcPs)
deriving instance Generic (FamilyInfo GhcRn)
deriving instance Generic (FamilyResultSig GhcPs)
deriving instance Generic (FamilyResultSig GhcRn)
deriving instance Generic (FieldLabelStrings GhcPs)
deriving instance Generic (FieldLabelStrings GhcRn)
deriving instance Generic (FieldOcc GhcPs)
deriving instance Generic (FieldOcc GhcRn)
deriving instance Generic (FixitySig GhcPs)
deriving instance Generic (FixitySig GhcRn)
deriving instance Generic (ForeignDecl GhcPs)
deriving instance Generic (ForeignDecl GhcRn)
deriving instance Generic (ForeignExport GhcPs)
deriving instance Generic (ForeignExport GhcRn)
deriving instance Generic (ForeignImport GhcPs)
deriving instance Generic (ForeignImport GhcRn)
deriving instance Generic (FunDep GhcPs)
deriving instance Generic (FunDep GhcRn)
deriving instance Generic (GRHS GhcPs a)
deriving instance Generic (GRHS GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving instance Generic (GRHS GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving instance Generic (GRHSs GhcPs a)
deriving instance Generic (GRHSs GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving instance Generic (GRHSs GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving instance Generic (GenInstantiatedUnit UnitId)
deriving instance Generic (GenLocated a b)
deriving instance Generic (GenModule Unit)
deriving instance Generic (GenUnit UnitId)
deriving instance Generic (GlobalRdrEltX GREInfo)
deriving instance Generic (HsArg GhcPs b c)
deriving instance Generic (HsArg GhcRn b c)
deriving instance Generic (HsArrow GhcPs)
deriving instance Generic (HsArrow GhcRn)
deriving instance Generic (HsBindLR GhcPs GhcPs)
deriving instance Generic (HsBindLR GhcRn GhcRn)
deriving instance Generic (HsBndrVis GhcPs)
deriving instance Generic (HsBndrVis GhcRn)
deriving instance Generic (HsCmd GhcPs)
deriving instance Generic (HsCmd GhcRn)
deriving instance Generic (HsCmdTop GhcPs)
deriving instance Generic (HsCmdTop GhcRn)
deriving instance Generic (HsConDeclGADTDetails GhcPs)
deriving instance Generic (HsConDeclGADTDetails GhcRn)
deriving instance Generic (HsConDetails a b c)
deriving instance Generic (HsConPatTyArg GhcPs)
deriving instance Generic (HsConPatTyArg GhcRn)
deriving instance Generic (HsDataDefn GhcPs)
deriving instance Generic (HsDataDefn GhcRn)
deriving instance Generic (HsDecl GhcPs)
deriving instance Generic (HsDecl GhcRn)
deriving instance Generic (HsDerivingClause GhcPs)
deriving instance Generic (HsDerivingClause GhcRn)
deriving instance Generic (HsExpansion a b)
deriving instance Generic (HsExpr GhcPs)
deriving instance Generic (HsExpr GhcRn)
deriving instance Generic (HsFieldBind a b)
deriving instance Generic (HsForAllTelescope GhcPs)
deriving instance Generic (HsForAllTelescope GhcRn)
deriving instance Generic (HsGroup GhcPs)
deriving instance Generic (HsGroup GhcRn)
deriving instance Generic (HsIPBinds GhcPs)
deriving instance Generic (HsIPBinds GhcRn)
deriving instance Generic (HsLinearArrowTokens GhcPs)
deriving instance Generic (HsLinearArrowTokens GhcRn)
deriving instance Generic (HsLit GhcPs)
deriving instance Generic (HsLit GhcRn)
deriving instance Generic (HsLocalBindsLR GhcPs GhcPs)
deriving instance Generic (HsLocalBindsLR GhcRn GhcRn)
deriving instance Generic (HsMatchContext GhcPs)
deriving instance Generic (HsMatchContext GhcRn)
deriving instance Generic (HsMatchContext GhcTc)
deriving instance Generic (HsModule GhcPs)
deriving instance Generic (HsOuterTyVarBndrs a GhcPs)
deriving instance Generic (HsOuterTyVarBndrs a GhcRn)
deriving instance Generic (HsOverLit GhcPs)
deriving instance Generic (HsOverLit GhcRn)
deriving instance Generic (HsPatExpansion a b)
deriving instance Generic (HsPatSigType GhcPs)
deriving instance Generic (HsPatSigType GhcRn)
deriving instance Generic (HsPatSynDir GhcPs)
deriving instance Generic (HsPatSynDir GhcRn)
deriving instance Generic (HsPragE GhcPs)
deriving instance Generic (HsPragE GhcRn)
deriving instance Generic (HsQuote GhcPs)
deriving instance Generic (HsQuote GhcRn)
deriving instance Generic (HsRecFields GhcPs a)
deriving instance Generic (HsRecFields GhcRn a)
deriving instance Generic (HsRecUpdParent GhcRn)
deriving instance Generic (HsScaled GhcPs a)
deriving instance Generic (HsScaled GhcRn a)
deriving instance Generic (HsSigType GhcPs)
deriving instance Generic (HsSigType GhcRn)
deriving instance Generic (HsStmtContext GhcPs)
deriving instance Generic (HsStmtContext GhcRn)
deriving instance Generic (HsStmtContext GhcTc)
deriving instance Generic (HsToken a)
deriving instance Generic (HsTupArg GhcPs)
deriving instance Generic (HsTupArg GhcRn)
deriving instance Generic (HsTyLit GhcPs)
deriving instance Generic (HsTyLit GhcRn)
deriving instance Generic (HsTyVarBndr a GhcPs)
deriving instance Generic (HsTyVarBndr a GhcRn)
deriving instance Generic (HsType GhcPs)
deriving instance Generic (HsType GhcRn)
deriving instance Generic (HsUniToken a b)
deriving instance Generic (HsUntypedSplice GhcPs)
deriving instance Generic (HsUntypedSplice GhcRn)
deriving instance Generic (HsUntypedSpliceResult a)
deriving instance Generic (HsValBindsLR GhcPs GhcPs)
deriving instance Generic (HsValBindsLR GhcRn GhcRn)
deriving instance Generic (HsWildCardBndrs GhcPs a)
deriving instance Generic (HsWildCardBndrs GhcRn a)
deriving instance Generic (IE GhcPs)
deriving instance Generic (IEWrappedName GhcPs)
deriving instance Generic (IPBind GhcPs)
deriving instance Generic (IPBind GhcRn)
deriving instance Generic (ImportDecl GhcPs)
deriving instance Generic (InjectivityAnn GhcPs)
deriving instance Generic (InjectivityAnn GhcRn)
deriving instance Generic (InstDecl GhcPs)
deriving instance Generic (InstDecl GhcRn)
deriving instance Generic (LHsQTyVars GhcPs)
deriving instance Generic (LHsQTyVars GhcRn)
deriving instance Generic (LHsRecUpdFields GhcPs)
deriving instance Generic (LHsRecUpdFields GhcRn)
deriving instance Generic (LayoutInfo GhcPs)
deriving instance Generic (LayoutInfo GhcRn)
deriving instance Generic (Match GhcPs a)
deriving instance Generic (Match GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving instance Generic (Match GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving instance Generic (MatchGroup GhcPs a)
deriving instance Generic (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving instance Generic (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving instance Generic (NHsValBindsLR GhcPs)
deriving instance Generic (NHsValBindsLR GhcRn)
deriving instance Generic (ParStmtBlock GhcPs GhcPs)
deriving instance Generic (ParStmtBlock GhcRn GhcRn)
deriving instance Generic (Pat GhcPs)
deriving instance Generic (Pat GhcRn)
deriving instance Generic (PatSynBind GhcPs GhcPs)
deriving instance Generic (PatSynBind GhcRn GhcRn)
deriving instance Generic (RecordPatSynField GhcPs)
deriving instance Generic (RecordPatSynField GhcRn)
deriving instance Generic (RoleAnnotDecl GhcPs)
deriving instance Generic (RoleAnnotDecl GhcRn)
deriving instance Generic (RuleBndr GhcPs)
deriving instance Generic (RuleBndr GhcRn)
deriving instance Generic (RuleDecl GhcPs)
deriving instance Generic (RuleDecl GhcRn)
deriving instance Generic (RuleDecls GhcPs)
deriving instance Generic (RuleDecls GhcRn)
deriving instance Generic (Sig GhcPs)
deriving instance Generic (Sig GhcRn)
deriving instance Generic (SpliceDecl GhcPs)
deriving instance Generic (SpliceDecl GhcRn)
deriving instance Generic (SrcSpanAnn' a)
deriving instance Generic (StandaloneKindSig GhcPs)
deriving instance Generic (StandaloneKindSig GhcRn)
deriving instance Generic (StmtLR GhcPs GhcPs a)
deriving instance Generic (StmtLR GhcRn GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving instance Generic (StmtLR GhcRn GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving instance Generic (Strict.Maybe a)
deriving instance Generic (TyClDecl GhcPs)
deriving instance Generic (TyClDecl GhcRn)
deriving instance Generic (TyClGroup GhcPs)
deriving instance Generic (TyClGroup GhcRn)
deriving instance Generic (TyConFlavour Name)
deriving instance Generic (TyFamInstDecl GhcPs)
deriving instance Generic (TyFamInstDecl GhcRn)
deriving instance Generic (VarBndr a b)
deriving instance Generic (WarnDecl GhcPs)
deriving instance Generic (WarnDecl GhcRn)
deriving instance Generic (WarnDecls GhcPs)
deriving instance Generic (WarnDecls GhcRn)
deriving instance Generic (WithHsDocIdentifiers HsDocString GhcPs)
deriving instance Generic (WithHsDocIdentifiers StringLiteral GhcPs)
deriving instance Generic (WithHsDocIdentifiers a GhcRn)
deriving instance Generic Activation
deriving instance Generic AddEpAnn
deriving instance Generic Anchor
deriving instance Generic AnchorOperation
deriving instance Generic AnnContext
deriving instance Generic AnnExplicitSum
deriving instance Generic AnnFieldLabel
deriving instance Generic AnnKeywordId
deriving instance Generic AnnList
deriving instance Generic AnnListItem
deriving instance Generic AnnParen
deriving instance Generic AnnPragma
deriving instance Generic AnnProjection
deriving instance Generic AnnSig
deriving instance Generic AnnSortKey
deriving instance Generic AnnsIf
deriving instance Generic AnnsModule
deriving instance Generic Boxity
deriving instance Generic BufPos
deriving instance Generic BufSpan
deriving instance Generic CCallConv
deriving instance Generic CCallTarget
deriving instance Generic CExportSpec
deriving instance Generic CImportSpec
deriving instance Generic CType
deriving instance Generic CoAxBranch
deriving instance Generic CoSel
deriving instance Generic Coercion
deriving instance Generic CoercionHole
deriving instance Generic ConInfo
deriving instance Generic ConLikeName
deriving instance Generic DataConCantHappen
deriving instance Generic DataDeclRn
deriving instance Generic DeltaPos
deriving instance Generic DoPmc
deriving instance Generic DuplicateRecordFields
deriving instance Generic EpAnnComments
deriving instance Generic EpAnnHsCase
deriving instance Generic EpAnnImportDecl
deriving instance Generic EpAnnSumPat
deriving instance Generic EpAnnUnboundVar
deriving instance Generic EpaComment
deriving instance Generic EpaCommentTok
deriving instance Generic EpaLocation
deriving instance Generic FieldLabel
deriving instance Generic FieldLabelString
deriving instance Generic FieldSelectors
deriving instance Generic Fixity
deriving instance Generic FixityDirection
deriving instance Generic ForAllTyFlag
deriving instance Generic FractionalExponentBase
deriving instance Generic FractionalLit
deriving instance Generic FunSel
deriving instance Generic FunTyFlag
deriving instance Generic GREInfo
deriving instance Generic GrhsAnn
deriving instance Generic Header
deriving instance Generic HsArrAppType
deriving instance Generic HsArrowMatchContext
deriving instance Generic HsDoFlavour
deriving instance Generic HsDocString
deriving instance Generic HsDocStringChunk
deriving instance Generic HsDocStringDecorator
deriving instance Generic HsIPName
deriving instance Generic HsPSRn
deriving instance Generic HsRuleAnn
deriving instance Generic HsRuleRn
deriving instance Generic HsSrcBang
deriving instance Generic HsTupleSort
deriving instance Generic IEWildcard
deriving instance Generic IdSig
deriving instance Generic ImpDeclSpec
deriving instance Generic ImpItemSpec
deriving instance Generic ImportDeclQualifiedStyle
deriving instance Generic ImportListInterpretation
deriving instance Generic ImportSpec
deriving instance Generic InWarningCategory
deriving instance Generic InlinePragma
deriving instance Generic InlineSpec
deriving instance Generic IntegralLit
deriving instance Generic IsBootInterface
deriving instance Generic IsUnicodeSyntax
deriving instance Generic LamCaseVariant
deriving instance Generic LeftOrRight
deriving instance Generic LexicalFixity
deriving instance Generic MCoercion
deriving instance Generic NameAdornment
deriving instance Generic NameAnn
deriving instance Generic NoEpAnns
deriving instance Generic NoExtField
deriving instance Generic Origin
deriving instance Generic OverLitRn
deriving instance Generic OverLitVal
deriving instance Generic OverlapMode
deriving instance Generic ParenType
deriving instance Generic Parent
deriving instance Generic PendingRnSplice
deriving instance Generic PromotionFlag
deriving instance Generic RawPkgQual
deriving instance Generic RdrName
deriving instance Generic RecFieldInfo
deriving instance Generic RecFieldsDotDot
deriving instance Generic RecFlag
deriving instance Generic Role
deriving instance Generic RuleMatchInfo
deriving instance Generic Safety
deriving instance Generic SourceText
deriving instance Generic Specificity
deriving instance Generic SpliceDecoration
deriving instance Generic SrcStrictness
deriving instance Generic SrcUnpackedness
deriving instance Generic StringLiteral
deriving instance Generic SyntaxExprRn
deriving instance Generic ThModFinalizers
deriving instance Generic TokenLocation
deriving instance Generic TopLevelFlag
deriving instance Generic TrailingAnn
deriving instance Generic TransForm
deriving instance Generic TyLit
deriving instance Generic Type
deriving instance Generic TypeOrData
deriving instance Generic UnitId
deriving instance Generic UnivCoProvenance
deriving instance Generic UntypedSpliceFlavour
deriving instance Generic WarningCategory
deriving instance Generic XBindStmtRn
deriving instance Generic XImportDeclPass
deriving instance Generic XModulePs
deriving instance Generic XViaStrategyPs

instance Pretty Name where
  pretty = pretty . nameStableString

instance Pretty OccName where
  pretty = pretty . occNameString

instance Pretty SrcSpan where
  pretty = \case
    RealSrcSpan x _ -> ppShow x
    UnhelpfulSpan _ -> "<unhelpful span>"

-- instance Data a => PPGenericOverride a where
--   ppGenericOverride = compositeMetaDoc . ppData

-- instance {-# OVERLAPPABLE #-} Data a => Pretty a where
--   pretty = ppData

instance Pretty a => Pretty (SrcSpanAnn' a) where
  pretty = ppGeneric
  -- pretty x = ppDictHeader "SrcSpanAnn'"
  --   [ "ann" --> ann x
  --   ]

instance Pretty Anchor where
  -- pretty = ppGeneric
  pretty x = ppDictHeader "Anchor"
    [ "anchor_op" --> anchor_op x
    ]

instance Pretty a => Pretty (UniqDSet a) where
  pretty = pretty . uniqDSetToList

instance Pretty a => Pretty (UniqSet a) where
  pretty = pretty . nonDetEltsUniqSet

instance Pretty Var where
  pretty = pretty . GHC.Types.Var.varName

instance (Pretty a, Pretty b) => Pretty (Array a b) where
  pretty = pretty . elems

instance Pretty (IORef a) where
  pretty _ = "IORef"


newtype PPOutputable a = PPOutputable { unPPOutputable :: a }

instance Outputable a => Pretty (PPOutputable a) where
  pretty = pretty . showSDocUnsafe . ppr . unPPOutputable

instance Pretty a => Pretty (Bag a) where
  pretty = pretty . bagToList

instance Pretty ThModFinalizers where
  pretty _ = "ThModFinalizers{}"

deriving via PPOutputable TyCon instance Pretty TyCon
deriving via PPOutputable CoAxiomRule instance Pretty CoAxiomRule


deriving via PPShow RealSrcSpan instance Pretty RealSrcSpan
deriving via PPShow ModuleName instance Pretty ModuleName
deriving via PPShow FastString instance Pretty FastString
deriving via PPShow Unique instance Pretty Unique

deriving via PPGeneric (AmbiguousFieldOcc GhcPs) instance Pretty (AmbiguousFieldOcc GhcPs)
deriving via PPGeneric (AmbiguousFieldOcc GhcRn) instance Pretty (AmbiguousFieldOcc GhcRn)
deriving via PPGeneric (AnnDecl GhcPs) instance Pretty (AnnDecl GhcPs)
deriving via PPGeneric (AnnDecl GhcRn) instance Pretty (AnnDecl GhcRn)
deriving via PPGeneric (AnnProvenance GhcPs) instance Pretty (AnnProvenance GhcPs)
deriving via PPGeneric (AnnProvenance GhcRn) instance Pretty (AnnProvenance GhcRn)
deriving via PPGeneric (ApplicativeArg GhcPs) instance Pretty (ApplicativeArg GhcPs)
deriving via PPGeneric (ApplicativeArg GhcRn) instance Pretty (ApplicativeArg GhcRn)
deriving via PPGeneric (ArithSeqInfo GhcPs) instance Pretty (ArithSeqInfo GhcPs)
deriving via PPGeneric (ArithSeqInfo GhcRn) instance Pretty (ArithSeqInfo GhcRn)
deriving via PPGeneric (BooleanFormula a) instance Pretty a => Pretty (BooleanFormula a)
deriving via PPGeneric (Branches a) instance Pretty (Branches a)
deriving via PPGeneric (ClsInstDecl GhcPs) instance Pretty (ClsInstDecl GhcPs)
deriving via PPGeneric (ClsInstDecl GhcRn) instance Pretty (ClsInstDecl GhcRn)
deriving via PPGeneric (CoAxiom a) instance Pretty (CoAxiom a)
deriving via PPGeneric (ConDecl GhcPs) instance Pretty (ConDecl GhcPs)
deriving via PPGeneric (ConDecl GhcRn) instance Pretty (ConDecl GhcRn)
deriving via PPGeneric (ConDeclField GhcPs) instance Pretty (ConDeclField GhcPs)
deriving via PPGeneric (ConDeclField GhcRn) instance Pretty (ConDeclField GhcRn)
deriving via PPGeneric (DataDefnCons a) instance Pretty a => Pretty (DataDefnCons a)
deriving via PPGeneric (DataFamInstDecl GhcPs) instance Pretty (DataFamInstDecl GhcPs)
deriving via PPGeneric (DataFamInstDecl GhcRn) instance Pretty (DataFamInstDecl GhcRn)
deriving via PPGeneric (DefaultDecl GhcPs) instance Pretty (DefaultDecl GhcPs)
deriving via PPGeneric (DefaultDecl GhcRn) instance Pretty (DefaultDecl GhcRn)
deriving via PPGeneric (Definite UnitId) instance Pretty (Definite UnitId)
deriving via PPGeneric (DerivClauseTys GhcPs) instance Pretty (DerivClauseTys GhcPs)
deriving via PPGeneric (DerivClauseTys GhcRn) instance Pretty (DerivClauseTys GhcRn)
deriving via PPGeneric (DerivDecl GhcPs) instance Pretty (DerivDecl GhcPs)
deriving via PPGeneric (DerivDecl GhcRn) instance Pretty (DerivDecl GhcRn)
deriving via PPGeneric (DerivStrategy GhcPs) instance Pretty (DerivStrategy GhcPs)
deriving via PPGeneric (DerivStrategy GhcRn) instance Pretty (DerivStrategy GhcRn)
deriving via PPGeneric (DocDecl GhcPs) instance Pretty (DocDecl GhcPs)
deriving via PPGeneric (DocDecl GhcRn) instance Pretty (DocDecl GhcRn)
deriving via PPGeneric (DotFieldOcc GhcPs) instance Pretty (DotFieldOcc GhcPs)
deriving via PPGeneric (DotFieldOcc GhcRn) instance Pretty (DotFieldOcc GhcRn)
deriving via PPGeneric (Either a b) instance (Pretty a, Pretty b) => Pretty (Either a b)
deriving via PPGeneric (EpAnn ann) instance Pretty ann => Pretty (EpAnn ann)
deriving via PPGeneric (FamEqn GhcPs a) instance Pretty a => Pretty (FamEqn GhcPs a)
deriving via PPGeneric (FamEqn GhcRn a) instance Pretty a => Pretty (FamEqn GhcRn a)
deriving via PPGeneric (FamilyDecl GhcPs) instance Pretty (FamilyDecl GhcPs)
deriving via PPGeneric (FamilyDecl GhcRn) instance Pretty (FamilyDecl GhcRn)
deriving via PPGeneric (FamilyInfo GhcPs) instance Pretty (FamilyInfo GhcPs)
deriving via PPGeneric (FamilyInfo GhcRn) instance Pretty (FamilyInfo GhcRn)
deriving via PPGeneric (FamilyResultSig GhcPs) instance Pretty (FamilyResultSig GhcPs)
deriving via PPGeneric (FamilyResultSig GhcRn) instance Pretty (FamilyResultSig GhcRn)
deriving via PPGeneric (FieldLabelStrings GhcPs) instance Pretty (FieldLabelStrings GhcPs)
deriving via PPGeneric (FieldLabelStrings GhcRn) instance Pretty (FieldLabelStrings GhcRn)
deriving via PPGeneric (FieldOcc GhcPs) instance Pretty (FieldOcc GhcPs)
deriving via PPGeneric (FieldOcc GhcRn) instance Pretty (FieldOcc GhcRn)
deriving via PPGeneric (FixitySig GhcPs) instance Pretty (FixitySig GhcPs)
deriving via PPGeneric (FixitySig GhcRn) instance Pretty (FixitySig GhcRn)
deriving via PPGeneric (ForeignDecl GhcPs) instance Pretty (ForeignDecl GhcPs)
deriving via PPGeneric (ForeignDecl GhcRn) instance Pretty (ForeignDecl GhcRn)
deriving via PPGeneric (ForeignExport GhcPs) instance Pretty (ForeignExport GhcPs)
deriving via PPGeneric (ForeignExport GhcRn) instance Pretty (ForeignExport GhcRn)
deriving via PPGeneric (ForeignImport GhcPs) instance Pretty (ForeignImport GhcPs)
deriving via PPGeneric (ForeignImport GhcRn) instance Pretty (ForeignImport GhcRn)
deriving via PPGeneric (FunDep GhcPs) instance Pretty (FunDep GhcPs)
deriving via PPGeneric (FunDep GhcRn) instance Pretty (FunDep GhcRn)
deriving via PPGeneric (GRHS GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving via PPGeneric (GRHS GhcPs (LocatedA (HsExpr GhcPs))) instance Pretty (GRHS GhcPs (LocatedA (HsExpr GhcPs)))
deriving via PPGeneric (GRHS GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn))) instance Pretty (GRHS GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving via PPGeneric (GRHS GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn))) instance Pretty (GRHS GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving via PPGeneric (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving via PPGeneric (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) instance Pretty (GRHSs GhcPs (LocatedA (HsExpr GhcPs)))
deriving via PPGeneric (GRHSs GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn))) instance Pretty (GRHSs GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving via PPGeneric (GRHSs GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn))) instance Pretty (GRHSs GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving via PPGeneric (GenInstantiatedUnit UnitId) instance Pretty (GenInstantiatedUnit UnitId)
deriving via PPGeneric (GenLocated a b) instance (Pretty a, Pretty b) => Pretty (GenLocated a b)
deriving via PPGeneric (GenModule Unit) instance Pretty (GenModule Unit)
deriving via PPGeneric (GenUnit UnitId) instance Pretty (GenUnit UnitId)
deriving via PPGeneric (GlobalRdrEltX GREInfo) instance Pretty (GlobalRdrEltX GREInfo)
deriving via PPGeneric (HsArg GhcPs b c) instance (Pretty b, Pretty c) => Pretty (HsArg GhcPs b c)
deriving via PPGeneric (HsArg GhcRn b c) instance (Pretty b, Pretty c) => Pretty (HsArg GhcRn b c)
deriving via PPGeneric (HsArrow GhcPs) instance Pretty (HsArrow GhcPs)
deriving via PPGeneric (HsArrow GhcRn) instance Pretty (HsArrow GhcRn)
deriving via PPGeneric (HsBindLR GhcPs GhcPs) instance Pretty (HsBindLR GhcPs GhcPs)
deriving via PPGeneric (HsBindLR GhcRn GhcRn) instance Pretty (HsBindLR GhcRn GhcRn)
deriving via PPGeneric (HsBndrVis GhcPs) instance Pretty (HsBndrVis GhcPs)
deriving via PPGeneric (HsBndrVis GhcRn) instance Pretty (HsBndrVis GhcRn)
deriving via PPGeneric (HsCmd GhcPs) instance Pretty (HsCmd GhcPs)
deriving via PPGeneric (HsCmd GhcRn) instance Pretty (HsCmd GhcRn)
deriving via PPGeneric (HsCmdTop GhcPs) instance Pretty (HsCmdTop GhcPs)
deriving via PPGeneric (HsCmdTop GhcRn) instance Pretty (HsCmdTop GhcRn)
deriving via PPGeneric (HsConDeclGADTDetails GhcPs) instance Pretty (HsConDeclGADTDetails GhcPs)
deriving via PPGeneric (HsConDeclGADTDetails GhcRn) instance Pretty (HsConDeclGADTDetails GhcRn)
deriving via PPGeneric (HsConDetails a b c) instance (Pretty a, Pretty b, Pretty c) => Pretty (HsConDetails a b c)
deriving via PPGeneric (HsConPatTyArg GhcPs) instance Pretty (HsConPatTyArg GhcPs)
deriving via PPGeneric (HsConPatTyArg GhcRn) instance Pretty (HsConPatTyArg GhcRn)
deriving via PPGeneric (HsDataDefn GhcPs) instance Pretty (HsDataDefn GhcPs)
deriving via PPGeneric (HsDataDefn GhcRn) instance Pretty (HsDataDefn GhcRn)
deriving via PPGeneric (HsDecl GhcPs) instance Pretty (HsDecl GhcPs)
deriving via PPGeneric (HsDecl GhcRn) instance Pretty (HsDecl GhcRn)
deriving via PPGeneric (HsDerivingClause GhcPs) instance Pretty (HsDerivingClause GhcPs)
deriving via PPGeneric (HsDerivingClause GhcRn) instance Pretty (HsDerivingClause GhcRn)
deriving via PPGeneric (HsExpansion a b) instance (Pretty a, Pretty b) => Pretty (HsExpansion a b)
deriving via PPGeneric (HsExpr GhcPs) instance Pretty (HsExpr GhcPs)
deriving via PPGeneric (HsExpr GhcRn) instance Pretty (HsExpr GhcRn)
deriving via PPGeneric (HsFieldBind a b) instance (Pretty a, Pretty b) => Pretty (HsFieldBind a b)
deriving via PPGeneric (HsForAllTelescope GhcPs) instance Pretty (HsForAllTelescope GhcPs)
deriving via PPGeneric (HsForAllTelescope GhcRn) instance Pretty (HsForAllTelescope GhcRn)
deriving via PPGeneric (HsGroup GhcPs) instance Pretty (HsGroup GhcPs)
deriving via PPGeneric (HsGroup GhcRn) instance Pretty (HsGroup GhcRn)
deriving via PPGeneric (HsIPBinds GhcPs) instance Pretty (HsIPBinds GhcPs)
deriving via PPGeneric (HsIPBinds GhcRn) instance Pretty (HsIPBinds GhcRn)
deriving via PPGeneric (HsLinearArrowTokens GhcPs) instance Pretty (HsLinearArrowTokens GhcPs)
deriving via PPGeneric (HsLinearArrowTokens GhcRn) instance Pretty (HsLinearArrowTokens GhcRn)
deriving via PPGeneric (HsLit GhcPs) instance Pretty (HsLit GhcPs)
deriving via PPGeneric (HsLit GhcRn) instance Pretty (HsLit GhcRn)
deriving via PPGeneric (HsLocalBindsLR GhcPs GhcPs) instance Pretty (HsLocalBindsLR GhcPs GhcPs)
deriving via PPGeneric (HsLocalBindsLR GhcRn GhcRn) instance Pretty (HsLocalBindsLR GhcRn GhcRn)
deriving via PPGeneric (HsMatchContext GhcPs) instance Pretty (HsMatchContext GhcPs)
deriving via PPGeneric (HsMatchContext GhcRn) instance Pretty (HsMatchContext GhcRn)
deriving via PPGeneric (HsMatchContext GhcTc) instance Pretty (HsMatchContext GhcTc)
deriving via PPGeneric (HsModule GhcPs) instance Pretty (HsModule GhcPs)
deriving via PPGeneric (HsOuterTyVarBndrs a GhcPs) instance Pretty a => Pretty (HsOuterTyVarBndrs a GhcPs)
deriving via PPGeneric (HsOuterTyVarBndrs a GhcRn) instance Pretty a => Pretty (HsOuterTyVarBndrs a GhcRn)
deriving via PPGeneric (HsOverLit GhcPs) instance Pretty (HsOverLit GhcPs)
deriving via PPGeneric (HsOverLit GhcRn) instance Pretty (HsOverLit GhcRn)
deriving via PPGeneric (HsPatExpansion a b) instance (Pretty a, Pretty b) => Pretty (HsPatExpansion a b)
deriving via PPGeneric (HsPatSigType GhcPs) instance Pretty (HsPatSigType GhcPs)
deriving via PPGeneric (HsPatSigType GhcRn) instance Pretty (HsPatSigType GhcRn)
deriving via PPGeneric (HsPatSynDir GhcPs) instance Pretty (HsPatSynDir GhcPs)
deriving via PPGeneric (HsPatSynDir GhcRn) instance Pretty (HsPatSynDir GhcRn)
deriving via PPGeneric (HsPragE GhcPs) instance Pretty (HsPragE GhcPs)
deriving via PPGeneric (HsPragE GhcRn) instance Pretty (HsPragE GhcRn)
deriving via PPGeneric (HsQuote GhcPs) instance Pretty (HsQuote GhcPs)
deriving via PPGeneric (HsQuote GhcRn) instance Pretty (HsQuote GhcRn)
deriving via PPGeneric (HsRecFields GhcPs a) instance Pretty a => Pretty (HsRecFields GhcPs a)
deriving via PPGeneric (HsRecFields GhcRn a) instance Pretty a => Pretty (HsRecFields GhcRn a)
deriving via PPGeneric (HsRecUpdParent GhcRn) instance Pretty (HsRecUpdParent GhcRn)
deriving via PPGeneric (HsScaled GhcPs a) instance Pretty a => Pretty (HsScaled GhcPs a)
deriving via PPGeneric (HsScaled GhcRn a) instance Pretty a => Pretty (HsScaled GhcRn a)
deriving via PPGeneric (HsSigType GhcPs) instance Pretty (HsSigType GhcPs)
deriving via PPGeneric (HsSigType GhcRn) instance Pretty (HsSigType GhcRn)
deriving via PPGeneric (HsStmtContext GhcPs) instance Pretty (HsStmtContext GhcPs)
deriving via PPGeneric (HsStmtContext GhcRn) instance Pretty (HsStmtContext GhcRn)
deriving via PPGeneric (HsStmtContext GhcTc) instance Pretty (HsStmtContext GhcTc)
deriving via PPGeneric (HsToken a) instance Pretty (HsToken a)
deriving via PPGeneric (HsTupArg GhcPs) instance Pretty (HsTupArg GhcPs)
deriving via PPGeneric (HsTupArg GhcRn) instance Pretty (HsTupArg GhcRn)
deriving via PPGeneric (HsTyLit GhcPs) instance Pretty (HsTyLit GhcPs)
deriving via PPGeneric (HsTyLit GhcRn) instance Pretty (HsTyLit GhcRn)
deriving via PPGeneric (HsTyVarBndr a GhcPs) instance Pretty a => Pretty (HsTyVarBndr a GhcPs)
deriving via PPGeneric (HsTyVarBndr a GhcRn) instance Pretty a => Pretty (HsTyVarBndr a GhcRn)
deriving via PPGeneric (HsType GhcPs) instance Pretty (HsType GhcPs)
deriving via PPGeneric (HsType GhcRn) instance Pretty (HsType GhcRn)
deriving via PPGeneric (HsUniToken a b) instance Pretty (HsUniToken a b)
deriving via PPGeneric (HsUntypedSplice GhcPs) instance Pretty (HsUntypedSplice GhcPs)
deriving via PPGeneric (HsUntypedSplice GhcRn) instance Pretty (HsUntypedSplice GhcRn)
deriving via PPGeneric (HsUntypedSpliceResult a) instance Pretty a => Pretty (HsUntypedSpliceResult a)
deriving via PPGeneric (HsValBindsLR GhcPs GhcPs) instance Pretty (HsValBindsLR GhcPs GhcPs)
deriving via PPGeneric (HsValBindsLR GhcRn GhcRn) instance Pretty (HsValBindsLR GhcRn GhcRn)
deriving via PPGeneric (HsWildCardBndrs GhcPs a) instance Pretty a => Pretty (HsWildCardBndrs GhcPs a)
deriving via PPGeneric (HsWildCardBndrs GhcRn a) instance Pretty a => Pretty (HsWildCardBndrs GhcRn a)
deriving via PPGeneric (IE GhcPs) instance Pretty (IE GhcPs)
deriving via PPGeneric (IEWrappedName GhcPs) instance Pretty (IEWrappedName GhcPs)
deriving via PPGeneric (IPBind GhcPs) instance Pretty (IPBind GhcPs)
deriving via PPGeneric (IPBind GhcRn) instance Pretty (IPBind GhcRn)
deriving via PPGeneric (ImportDecl GhcPs) instance Pretty (ImportDecl GhcPs)
deriving via PPGeneric (InjectivityAnn GhcPs) instance Pretty (InjectivityAnn GhcPs)
deriving via PPGeneric (InjectivityAnn GhcRn) instance Pretty (InjectivityAnn GhcRn)
deriving via PPGeneric (InstDecl GhcPs) instance Pretty (InstDecl GhcPs)
deriving via PPGeneric (InstDecl GhcRn) instance Pretty (InstDecl GhcRn)
deriving via PPGeneric (LHsQTyVars GhcPs) instance Pretty (LHsQTyVars GhcPs)
deriving via PPGeneric (LHsQTyVars GhcRn) instance Pretty (LHsQTyVars GhcRn)
deriving via PPGeneric (LHsRecUpdFields GhcPs) instance Pretty (LHsRecUpdFields GhcPs)
deriving via PPGeneric (LHsRecUpdFields GhcRn) instance Pretty (LHsRecUpdFields GhcRn)
deriving via PPGeneric (LayoutInfo GhcPs) instance Pretty (LayoutInfo GhcPs)
deriving via PPGeneric (LayoutInfo GhcRn) instance Pretty (LayoutInfo GhcRn)
deriving via PPGeneric (Match GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving via PPGeneric (Match GhcPs (LocatedA (HsExpr GhcPs))) instance Pretty (Match GhcPs (LocatedA (HsExpr GhcPs)))
deriving via PPGeneric (Match GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn))) instance Pretty (Match GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving via PPGeneric (Match GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn))) instance Pretty (Match GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving via PPGeneric (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving via PPGeneric (MatchGroup GhcPs (LocatedA (HsExpr GhcPs))) instance Pretty (MatchGroup GhcPs (LocatedA (HsExpr GhcPs)))
deriving via PPGeneric (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn))) instance Pretty (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving via PPGeneric (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn))) instance Pretty (MatchGroup GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving via PPGeneric (NHsValBindsLR GhcPs) instance Pretty (NHsValBindsLR GhcPs)
deriving via PPGeneric (NHsValBindsLR GhcRn) instance Pretty (NHsValBindsLR GhcRn)
deriving via PPGeneric (ParStmtBlock GhcPs GhcPs) instance Pretty (ParStmtBlock GhcPs GhcPs)
deriving via PPGeneric (ParStmtBlock GhcRn GhcRn) instance Pretty (ParStmtBlock GhcRn GhcRn)
deriving via PPGeneric (Pat GhcPs) instance Pretty (Pat GhcPs)
deriving via PPGeneric (Pat GhcRn) instance Pretty (Pat GhcRn)
deriving via PPGeneric (PatSynBind GhcPs GhcPs) instance Pretty (PatSynBind GhcPs GhcPs)
deriving via PPGeneric (PatSynBind GhcRn GhcRn) instance Pretty (PatSynBind GhcRn GhcRn)
deriving via PPGeneric (RecordPatSynField GhcPs) instance Pretty (RecordPatSynField GhcPs)
deriving via PPGeneric (RecordPatSynField GhcRn) instance Pretty (RecordPatSynField GhcRn)
deriving via PPGeneric (RoleAnnotDecl GhcPs) instance Pretty (RoleAnnotDecl GhcPs)
deriving via PPGeneric (RoleAnnotDecl GhcRn) instance Pretty (RoleAnnotDecl GhcRn)
deriving via PPGeneric (RuleBndr GhcPs) instance Pretty (RuleBndr GhcPs)
deriving via PPGeneric (RuleBndr GhcRn) instance Pretty (RuleBndr GhcRn)
deriving via PPGeneric (RuleDecl GhcPs) instance Pretty (RuleDecl GhcPs)
deriving via PPGeneric (RuleDecl GhcRn) instance Pretty (RuleDecl GhcRn)
deriving via PPGeneric (RuleDecls GhcPs) instance Pretty (RuleDecls GhcPs)
deriving via PPGeneric (RuleDecls GhcRn) instance Pretty (RuleDecls GhcRn)
deriving via PPGeneric (Sig GhcPs) instance Pretty (Sig GhcPs)
deriving via PPGeneric (Sig GhcRn) instance Pretty (Sig GhcRn)
deriving via PPGeneric (SpliceDecl GhcPs) instance Pretty (SpliceDecl GhcPs)
deriving via PPGeneric (SpliceDecl GhcRn) instance Pretty (SpliceDecl GhcRn)
deriving via PPGeneric (StandaloneKindSig GhcPs) instance Pretty (StandaloneKindSig GhcPs)
deriving via PPGeneric (StandaloneKindSig GhcRn) instance Pretty (StandaloneKindSig GhcRn)
deriving via PPGeneric (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs)))
deriving via PPGeneric (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs))) instance Pretty (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))
deriving via PPGeneric (StmtLR GhcRn GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn))) instance Pretty (StmtLR GhcRn GhcRn (GenLocated SrcSpanAnnA (HsCmd GhcRn)))
deriving via PPGeneric (StmtLR GhcRn GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn))) instance Pretty (StmtLR GhcRn GhcRn (GenLocated SrcSpanAnnA (HsExpr GhcRn)))
deriving via PPGeneric (Strict.Maybe a) instance Pretty a => Pretty (Strict.Maybe a)
deriving via PPGeneric (TyClDecl GhcPs) instance Pretty (TyClDecl GhcPs)
deriving via PPGeneric (TyClDecl GhcRn) instance Pretty (TyClDecl GhcRn)
deriving via PPGeneric (TyClGroup GhcPs) instance Pretty (TyClGroup GhcPs)
deriving via PPGeneric (TyClGroup GhcRn) instance Pretty (TyClGroup GhcRn)
deriving via PPGeneric (TyConFlavour Name) instance Pretty (TyConFlavour Name)
deriving via PPGeneric (TyFamInstDecl GhcPs) instance Pretty (TyFamInstDecl GhcPs)
deriving via PPGeneric (TyFamInstDecl GhcRn) instance Pretty (TyFamInstDecl GhcRn)
deriving via PPGeneric (VarBndr a b) instance (Pretty a, Pretty b) => Pretty (VarBndr a b)
deriving via PPGeneric (WarnDecl GhcPs) instance Pretty (WarnDecl GhcPs)
deriving via PPGeneric (WarnDecl GhcRn) instance Pretty (WarnDecl GhcRn)
deriving via PPGeneric (WarnDecls GhcPs) instance Pretty (WarnDecls GhcPs)
deriving via PPGeneric (WarnDecls GhcRn) instance Pretty (WarnDecls GhcRn)
deriving via PPGeneric (WarningTxt GhcPs) instance Pretty (WarningTxt GhcPs)
deriving via PPGeneric (WarningTxt GhcRn) instance Pretty (WarningTxt GhcRn)
deriving via PPGeneric (WithHsDocIdentifiers HsDocString GhcPs) instance Pretty (WithHsDocIdentifiers HsDocString GhcPs)
deriving via PPGeneric (WithHsDocIdentifiers StringLiteral GhcPs) instance Pretty (WithHsDocIdentifiers StringLiteral GhcPs)
deriving via PPGeneric (WithHsDocIdentifiers a GhcRn) instance Pretty a => Pretty (WithHsDocIdentifiers a GhcRn)
deriving via PPGeneric Activation instance Pretty Activation
deriving via PPGeneric AddEpAnn instance Pretty AddEpAnn
deriving via PPGeneric AnchorOperation instance Pretty AnchorOperation
deriving via PPGeneric AnnContext instance Pretty AnnContext
deriving via PPGeneric AnnExplicitSum instance Pretty AnnExplicitSum
deriving via PPGeneric AnnFieldLabel instance Pretty AnnFieldLabel
deriving via PPGeneric AnnKeywordId instance Pretty AnnKeywordId
deriving via PPGeneric AnnList instance Pretty AnnList
deriving via PPGeneric AnnListItem instance Pretty AnnListItem
deriving via PPGeneric AnnParen instance Pretty AnnParen
deriving via PPGeneric AnnPragma instance Pretty AnnPragma
deriving via PPGeneric AnnProjection instance Pretty AnnProjection
deriving via PPGeneric AnnSig instance Pretty AnnSig
deriving via PPGeneric AnnSortKey instance Pretty AnnSortKey
deriving via PPGeneric AnnsIf instance Pretty AnnsIf
deriving via PPGeneric AnnsModule instance Pretty AnnsModule
deriving via PPGeneric Boxity instance Pretty Boxity
deriving via PPGeneric BufPos instance Pretty BufPos
deriving via PPGeneric BufSpan instance Pretty BufSpan
deriving via PPGeneric CCallConv instance Pretty CCallConv
deriving via PPGeneric CCallTarget instance Pretty CCallTarget
deriving via PPGeneric CExportSpec instance Pretty CExportSpec
deriving via PPGeneric CImportSpec instance Pretty CImportSpec
deriving via PPGeneric CType instance Pretty CType
deriving via PPGeneric CoAxBranch instance Pretty CoAxBranch
deriving via PPGeneric CoSel instance Pretty CoSel
deriving via PPGeneric Coercion instance Pretty Coercion
deriving via PPGeneric CoercionHole instance Pretty CoercionHole
deriving via PPGeneric ConInfo instance Pretty ConInfo
deriving via PPGeneric ConLikeName instance Pretty ConLikeName
deriving via PPGeneric DataConCantHappen instance Pretty DataConCantHappen
deriving via PPGeneric DataDeclRn instance Pretty DataDeclRn
deriving via PPGeneric DeltaPos instance Pretty DeltaPos
deriving via PPGeneric DoPmc instance Pretty DoPmc
deriving via PPGeneric DuplicateRecordFields instance Pretty DuplicateRecordFields
deriving via PPGeneric EpAnnComments instance Pretty EpAnnComments
deriving via PPGeneric EpAnnHsCase instance Pretty EpAnnHsCase
deriving via PPGeneric EpAnnImportDecl instance Pretty EpAnnImportDecl
deriving via PPGeneric EpAnnSumPat instance Pretty EpAnnSumPat
deriving via PPGeneric EpAnnUnboundVar instance Pretty EpAnnUnboundVar
deriving via PPGeneric EpaComment instance Pretty EpaComment
deriving via PPGeneric EpaCommentTok instance Pretty EpaCommentTok
deriving via PPGeneric EpaLocation instance Pretty EpaLocation
deriving via PPGeneric FieldLabel instance Pretty FieldLabel
deriving via PPGeneric FieldLabelString instance Pretty FieldLabelString
deriving via PPGeneric FieldSelectors instance Pretty FieldSelectors
deriving via PPGeneric Fixity instance Pretty Fixity
deriving via PPGeneric FixityDirection instance Pretty FixityDirection
deriving via PPGeneric ForAllTyFlag instance Pretty ForAllTyFlag
deriving via PPGeneric FractionalExponentBase instance Pretty FractionalExponentBase
deriving via PPGeneric FractionalLit instance Pretty FractionalLit
deriving via PPGeneric FunSel instance Pretty FunSel
deriving via PPGeneric FunTyFlag instance Pretty FunTyFlag
deriving via PPGeneric GREInfo instance Pretty GREInfo
deriving via PPGeneric GrhsAnn instance Pretty GrhsAnn
deriving via PPGeneric Header instance Pretty Header
deriving via PPGeneric HsArrAppType instance Pretty HsArrAppType
deriving via PPGeneric HsArrowMatchContext instance Pretty HsArrowMatchContext
deriving via PPGeneric HsDoFlavour instance Pretty HsDoFlavour
deriving via PPGeneric HsDocString instance Pretty HsDocString
deriving via PPGeneric HsDocStringChunk instance Pretty HsDocStringChunk
deriving via PPGeneric HsDocStringDecorator instance Pretty HsDocStringDecorator
deriving via PPGeneric HsIPName instance Pretty HsIPName
deriving via PPGeneric HsPSRn instance Pretty HsPSRn
deriving via PPGeneric HsRuleAnn instance Pretty HsRuleAnn
deriving via PPGeneric HsRuleRn instance Pretty HsRuleRn
deriving via PPGeneric HsSrcBang instance Pretty HsSrcBang
deriving via PPGeneric HsTupleSort instance Pretty HsTupleSort
deriving via PPGeneric IEWildcard instance Pretty IEWildcard
deriving via PPGeneric IdSig instance Pretty IdSig
deriving via PPGeneric ImpDeclSpec instance Pretty ImpDeclSpec
deriving via PPGeneric ImpItemSpec instance Pretty ImpItemSpec
deriving via PPGeneric ImportDeclQualifiedStyle instance Pretty ImportDeclQualifiedStyle
deriving via PPGeneric ImportListInterpretation instance Pretty ImportListInterpretation
deriving via PPGeneric ImportSpec instance Pretty ImportSpec
deriving via PPGeneric InWarningCategory instance Pretty InWarningCategory
deriving via PPGeneric InlinePragma instance Pretty InlinePragma
deriving via PPGeneric InlineSpec instance Pretty InlineSpec
deriving via PPGeneric IntegralLit instance Pretty IntegralLit
deriving via PPGeneric IsBootInterface instance Pretty IsBootInterface
deriving via PPGeneric IsUnicodeSyntax instance Pretty IsUnicodeSyntax
deriving via PPGeneric LamCaseVariant instance Pretty LamCaseVariant
deriving via PPGeneric LeftOrRight instance Pretty LeftOrRight
deriving via PPGeneric LexicalFixity instance Pretty LexicalFixity
deriving via PPGeneric MCoercion instance Pretty MCoercion
deriving via PPGeneric NameAdornment instance Pretty NameAdornment
deriving via PPGeneric NameAnn instance Pretty NameAnn
deriving via PPGeneric NoEpAnns instance Pretty NoEpAnns
deriving via PPGeneric NoExtField instance Pretty NoExtField
deriving via PPGeneric Origin instance Pretty Origin
deriving via PPGeneric OverLitRn instance Pretty OverLitRn
deriving via PPGeneric OverLitVal instance Pretty OverLitVal
deriving via PPGeneric OverlapMode instance Pretty OverlapMode
deriving via PPGeneric ParenType instance Pretty ParenType
deriving via PPGeneric Parent instance Pretty Parent
deriving via PPGeneric PendingRnSplice instance Pretty PendingRnSplice
deriving via PPGeneric PromotionFlag instance Pretty PromotionFlag
deriving via PPGeneric RawPkgQual instance Pretty RawPkgQual
deriving via PPGeneric RdrName instance Pretty RdrName
deriving via PPGeneric RecFieldInfo instance Pretty RecFieldInfo
deriving via PPGeneric RecFieldsDotDot instance Pretty RecFieldsDotDot
deriving via PPGeneric RecFlag instance Pretty RecFlag
deriving via PPGeneric Role instance Pretty Role
deriving via PPGeneric RuleMatchInfo instance Pretty RuleMatchInfo
deriving via PPGeneric Safety instance Pretty Safety
deriving via PPGeneric SourceText instance Pretty SourceText
deriving via PPGeneric Specificity instance Pretty Specificity
deriving via PPGeneric SpliceDecoration instance Pretty SpliceDecoration
deriving via PPGeneric SrcStrictness instance Pretty SrcStrictness
deriving via PPGeneric SrcUnpackedness instance Pretty SrcUnpackedness
deriving via PPGeneric StringLiteral instance Pretty StringLiteral
deriving via PPGeneric SyntaxExprRn instance Pretty SyntaxExprRn
deriving via PPGeneric TokenLocation instance Pretty TokenLocation
deriving via PPGeneric TopLevelFlag instance Pretty TopLevelFlag
deriving via PPGeneric TrailingAnn instance Pretty TrailingAnn
deriving via PPGeneric TransForm instance Pretty TransForm
deriving via PPGeneric TyLit instance Pretty TyLit
deriving via PPGeneric Type instance Pretty Type
deriving via PPGeneric TypeOrData instance Pretty TypeOrData
deriving via PPGeneric UnitId instance Pretty UnitId
deriving via PPGeneric UnivCoProvenance instance Pretty UnivCoProvenance
deriving via PPGeneric UntypedSpliceFlavour instance Pretty UntypedSpliceFlavour
deriving via PPGeneric WarningCategory instance Pretty WarningCategory
deriving via PPGeneric XBindStmtRn instance Pretty XBindStmtRn
deriving via PPGeneric XImportDeclPass instance Pretty XImportDeclPass
deriving via PPGeneric XModulePs instance Pretty XModulePs
deriving via PPGeneric XViaStrategyPs instance Pretty XViaStrategyPs

instance Pretty (GhcPass ix) where
  pretty = \case
    GhcPs -> "GhcPs"
    GhcRn -> "GhcRn"
    GhcTc -> "GhcTc"
