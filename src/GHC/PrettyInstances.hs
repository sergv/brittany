----------------------------------------------------------------------------
-- |
-- Module      :  GHC.PrettyInstances
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.PrettyInstances () where

import Prettyprinter.Combinators
import Prettyprinter.Data
import Prettyprinter.Generics
import Prettyprinter.Show

import GHC hiding (parseModule)

import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import GHC.Types.Var

-- import Language.Haskell.TH.ReifyMany
-- import Language.Haskell.TH.ReifyMany.Internal

deriving instance Generic Anchor
deriving instance Generic (EpAnn ann)
deriving instance Generic EpaComment
deriving instance Generic ParsedSource
deriving instance Generic HsModule
deriving instance Generic (GenLocated a b)
deriving instance Generic EpAnnComments
deriving instance Generic (SrcSpanAnn' a)

deriving instance Generic (HsDecl GhcPs)
deriving instance Generic (HsBindLR GhcPs GhcPs)
deriving instance Generic (PatSynBind GhcPs GhcPs)
-- deriving instance Generic (IdP GhcPs)
deriving instance Generic (MatchGroup GhcPs a)
deriving instance Generic (Match GhcPs a)
deriving instance Generic (GRHSs GhcPs a)
deriving instance Generic (GRHS GhcPs a)
deriving instance Generic (StmtLR GhcPs GhcPs a)
deriving instance Generic (HsLocalBindsLR GhcPs GhcPs)
deriving instance Generic (HsValBindsLR GhcPs GhcPs)
deriving instance Generic (HsExpr GhcPs)
deriving instance Generic (HsConDetails a b c)
deriving instance Generic (Pat GhcPs)
deriving instance Generic (RecordPatSynField GhcPs)

deriving instance Generic (HsWildCardBndrs GhcPs a)
deriving instance Generic (Sig GhcPs)
deriving instance Generic (HsSigType GhcPs)
deriving instance Generic (HsType GhcPs)
deriving instance Generic (HsForAllTelescope GhcPs)
deriving instance Generic (HsTyVarBndr a GhcPs)

deriving instance Generic RdrName
deriving instance Generic NameAnn
deriving instance Generic TrailingAnn
deriving instance Generic NameAdornment
deriving instance Generic AddEpAnn
deriving instance Generic AnnKeywordId
deriving instance Generic EpaLocation
deriving instance Generic DeltaPos
deriving instance Generic AnnListItem
deriving instance Generic NoExtCon
deriving instance Generic Specificity



-- instance Pretty ParsedSource where
--   pretty = ppGeneric

instance Pretty OccName where
  pretty = pretty . occNameString


instance Pretty RealSrcSpan where
  pretty = ppShow

instance Pretty SrcSpan where
  pretty = \case
    RealSrcSpan x _ -> ppShow x
    UnhelpfulSpan _ -> "<unhelpful span>"

instance Pretty ModuleName where
  pretty = ppShow

-- instance Data a => PPGenericOverride a where
--   ppGenericOverride = compositeMetaDoc . ppData

instance {-# OVERLAPPABLE #-} Data a => Pretty a where
  pretty = ppData

instance Pretty ann => Pretty (EpAnn ann) where
  pretty = ppGeneric

instance Pretty a => Pretty (SrcSpanAnn' a) where
  pretty = ppGeneric
  -- pretty x = ppDictHeader "SrcSpanAnn'"
  --   [ "ann" --> ann x
  --   ]

instance Pretty EpaComment where
  pretty = ppGeneric

instance Pretty Anchor where
  pretty = ppGeneric
  -- pretty x = ppDictHeader "Anchor"
  --   [ "anchor_op" --> anchor_op x
  --   ]

instance Pretty HsModule where
  pretty = ppGeneric

instance Pretty (HsDecl GhcPs) where
  pretty = ppGeneric

instance Pretty (HsBindLR GhcPs GhcPs) where
  pretty = ppGeneric

instance Pretty (PatSynBind GhcPs GhcPs) where
  pretty = ppGeneric

instance Pretty (Pat GhcPs) where
  pretty = ppGeneric

instance (Pretty a, Pretty b, Pretty c) => Pretty (HsConDetails a b c) where
  pretty = ppGeneric

instance Pretty (RecordPatSynField GhcPs) where
  pretty = ppGeneric

instance Pretty (MatchGroup GhcPs (LocatedA (HsExpr GhcPs))) where
  pretty = ppGeneric

instance Pretty (Match GhcPs (LocatedA (HsExpr GhcPs))) where
  pretty = ppGeneric

instance Pretty (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  pretty = ppGeneric

instance Pretty (GRHS GhcPs (LocatedA (HsExpr GhcPs))) where
  pretty = ppGeneric

instance Pretty (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs))) where
  pretty = ppGeneric

instance Pretty (HsLocalBindsLR GhcPs GhcPs) where
  pretty = ppGeneric

instance Pretty (HsValBindsLR GhcPs GhcPs) where
  pretty = ppGeneric

instance Pretty (HsExpr GhcPs) where
  pretty = ppGeneric

instance Pretty a => Pretty (HsWildCardBndrs GhcPs a) where
  pretty = ppGeneric

instance Pretty (Sig GhcPs) where
  pretty = ppGeneric

instance Pretty (HsSigType GhcPs) where
  pretty = ppGeneric

instance Pretty (HsType GhcPs) where
  pretty = ppGeneric

instance Pretty (HsForAllTelescope GhcPs) where
  pretty = ppGeneric

instance Pretty a => Pretty (HsTyVarBndr a GhcPs) where
  pretty = ppGeneric

instance Pretty RdrName where
  pretty = ppGeneric

instance Pretty NameAnn where
  pretty = ppGeneric

instance Pretty TrailingAnn where
  pretty = ppGeneric

instance Pretty NameAdornment where
  pretty = ppGeneric

instance Pretty AddEpAnn where
  pretty = ppGeneric

instance Pretty AnnKeywordId where
  pretty = ppGeneric

instance Pretty EpaLocation where
  pretty = ppGeneric

instance Pretty DeltaPos where
  pretty = ppGeneric

instance Pretty AnnListItem where
  pretty = ppGeneric

instance Pretty NoExtCon where
  pretty = ppGeneric

instance Pretty Specificity where
  pretty = ppGeneric

-- instance Pretty HsTyVarBndr () pass
-- HsTyVarBndr Specificity pass


-- instance Pretty (IdP GhcPs) where
--   pretty = ppGeneric

-- instance Pretty (GenLocated a b) where
instance (Pretty a, Pretty b) => Pretty (GenLocated a b) where
  pretty = ppGeneric

instance Pretty EpAnnComments where
  pretty = ppGeneric


