-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
buildG bounds0 edges0 = accumArray (flip (:)) [] bounds0 (map reassoc edges0)
  where
    reassoc (v, e, w) = (v, (e, w))
