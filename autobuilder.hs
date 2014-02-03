{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

-- Currently this will not run as a script even with this: #!/usr/bin/env runhaskell -package=base-3.0.3.0
-- The reason is unclear.  Either use the wrapper script in
-- /usr/bin/autobuilder or run in the interpreter:
--
--   ghci
--   > :load autobuilder.hs
--   > getParams ["precise-seereason", "--all-targets", "--flush-pool"] >>= Debian.AutoBuilder.Main.main
--
-- This may run very slowly.

import qualified Debian.AutoBuilder.Main as M
import Debian.AutoBuilder.Details (myParams)
import Debian.Debianize.Details (seereasonDefaultAtoms)

main = M.main seereasonDefaultAtoms myParams
