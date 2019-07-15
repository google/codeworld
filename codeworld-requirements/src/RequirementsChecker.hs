module RequirementsChecker (plugin) where

    import Requirements
    import Control.Monad.IO.Class
    import qualified Data.ByteString as B

    import Bag
    import ErrUtils
    import HscTypes
    import Outputable
    import Plugins
    import SrcLoc
    import TcRnTypes


    plugin :: Plugin
    plugin = defaultPlugin {
        parsedResultAction = \_args -> parsedStep,
        typeCheckResultAction = \_args -> typeCheckedStep
    }

    parsedStep :: ModSummary -> HsParsedModule -> Hsc HsParsedModule
    parsedStep summary mod = do
        src <- liftIO (B.readFile $ ms_hspp_file summary)

        let flags = ms_hspp_opts summary
            (L _ code) = hpm_module mod
            req = checkRequirements code src

        case req of
            Nothing -> return ()
            Just r -> liftIO (putMsg flags $ text r)

        pure mod

    typeCheckedStep :: ModSummary -> TcGblEnv -> TcM TcGblEnv
    typeCheckedStep summary env = pure env