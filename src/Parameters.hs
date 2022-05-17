module Parameters (
    Parameters(..),
    argParser,
    paramInfo
) where

import Options.Applicative

newtype Parameters = Parameters
  { config :: FilePath }

argParser :: Parser Parameters
argParser = Parameters
        <$> strOption
          ( long "config"
          <> help "File path config is loaded from.")

paramInfo :: IO Parameters
paramInfo = execParser (info argParser mempty)