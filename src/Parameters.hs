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
          <> metavar "FILEPATH"
          <> value "config.toml"
          <> help "filepath where config is loaded from")

paramInfo :: IO Parameters
paramInfo = execParser (
  info (
    argParser <**> helper
    )
  (
      fullDesc
      <> progDesc ""
      <> header "Ogmios Datum Cache"

    )
  )