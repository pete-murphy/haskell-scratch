{-# LANGUAGE BlockArguments, FlexibleContexts, LambdaCase #-}

import Control.Comonad
import Control.Comonad.Traced
import Control.Monad.Reader
import Data.Function ((&))

data ReportStyle = Detailed | Summary

toggleReportStyle :: ReportStyle -> ReportStyle
toggleReportStyle = \case
  Detailed -> Summary
  Summary -> Detailed

reportConfig :: ReportStyle -> (Traced (Sum Int)) Float
reportConfig _ = traced projections
  where
    projections (Sum month) = 1.2 ^ (max 0 month) * 100

-- Queries
previousMonth, nextMonth :: ComonadTraced (Sum Int) w => w a -> a
previousMonth = trace (Sum (-1))
nextMonth = trace (Sum 1)

detailedReport :: ComonadTraced (Sum Int) w => w Float -> String
detailedReport = do
  salesAmt <- extract
  prev <- previousMonth
  next <- nextMonth
  let report =
        [ "This month's sales are:  " <> show salesAmt
        , "Previous month's sales:  " <> show prev
        , "Next month's projection: " <> show next
        ]
  pure (unlines report)

buildHeader :: MonadReader ReportStyle w => w a -> String
buildHeader = do
  style <- asks \case
    Summary -> "SUMMARY"
    Detailed -> "DETAILED"
  pure ("Please find enclosed your " <> style <> " report:\n")

buildReport
  :: (ComonadTraced (Sum Int) w)
  => ReportStyle -> w Float -> String
buildReport reportStyle = do
  header <- buildHeader
  salesAmt <- extract
  case reportStyle of
    Summary ->
      pure (header <> "We achieved: " <> show salesAmt <> " in sales!")
    Detailed -> do
      report <- detailedReport
      pure (header <> report)

main :: IO ()
main = do
  putStrLn (buildReport reportConfig)
  putStrLn (reportConfig =>> nextMonth =>> buildReport & extract)
  putStrLn (reportConfig =>> nextMonth =>> buildReport . local toggleReportStyle & extract)
