{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Pandoc

latexOpts = def {readerExtensions = extensionsFromList [Ext_latex_macros]}

main :: IO ()
main = do
  input <- TIO.getContents
  result <- runIO $ do
    doc <- readLaTeX latexOpts input
    writeForester def doc
  rst <- handleError result
  TIO.putStrLn rst

writeForester :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeForester _opts (Pandoc _meta blocks) = blockListToForester blocks

blockListToForester :: PandocMonad m => [Block] -> m Text
blockListToForester = fmap T.unlines . mapM blockToForester

blockToForester :: PandocMonad m => Block -> m Text
blockToForester = \case
  Para is -> cmd "p" <$> inlineListToForester is
  CodeBlock _ c -> pure . cmd "p" $ "\\pre\\verb<<<|\n" <> c <> "<<<\n"
  BulletList items -> cmd "ul" . T.unlines <$> mapM (fmap (cmd "li") . blockListToForester) items
  OrderedList _ items -> cmd "ol" . T.unlines <$> mapM (fmap (cmd "li") . blockListToForester) items
  Div (_, cls : _, _) bs -> (("\\taxon{" <> clsName cls <> "}\n") <>) <$> blockListToForester bs
  Header _ _ is -> cmd "title" <$> inlineListToForester is
  b -> pure $ "!!!{" <> T.pack (show b) <> "}"

clsName :: Text -> Text
clsName = \case
  "rem" -> "Remark"
  "defn" -> "Definition"
  "ex" -> "Example"
  "thm" -> "Theorem"
  cls -> cls

inlineListToForester :: PandocMonad m => [Inline] -> m Text
inlineListToForester = fmap T.concat . mapM inlineToForester

inlineToForester :: PandocMonad m => Inline -> m Text
inlineToForester = \case
  Str t -> pure $ fixQuotes t
  Space -> pure " "
  SoftBreak -> pure " "
  Quoted qs is -> quoted qs <$> inlineListToForester is
  Emph is -> cmd "em" <$> inlineListToForester is
  Strong is -> cmd "strong" <$> inlineListToForester is
  Math mathType txt -> pure $ math mathType txt
  Code _ c -> pure $ cmd "code" c
  i -> pure $ "!!{" <> T.pack (show i) <> "}"

fixQuotes :: Text -> Text
fixQuotes = T.replace "``" "“"

quoted :: QuoteType -> Text -> Text
quoted SingleQuote b = "‘" <> b <> "’"
quoted DoubleQuote b = "“" <> b <> "”"

cmd :: Text -> Text -> Text
cmd c b = "\\" <> c <> "{" <> b <> "}"

math :: MathType -> Text -> Text
math InlineMath body = "#{" <> body <> "}"
math DisplayMath body = "##{" <> body <> "}"
