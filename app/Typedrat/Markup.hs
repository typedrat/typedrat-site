module Typedrat.Markup (renderMarkdown, P.PandocError) where

import qualified Data.Set as S
import qualified Data.Text as T
import Lucid
import qualified Text.Pandoc as P

renderMarkdown :: (Monad m) => T.Text -> Either P.PandocError (HtmlT m ())
renderMarkdown = fmap (toHtmlRaw . P.writeHtmlString htmlOptions) .
                       P.readMarkdown markdownOpts . T.unpack
    where
        markdownOpts = P.def
            { P.readerParseRaw = True
            , P.readerSmart = True
            , P.readerExtensions = P.Ext_literate_haskell `S.insert` P.pandocExtensions
            }
        htmlOptions = P.def
            { P.writerHTMLMathMethod = P.MathJax ""
            , P.writerHighlight = True
            , P.writerHtml5 = True
            }
