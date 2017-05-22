module Typedrat.Templates.PostEditor (postEditorTemplate) where

import Lucid

import Typedrat.Templates.Types

postEditorTemplate :: RatTemplate xs ()
postEditorTemplate = article_ $  do
    form_ $ do
        textarea_ ""
