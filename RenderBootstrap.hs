module RenderBootstrap where

import Yesod.Form.Functions
import Yesod.Form.Types
import Yesod.Core (whamlet)
import Data.Maybe
import Data.Bool
import Control.Monad
import Prelude ((++))

bs3 :: FieldSettings m -> FieldSettings m
bs3 FieldSettings
    { fsLabel   = myFsLabel
    , fsTooltip = myFsTooltip
    , fsId      = myFsId
    , fsName    = myFsName
    , fsAttrs   = myFsAttrs
    } =
    FieldSettings
    { fsLabel   = myFsLabel
    , fsTooltip = myFsTooltip
    , fsId      = myFsId
    , fsName    = myFsName
    , fsAttrs   = myFsAttrs ++ [("class", "form-control")]
    }

-- | Render a form using Bootstrap-friendly HTML syntax.  Based
-- on 'renderDivs'.
renderBootstrap3 :: Monad m => FormRender m a
renderBootstrap3 aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div .form-group :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
        <label for=#{fvId view}>#{fvLabel view}
        ^{fvInput view}
        $maybe tt <- fvTooltip view
            <span .help-block>#{tt}
        $maybe err <- fvErrors view
            <span .help-block>#{err}
|]
    return (res, widget)
