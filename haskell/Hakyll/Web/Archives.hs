module Hakyll.Web.Archives
    ( Archives
    , buildArchives
    , archiveField
    , archiveRules)  where

import           Control.Applicative
import           Data.List                       (intercalate, sort)
import           Data.Time
import           System.Locale                   (defaultTimeLocale)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Hakyll                          hiding (getTags)

-----------------------------------------------------------------------------
-- | Data about archives
newtype Archives = Archives { tags :: Tags }


-----------------------------------------------------------------------------
-- | Build archives from a pattern, grouped by date formated in the fmt pattern
buildArchives :: (MonadMetadata m, Functor m) =>
                String
              -> Pattern
              -> (String -> Identifier)
              -> m Archives
buildArchives fmt pattern = (Archives <$>) .
                            buildTagsWith (getDate fmt) pattern
    where getDate f identifier = (return . formatTime defaultTimeLocale f)
                                 <$> getItemUTC defaultTimeLocale identifier


-----------------------------------------------------------------------------
-- | Render a list archives in HTML as a context
archiveField :: String -> Archives -> Context String
archiveField key archives =
    tagCloudFieldWith key
                      makeLinkList
                      (("<ul>" ++) . (++ "</ul>") . intercalate "\n"
                       . reverse . sort)
                      100.0
                      100.0
                      (tags archives)
        where makeLinkList _ _ tag url cnt _ _ = renderHtml $
                H.li $ H.a H.! A.href (H.toValue url) $
                 H.toHtml (tag ++ " (" ++ show cnt ++ ")")


-----------------------------------------------------------------------------
-- | The archiveRules function lets you generate a page for each archive tag
archiveRules :: Archives -> (String -> Pattern -> Rules ()) -> Rules ()
archiveRules archives = tagsRules (tags archives)
