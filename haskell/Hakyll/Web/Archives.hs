module Hakyll.Web.Archives
    ( Archives (..)
    , buildArchives
    , archiveCloudField
    , archiveRules)  where

import           Control.Applicative
import           Data.List                       (intercalate, sort)
import           Data.Time
import           System.Locale                   (defaultTimeLocale)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Hakyll                          hiding (getTags)

newtype Archives = Archives { getTags :: Tags }

buildArchives :: (MonadMetadata m, Functor m) =>
                String
              -> Pattern
              -> (String -> Identifier)
              -> m Archives
buildArchives fmt pattern = (Archives <$>) .
                            buildTagsWith (getDate fmt) pattern
    where getDate f identifier = (return . formatTime defaultTimeLocale f)
                                 <$> getItemUTC defaultTimeLocale identifier

archiveCloudField :: String -> Archives -> Context String
archiveCloudField key archives =
    tagCloudFieldWith key
                      makeLinkList
                      (("<ul>" ++) . (++ "</ul>") . intercalate "\n"
                       . reverse . sort)
                      100.0
                      100.0
                      (getTags archives)
        where makeLinkList _ _ tag url cnt _ _ = renderHtml $
                H.li $ H.a H.! A.href (H.toValue url) $
                 H.toHtml (tag ++ " (" ++ show cnt ++ ")")

archiveRules :: Archives -> (String -> Pattern -> Rules ()) -> Rules ()
archiveRules archives = tagsRules (getTags archives)
