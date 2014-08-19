{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Web.Face where

import           Control.Applicative
import           Control.Event.Handler
import           Control.Monad
import           Control.Monad.State
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Hm
import           Data.Monoid
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Time
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import qualified Text.Blaze.Html                 as Blz
import           Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Internal             as Blz

data Tag = Tag Text (HashMap Text Text)
data El  = El Tag (Seq El) | Tx Text

-- class DomWrite r where
--   type family Target r :: *
--   getTarget :: Text -> r (Maybe (Target r))
--   domWrite :: El -> Target r -> r ()

blaze :: El -> Blz.Html
blaze (Tx t) = Blz.Content (Blz.Text t)
blaze (El (Tag tag ats) cs) =
  addAttrs ats $ case Seq.null cs of
    True  -> Blz.CustomLeaf (Blz.Text tag) True
    False -> Blz.CustomParent (Blz.Text tag) (blazeSeq cs)

  where

    addAttrs :: HashMap Text Text -> Blz.Html -> Blz.Html
    addAttrs mp b0 = Hm.foldrWithKey add b0 mp where
      add k v blz = Blz.AddCustomAttribute (Blz.Text k) (Blz.Text v) blz

    blazeSeq :: Seq El -> Blz.Html
    blazeSeq =
      Seq.foldrWithIndex (\_ e b -> Blz.Append (blaze e) b) Blz.Empty

data AST t
  = Link (Event t UTCTime -> AST t)
  | Embed (Behavior t El)

t :: AST t
t = Link $ \eE -> Embed (stepper (Tx "nothing") $ fmap (\e -> Tx (Text.pack (show e))) eE)

int' :: Frameworks t => AST t -> Moment t (Behavior t El, [Handler UTCTime])
int' ast = runStateT (int ast) []

int :: Frameworks t => AST t -> StateT [Handler UTCTime] (Moment t) (Behavior t El)
int x = case x of
  Embed beh -> return beh
  Link f -> do
    (ev, h) <- lift newEvent
    modify (h :)
    res <- int (f ev)
    return res
