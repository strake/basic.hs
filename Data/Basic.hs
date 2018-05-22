{-# LANGUAGE TemplateHaskell #-}

module Data.Basic where

import Control.Concurrent.STM (STM)
import Control.Exception (Handler (..))
import Control.Monad (replicateM)
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select
import Control.Monad.Trans.State.Lazy as L
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer.Lazy as L
import Control.Monad.Trans.Writer.Strict as S
import Control.Monad.ST.Lazy as L (ST)
import Control.Monad.ST.Strict as S (ST)
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Alt (..))
import Data.Proxy
import Data.Semigroup (Arg (..))
import Language.Haskell.TH
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

-- | Laws if @'Functor' f@:
--
-- * @'liftBase' . 'fmap' f = 'fmap' f . 'liftBase'@
class Basic1 (f :: α -> *) where
    type Base f :: α -> *
    liftBase :: Base f a -> f a

$(let f :: Int -> Name -> Q Dec
      f n name =
          [InstanceD Nothing [] (ConT ''Basic1 `AppT` t)
           [TySynInstD ''Base $ TySynEqn [t] t,
            ValD (VarP 'liftBase) (NormalB (VarE 'id)) []]
            | t <- foldl' AppT (ConT name) <$> replicateM n (VarT <$> newName "a")]
  in traverse (uncurry f) [(0, ''IO), (1, ''L.ST), (1, ''S.ST), (0, ''STM),
                           (0, ''Maybe), (1, ''Either), (0, ''Identity),
                           (0, ''[]), (0, ''NonEmpty),
                           (0, ''Proxy), (1, ''Const),
                           (0, ''ReadP), (0, ''ReadPrec), (0, ''Handler),
                           (1, ''Arg), (1, ''Alt)])

instance Basic1 ((,) a) where
    type Base ((,) a) = (,) a
    liftBase = id

instance Basic1 ((->) a) where
    type Base ((->) a) = (->) a
    liftBase = id

instance (Monad f, Basic1 g) => Basic1 (Compose f g) where
    type Base (Compose f g) = Base g
    liftBase = Compose . pure . liftBase

instance (Basic1 f, Basic1 g, Base f ~ Base g) => Basic1 (Product f g) where
    type Base (Product f g) = Base f
    liftBase = Pair <$> liftBase <*> liftBase

$(let f :: Int -> Name -> Q Dec
      f n name = 
          [InstanceD Nothing [ConT ''Basic1 `AppT` f, ConT ''Monad `AppT` f] (ConT ''Basic1 `AppT` AppT t f)
           [TySynInstD ''Base $ TySynEqn [AppT t f] (ConT ''Base `AppT` f),
            ValD (VarP 'liftBase) (NormalB (foldl' AppE (VarE '(.)) (VarE <$> ['lift, 'liftBase]))) []]
            | t <- foldl' AppT (ConT name) <$> replicateM n (VarT <$> newName "a")
            , f <- VarT <$> newName "f"]
  in traverse (uncurry f) [(0, ''IdentityT), (0, ''MaybeT), (1, ''ContT), (1, ''SelectT),
                           (1, ''ReaderT), (1, ''L.StateT), (1, ''S.StateT), (1, ''ExceptT)])

instance (Basic1 f, Monad f, Monoid a) => Basic1 (L.WriterT a f) where
    type Base (L.WriterT a f) = Base f
    liftBase = lift . liftBase

instance (Basic1 f, Monad f, Monoid a) => Basic1 (S.WriterT a f) where
    type Base (S.WriterT a f) = Base f
    liftBase = lift . liftBase

instance (Basic1 f, Monad f, Monoid a) => Basic1 (AccumT a f) where
    type Base (AccumT a f) = Base f
    liftBase = lift . liftBase
