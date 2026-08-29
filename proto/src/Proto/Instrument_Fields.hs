{- This file was auto-generated from instrument.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Instrument_Fields where
import qualified Prelude
import qualified Data.Int
import qualified Data.Monoid
import qualified Data.Word
import qualified Data.ProtoLens
import qualified Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Field
import qualified Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Service.Types
import qualified Lens.Family2
import qualified Lens.Family2.Unchecked
import qualified Data.Text
import qualified Data.Map
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Text.Encoding
import qualified Data.Vector
import qualified Data.Vector.Generic
import qualified Data.Vector.Unboxed
import qualified Text.Read
encoded ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "encoded" a) =>
  Lens.Family2.LensLike' f s a
encoded = Data.ProtoLens.Field.field @"encoded"
entries ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "entries" a) =>
  Lens.Family2.LensLike' f s a
entries = Data.ProtoLens.Field.field @"entries"
extraGhcOptions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "extraGhcOptions" a) =>
  Lens.Family2.LensLike' f s a
extraGhcOptions = Data.ProtoLens.Field.field @"extraGhcOptions"
lastAccess ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lastAccess" a) =>
  Lens.Family2.LensLike' f s a
lastAccess = Data.ProtoLens.Field.field @"lastAccess"
moduleName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "moduleName" a) =>
  Lens.Family2.LensLike' f s a
moduleName = Data.ProtoLens.Field.field @"moduleName"
pendingEviction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "pendingEviction" a) =>
  Lens.Family2.LensLike' f s a
pendingEviction = Data.ProtoLens.Field.field @"pendingEviction"
rebuild ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "rebuild" a) =>
  Lens.Family2.LensLike' f s a
rebuild = Data.ProtoLens.Field.field @"rebuild"
resident ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resident" a) =>
  Lens.Family2.LensLike' f s a
resident = Data.ProtoLens.Field.field @"resident"
size ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "size" a) =>
  Lens.Family2.LensLike' f s a
size = Data.ProtoLens.Field.field @"size"
target ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "target" a) =>
  Lens.Family2.LensLike' f s a
target = Data.ProtoLens.Field.field @"target"
unitId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "unitId" a) =>
  Lens.Family2.LensLike' f s a
unitId = Data.ProtoLens.Field.field @"unitId"
vec'entries ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'entries" a) =>
  Lens.Family2.LensLike' f s a
vec'entries = Data.ProtoLens.Field.field @"vec'entries"