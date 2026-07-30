{- This file was auto-generated from instrument.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Instrument (
        Instrument(..), BcoCacheEntry(), BytecodeState(), Empty(), Event(),
        EvictBytecodeRequest(), Options(), RebuildRequest()
    ) where
import qualified Control.DeepSeq
import qualified Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Instrument_Fields.unitId' @:: Lens' BcoCacheEntry Data.Text.Text@
         * 'Proto.Instrument_Fields.moduleName' @:: Lens' BcoCacheEntry Data.Text.Text@
         * 'Proto.Instrument_Fields.size' @:: Lens' BcoCacheEntry Data.Int.Int64@
         * 'Proto.Instrument_Fields.lastAccess' @:: Lens' BcoCacheEntry Data.Int.Int64@
         * 'Proto.Instrument_Fields.resident' @:: Lens' BcoCacheEntry Prelude.Bool@
         * 'Proto.Instrument_Fields.pendingEviction' @:: Lens' BcoCacheEntry Prelude.Bool@ -}
data BcoCacheEntry
  = BcoCacheEntry'_constructor {_BcoCacheEntry'unitId :: !Data.Text.Text,
                                _BcoCacheEntry'moduleName :: !Data.Text.Text,
                                _BcoCacheEntry'size :: !Data.Int.Int64,
                                _BcoCacheEntry'lastAccess :: !Data.Int.Int64,
                                _BcoCacheEntry'resident :: !Prelude.Bool,
                                _BcoCacheEntry'pendingEviction :: !Prelude.Bool,
                                _BcoCacheEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BcoCacheEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BcoCacheEntry "unitId" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BcoCacheEntry'unitId
           (\ x__ y__ -> x__ {_BcoCacheEntry'unitId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BcoCacheEntry "moduleName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BcoCacheEntry'moduleName
           (\ x__ y__ -> x__ {_BcoCacheEntry'moduleName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BcoCacheEntry "size" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BcoCacheEntry'size (\ x__ y__ -> x__ {_BcoCacheEntry'size = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BcoCacheEntry "lastAccess" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BcoCacheEntry'lastAccess
           (\ x__ y__ -> x__ {_BcoCacheEntry'lastAccess = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BcoCacheEntry "resident" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BcoCacheEntry'resident
           (\ x__ y__ -> x__ {_BcoCacheEntry'resident = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BcoCacheEntry "pendingEviction" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BcoCacheEntry'pendingEviction
           (\ x__ y__ -> x__ {_BcoCacheEntry'pendingEviction = y__}))
        Prelude.id
instance Data.ProtoLens.Message BcoCacheEntry where
  messageName _ = Data.Text.pack "instrument.BcoCacheEntry"
  packedMessageDescriptor _
    = "\n\
      \\rBcoCacheEntry\DC2\ETB\n\
      \\aunit_id\CAN\SOH \SOH(\tR\ACKunitId\DC2\US\n\
      \\vmodule_name\CAN\STX \SOH(\tR\n\
      \moduleName\DC2\DC2\n\
      \\EOTsize\CAN\ETX \SOH(\ETXR\EOTsize\DC2\US\n\
      \\vlast_access\CAN\EOT \SOH(\ETXR\n\
      \lastAccess\DC2\SUB\n\
      \\bresident\CAN\ENQ \SOH(\bR\bresident\DC2)\n\
      \\DLEpending_eviction\CAN\ACK \SOH(\bR\SIpendingEviction"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        unitId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "unit_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"unitId")) ::
              Data.ProtoLens.FieldDescriptor BcoCacheEntry
        moduleName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "module_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"moduleName")) ::
              Data.ProtoLens.FieldDescriptor BcoCacheEntry
        size__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"size")) ::
              Data.ProtoLens.FieldDescriptor BcoCacheEntry
        lastAccess__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "last_access"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"lastAccess")) ::
              Data.ProtoLens.FieldDescriptor BcoCacheEntry
        resident__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resident"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"resident")) ::
              Data.ProtoLens.FieldDescriptor BcoCacheEntry
        pendingEviction__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "pending_eviction"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"pendingEviction")) ::
              Data.ProtoLens.FieldDescriptor BcoCacheEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, unitId__field_descriptor),
           (Data.ProtoLens.Tag 2, moduleName__field_descriptor),
           (Data.ProtoLens.Tag 3, size__field_descriptor),
           (Data.ProtoLens.Tag 4, lastAccess__field_descriptor),
           (Data.ProtoLens.Tag 5, resident__field_descriptor),
           (Data.ProtoLens.Tag 6, pendingEviction__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BcoCacheEntry'_unknownFields
        (\ x__ y__ -> x__ {_BcoCacheEntry'_unknownFields = y__})
  defMessage
    = BcoCacheEntry'_constructor
        {_BcoCacheEntry'unitId = Data.ProtoLens.fieldDefault,
         _BcoCacheEntry'moduleName = Data.ProtoLens.fieldDefault,
         _BcoCacheEntry'size = Data.ProtoLens.fieldDefault,
         _BcoCacheEntry'lastAccess = Data.ProtoLens.fieldDefault,
         _BcoCacheEntry'resident = Data.ProtoLens.fieldDefault,
         _BcoCacheEntry'pendingEviction = Data.ProtoLens.fieldDefault,
         _BcoCacheEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          BcoCacheEntry -> Data.ProtoLens.Encoding.Bytes.Parser BcoCacheEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "unit_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"unitId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "module_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"moduleName") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "size"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"size") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "last_access"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"lastAccess") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "resident"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"resident") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "pending_eviction"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"pendingEviction") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "BcoCacheEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"unitId") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"moduleName") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"size") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"lastAccess") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      ((Data.Monoid.<>)
                         (let
                            _v = Lens.Family2.view (Data.ProtoLens.Field.field @"resident") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (\ b -> if b then 1 else 0) _v))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"pendingEviction") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (\ b -> if b then 1 else 0) _v))
                            (Data.ProtoLens.Encoding.Wire.buildFieldSet
                               (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))
instance Control.DeepSeq.NFData BcoCacheEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BcoCacheEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_BcoCacheEntry'unitId x__)
                (Control.DeepSeq.deepseq
                   (_BcoCacheEntry'moduleName x__)
                   (Control.DeepSeq.deepseq
                      (_BcoCacheEntry'size x__)
                      (Control.DeepSeq.deepseq
                         (_BcoCacheEntry'lastAccess x__)
                         (Control.DeepSeq.deepseq
                            (_BcoCacheEntry'resident x__)
                            (Control.DeepSeq.deepseq
                               (_BcoCacheEntry'pendingEviction x__) ()))))))
{- | Fields :
     
         * 'Proto.Instrument_Fields.entries' @:: Lens' BytecodeState [BcoCacheEntry]@
         * 'Proto.Instrument_Fields.vec'entries' @:: Lens' BytecodeState (Data.Vector.Vector BcoCacheEntry)@ -}
data BytecodeState
  = BytecodeState'_constructor {_BytecodeState'entries :: !(Data.Vector.Vector BcoCacheEntry),
                                _BytecodeState'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BytecodeState where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BytecodeState "entries" [BcoCacheEntry] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BytecodeState'entries
           (\ x__ y__ -> x__ {_BytecodeState'entries = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField BytecodeState "vec'entries" (Data.Vector.Vector BcoCacheEntry) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BytecodeState'entries
           (\ x__ y__ -> x__ {_BytecodeState'entries = y__}))
        Prelude.id
instance Data.ProtoLens.Message BytecodeState where
  messageName _ = Data.Text.pack "instrument.BytecodeState"
  packedMessageDescriptor _
    = "\n\
      \\rBytecodeState\DC23\n\
      \\aentries\CAN\SOH \ETX(\v2\EM.instrument.BcoCacheEntryR\aentries"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        entries__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "entries"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BcoCacheEntry)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"entries")) ::
              Data.ProtoLens.FieldDescriptor BytecodeState
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, entries__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BytecodeState'_unknownFields
        (\ x__ y__ -> x__ {_BytecodeState'_unknownFields = y__})
  defMessage
    = BytecodeState'_constructor
        {_BytecodeState'entries = Data.Vector.Generic.empty,
         _BytecodeState'_unknownFields = []}
  parseMessage
    = let
        loop ::
          BytecodeState
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld BcoCacheEntry
             -> Data.ProtoLens.Encoding.Bytes.Parser BytecodeState
        loop x mutable'entries
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'entries <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'entries)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'entries") frozen'entries x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "entries"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'entries y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'entries
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'entries <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'entries)
          "BytecodeState"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'entries") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData BytecodeState where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BytecodeState'_unknownFields x__)
             (Control.DeepSeq.deepseq (_BytecodeState'entries x__) ())
{- | Fields :
      -}
data Empty
  = Empty'_constructor {_Empty'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Empty where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message Empty where
  messageName _ = Data.Text.pack "instrument.Empty"
  packedMessageDescriptor _
    = "\n\
      \\ENQEmpty"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Empty'_unknownFields
        (\ x__ y__ -> x__ {_Empty'_unknownFields = y__})
  defMessage = Empty'_constructor {_Empty'_unknownFields = []}
  parseMessage
    = let
        loop :: Empty -> Data.ProtoLens.Encoding.Bytes.Parser Empty
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Empty"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData Empty where
  rnf
    = \ x__ -> Control.DeepSeq.deepseq (_Empty'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Instrument_Fields.encoded' @:: Lens' Event Data.ByteString.ByteString@ -}
data Event
  = Event'_constructor {_Event'encoded :: !Data.ByteString.ByteString,
                        _Event'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Event where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Event "encoded" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Event'encoded (\ x__ y__ -> x__ {_Event'encoded = y__}))
        Prelude.id
instance Data.ProtoLens.Message Event where
  messageName _ = Data.Text.pack "instrument.Event"
  packedMessageDescriptor _
    = "\n\
      \\ENQEvent\DC2\CAN\n\
      \\aencoded\CAN\SOH \SOH(\fR\aencoded"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        encoded__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "encoded"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"encoded")) ::
              Data.ProtoLens.FieldDescriptor Event
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, encoded__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Event'_unknownFields
        (\ x__ y__ -> x__ {_Event'_unknownFields = y__})
  defMessage
    = Event'_constructor
        {_Event'encoded = Data.ProtoLens.fieldDefault,
         _Event'_unknownFields = []}
  parseMessage
    = let
        loop :: Event -> Data.ProtoLens.Encoding.Bytes.Parser Event
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "encoded"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"encoded") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Event"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"encoded") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Event where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Event'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Event'encoded x__) ())
{- | Fields :
     
         * 'Proto.Instrument_Fields.unitId' @:: Lens' EvictBytecodeRequest Data.Text.Text@
         * 'Proto.Instrument_Fields.moduleName' @:: Lens' EvictBytecodeRequest Data.Text.Text@ -}
data EvictBytecodeRequest
  = EvictBytecodeRequest'_constructor {_EvictBytecodeRequest'unitId :: !Data.Text.Text,
                                       _EvictBytecodeRequest'moduleName :: !Data.Text.Text,
                                       _EvictBytecodeRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EvictBytecodeRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField EvictBytecodeRequest "unitId" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EvictBytecodeRequest'unitId
           (\ x__ y__ -> x__ {_EvictBytecodeRequest'unitId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField EvictBytecodeRequest "moduleName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _EvictBytecodeRequest'moduleName
           (\ x__ y__ -> x__ {_EvictBytecodeRequest'moduleName = y__}))
        Prelude.id
instance Data.ProtoLens.Message EvictBytecodeRequest where
  messageName _ = Data.Text.pack "instrument.EvictBytecodeRequest"
  packedMessageDescriptor _
    = "\n\
      \\DC4EvictBytecodeRequest\DC2\ETB\n\
      \\aunit_id\CAN\SOH \SOH(\tR\ACKunitId\DC2\US\n\
      \\vmodule_name\CAN\STX \SOH(\tR\n\
      \moduleName"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        unitId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "unit_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"unitId")) ::
              Data.ProtoLens.FieldDescriptor EvictBytecodeRequest
        moduleName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "module_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"moduleName")) ::
              Data.ProtoLens.FieldDescriptor EvictBytecodeRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, unitId__field_descriptor),
           (Data.ProtoLens.Tag 2, moduleName__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _EvictBytecodeRequest'_unknownFields
        (\ x__ y__ -> x__ {_EvictBytecodeRequest'_unknownFields = y__})
  defMessage
    = EvictBytecodeRequest'_constructor
        {_EvictBytecodeRequest'unitId = Data.ProtoLens.fieldDefault,
         _EvictBytecodeRequest'moduleName = Data.ProtoLens.fieldDefault,
         _EvictBytecodeRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          EvictBytecodeRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser EvictBytecodeRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "unit_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"unitId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "module_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"moduleName") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EvictBytecodeRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"unitId") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"moduleName") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData EvictBytecodeRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_EvictBytecodeRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_EvictBytecodeRequest'unitId x__)
                (Control.DeepSeq.deepseq
                   (_EvictBytecodeRequest'moduleName x__) ()))
{- | Fields :
     
         * 'Proto.Instrument_Fields.extraGhcOptions' @:: Lens' Options Data.Text.Text@ -}
data Options
  = Options'_constructor {_Options'extraGhcOptions :: !Data.Text.Text,
                          _Options'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Options where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Options "extraGhcOptions" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Options'extraGhcOptions
           (\ x__ y__ -> x__ {_Options'extraGhcOptions = y__}))
        Prelude.id
instance Data.ProtoLens.Message Options where
  messageName _ = Data.Text.pack "instrument.Options"
  packedMessageDescriptor _
    = "\n\
      \\aOptions\DC2*\n\
      \\DC1extra_ghc_options\CAN\SOH \SOH(\tR\SIextraGhcOptions"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        extraGhcOptions__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "extra_ghc_options"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"extraGhcOptions")) ::
              Data.ProtoLens.FieldDescriptor Options
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, extraGhcOptions__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Options'_unknownFields
        (\ x__ y__ -> x__ {_Options'_unknownFields = y__})
  defMessage
    = Options'_constructor
        {_Options'extraGhcOptions = Data.ProtoLens.fieldDefault,
         _Options'_unknownFields = []}
  parseMessage
    = let
        loop :: Options -> Data.ProtoLens.Encoding.Bytes.Parser Options
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "extra_ghc_options"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"extraGhcOptions") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Options"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"extraGhcOptions") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Options where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Options'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Options'extraGhcOptions x__) ())
{- | Fields :
     
         * 'Proto.Instrument_Fields.target' @:: Lens' RebuildRequest Data.Text.Text@ -}
data RebuildRequest
  = RebuildRequest'_constructor {_RebuildRequest'target :: !Data.Text.Text,
                                 _RebuildRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RebuildRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RebuildRequest "target" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RebuildRequest'target
           (\ x__ y__ -> x__ {_RebuildRequest'target = y__}))
        Prelude.id
instance Data.ProtoLens.Message RebuildRequest where
  messageName _ = Data.Text.pack "instrument.RebuildRequest"
  packedMessageDescriptor _
    = "\n\
      \\SORebuildRequest\DC2\SYN\n\
      \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        target__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "target"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"target")) ::
              Data.ProtoLens.FieldDescriptor RebuildRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, target__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RebuildRequest'_unknownFields
        (\ x__ y__ -> x__ {_RebuildRequest'_unknownFields = y__})
  defMessage
    = RebuildRequest'_constructor
        {_RebuildRequest'target = Data.ProtoLens.fieldDefault,
         _RebuildRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          RebuildRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser RebuildRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "target"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"target") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "RebuildRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"target") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData RebuildRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RebuildRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_RebuildRequest'target x__) ())
data Instrument = Instrument {}
instance Data.ProtoLens.Service.Types.Service Instrument where
  type ServiceName Instrument = "Instrument"
  type ServicePackage Instrument = "instrument"
  type ServiceMethods Instrument = '["evictBytecode",
                                     "getBytecodeState",
                                     "notifyMe",
                                     "setOptions",
                                     "triggerRebuild"]
  packedServiceDescriptor _
    = "\n\
      \\n\
      \Instrument\DC24\n\
      \\bNotifyMe\DC2\DC1.instrument.Empty\SUB\DC1.instrument.Event\"\NUL0\SOH\DC26\n\
      \\n\
      \SetOptions\DC2\DC3.instrument.Options\SUB\DC1.instrument.Empty\"\NUL\DC2A\n\
      \\SOTriggerRebuild\DC2\SUB.instrument.RebuildRequest\SUB\DC1.instrument.Empty\"\NUL\DC2B\n\
      \\DLEGetBytecodeState\DC2\DC1.instrument.Empty\SUB\EM.instrument.BytecodeState\"\NUL\DC2F\n\
      \\rEvictBytecode\DC2 .instrument.EvictBytecodeRequest\SUB\DC1.instrument.Empty\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "notifyMe" where
  type MethodName Instrument "notifyMe" = "NotifyMe"
  type MethodInput Instrument "notifyMe" = Empty
  type MethodOutput Instrument "notifyMe" = Event
  type MethodStreamingType Instrument "notifyMe" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "setOptions" where
  type MethodName Instrument "setOptions" = "SetOptions"
  type MethodInput Instrument "setOptions" = Options
  type MethodOutput Instrument "setOptions" = Empty
  type MethodStreamingType Instrument "setOptions" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "triggerRebuild" where
  type MethodName Instrument "triggerRebuild" = "TriggerRebuild"
  type MethodInput Instrument "triggerRebuild" = RebuildRequest
  type MethodOutput Instrument "triggerRebuild" = Empty
  type MethodStreamingType Instrument "triggerRebuild" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "getBytecodeState" where
  type MethodName Instrument "getBytecodeState" = "GetBytecodeState"
  type MethodInput Instrument "getBytecodeState" = Empty
  type MethodOutput Instrument "getBytecodeState" = BytecodeState
  type MethodStreamingType Instrument "getBytecodeState" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "evictBytecode" where
  type MethodName Instrument "evictBytecode" = "EvictBytecode"
  type MethodInput Instrument "evictBytecode" = EvictBytecodeRequest
  type MethodOutput Instrument "evictBytecode" = Empty
  type MethodStreamingType Instrument "evictBytecode" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DLEinstrument.proto\DC2\n\
    \instrument\"\a\n\
    \\ENQEmpty\"!\n\
    \\ENQEvent\DC2\CAN\n\
    \\aencoded\CAN\SOH \SOH(\fR\aencoded\"5\n\
    \\aOptions\DC2*\n\
    \\DC1extra_ghc_options\CAN\SOH \SOH(\tR\SIextraGhcOptions\"(\n\
    \\SORebuildRequest\DC2\SYN\n\
    \\ACKtarget\CAN\SOH \SOH(\tR\ACKtarget\"\197\SOH\n\
    \\rBcoCacheEntry\DC2\ETB\n\
    \\aunit_id\CAN\SOH \SOH(\tR\ACKunitId\DC2\US\n\
    \\vmodule_name\CAN\STX \SOH(\tR\n\
    \moduleName\DC2\DC2\n\
    \\EOTsize\CAN\ETX \SOH(\ETXR\EOTsize\DC2\US\n\
    \\vlast_access\CAN\EOT \SOH(\ETXR\n\
    \lastAccess\DC2\SUB\n\
    \\bresident\CAN\ENQ \SOH(\bR\bresident\DC2)\n\
    \\DLEpending_eviction\CAN\ACK \SOH(\bR\SIpendingEviction\"D\n\
    \\rBytecodeState\DC23\n\
    \\aentries\CAN\SOH \ETX(\v2\EM.instrument.BcoCacheEntryR\aentries\"P\n\
    \\DC4EvictBytecodeRequest\DC2\ETB\n\
    \\aunit_id\CAN\SOH \SOH(\tR\ACKunitId\DC2\US\n\
    \\vmodule_name\CAN\STX \SOH(\tR\n\
    \moduleName2\201\STX\n\
    \\n\
    \Instrument\DC24\n\
    \\bNotifyMe\DC2\DC1.instrument.Empty\SUB\DC1.instrument.Event\"\NUL0\SOH\DC26\n\
    \\n\
    \SetOptions\DC2\DC3.instrument.Options\SUB\DC1.instrument.Empty\"\NUL\DC2A\n\
    \\SOTriggerRebuild\DC2\SUB.instrument.RebuildRequest\SUB\DC1.instrument.Empty\"\NUL\DC2B\n\
    \\DLEGetBytecodeState\DC2\DC1.instrument.Empty\SUB\EM.instrument.BytecodeState\"\NUL\DC2F\n\
    \\rEvictBytecode\DC2 .instrument.EvictBytecodeRequest\SUB\DC1.instrument.Empty\"\NULJ\154\v\n\
    \\ACK\DC2\EOT\NUL\NUL-\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DC3\n\
    \\t\n\
    \\STX\EOT\NUL\DC2\ETX\EOT\NUL\DLE\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\r\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\ACK\NUL\b\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\ACK\b\r\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\a\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\a\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\a\b\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\a\DC2\DC3\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\n\
    \\NUL\f\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\n\
    \\b\SI\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\v\STX\US\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\v\t\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\v\GS\RS\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\SO\NUL\DLE\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\SO\b\SYN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\SI\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\SI\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\SI\t\SI\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\SI\DC2\DC3\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT\DC2\NUL\FS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX\DC2\b\NAK\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX\DC3\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETX\DC3\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX\DC3\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX\DC3\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX\DC4\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETX\DC4\STX\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX\DC4\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX\DC4\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETX\NAK\STX\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ENQ\DC2\ETX\NAK\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETX\NAK\b\f\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETX\NAK\SI\DLE\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\ETX\DC2\ETX\SYN\STX\CAN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ENQ\DC2\ETX\SYN\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\SOH\DC2\ETX\SYN\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ETX\DC2\ETX\SYN\SYN\ETB\n\
    \{\n\
    \\EOT\EOT\EOT\STX\EOT\DC2\ETX\CAN\STX\DC4\SUBn Whether the module is currently present in the worker's loader state (home package table), i.e. not evicted.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ENQ\DC2\ETX\CAN\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\SOH\DC2\ETX\CAN\a\SI\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ETX\DC2\ETX\CAN\DC2\DC3\n\
    \\150\SOH\n\
    \\EOT\EOT\EOT\STX\ENQ\DC2\ETX\ESC\STX\FS\SUB\136\SOH Whether the module has been requested for eviction but the request hasn't been applied yet (see\n\
    \ 'Types.State.Make.pendingEvictions').\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ENQ\DC2\ETX\ESC\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\SOH\DC2\ETX\ESC\a\ETB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ETX\DC2\ETX\ESC\SUB\ESC\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT\RS\NUL \SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX\RS\b\NAK\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX\US\STX%\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETX\US\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX\US\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX\US\EM \n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX\US#$\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOT\"\NUL%\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX\"\b\FS\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX#\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX#\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX#\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX#\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX$\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\ETX$\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX$\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX$\ETB\CAN\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT'\NUL-\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX'\b\DC2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX(\STX/\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX(\ACK\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX(\SI\DC4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ACK\DC2\ETX(\US%\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX(&+\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX)\STX,\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX)\ACK\DLE\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX)\DC1\CAN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX)#(\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX*\STX7\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX*\ACK\DC4\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX*\NAK#\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX*.3\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETX+\STX8\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETX+\ACK\SYN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETX+\ETB\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETX+'4\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\EOT\DC2\ETX,\STX<\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\SOH\DC2\ETX,\ACK\DC3\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\STX\DC2\ETX,\DC4(\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\EOT\ETX\DC2\ETX,38b\ACKproto3"