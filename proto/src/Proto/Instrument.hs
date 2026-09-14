{- This file was auto-generated from instrument.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Instrument (
        Instrument(..), Empty(), Encoded()
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
     
         * 'Proto.Instrument_Fields.payload' @:: Lens' Encoded Data.ByteString.ByteString@ -}
data Encoded
  = Encoded'_constructor {_Encoded'payload :: !Data.ByteString.ByteString,
                          _Encoded'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Encoded where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Encoded "payload" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Encoded'payload (\ x__ y__ -> x__ {_Encoded'payload = y__}))
        Prelude.id
instance Data.ProtoLens.Message Encoded where
  messageName _ = Data.Text.pack "instrument.Encoded"
  packedMessageDescriptor _
    = "\n\
      \\aEncoded\DC2\CAN\n\
      \\apayload\CAN\SOH \SOH(\fR\apayload"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        payload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "payload"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"payload")) ::
              Data.ProtoLens.FieldDescriptor Encoded
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, payload__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Encoded'_unknownFields
        (\ x__ y__ -> x__ {_Encoded'_unknownFields = y__})
  defMessage
    = Encoded'_constructor
        {_Encoded'payload = Data.ProtoLens.fieldDefault,
         _Encoded'_unknownFields = []}
  parseMessage
    = let
        loop :: Encoded -> Data.ProtoLens.Encoding.Bytes.Parser Encoded
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
                                       "payload"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"payload") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Encoded"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"payload") _x
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
instance Control.DeepSeq.NFData Encoded where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Encoded'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Encoded'payload x__) ())
data Instrument = Instrument {}
instance Data.ProtoLens.Service.Types.Service Instrument where
  type ServiceName Instrument = "Instrument"
  type ServicePackage Instrument = "instrument"
  type ServiceMethods Instrument = '["api", "events"]
  packedServiceDescriptor _
    = "\n\
      \\n\
      \Instrument\DC24\n\
      \\ACKEvents\DC2\DC1.instrument.Empty\SUB\DC3.instrument.Encoded\"\NUL0\SOH\DC21\n\
      \\ETXApi\DC2\DC3.instrument.Encoded\SUB\DC3.instrument.Encoded\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "events" where
  type MethodName Instrument "events" = "Events"
  type MethodInput Instrument "events" = Empty
  type MethodOutput Instrument "events" = Encoded
  type MethodStreamingType Instrument "events" = 'Data.ProtoLens.Service.Types.ServerStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl Instrument "api" where
  type MethodName Instrument "api" = "Api"
  type MethodInput Instrument "api" = Encoded
  type MethodOutput Instrument "api" = Encoded
  type MethodStreamingType Instrument "api" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DLEinstrument.proto\DC2\n\
    \instrument\"\a\n\
    \\ENQEmpty\"#\n\
    \\aEncoded\DC2\CAN\n\
    \\apayload\CAN\SOH \SOH(\fR\apayload2u\n\
    \\n\
    \Instrument\DC24\n\
    \\ACKEvents\DC2\DC1.instrument.Empty\SUB\DC3.instrument.Encoded\"\NUL0\SOH\DC21\n\
    \\ETXApi\DC2\DC3.instrument.Encoded\SUB\DC3.instrument.Encoded\"\NULJ\150\STX\n\
    \\ACK\DC2\EOT\NUL\NUL\r\SOH\n\
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
    \\ETX\EOT\SOH\SOH\DC2\ETX\ACK\b\SI\n\
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
    \\STX\ACK\NUL\DC2\EOT\n\
    \\NUL\r\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\n\
    \\b\DC2\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\v\STX/\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\v\ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\v\r\DC2\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ACK\DC2\ETX\v\GS#\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\v$+\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\f\STX'\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\f\ACK\t\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\f\n\
    \\DC1\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\f\FS#b\ACKproto3"