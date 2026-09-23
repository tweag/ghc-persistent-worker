{- This file was auto-generated from executor.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Executor (
        Executor(..), Encoded()
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
     
         * 'Proto.Executor_Fields.payload' @:: Lens' Encoded Data.ByteString.ByteString@ -}
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
  messageName _ = Data.Text.pack "executor.Encoded"
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
data Executor = Executor {}
instance Data.ProtoLens.Service.Types.Service Executor where
  type ServiceName Executor = "Executor"
  type ServicePackage Executor = "executor"
  type ServiceMethods Executor = '["execute"]
  packedServiceDescriptor _
    = "\n\
      \\bExecutor\DC21\n\
      \\aExecute\DC2\DC1.executor.Encoded\SUB\DC1.executor.Encoded\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl Executor "execute" where
  type MethodName Executor "execute" = "Execute"
  type MethodInput Executor "execute" = Encoded
  type MethodOutput Executor "execute" = Encoded
  type MethodStreamingType Executor "execute" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\SOexecutor.proto\DC2\bexecutor\"#\n\
    \\aEncoded\DC2\CAN\n\
    \\apayload\CAN\SOH \SOH(\fR\apayload2=\n\
    \\bExecutor\DC21\n\
    \\aExecute\DC2\DC1.executor.Encoded\SUB\DC1.executor.Encoded\"\NULJ\136\ETX\n\
    \\ACK\DC2\EOT\NUL\NUL\f\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DC1\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\EOT\NUL\ACK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\SI\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ENQ\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\ENQ\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ENQ\b\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ENQ\DC2\DC3\n\
    \\215\SOH\n\
    \\STX\ACK\NUL\DC2\EOT\n\
    \\NUL\f\SOH\SUB\202\SOH A persistent execute-task subprocess spawned by ghc-server (see GhcServer.Build.Executor).\n\
    \ The payload of a request is a JSON-encoded ProcessEvalConfig, the response a JSON-encoded ProcessEvalResult.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\n\
    \\b\DLE\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\v\STX+\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\v\ACK\r\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\v\SO\NAK\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\v 'b\ACKproto3"