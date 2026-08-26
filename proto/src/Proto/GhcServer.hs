{- This file was auto-generated from ghc-server.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.GhcServer (
        GhcServer(..), CleanRequest(), CleanResponse(), ExecuteCommand(),
        ExecuteCommand'EnvironmentEntry(), ExecuteResponse()
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
data CleanRequest
  = CleanRequest'_constructor {_CleanRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CleanRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message CleanRequest where
  messageName _ = Data.Text.pack "ghcserver.CleanRequest"
  packedMessageDescriptor _
    = "\n\
      \\fCleanRequest"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CleanRequest'_unknownFields
        (\ x__ y__ -> x__ {_CleanRequest'_unknownFields = y__})
  defMessage
    = CleanRequest'_constructor {_CleanRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CleanRequest -> Data.ProtoLens.Encoding.Bytes.Parser CleanRequest
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
          (do loop Data.ProtoLens.defMessage) "CleanRequest"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData CleanRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_CleanRequest'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.GhcServer_Fields.success' @:: Lens' CleanResponse Prelude.Bool@
         * 'Proto.GhcServer_Fields.message' @:: Lens' CleanResponse Data.Text.Text@ -}
data CleanResponse
  = CleanResponse'_constructor {_CleanResponse'success :: !Prelude.Bool,
                                _CleanResponse'message :: !Data.Text.Text,
                                _CleanResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CleanResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CleanResponse "success" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CleanResponse'success
           (\ x__ y__ -> x__ {_CleanResponse'success = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CleanResponse "message" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CleanResponse'message
           (\ x__ y__ -> x__ {_CleanResponse'message = y__}))
        Prelude.id
instance Data.ProtoLens.Message CleanResponse where
  messageName _ = Data.Text.pack "ghcserver.CleanResponse"
  packedMessageDescriptor _
    = "\n\
      \\rCleanResponse\DC2\CAN\n\
      \\asuccess\CAN\SOH \SOH(\bR\asuccess\DC2\CAN\n\
      \\amessage\CAN\STX \SOH(\tR\amessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        success__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "success"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"success")) ::
              Data.ProtoLens.FieldDescriptor CleanResponse
        message__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"message")) ::
              Data.ProtoLens.FieldDescriptor CleanResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, success__field_descriptor),
           (Data.ProtoLens.Tag 2, message__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CleanResponse'_unknownFields
        (\ x__ y__ -> x__ {_CleanResponse'_unknownFields = y__})
  defMessage
    = CleanResponse'_constructor
        {_CleanResponse'success = Data.ProtoLens.fieldDefault,
         _CleanResponse'message = Data.ProtoLens.fieldDefault,
         _CleanResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CleanResponse -> Data.ProtoLens.Encoding.Bytes.Parser CleanResponse
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
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "success"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"success") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "message"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"message") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CleanResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"success") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"message") _x
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
instance Control.DeepSeq.NFData CleanResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CleanResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CleanResponse'success x__)
                (Control.DeepSeq.deepseq (_CleanResponse'message x__) ()))
{- | Fields :
     
         * 'Proto.GhcServer_Fields.argv' @:: Lens' ExecuteCommand [Data.ByteString.ByteString]@
         * 'Proto.GhcServer_Fields.vec'argv' @:: Lens' ExecuteCommand (Data.Vector.Vector Data.ByteString.ByteString)@
         * 'Proto.GhcServer_Fields.env' @:: Lens' ExecuteCommand [ExecuteCommand'EnvironmentEntry]@
         * 'Proto.GhcServer_Fields.vec'env' @:: Lens' ExecuteCommand (Data.Vector.Vector ExecuteCommand'EnvironmentEntry)@ -}
data ExecuteCommand
  = ExecuteCommand'_constructor {_ExecuteCommand'argv :: !(Data.Vector.Vector Data.ByteString.ByteString),
                                 _ExecuteCommand'env :: !(Data.Vector.Vector ExecuteCommand'EnvironmentEntry),
                                 _ExecuteCommand'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteCommand where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExecuteCommand "argv" [Data.ByteString.ByteString] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'argv
           (\ x__ y__ -> x__ {_ExecuteCommand'argv = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExecuteCommand "vec'argv" (Data.Vector.Vector Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'argv
           (\ x__ y__ -> x__ {_ExecuteCommand'argv = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteCommand "env" [ExecuteCommand'EnvironmentEntry] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'env (\ x__ y__ -> x__ {_ExecuteCommand'env = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExecuteCommand "vec'env" (Data.Vector.Vector ExecuteCommand'EnvironmentEntry) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'env (\ x__ y__ -> x__ {_ExecuteCommand'env = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExecuteCommand where
  messageName _ = Data.Text.pack "ghcserver.ExecuteCommand"
  packedMessageDescriptor _
    = "\n\
      \\SOExecuteCommand\DC2\DC2\n\
      \\EOTargv\CAN\SOH \ETX(\fR\EOTargv\DC2<\n\
      \\ETXenv\CAN\STX \ETX(\v2*.ghcserver.ExecuteCommand.EnvironmentEntryR\ETXenv\SUB:\n\
      \\DLEEnvironmentEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        argv__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "argv"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"argv")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand
        env__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "env"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExecuteCommand'EnvironmentEntry)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"env")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, argv__field_descriptor),
           (Data.ProtoLens.Tag 2, env__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteCommand'_unknownFields
        (\ x__ y__ -> x__ {_ExecuteCommand'_unknownFields = y__})
  defMessage
    = ExecuteCommand'_constructor
        {_ExecuteCommand'argv = Data.Vector.Generic.empty,
         _ExecuteCommand'env = Data.Vector.Generic.empty,
         _ExecuteCommand'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteCommand
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.ByteString.ByteString
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ExecuteCommand'EnvironmentEntry
                -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteCommand
        loop x mutable'argv mutable'env
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'argv <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'argv)
                      frozen'env <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'env)
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
                              (Data.ProtoLens.Field.field @"vec'argv") frozen'argv
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'env") frozen'env x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getBytes
                                              (Prelude.fromIntegral len))
                                        "argv"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'argv y)
                                loop x v mutable'env
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "env"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'env y)
                                loop x mutable'argv v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'argv mutable'env
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'argv <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                Data.ProtoLens.Encoding.Growing.new
              mutable'env <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                               Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'argv mutable'env)
          "ExecuteCommand"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'argv") _x))
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'env") _x))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExecuteCommand where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteCommand'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExecuteCommand'argv x__)
                (Control.DeepSeq.deepseq (_ExecuteCommand'env x__) ()))
{- | Fields :
     
         * 'Proto.GhcServer_Fields.key' @:: Lens' ExecuteCommand'EnvironmentEntry Data.ByteString.ByteString@
         * 'Proto.GhcServer_Fields.value' @:: Lens' ExecuteCommand'EnvironmentEntry Data.ByteString.ByteString@ -}
data ExecuteCommand'EnvironmentEntry
  = ExecuteCommand'EnvironmentEntry'_constructor {_ExecuteCommand'EnvironmentEntry'key :: !Data.ByteString.ByteString,
                                                  _ExecuteCommand'EnvironmentEntry'value :: !Data.ByteString.ByteString,
                                                  _ExecuteCommand'EnvironmentEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteCommand'EnvironmentEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExecuteCommand'EnvironmentEntry "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'EnvironmentEntry'key
           (\ x__ y__ -> x__ {_ExecuteCommand'EnvironmentEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteCommand'EnvironmentEntry "value" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteCommand'EnvironmentEntry'value
           (\ x__ y__ -> x__ {_ExecuteCommand'EnvironmentEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExecuteCommand'EnvironmentEntry where
  messageName _
    = Data.Text.pack "ghcserver.ExecuteCommand.EnvironmentEntry"
  packedMessageDescriptor _
    = "\n\
      \\DLEEnvironmentEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand'EnvironmentEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor ExecuteCommand'EnvironmentEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteCommand'EnvironmentEntry'_unknownFields
        (\ x__ y__
           -> x__ {_ExecuteCommand'EnvironmentEntry'_unknownFields = y__})
  defMessage
    = ExecuteCommand'EnvironmentEntry'_constructor
        {_ExecuteCommand'EnvironmentEntry'key = Data.ProtoLens.fieldDefault,
         _ExecuteCommand'EnvironmentEntry'value = Data.ProtoLens.fieldDefault,
         _ExecuteCommand'EnvironmentEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteCommand'EnvironmentEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteCommand'EnvironmentEntry
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
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "EnvironmentEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
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
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExecuteCommand'EnvironmentEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteCommand'EnvironmentEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExecuteCommand'EnvironmentEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_ExecuteCommand'EnvironmentEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.GhcServer_Fields.exitCode' @:: Lens' ExecuteResponse Data.Int.Int32@
         * 'Proto.GhcServer_Fields.stderr' @:: Lens' ExecuteResponse Data.Text.Text@ -}
data ExecuteResponse
  = ExecuteResponse'_constructor {_ExecuteResponse'exitCode :: !Data.Int.Int32,
                                  _ExecuteResponse'stderr :: !Data.Text.Text,
                                  _ExecuteResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExecuteResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExecuteResponse "exitCode" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteResponse'exitCode
           (\ x__ y__ -> x__ {_ExecuteResponse'exitCode = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExecuteResponse "stderr" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExecuteResponse'stderr
           (\ x__ y__ -> x__ {_ExecuteResponse'stderr = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExecuteResponse where
  messageName _ = Data.Text.pack "ghcserver.ExecuteResponse"
  packedMessageDescriptor _
    = "\n\
      \\SIExecuteResponse\DC2\ESC\n\
      \\texit_code\CAN\SOH \SOH(\ENQR\bexitCode\DC2\SYN\n\
      \\ACKstderr\CAN\STX \SOH(\tR\ACKstderr"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        exitCode__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "exit_code"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"exitCode")) ::
              Data.ProtoLens.FieldDescriptor ExecuteResponse
        stderr__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stderr"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"stderr")) ::
              Data.ProtoLens.FieldDescriptor ExecuteResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, exitCode__field_descriptor),
           (Data.ProtoLens.Tag 2, stderr__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExecuteResponse'_unknownFields
        (\ x__ y__ -> x__ {_ExecuteResponse'_unknownFields = y__})
  defMessage
    = ExecuteResponse'_constructor
        {_ExecuteResponse'exitCode = Data.ProtoLens.fieldDefault,
         _ExecuteResponse'stderr = Data.ProtoLens.fieldDefault,
         _ExecuteResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExecuteResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ExecuteResponse
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
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "exit_code"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"exitCode") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "stderr"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"stderr") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExecuteResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"exitCode") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"stderr") _x
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
instance Control.DeepSeq.NFData ExecuteResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExecuteResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExecuteResponse'exitCode x__)
                (Control.DeepSeq.deepseq (_ExecuteResponse'stderr x__) ()))
data GhcServer = GhcServer {}
instance Data.ProtoLens.Service.Types.Service GhcServer where
  type ServiceName GhcServer = "GhcServer"
  type ServicePackage GhcServer = "ghcserver"
  type ServiceMethods GhcServer = '["clean", "execute"]
  packedServiceDescriptor _
    = "\n\
      \\tGhcServer\DC2B\n\
      \\aExecute\DC2\EM.ghcserver.ExecuteCommand\SUB\SUB.ghcserver.ExecuteResponse\"\NUL\DC2<\n\
      \\ENQClean\DC2\ETB.ghcserver.CleanRequest\SUB\CAN.ghcserver.CleanResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl GhcServer "execute" where
  type MethodName GhcServer "execute" = "Execute"
  type MethodInput GhcServer "execute" = ExecuteCommand
  type MethodOutput GhcServer "execute" = ExecuteResponse
  type MethodStreamingType GhcServer "execute" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl GhcServer "clean" where
  type MethodName GhcServer "clean" = "Clean"
  type MethodInput GhcServer "clean" = CleanRequest
  type MethodOutput GhcServer "clean" = CleanResponse
  type MethodStreamingType GhcServer "clean" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DLEghc-server.proto\DC2\tghcserver\"\158\SOH\n\
    \\SOExecuteCommand\DC2\DC2\n\
    \\EOTargv\CAN\SOH \ETX(\fR\EOTargv\DC2<\n\
    \\ETXenv\CAN\STX \ETX(\v2*.ghcserver.ExecuteCommand.EnvironmentEntryR\ETXenv\SUB:\n\
    \\DLEEnvironmentEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue\"F\n\
    \\SIExecuteResponse\DC2\ESC\n\
    \\texit_code\CAN\SOH \SOH(\ENQR\bexitCode\DC2\SYN\n\
    \\ACKstderr\CAN\STX \SOH(\tR\ACKstderr\"\SO\n\
    \\fCleanRequest\"C\n\
    \\rCleanResponse\DC2\CAN\n\
    \\asuccess\CAN\SOH \SOH(\bR\asuccess\DC2\CAN\n\
    \\amessage\CAN\STX \SOH(\tR\amessage2\141\SOH\n\
    \\tGhcServer\DC2B\n\
    \\aExecute\DC2\EM.ghcserver.ExecuteCommand\SUB\SUB.ghcserver.ExecuteResponse\"\NUL\DC2<\n\
    \\ENQClean\DC2\ETB.ghcserver.CleanRequest\SUB\CAN.ghcserver.CleanResponse\"\NULJ\140\n\
    \\n\
    \\ACK\DC2\EOT\NUL\NUL#\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DC2\n\
    \\186\SOH\n\
    \\STX\EOT\NUL\DC2\EOT\ACK\NUL\SO\SOH\SUB\173\SOH Mirrors 'worker.proto''s 'ExecuteCommand'/'ExecuteResponse', duplicated here so 'ghc-server''s own protocol can\n\
    \ evolve independently of Buck's persistent-worker protocol.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ACK\b\SYN\n\
    \\f\n\
    \\EOT\EOT\NUL\ETX\NUL\DC2\EOT\a\STX\n\
    \\ETX\n\
    \\f\n\
    \\ENQ\EOT\NUL\ETX\NUL\SOH\DC2\ETX\a\n\
    \\SUB\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\NUL\DC2\ETX\b\EOT\DC2\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ENQ\DC2\ETX\b\EOT\t\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\SOH\DC2\ETX\b\n\
    \\r\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\NUL\ETX\DC2\ETX\b\DLE\DC1\n\
    \\r\n\
    \\ACK\EOT\NUL\ETX\NUL\STX\SOH\DC2\ETX\t\EOT\DC4\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ENQ\DC2\ETX\t\EOT\t\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\SOH\DC2\ETX\t\n\
    \\SI\n\
    \\SO\n\
    \\a\EOT\NUL\ETX\NUL\STX\SOH\ETX\DC2\ETX\t\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\f\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX\f\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\f\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\f\DC1\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\f\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\r\STX$\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\EOT\DC2\ETX\r\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\r\v\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\r\FS\US\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\r\"#\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\DLE\NUL\DC3\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\DLE\b\ETB\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\DC1\STX\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\DC1\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\DC1\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\DC1\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\DC2\STX\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX\DC2\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\DC2\t\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\DC2\DC2\DC3\n\
    \\235\SOH\n\
    \\STX\EOT\STX\DC2\ETX\ETB\NUL\ETB\SUB\223\SOH Empty: the paths to clean (the project's 'output' and 'cache' directories) are hardcoded server-side rather\n\
    \ than sent by the client, since duplicating that assumption in a client is exactly what this RPC exists to avoid.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\ETB\b\DC4\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\EM\NUL\FS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\EM\b\NAK\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\SUB\STX\DC3\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\SUB\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\SUB\a\SO\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\SUB\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\ETX\ESC\STX\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\ETX\ESC\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\ETX\ESC\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\ETX\ESC\DC3\DC4\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT\RS\NUL#\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\RS\b\DC1\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\US\STX:\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\US\ACK\r\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\US\SO\FS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\US'6\n\
    \\129\SOH\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX\"\STX4\SUBt Removes the project's 'output' and 'cache' directories (server-generated, fully reconstructible build\n\
    \ artifacts).\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX\"\ACK\v\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX\"\f\CAN\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX\"#0b\ACKproto3"