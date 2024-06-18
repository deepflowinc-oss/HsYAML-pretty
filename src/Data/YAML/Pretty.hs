{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.YAML.Pretty (
  Codec (..),
  Context (..),
  encodeYAMLWith,
  fromNodeWith,
  fmapEither,
  fmapMaybe,
  decode1With,
  decode1StrictWith,
  dimapEither,
  dimapMaybe,
  divideCodec,
  json,
  null,
  bool,
  FloatFormatter (..),
  defaultFloatFormatter,
  scientific,
  scientificWith,
  float,
  floatWith,
  IntegerFormatter (..),
  defaultIntegerFormatter,
  integer,
  integerWith,
  integral,
  integralWith,
  Description,
  FieldName,
  ObjectFormatter (..),
  defaultObjectFormatter,
  object,
  objectWith,
  BlockFormatter (..),
  defaultArrayFormatter,
  arrayOf,
  arrayOfWith,
  requiredField',
  requiredField,
  optionalField,
  optionalField',
  ValueFormatters (..),
  defaultValueFormatters,
) where

import Control.Lens (Fold, (^?), _2, _3)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.Bifunctor qualified as Bi
import Data.Bitraversable qualified as Bi
import Data.Bits (Bits, toIntegralSized)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Scientific (FPFormat (..), Scientific, formatScientific, fromFloatDigits, toRealFloat)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as V
import Data.YAML (Node, Scalar (..))
import Data.YAML qualified as Y
import Data.YAML.Event
import GHC.Generics (Generic)
import Prelude hiding (null)

data Context = Value | Object
  deriving (Show, Eq, Ord)

data FloatFormatter = FloatFormatter {format :: !FPFormat, places :: !(Maybe Int)}
  deriving (Show, Generic)

data IntegerFormatter = Base10
  deriving (Show, Eq, Ord, Generic)

type DEvStream = DList Event

newtype BlockFormatter = BlockFormatter
  { style :: NodeStyle
  -- ^ Style (flow or block)
  }
  deriving (Show, Eq, Ord)

data ObjectFormatter = ObjectFormatter
  { style :: !NodeStyle
  -- ^ Value formatter
  , keyOrdering :: !(T.Text -> T.Text -> Ordering)
  }
  deriving (Generic)

type Description = T.Text

type FieldName = T.Text

type TextFormatter = ScalarStyle

data ValueFormatters = ValueFormatters
  { float :: !FloatFormatter
  , integer :: !IntegerFormatter
  , text :: !TextFormatter
  , object :: !ObjectFormatter
  , array :: !BlockFormatter
  }
  deriving (Generic)

defaultValueFormatters :: ValueFormatters
defaultValueFormatters =
  ValueFormatters
    { float = defaultFloatFormatter
    , integer = defaultIntegerFormatter
    , text = defaultTextFormatter
    , object = defaultObjectFormatter
    , array = defaultArrayFormatter
    }

defaultObjectFormatter :: ObjectFormatter
defaultObjectFormatter = ObjectFormatter {style = Block, keyOrdering = compare}

defaultArrayFormatter :: BlockFormatter
defaultArrayFormatter = BlockFormatter {style = Block}

defaultFloatFormatter :: FloatFormatter
defaultFloatFormatter = FloatFormatter {format = Exponent, places = Nothing}

defaultIntegerFormatter :: IntegerFormatter
defaultIntegerFormatter = Base10

defaultTextFormatter :: TextFormatter
defaultTextFormatter = Plain

bool :: Codec 'Value Bool Bool
bool = BoolCodec

scientific :: Codec 'Value Scientific Scientific
scientific = FloatCodec defaultFloatFormatter

scientificWith :: FloatFormatter -> Codec 'Value Scientific Scientific
scientificWith = FloatCodec

float :: (RealFloat a) => Codec 'Value a a
float = dimap fromFloatDigits toRealFloat scientific

integer :: Codec 'Value Integer Integer
integer = IntCodec defaultIntegerFormatter

integerWith :: IntegerFormatter -> Codec 'Value Integer Integer
integerWith = IntCodec

floatWith :: (RealFloat a) => FloatFormatter -> Codec 'Value a a
floatWith = dimap fromFloatDigits toRealFloat . scientificWith

dimapEither ::
  (input' -> input) ->
  (output -> Either String output') ->
  Codec context input output ->
  Codec context input' output'
dimapEither = DiMapCodec

dimapMaybe ::
  (input' -> input) ->
  (output -> Maybe output') ->
  Codec context input output ->
  Codec context input' output'
dimapMaybe f = DiMapCodec f . (maybe (Left "Parse Failed: Nothing") Right .)

fmapEither ::
  (output -> Either String output') ->
  Codec context input output ->
  Codec context input output'
fmapEither = DiMapCodec id

fmapMaybe ::
  (output -> Maybe output') ->
  Codec context input output ->
  Codec context input output'
fmapMaybe f = fmapEither (maybe (Left "Parse Failed: Nothing") Right . f)

integralWith ::
  (Integral a, Bits a) =>
  IntegerFormatter ->
  Codec 'Value a a
integralWith opt =
  dimapEither
    toInteger
    ( \i ->
        maybe
          (Left $ "out of bound: " <> show i)
          Right
          $ toIntegralSized @Integer i
    )
    (integerWith opt)

integral :: (Integral a, Bits a) => Codec 'Value a a
integral = integralWith defaultIntegerFormatter

objectWith :: ObjectFormatter -> Codec 'Object i o -> Codec 'Value i o
objectWith = ObjectCodec

object :: Codec 'Object i o -> Codec 'Value i o
object = ObjectCodec defaultObjectFormatter

arrayOfWith :: BlockFormatter -> Codec 'Value i o -> Codec 'Value (V.Vector i) (V.Vector o)
arrayOfWith = ArrayCodec

arrayOf :: Codec 'Value i o -> Codec 'Value (V.Vector i) (V.Vector o)
arrayOf = arrayOfWith defaultArrayFormatter

requiredField' :: FieldName -> Maybe Description -> Maybe v -> Codec 'Value v v -> Codec 'Object v v
requiredField' = RequiredFieldCodec

requiredField :: FieldName -> Codec 'Value v v -> Codec 'Object v v
requiredField f = requiredField' f Nothing Nothing

optionalField' :: FieldName -> Maybe Description -> Bool -> Codec 'Value v v -> Codec 'Object (Maybe v) (Maybe v)
optionalField' = OptionalFieldCodec

optionalField :: FieldName -> Codec 'Value v v -> Codec 'Object (Maybe v) (Maybe v)
optionalField f = OptionalFieldCodec f Nothing True

type Codec :: Context -> Type -> Type -> Type
data Codec context input output where
  PureCodec :: output -> Codec context input output
  ViaJSON :: !ValueFormatters -> Codec Value J.Value J.Value
  NullCodec :: Codec Value () ()
  BoolCodec :: Codec Value Bool Bool
  FloatCodec :: !FloatFormatter -> Codec Value Scientific Scientific
  IntCodec :: !IntegerFormatter -> Codec Value Integer Integer
  TextCodec :: !ScalarStyle -> Codec Value T.Text T.Text
  ObjectCodec ::
    !ObjectFormatter ->
    Codec Object i o ->
    Codec Value i o
  ArrayCodec ::
    !BlockFormatter ->
    Codec Value i o ->
    Codec Value (V.Vector i) (V.Vector o)
  RequiredFieldCodec ::
    -- | Field name
    !FieldName ->
    -- | Help text
    !(Maybe Description) ->
    -- | Optional default value
    !(Maybe v) ->
    !(Codec Value v v) ->
    Codec Object v v
  OptionalFieldCodec ::
    -- | Field name
    !FieldName ->
    -- | Help text
    !(Maybe Description) ->
    -- | True if print null
    !Bool ->
    !(Codec Value v v) ->
    Codec Object (Maybe v) (Maybe v)
  ApCodec ::
    Codec Object input (output -> output') ->
    Codec Object input output ->
    Codec Object input output'
  DiMapCodec ::
    (input' -> input) ->
    (output -> Either String output') ->
    Codec context input output ->
    Codec context input' output'
  DivideCodec ::
    (input -> (a, b)) ->
    Codec context a output ->
    Codec context b output ->
    Codec context input output

divideCodec :: (input -> (a, b)) -> Codec context a output -> Codec context b output -> Codec context input output
divideCodec = DivideCodec

instance Functor (Codec context input) where
  fmap f (PureCodec x) = PureCodec (f x)
  fmap f (DiMapCodec i o x) = DiMapCodec i (fmap f . o) x
  fmap f x = DiMapCodec id (Right . f) x

instance (context ~ Object) => Applicative (Codec context input) where
  pure = PureCodec
  (<*>) = ApCodec

instance Profunctor (Codec context) where
  dimap i o (PureCodec x) = DiMapCodec i Right $ PureCodec $ o x
  dimap i o (DiMapCodec i' o' x) = DiMapCodec (i' . i) (fmap o . o') x
  dimap i o x = DiMapCodec i (Right . o) x

json :: (FromJSON a, ToJSON a) => Codec Value a a
json = DiMapCodec J.toJSON (eitherResult . J.fromJSON) $ ViaJSON defaultValueFormatters

null :: Codec 'Value () ()
null = NullCodec

eitherResult :: J.Result a -> Either String a
eitherResult = \case
  J.Error e -> Left e
  J.Success a -> Right a

nullEvt :: Event
nullEvt = Scalar Nothing untagged Plain "null"

trueEvt :: Event
trueEvt = Scalar Nothing untagged Plain "true"

falseEvt :: Event
falseEvt = Scalar Nothing untagged Plain "false"

boolEvt :: Bool -> Event
boolEvt = \case
  True -> trueEvt
  False -> falseEvt

floatEvt :: FloatFormatter -> Scientific -> Event
floatEvt FloatFormatter {..} p =
  Scalar Nothing untagged Plain $
    T.pack $
      formatScientific format places p

intEvt :: IntegerFormatter -> Integer -> Event
intEvt Base10 = Scalar Nothing untagged Plain . T.pack . show

textEvt :: TextFormatter -> T.Text -> Event
textEvt = Scalar Nothing untagged

encodeYAMLWith :: Codec Value a x -> a -> LT.Text
encodeYAMLWith codec = writeEventsText . DL.toList . encodeWith_ codec

encodeWith_ :: Codec o a x -> a -> DEvStream
encodeWith_ PureCodec {} _ = mempty
encodeWith_ (ViaJSON !opts) a = go a
  where
    go = \case
      J.Null -> DL.singleton nullEvt
      J.Bool b -> DL.singleton $ boolEvt b
      J.Number n -> DL.singleton $ floatEvt opts.float n
      J.String s -> DL.singleton $ textEvt opts.text s
      J.Array v ->
        DL.singleton (SequenceStart Nothing untagged opts.array.style)
          <> foldMap go v
          <> DL.singleton SequenceEnd
      J.Object o ->
        DL.singleton (MappingStart Nothing untagged opts.array.style)
          <> foldMap (\(f, v) -> DL.cons (Scalar Nothing untagged Plain $ AK.toText f) (go v)) (AKM.toList o)
          <> DL.singleton MappingEnd
encodeWith_ NullCodec () = DL.singleton nullEvt
encodeWith_ BoolCodec p = DL.singleton $ boolEvt p
encodeWith_ (FloatCodec opt) p = DL.singleton $ floatEvt opt p
encodeWith_ (IntCodec opt) p = DL.singleton $ intEvt opt p
encodeWith_ (TextCodec sty) p = DL.singleton $ textEvt sty p
encodeWith_ (ObjectCodec opts v) p =
  DL.singleton (MappingStart Nothing untagged opts.style)
    <> encodeWith_ v p
    <> DL.singleton MappingEnd
encodeWith_ (ArrayCodec opts v) p =
  DL.singleton (SequenceStart Nothing untagged opts.style)
    <> foldMap (encodeWith_ v) p
    <> DL.singleton SequenceEnd
encodeWith_ (DiMapCodec f _ a) x =
  encodeWith_ a (f x)
encodeWith_ (DivideCodec f cl cr) x =
  let (l, r) = f x
   in encodeWith_ cl l <> encodeWith_ cr r
encodeWith_ (ApCodec f x) p =
  encodeWith_ f p <> encodeWith_ x p
encodeWith_ (RequiredFieldCodec f mcomm _ v) p =
  maybe id (DL.cons . Comment) mcomm $
    Scalar Nothing untagged Plain f
      `DL.cons` encodeWith_ v p
encodeWith_ (OptionalFieldCodec f mcomm putNull v) p =
  let withComm =
        foldMap $
          maybe
            id
            (DL.cons . Comment)
            mcomm
   in withComm $ case p of
        Nothing
          | putNull ->
              Just $
                Scalar Nothing untagged Plain f `DL.cons` DL.singleton nullEvt
          | otherwise -> Nothing
        Just x ->
          Just $
            Scalar Nothing untagged Plain f `DL.cons` encodeWith_ v x

fromNodeWith :: Codec Value i a -> Node Pos -> Either (Pos, String) a
fromNodeWith = valueFromNode_

expect ::
  (Show loc) =>
  String ->
  Fold (Node loc) a ->
  Node loc ->
  Either (loc, String) a
expect name p n =
  case n ^? p of
    Nothing -> Left (posOf n, "Expected " <> name <> ", but got: " <> show n)
    Just x -> Right x

expectScalar ::
  (Show loc) =>
  String ->
  Fold Scalar a ->
  Node loc ->
  Either (loc, String) a
expectScalar name p = expect name (#_Scalar . _2 . p)

valueFromNode_ ::
  (Show loc) =>
  Codec Value i a ->
  Node loc ->
  Either (loc, String) a
valueFromNode_ (PureCodec a) _ = pure a
valueFromNode_ (ViaJSON _) v = nodeToJSON v
valueFromNode_ NullCodec v
  | Y.Scalar _ Y.SNull <- v = pure ()
  | otherwise = Left (posOf v, "Expected null, but got: " <> show v)
valueFromNode_ BoolCodec v = expectScalar "bool" #_SBool v
valueFromNode_ FloatCodec {} v = expectScalar "float" #_SFloat v
valueFromNode_ IntCodec {} v = expectScalar "integer" #_SInt v
valueFromNode_ TextCodec {} v = expectScalar "bool" #_SStr v
valueFromNode_ (ObjectCodec _ p) v =
  objectFromNode_ (posOf v) p . Map.fromList
    =<< mapM (Bi.bitraverse (valueFromNode_ (TextCodec Plain)) pure) . Map.toList
    =<< expect "object" (#_Mapping . _3) v
valueFromNode_ (ArrayCodec _ p) v =
  V.imapM
    ( \i ->
        Bi.first (Bi.second ((show i <> "th element: ") <>))
          . valueFromNode_ p
    )
    . V.fromList
    =<< expect "array" (#_Sequence . _3) v
valueFromNode_ (DiMapCodec _ o p) v =
  Bi.first (posOf v,) . o =<< valueFromNode_ p v
valueFromNode_ (DivideCodec _ _ cr) v =
  valueFromNode_ cr v

nodeToJSON :: (Show loc) => Node loc -> Either (loc, String) J.Value
nodeToJSON = \case
  Y.Anchor _ _ n -> nodeToJSON n
  Y.Scalar _ s -> scalarToJSON s
  Y.Sequence _ _ vs ->
    J.Array . V.fromList <$> traverse nodeToJSON vs
  Y.Mapping _ _ dic ->
    J.Object . AKM.fromList
      <$> traverse
        ( \case
            (Y.Scalar sloc t, v) ->
              (,)
                <$> maybe
                  (Left (sloc, "Invalid object key: " <> show t))
                  Right
                  (nodeToScalarText t)
                <*> nodeToJSON v
            (v, _) -> Left (posOf v, "Invalid object key: " <> show v)
        )
        (Map.toList dic)

scalarToJSON :: Scalar -> Either (loc, String) J.Value
scalarToJSON = \case
  Y.SStr t -> Right $ J.String t
  Y.SInt i -> Right $ J.Number $ fromIntegral i
  Y.SFloat f -> Right $ J.Number f
  Y.SBool b -> Right $ J.Bool b
  Y.SNull -> Right J.Null
  Y.SUnknown _ t -> Right $ J.String t

nodeToScalarText :: Scalar -> Maybe AKM.Key
nodeToScalarText (Y.SStr a) = Just $ AK.fromText a
nodeToScalarText (Y.SUnknown _ a) = Just $ AK.fromText a
nodeToScalarText _ = Nothing

posOf :: Node loc -> loc
posOf (Y.Scalar loc _) = loc
posOf (Y.Sequence loc _ _) = loc
posOf (Y.Mapping loc _ _) = loc
posOf (Y.Anchor loc _ _) = loc

decode1With ::
  Codec Value a output ->
  LBS.ByteString ->
  Either (Pos, String) output
decode1With codec = fromNodeWith codec <=< Y.decode1

decode1StrictWith ::
  Codec Value a output ->
  BS.ByteString ->
  Either (Pos, String) output
decode1StrictWith codec = fromNodeWith codec <=< Y.decode1Strict

objectFromNode_ ::
  (Show loc) =>
  loc ->
  Codec 'Object x a ->
  Map T.Text (Node loc) ->
  Either (loc, String) a
objectFromNode_ _ (PureCodec x) _ = pure x
objectFromNode_ pos (RequiredFieldCodec f _ mdef v) dic =
  case Map.lookup f dic of
    Just x -> valueFromNode_ v x
    Nothing
      | Just z <- mdef -> pure z
      | otherwise -> Left (pos, "Missing required field: " <> show f)
objectFromNode_ _ (OptionalFieldCodec f _ _ v) dic =
  mapM (valueFromNode_ v) $ Map.lookup f dic
objectFromNode_ pos (ApCodec f x) dic =
  objectFromNode_ pos f dic <*> objectFromNode_ pos x dic
objectFromNode_ pos (DiMapCodec _ o p) dic =
  Bi.first (pos,) . o =<< objectFromNode_ pos p dic
objectFromNode_ pos (DivideCodec _ _ cr) dic =
  objectFromNode_ pos cr dic
