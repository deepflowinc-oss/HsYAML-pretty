{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Data.YAML.Pretty (
  Codec (..),
  RequiredField (..),
  reqField',
  defaultReq,
  abort,
  OptionalField (..),
  optField',
  defaultOpt,
  Options (..),
  GenericCodecDefault (..),
  genericKeyOrdering,
  withKeyOrdering,
  defaultOptions,
  genericCodecWith,
  genericCodecDefaultWith,
  genericObjectCodecWith,
  genericObjectCodecDefaultWith,
  parserOnly,
  decodeEncode,
  orParse,
  asumParsers,
  (<!>),
  (@=),
  eitherCodec,
  Context (..),
  encode,
  encodeWith,
  fromNodeWith,
  fmapEither,
  fmapMaybe,
  selectCodec,
  decode1,
  decode1With,
  decode1Strict,
  decode1StrictWith,
  dimap,
  dimapEither,
  dimapMaybe,
  maybeCodec,
  divideCodec,
  prod,
  prodWith,
  elem,
  json,
  null,
  bool,
  text,
  textWith,
  TextFormatter,
  ScalarStyle (..),
  NodeStyle (..),
  parseText,
  literalText,
  literalTextWith,
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
  HasObjectCodec (..),
  HasValueCodec (..),
  HasFieldSpec (..),
  FieldSpec (..),
  reqFieldSpec,
  optFieldSpec,
  Selective (..),
  pretty,
) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Lens hiding (Context, index)
import Control.Monad (forM_, when, (<=<))
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.Trans.RWS.Strict (RWS, RWST (..), execRWS)
import Control.Monad.Trans.Writer.CPS qualified as W
import Control.Monad.Writer.Class (tell)
import Control.Selective
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.Bifunctor qualified as Bi
import Data.Bitraversable qualified as Bi
import Data.Bits (Bits, toIntegralSized)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable (foldl', foldlM)
import Data.Function (on)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Int
import Data.Kind (Constraint, Type)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Monoid (Any (..))
import Data.Ord (comparing)
import Data.Pointed (Pointed (..))
import Data.Proxy (Proxy (..))
import Data.Scientific (FPFormat (..), Scientific, floatingOrInteger, formatScientific, fromFloatDigits, toRealFloat)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TB
import Data.Tuple
import Data.Type.Bool
import Data.Type.Equality
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as P
import Data.Vector.Storable qualified as S
import Data.Vector.Unboxed qualified as U
import Data.Void (Void, absurd)
import Data.Word
import Data.YAML (Node, Scalar (..))
import Data.YAML qualified as Y
import Data.YAML.Event
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics
import GHC.Generics qualified as G
import GHC.Records qualified as GHC
import GHC.TypeLits (KnownSymbol, symbolVal, symbolVal')
import Numeric (showHex)
import Numeric.Natural (Natural)
import Prelude hiding (elem, null)
import Prelude qualified hiding (elem)

data Context = Value | Object | Product
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

textWith :: TextFormatter -> Codec 'Value T.Text T.Text
textWith = TextCodec

text :: Codec 'Value T.Text T.Text
text = textWith defaultTextFormatter

decodeEncode ::
  ((context == Product) ~ 'False) =>
  Codec context Void o ->
  Codec context i Void ->
  Codec context i o
decodeEncode p e =
  dimap Right (either id absurd) $ AltCodec p e

parseText :: T.Text -> (T.Text -> Either String output) -> Codec 'Value Void output
parseText = ParseText

literalTextWith :: TextFormatter -> T.Text -> Codec 'Value () ()
literalTextWith = FixedTextCodec

literalText :: T.Text -> Codec 'Value () ()
literalText = literalTextWith Plain

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
  ParseText :: T.Text -> (T.Text -> Either String output) -> Codec Value Void output
  NullCodec :: Codec Value () ()
  BoolCodec :: Codec Value Bool Bool
  FloatCodec :: !FloatFormatter -> Codec Value Scientific Scientific
  IntCodec :: !IntegerFormatter -> Codec Value Integer Integer
  TextCodec :: !ScalarStyle -> Codec Value T.Text T.Text
  FixedTextCodec :: !TextFormatter -> !T.Text -> Codec Value () ()
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
    (((context == Object) || (context == Product)) ~ 'True) =>
    Codec context input (output -> output') ->
    Codec context input output ->
    Codec context input output'
  ElementCodec ::
    Codec Value input output ->
    Codec Product input output
  ProductCodec :: NodeStyle -> Codec Product i o -> Codec Value i o
  AltCodec ::
    ((context == Product) ~ 'False) =>
    Codec context input output ->
    Codec context input' output' ->
    Codec context (Either input input') (Either output output')
  DiMapCodec ::
    (input' -> input) ->
    (output -> Either String output') ->
    Codec context input output ->
    Codec context input' output'
  Fail :: String -> Codec context Void a
  DivideCodec ::
    ((context == Product) ~ 'False) =>
    (input -> (a, b)) ->
    Codec context a output ->
    Codec context b output' ->
    Codec context input (output, output')
  SelectCodec ::
    Codec Object input (Either a b) ->
    Codec Object input (a -> b) ->
    Codec Object input b
  JoinCodec ::
    Codec Object input (Codec Object input output) ->
    Codec Object input output

divideCodec ::
  ((context == Product) ~ 'False) =>
  (input -> (a, b)) ->
  Codec context a output ->
  Codec context b output' ->
  Codec context input (output, output')
divideCodec = DivideCodec

instance Pointed (Codec context i) where
  point = PureCodec

instance Functor (Codec context input) where
  fmap f (PureCodec x) = PureCodec (f x)
  fmap f (DiMapCodec i o x) = DiMapCodec i (fmap f . o) x
  fmap f x = DiMapCodec id (Right . f) x

instance
  (((context == Object) || (context == Product)) ~ True) =>
  Applicative (Codec context input)
  where
  pure = PureCodec
  (<*>) = ApCodec

instance (context ~ Object) => Monad (Codec context input) where
  mx >>= f = JoinCodec $ f <$> mx

instance (context ~ Object) => MonadFail (Codec context Void) where
  fail = Fail

instance (context ~ Object, v ~ Void) => Alternative (Codec context v) where
  empty = Fail "empty"
  l <|> r = dimap absurd (either id id) (AltCodec l r)

infixl 3 <!>

(<!>) ::
  ((context == Product) ~ 'False) =>
  Codec context input output ->
  Codec context input' output ->
  Codec context (Either input input') output
l <!> r = either id id <$> AltCodec l r

eitherCodec ::
  ((context == Product) ~ 'False) =>
  Codec context input output ->
  Codec context input' output' ->
  Codec context (Either input input') (Either output output')
eitherCodec = AltCodec

instance Profunctor (Codec context) where
  dimap i o (PureCodec x) = DiMapCodec i Right $ PureCodec $ o x
  dimap i o (DiMapCodec i' o' x) = DiMapCodec (i' . i) (fmap o . o') x
  dimap i o x = DiMapCodec i (Right . o) x

parserOnly :: Codec context i o -> Codec context Void o
parserOnly = lmap absurd

asumParsers ::
  (Foldable t, (context == Product) ~ 'False) =>
  Codec context i o ->
  t (Codec context Void o) ->
  Codec context i o
asumParsers = foldl' orParse

orParse :: ((context == Product) ~ 'False) => Codec context i o -> Codec context Void o -> Codec context i o
orParse main alt =
  dimap Left (either id id) $ AltCodec main alt

json :: (FromJSON a, ToJSON a) => Codec Value a a
json = DiMapCodec J.toJSON (eitherResult . J.fromJSON) $ ViaJSON defaultValueFormatters

prodWith :: NodeStyle -> Codec 'Product i o -> Codec 'Value i o
prodWith = ProductCodec

infixl 8 @=

(@=) :: Codec v i o -> (i' -> i) -> Codec v i' o
(@=) = flip lmap

data RequiredField b = RequiredField
  { description :: !(Maybe T.Text)
  , defaultValue :: !(Maybe b)
  }
  deriving (Show, Eq, Ord, Generic)

defaultReq :: RequiredField a
defaultReq = RequiredField {description = Nothing, defaultValue = Nothing}

data OptionalField = OptionalField
  { description :: !(Maybe T.Text)
  , showNull :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)

defaultOpt :: OptionalField
defaultOpt = OptionalField {description = Nothing, showNull = True}

reqField' ::
  forall l a b.
  (GHC.HasField l a b, KnownSymbol l) =>
  RequiredField b ->
  Codec Value b b ->
  Codec Object a b
reqField' RequiredField {..} =
  lmap (GHC.getField @l)
    . requiredField' (T.pack $ symbolVal @l Proxy) description defaultValue

optField' ::
  forall l a b.
  (GHC.HasField l a (Maybe b), KnownSymbol l) =>
  OptionalField ->
  Codec Value b b ->
  Codec Object a (Maybe b)
optField' OptionalField {..} =
  lmap (GHC.getField @l)
    . optionalField' (T.pack $ symbolVal @l Proxy) description showNull

prod :: Codec 'Product i o -> Codec 'Value i o
prod = ProductCodec Flow

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

encode :: (HasValueCodec a) => a -> LT.Text
encode = encodeWith valueCodec

encodeWith :: Codec Value a x -> a -> LT.Text
encodeWith codec = writeEventsText . adjustComment . DL.toList . (DL.fromList [StreamStart, DocumentStart NoDirEndMarker] <>) . (<> DL.fromList [DocumentEnd False, StreamEnd]) . encodeWith_ codec

adjustComment :: [Event] -> [Event]
adjustComment [] = []
adjustComment (st@StreamStart {} : com@Comment {} : xs) =
  com : adjustComment (st : xs)
adjustComment (x : xs) = x : adjustComment xs

encodeWith_ :: ((c == Object) ~ 'False) => Codec c i o -> i -> DEvStream
encodeWith_ PureCodec {} _ = mempty
encodeWith_ (FixedTextCodec sty t) () = DL.singleton $ textEvt sty t
encodeWith_ (Fail _) x = absurd x
encodeWith_ (AltCodec el _) (Left l) = encodeWith_ el l
encodeWith_ (AltCodec _ er) (Right r) = encodeWith_ er r
encodeWith_ ParseText {} b = absurd b
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
        encodeObjLike opts.object go $
          fmap (Nothing,) $
            Map.mapKeys AK.toText $
              AKM.toMap o
encodeWith_ NullCodec () = DL.singleton nullEvt
encodeWith_ BoolCodec p = DL.singleton $ boolEvt p
encodeWith_ (FloatCodec opt) p = DL.singleton $ floatEvt opt p
encodeWith_ (IntCodec opt) p = DL.singleton $ intEvt opt p
encodeWith_ (TextCodec sty) p = DL.singleton $ textEvt sty p
encodeWith_ (ObjectCodec opts v) p =
  encodeObjLike opts id (encodeFieldMap v p)
encodeWith_ (ArrayCodec opts v) p =
  DL.singleton (SequenceStart Nothing untagged opts.style)
    <> foldMap (encodeWith_ v) p
    <> DL.singleton SequenceEnd
encodeWith_ (ProductCodec opt v) p =
  DL.singleton
    (SequenceStart Nothing untagged opt)
    <> encodeWith_ v p
    <> DL.singleton SequenceEnd
encodeWith_ (ElementCodec a) x = encodeWith_ a x
encodeWith_ (DiMapCodec f _ a) x = encodeWith_ a (f x)
encodeWith_ (DivideCodec f cl cr) x =
  let (l, r) = f x
   in encodeWith_ cl l <> encodeWith_ cr r
encodeWith_ (ApCodec f x) p =
  encodeWith_ f p <> encodeWith_ x p

encodeFieldMap :: Codec 'Object a x -> a -> Map T.Text (Maybe T.Text, DEvStream)
encodeFieldMap = fmap snd . go mempty
  where
    go :: Map T.Text (Maybe T.Text, DEvStream) -> Codec 'Object v x -> v -> (x, Map T.Text (Maybe T.Text, DEvStream))
    go !acc (PureCodec x) _ = (x, acc)
    go !acc (JoinCodec x) a =
      let (x', acc') = go acc x a
       in go acc' x' a
    go !acc (ApCodec l r) a =
      let (f, acc') = go acc l a
          (x, acc'') = go acc' r a
       in (f x, acc'')
    go !acc (RequiredFieldCodec f mcomm _ v) p =
      (p, Map.insert f (mcomm, encodeWith_ v p) acc)
    go !acc (OptionalFieldCodec f mcomm showN v) p =
      let acc' = case p of
            Nothing
              | showN -> Map.insert f (mcomm, DL.singleton nullEvt) acc
              | otherwise -> acc
            Just x -> Map.insert f (mcomm, encodeWith_ v x) acc
       in (p, acc')
    go !acc (AltCodec l _) (Left x) = Bi.first Left $ go acc l x
    go !acc (AltCodec _ r) (Right x) = Bi.first Right $ go acc r x
    go !acc (SelectCodec alts decons) x =
      case go acc alts x of
        (Right v, acc') -> (v, acc')
        (Left b, acc') ->
          let (dec, acc'') = go acc' decons x
           in (dec b, acc'')
    go !acc (DiMapCodec l r a) x =
      Bi.first (either (error . ("dimap in encodeFieldMap" <>)) id . r) $
        go acc a $
          l x
    go !_ (Fail _) x = absurd x
    go !acc (DivideCodec f cl cr) x =
      let (l, r) = f x
          (l', acc') = go acc cl l
          (r', acc'') = go acc' cr r
       in ((l', r'), acc'')

encodeObjLike :: ObjectFormatter -> (a -> DEvStream) -> Map T.Text (Maybe T.Text, a) -> DEvStream
encodeObjLike opts go obj =
  DL.singleton (MappingStart Nothing untagged opts.style)
    <> foldMap
      ( \(f, (mcomm, v)) ->
          foldMap (DL.singleton . Comment . (" " <>)) mcomm
            <> DL.cons
              (Scalar Nothing untagged Plain f)
              (go v)
      )
      (sortBy (opts.keyOrdering `on` fst) $ Map.toList obj)
    <> DL.singleton MappingEnd

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
valueFromNode_ (Fail msg) x = Left (posOf x, msg)
valueFromNode_ (PureCodec a) _ = pure a
valueFromNode_ (AltCodec el er) v =
  case valueFromNode_ el v of
    Right x -> Right (Left x)
    Left {} -> valueFromNode_ (Right <$> er) v
valueFromNode_ (ViaJSON _) v = nodeToJSON v
valueFromNode_ (ParseText placeholder p) v =
  Bi.first (posOf v,) . p =<< expectScalar (T.unpack placeholder) #_SStr v
valueFromNode_ NullCodec v
  | Y.Scalar _ Y.SNull <- v = pure ()
  | otherwise = Left (posOf v, "Expected null, but got: " <> show v)
valueFromNode_ BoolCodec v = expectScalar "bool" #_SBool v
valueFromNode_ FloatCodec {} v = expectScalar "float" #_SFloat v
valueFromNode_ IntCodec {} v = expectScalar "integer" #_SInt v
valueFromNode_ TextCodec {} v = expectScalar "bool" #_SStr v
valueFromNode_ (FixedTextCodec _ t) v = expectScalar (T.unpack t) (#_SStr . only t) v
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
valueFromNode_ (ProductCodec _ x) v =
  (\(a, p) -> if Prelude.null p then pure a else Left (posOf v, "Invalid length of tuple"))
    =<< (\(loc, _, p) -> productFromNode_ loc x p)
    =<< expect "array" #_Sequence v
valueFromNode_ (DiMapCodec _ o p) v =
  Bi.first (posOf v,) . o =<< valueFromNode_ p v
valueFromNode_ (DivideCodec _ cl cr) v =
  (,) <$> valueFromNode_ cl v <*> valueFromNode_ cr v

productFromNode_ ::
  (Show loc) =>
  loc ->
  Codec 'Product i a ->
  [Node loc] ->
  Either (loc, String) (a, [Node loc])
productFromNode_ loc (Fail msg) _ = Left (loc, msg)
productFromNode_ loc (DiMapCodec _ r v) xs =
  productFromNode_ loc v xs >>= \(a, rest) ->
    Bi.bimap (loc,) (,rest) $ r a
productFromNode_ _ (PureCodec v) xs = pure (v, xs)
productFromNode_ _ (ElementCodec v) (x : xs) = (,xs) <$> valueFromNode_ v x
productFromNode_ loc (ElementCodec _) [] = Left (loc, "Insufficient length of tuple")
productFromNode_ loc (ApCodec l r) xs = do
  (f, rest) <- productFromNode_ loc l xs
  (x, rest') <- productFromNode_ loc r rest
  pure (f x, rest')

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

decode1 ::
  (HasValueCodec a) =>
  LBS.ByteString ->
  Either (Pos, String) a
decode1 = decode1With valueCodec

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

decode1Strict :: (HasValueCodec a) => BS.ByteString -> Either (Pos, String) a
decode1Strict = decode1StrictWith valueCodec

objectFromNode_ ::
  (Show loc) =>
  loc ->
  Codec 'Object x a ->
  Map T.Text (Node loc) ->
  Either (loc, String) a
objectFromNode_ _ (PureCodec x) _ = pure x
objectFromNode_ loc (Fail msg) _ = Left (loc, msg)
objectFromNode_ pos (AltCodec el er) dic =
  case objectFromNode_ pos (Left <$> el) dic of
    Right v -> pure v
    Left {} -> objectFromNode_ pos (Right <$> er) dic
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
objectFromNode_ pos (SelectCodec alts fcons) dic = do
  alt <- objectFromNode_ pos alts dic
  case alt of
    Right a -> pure a
    Left b -> do
      dec <- objectFromNode_ pos fcons dic
      pure $ dec b
objectFromNode_ pos (JoinCodec alts) dic = do
  alt <- objectFromNode_ pos alts dic
  objectFromNode_ pos alt dic
objectFromNode_ pos (DivideCodec _ cl cr) dic =
  (,)
    <$> objectFromNode_ pos cl dic
    <*> objectFromNode_ pos cr dic

class HasValueCodec a where
  valueCodec :: Codec Value a a

class HasObjectCodec a where
  objectCodec :: Codec Object a a

instance HasValueCodec Word where
  valueCodec = integral

instance HasValueCodec Word64 where
  valueCodec = integral

instance HasValueCodec Word32 where
  valueCodec = integral

instance HasValueCodec Word16 where
  valueCodec = integral

instance HasValueCodec Word8 where
  valueCodec = integral

instance HasValueCodec Int where
  valueCodec = integral

instance HasValueCodec Int64 where
  valueCodec = integral

instance HasValueCodec Int32 where
  valueCodec = integral

instance HasValueCodec Int16 where
  valueCodec = integral

instance HasValueCodec Int8 where
  valueCodec = integral

instance HasValueCodec Natural where
  valueCodec = integral

instance HasValueCodec Double where
  valueCodec = float

instance HasValueCodec Float where
  valueCodec = float

instance HasValueCodec T.Text where
  valueCodec = text

instance HasValueCodec LT.Text where
  valueCodec = dimap LT.toStrict LT.fromStrict text

instance {-# OVERLAPPING #-} HasValueCodec String where
  valueCodec = dimap T.pack T.unpack text

instance {-# OVERLAPPABLE #-} (HasValueCodec a) => HasValueCodec [a] where
  valueCodec = dimap V.fromList V.toList $ arrayOf valueCodec

instance (HasValueCodec a) => HasValueCodec (V.Vector a) where
  valueCodec = arrayOf valueCodec

instance (HasValueCodec a, U.Unbox a) => HasValueCodec (U.Vector a) where
  valueCodec = dimap U.convert (U.convert @V.Vector) valueCodec

instance (HasValueCodec a, S.Storable a) => HasValueCodec (S.Vector a) where
  valueCodec = dimap U.convert (U.convert @V.Vector) valueCodec

instance (HasValueCodec a, P.Prim a) => HasValueCodec (P.Vector a) where
  valueCodec = dimap U.convert (U.convert @V.Vector) valueCodec

instance (HasValueCodec a) => HasValueCodec (Maybe a) where
  valueCodec = maybeCodec valueCodec

instance (HasValueCodec a) => HasValueCodec (Solo a) where
  valueCodec = dimap Solo (\(Solo v) -> v) valueCodec

instance (HasValueCodec a, HasValueCodec b) => HasValueCodec (a, b) where
  valueCodec =
    prod $
      (,)
        <$> lmap fst (elem valueCodec)
        <*> lmap snd (elem valueCodec)

instance
  (HasValueCodec a, HasValueCodec b, HasValueCodec c) =>
  HasValueCodec (a, b, c)
  where
  valueCodec =
    prod $
      (,,)
        <$> lmap (view _1) (elem valueCodec)
        <*> lmap (view _2) (elem valueCodec)
        <*> lmap (view _3) (elem valueCodec)

instance
  (HasValueCodec a, HasValueCodec b, HasValueCodec c, HasValueCodec d) =>
  HasValueCodec (a, b, c, d)
  where
  valueCodec =
    prod $
      (,,,)
        <$> lmap (view _1) (elem valueCodec)
        <*> lmap (view _2) (elem valueCodec)
        <*> lmap (view _3) (elem valueCodec)
        <*> lmap (view _4) (elem valueCodec)

instance
  (HasValueCodec a, HasValueCodec b, HasValueCodec c, HasValueCodec d, HasValueCodec e) =>
  HasValueCodec (a, b, c, d, e)
  where
  valueCodec =
    prod $
      (,,,,)
        <$> lmap (view _1) (elem valueCodec)
        <*> lmap (view _2) (elem valueCodec)
        <*> lmap (view _3) (elem valueCodec)
        <*> lmap (view _4) (elem valueCodec)
        <*> lmap (view _5) (elem valueCodec)

elem :: Codec 'Value input output -> Codec 'Product input output
elem = ElementCodec

maybeCodec :: Codec Value i o -> Codec Value (Maybe i) (Maybe o)
maybeCodec codec = dimap (maybe (Right ()) Left) (either Just (const Nothing)) $ AltCodec codec null

data Flavour = Simple | WithDefault

data Options = Options
  { fieldLabelModifier :: String -> String
  , constructorTagModifier :: String -> String
  , keyOrdering :: T.Text -> T.Text -> Ordering
  , objectStyle :: NodeStyle
  }
  deriving (Generic)

defaultOptions :: Options
defaultOptions =
  Options
    { fieldLabelModifier = id
    , constructorTagModifier = id
    , keyOrdering = compare
    , objectStyle = Block
    }

type GHasValueCodec :: Flavour -> (Type -> Type) -> Constraint
class GHasValueCodec flav f where
  gvalueCodecWith :: Proxy# flav -> Options -> Codec Value (f ()) (f ())

type GHasProductCodec :: Flavour -> (Type -> Type) -> Constraint
class GHasProductCodec flav f where
  gprodCodecWith :: Proxy# flav -> Options -> Codec Product (f ()) (f ())

instance GHasProductCodec flav U1 where
  gprodCodecWith _ _ = pure U1

instance
  ( m ~ 'Nothing
  , GHasValueCodec flav f
  ) =>
  GHasProductCodec flav (S1 ('MetaSel m x y z) f)
  where
  gprodCodecWith _ opt = elem $ dimap unM1 M1 $ gvalueCodecWith (proxy# @flav) opt

instance
  (GHasProductCodec flav l, GHasProductCodec flav r) =>
  GHasProductCodec flav (l :*: r)
  where
  gprodCodecWith p opt =
    (:*:)
      <$> lmap (\(l :*: _) -> l) (gprodCodecWith p opt)
      <*> lmap (\(_ :*: r) -> r) (gprodCodecWith p opt)

data FieldSpec a = FieldSpec
  { description :: Maybe T.Text
  , defaultValue :: Maybe a
  , showNull :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

reqFieldSpec :: FieldSpec a
reqFieldSpec =
  FieldSpec
    { description = Nothing
    , defaultValue = Nothing
    , showNull = False
    }

optFieldSpec :: FieldSpec a
optFieldSpec =
  FieldSpec
    { description = Nothing
    , defaultValue = Nothing
    , showNull = True
    }

class HasFieldSpec a where
  fieldSpec :: FieldSpec a

type GHasObjectCodec :: Flavour -> (Type -> Type) -> Constraint
class GHasObjectCodec flav f where
  gobjCodecWith :: Proxy# flav -> Options -> Codec Object (f ()) (f ())

instance
  (GHasObjectCodec f l, GHasObjectCodec f r) =>
  GHasObjectCodec f (l :*: r)
  where
  gobjCodecWith p opts =
    (:*:)
      <$> lmap (\(l :*: _) -> l) (gobjCodecWith p opts)
      <*> lmap (\(_ :*: r) -> r) (gobjCodecWith p opts)

instance {-# OVERLAPPABLE #-} (GHasObjectCodec flav f) => GHasObjectCodec flav (M1 i c f) where
  gobjCodecWith p = dimap unM1 M1 . gobjCodecWith p

instance
  {-# OVERLAPPING #-}
  ( m ~ 'Just sel
  , KnownSymbol sel
  , HasValueCodec a
  ) =>
  GHasObjectCodec 'Simple (S1 ('MetaSel m x y z) (K1 i (Maybe a)))
  where
  gobjCodecWith _ opt =
    dimap (unK1 . unM1) (M1 . K1) $
      optionalField' (T.pack $ opt.fieldLabelModifier $ symbolVal @sel Proxy) Nothing True valueCodec

instance
  {-# OVERLAPPABLE #-}
  ( m ~ 'Just sel
  , KnownSymbol sel
  , HasValueCodec a
  ) =>
  GHasObjectCodec 'Simple (S1 ('MetaSel m x y z) (K1 i a))
  where
  gobjCodecWith _ opts =
    dimap (unK1 . unM1) (M1 . K1) $
      requiredField' (T.pack $ opts.fieldLabelModifier $ symbolVal @sel Proxy) Nothing Nothing (valueCodec @a)

instance
  {-# OVERLAPPING #-}
  ( m ~ 'Just sel
  , KnownSymbol sel
  , HasValueCodec a
  , HasFieldSpec a
  ) =>
  GHasObjectCodec WithDefault (S1 ('MetaSel m x y z) (K1 i (Maybe a)))
  where
  gobjCodecWith _ opts =
    let FieldSpec {..} = fieldSpec @a
     in dimap (unK1 . unM1) (M1 . K1) $
          optionalField' (T.pack $ opts.fieldLabelModifier $ symbolVal @sel Proxy) description showNull (valueCodec @a)

instance
  {-# OVERLAPPING #-}
  ( m ~ 'Just sel
  , KnownSymbol sel
  , HasValueCodec a
  , HasFieldSpec a
  ) =>
  GHasObjectCodec WithDefault (S1 ('MetaSel m x y z) (K1 i a))
  where
  gobjCodecWith _ opts =
    let FieldSpec {..} = fieldSpec @a
     in dimap (unK1 . unM1) (M1 . K1) $
          requiredField' (T.pack $ opts.fieldLabelModifier $ symbolVal @sel Proxy) description defaultValue (valueCodec @a)

instance (HasValueCodec c) => GHasValueCodec flav (K1 i c) where
  gvalueCodecWith _ _ = dimap unK1 K1 valueCodec

data ProdType = IsProd | IsObj

data SProdType pt where
  SIsProd :: SProdType IsProd
  SIsObj :: SProdType IsObj

class KnownProdType pt where
  sProdType :: SProdType pt

instance KnownProdType 'IsProd where
  sProdType = SIsProd

instance KnownProdType 'IsObj where
  sProdType = SIsObj

type family ProductType f where
  ProductType (l :*: r) = ProductType l
  ProductType (S1 (MetaSel ('Just _) _ _ _) _) = IsObj
  ProductType (S1 (MetaSel 'Nothing _ _ _) _) = IsProd
  ProductType U1 = IsProd

type family GHasGoodContext mode pt where
  GHasGoodContext mode IsProd = GHasProductCodec mode
  GHasGoodContext mode IsObj = GHasObjectCodec mode

instance {-# OVERLAPPABLE #-} (GHasValueCodec mode f) => GHasValueCodec mode (M1 i c f) where
  gvalueCodecWith p = dimap unM1 M1 . gvalueCodecWith p

instance
  ( GHasGoodContext mode (ProductType (l :*: r)) (l :*: r)
  , KnownProdType (ProductType (l :*: r))
  ) =>
  GHasValueCodec mode (l :*: r)
  where
  gvalueCodecWith p opts = case sProdType @(ProductType (l :*: r)) of
    SIsProd -> prod $ gprodCodecWith p opts
    SIsObj ->
      objectWith
        defaultObjectFormatter
          { style = opts.objectStyle
          , keyOrdering = opts.keyOrdering
          }
        $ gobjCodecWith p opts

instance {-# OVERLAPPING #-} (Constructor i) => GHasValueCodec mode (C1 i U1) where
  gvalueCodecWith _ opts =
    dimap (\(M1 U1) -> ()) (\() -> M1 U1) $
      literalText $
        T.pack $
          opts.constructorTagModifier $
            conName (undefined :: C1 i U1 ())

instance (GHasValueCodec mode l, GHasValueCodec mode r) => GHasValueCodec mode (l :+: r) where
  gvalueCodecWith p opts =
    lmap (\case L1 x -> Left x; R1 y -> Right y) $
      L1 <$> gvalueCodecWith p opts <!> R1 <$> gvalueCodecWith p opts

instance
  (GHasValueCodec 'Simple (Rep x), Generic x) =>
  HasValueCodec (Generically x)
  where
  valueCodec = dimap (\(Generically a) -> G.from a) (Generically . G.to) $ gvalueCodecWith (proxy# @'Simple) defaultOptions

genericCodecWith :: (GHasValueCodec 'Simple (Rep a), Generic a) => Options -> Codec 'Value a a
genericCodecWith = dimap G.from G.to . gvalueCodecWith (proxy# @'Simple)

newtype GenericCodecDefault a = GenericCodecDefault a
  deriving (Show, Eq, Ord)

instance
  (GHasValueCodec 'WithDefault (Rep x), Generic x) =>
  HasValueCodec (GenericCodecDefault x)
  where
  valueCodec = dimap (\(GenericCodecDefault a) -> G.from a) (GenericCodecDefault . G.to) $ gvalueCodecWith (proxy# @'WithDefault) defaultOptions

class GHasKeyOrdering f where
  gkeyorder' :: Proxy# f -> DList T.Text

instance
  {-# OVERLAPPING #-}
  (KnownSymbol s, m ~ 'Just s) =>
  GHasKeyOrdering (S1 (MetaSel m x y z) f)
  where
  gkeyorder' _ = DL.singleton $ T.pack $ symbolVal' @s proxy#

instance GHasKeyOrdering U1 where
  gkeyorder' _ = mempty

instance {-# OVERLAPPABLE #-} (GHasKeyOrdering f) => GHasKeyOrdering (M1 i c f) where
  gkeyorder' _ = gkeyorder' @f proxy#

instance (GHasKeyOrdering f, GHasKeyOrdering g) => GHasKeyOrdering (f :*: g) where
  gkeyorder' _ = gkeyorder' (proxy# @f) <> gkeyorder' (proxy# @g)

type GenericKeyOrdering a = GHasKeyOrdering (Rep a)

genericKeyOrdering :: forall a. (GenericKeyOrdering a) => T.Text -> T.Text -> Ordering
genericKeyOrdering =
  let dic = Map.fromList $ zip (DL.toList $ gkeyorder' @(Rep a) proxy#) [0 ..]
      len = Map.size dic
   in comparing $ \k ->
        maybe (len, k) (,k) $ Map.lookup k dic

withKeyOrdering :: forall a. (GenericKeyOrdering a) => Options -> Options
withKeyOrdering = #keyOrdering .~ genericKeyOrdering @a

genericCodecDefaultWith ::
  (GHasValueCodec 'WithDefault (Rep a), Generic a) =>
  Options ->
  Codec 'Value a a
genericCodecDefaultWith =
  dimap G.from G.to . gvalueCodecWith (proxy# @'WithDefault)

genericObjectCodecWith ::
  (GHasObjectCodec 'Simple (Rep a), Generic a) =>
  Options ->
  Codec 'Object a a
genericObjectCodecWith =
  dimap G.from G.to . gobjCodecWith (proxy# @'Simple)

genericObjectCodecDefaultWith ::
  (GHasObjectCodec 'WithDefault (Rep a), Generic a) =>
  Options ->
  Codec 'Object a a
genericObjectCodecDefaultWith =
  dimap G.from G.to . gobjCodecWith (proxy# @'WithDefault)

selectCodec :: Codec 'Object input (Either a b) -> Codec 'Object input (a -> b) -> Codec 'Object input b
selectCodec = SelectCodec

instance (ctx ~ Object) => Selective (Codec ctx input) where
  select = selectCodec

abort :: String -> Codec context Void a
abort = Fail

data CommentInfo = NoComment | HasComment
  deriving (Show, Eq, Ord, Generic)

data PrinterContext ctx where
  TopLevel :: PrinterContext 'Value
  ExpectFieldLabel :: !NodeStyle -> !CommentInfo -> PrinterContext 'Object
  ExpectFieldValue :: NodeStyle -> PrinterContext 'Value
  ExpectArrayElem :: {style :: !NodeStyle, index :: !Int} -> PrinterContext 'Value

deriving instance Show (PrinterContext ctx)

data PrintEnv ctx = PrintEnv
  {level :: !Word, context :: !(PrinterContext ctx)}
  deriving (Show, Generic)

newtype Printer ctx a = Printer {unPrinter :: RWS () TB.Builder (PrintEnv ctx) a}
  deriving (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (PrintEnv ctx)
    )

pretty :: Codec Value a x -> a -> TB.Builder
pretty codec x =
  snd $
    execRWS (prettyValue codec x).unPrinter () $
      PrintEnv {level = 0, context = TopLevel}

prettyValue :: Codec Value i x -> i -> Printer Value ()
prettyValue PureCodec {} _ = pure ()
prettyValue (ViaJSON fs) val = prettyJSON fs val
prettyValue Fail {} x = absurd x
prettyValue (ParseText _ _) x = absurd x
prettyValue NullCodec () = putNull
prettyValue BoolCodec p = putBool p
prettyValue (FloatCodec fmt) p =
  putFloat fmt p
prettyValue (IntCodec Base10) p = putInteger p
prettyValue (TextCodec sty) p = putText sty p
prettyValue (FixedTextCodec sty x) () = putText sty x
prettyValue (ArrayCodec sty av) p = putArray sty av p
prettyValue (ObjectCodec opts v) p = putObject opts v p
prettyValue (ProductCodec sty v) p = putProduct sty v p
prettyValue (AltCodec el _) (Left l) = prettyValue el l
prettyValue (AltCodec _ er) (Right r) = prettyValue er r
prettyValue (DiMapCodec f _ p) x = prettyValue p $ f x
prettyValue (DivideCodec f cl cr) x =
  let (l, r) = f x
   in prettyValue cl l >> prettyValue cr r

putFloat :: FloatFormatter -> Scientific -> Printer 'Value ()
putFloat FloatFormatter {..} p =
  putLit $ fromString $ formatScientific format places p

putBool :: Bool -> Printer 'Value ()
putBool p = putLit $ if p then "true" else "false"

putNull :: Printer 'Value ()
putNull = putLit "null"

localState :: PrintEnv ctx -> Printer ctx a -> Printer ctx' a
localState s' act = Printer $ RWST \r s -> do
  (a, _, w) <- runRWST act.unPrinter r s'
  pure (a, s, w)

putProduct ::
  NodeStyle ->
  Codec Product i x ->
  i ->
  Printer Value ()
putProduct sty = go
  where
    go :: Codec Product i x -> i -> Printer Value ()
    go PureCodec {} _ = pure ()
    go Fail {} x = absurd x
    go (DiMapCodec f _ v) x = go v (f x)
    go (ApCodec l r) x = go l x *> go r x
    go (ElementCodec a) x = do
      lvl <- gets (.level)
      localState
        PrintEnv {level = lvl + 2, context = ExpectArrayElem sty 0}
        $ prettyValue a x

showHexExp2 :: Int -> TB.Builder
showHexExp2 n =
  let orig = T.pack $ showHex n ""
      len = T.length orig
   in if
        | len <= 2 ->
            "\\0x" <> TB.fromText (T.replicate (2 - len) "0") <> TB.fromText orig
        | len <= 4 ->
            "\\0u" <> TB.fromText (T.replicate (4 - len) "0") <> TB.fromText orig
        | otherwise ->
            "\\0U" <> TB.fromText (T.replicate (8 - len) "0") <> TB.fromText orig

putText :: ScalarStyle -> T.Text -> Printer Value ()
putText sty = render' <$> selectSuitableStyle sty <*> id
  where
    escape '\NUL' = "\\0"
    escape '\a' = "\\a"
    escape '\b' = "\\b"
    escape '\t' = "\\t"
    escape '\n' = "\\n"
    escape '\v' = "\\v"
    escape '\f' = "\\f"
    escape '\r' = "\\r"
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\x85' = "\\N"
    escape '\xA0' = "\\_"
    escape '\x2028' = "\\L"
    escape '\x2029' = "\\P"
    escape c
      | C.isPrint c = TB.fromText $ T.singleton c
      | otherwise = showHexExp2 $ C.ord c

    render' Plain t = putLit $ TB.fromText t
    render' DoubleQuoted t = do
      putLit $ "\"" <> foldMap escape (T.unpack t) <> "\""
    render' SingleQuoted t =
      putLit $ "'" <> TB.fromText (T.replace "'" "''" t) <> "'"
    render' (Folded ch ind) t = render' (Literal ch ind) t
    render' (Literal ch ind) t = do
      i <- gets (.level)
      let (offInd, off) = toIndOff ind
          chompInd = renderChomp ch
          ls = T.splitOn "\n" t
          indent = fromIntegral i + off
      putLit $
        "|"
          <> offInd
          <> chompInd
          <> "\n"
          <> foldMap
            ( \l ->
                fromString (replicate indent ' ') <> TB.fromText l <> "\n"
            )
            ls

renderChomp :: Chomp -> TB.Builder
renderChomp Strip = "-"
renderChomp Clip = ""
renderChomp Keep = "+"

toIndOff :: IndentOfs -> (TB.Builder, Int)
toIndOff = maybe ("", 2) ((,) <$> fromString . show <*> id) . fromInd

fromInd :: IndentOfs -> Maybe Int
fromInd IndentAuto = Nothing
fromInd IndentOfs1 = Just 1
fromInd IndentOfs2 = Just 2
fromInd IndentOfs3 = Just 3
fromInd IndentOfs4 = Just 4
fromInd IndentOfs5 = Just 5
fromInd IndentOfs6 = Just 6
fromInd IndentOfs7 = Just 7
fromInd IndentOfs8 = Just 8
fromInd IndentOfs9 = Just 9

breakChar :: Char -> Bool
breakChar c = c == '\r' || c == '\n'

nonBreakChar :: Char -> Bool
nonBreakChar c =
  C.isPrint c && not (breakChar c) && c /= '\xFEFF'

whiteChar :: Char -> Bool
whiteChar c = c == '\t' || c == ' '

nonspaceChar :: Char -> Bool
nonspaceChar c = nonBreakChar c && not (whiteChar c)

validPlainStart :: T.Text -> Bool
validPlainStart t =
  case T.uncons t of
    Nothing -> False
    Just (c, rest) ->
      nonspaceChar c
        || (c == '?' || c == ':' || c == '-')
          && maybe False (isPlainSafe . fst) (T.uncons rest)

isPlainSafe :: Char -> Bool
isPlainSafe c = nonspaceChar c && c /= ',' && c /= '[' && c /= ']' && c /= '{' && c /= '}'

validPlainText :: T.Text -> Bool
validPlainText txt
  | validPlainStart txt = go Nothing txt
  where
    go :: Maybe Char -> T.Text -> Bool
    go _ "" = True
    go (Just cls) x =
      case T.uncons x of
        Nothing
          | ':' <- cls -> False
          | otherwise -> True
        Just (c, rest) ->
          case c of
            ':' -> go (Just c) rest
            '#'
              | nonspaceChar c -> go (Just c) rest
              | otherwise -> False
            _
              | nonspaceChar c -> go (Just c) rest
              | otherwise -> False
    go Nothing t =
      case T.uncons t of
        Nothing -> False
        Just (c, rest) -> go (Just c) rest
validPlainText _ = False

selectSuitableStyle :: ScalarStyle -> T.Text -> ScalarStyle
selectSuitableStyle DoubleQuoted _ = DoubleQuoted
selectSuitableStyle Plain t
  | validPlainText t = Plain
  | otherwise = DoubleQuoted
selectSuitableStyle SingleQuoted t =
  let nonPrint = T.any (not . C.isPrint) t
      emptyLine = "\n\n" `T.isInfixOf` t
      leadingTrailingSpaces =
        any
          ( on (||) (not . T.null)
              <$> T.takeWhile C.isSpace
              <*> T.takeWhileEnd C.isSpace
          )
          $ T.lines t
      invalid = nonPrint || emptyLine || leadingTrailingSpaces
   in if invalid then DoubleQuoted else SingleQuoted
selectSuitableStyle l@Literal {} t =
  if T.any (not . nonBreakChar) t
    then DoubleQuoted
    else l
selectSuitableStyle l@Folded {} t =
  if T.any (not . nonBreakChar) t
    then DoubleQuoted
    else l

data ObjectStatus = ObjectStatus
  { objects :: Map T.Text (Maybe T.Text, Printer 'Value ())
  , anyComments :: !Any
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via Generically ObjectStatus

collectObjects :: Codec 'Object i x -> i -> ObjectStatus
collectObjects = fmap W.execWriter . go
  where
    go :: Codec 'Object a x -> a -> W.Writer ObjectStatus x
    go (PureCodec x) _ = pure x
    go Fail {} x = absurd x
    go (AltCodec el _) (Left l) = Left <$> go el l
    go (AltCodec _ er) (Right r) = Right <$> go er r
    go (DiMapCodec f g v) x =
      go v (f x) <&> \a ->
        case g a of
          Right y -> y
          Left err -> error $ "collectObjects: DiMapCodec: " <> err
    go (RequiredFieldCodec f mcomm _ v) p = do
      W.tell $
        ObjectStatus
          { objects =
              Map.singleton f (mcomm, prettyValue v p)
          , anyComments = Any $ isJust mcomm
          }
      pure p
    go (OptionalFieldCodec f mcomm prNull v) p = do
      when (isJust mcomm || prNull) $
        W.tell $
          ObjectStatus
            { objects =
                Map.singleton f (mcomm, maybe putNull (prettyValue v) p)
            , anyComments = Any $ isJust mcomm
            }
      pure p
    go (ApCodec l r) x = go l x <*> go r x
    go (DivideCodec f lp rp) x = do
      let (l, r) = f x
      l' <- go lp l
      r' <- go rp r
      pure (l', r')
    go (SelectCodec eith mp) v = do
      x <- go eith v
      case x of
        Left l -> do
          f <- go mp v
          pure $ f l
        Right r -> pure r
    go (JoinCodec mf) v = do
      f <- go mf v
      go f v

putObject :: ObjectFormatter -> Codec 'Object i x -> i -> Printer 'Value ()
putObject objf p i = do
  let ObjectStatus {anyComments = Any hasComment, ..} = collectObjects p i
      ents = sortBy (objf.keyOrdering `on` fst) $ Map.toList objects
  lvl' <-
    gets (.context) >>= \case
      TopLevel -> gets (.level)
      ExpectFieldValue _ -> gets (.level)
      ExpectArrayElem {style = Flow, ..} -> do
        when (index > 0) $ Printer $ tell ", "
        gets (.level)
      ExpectArrayElem {style = Block} -> do
        putIndent
        Printer $ tell "- "
        gets ((+ 2) . (.level))
  case objf.style of
    Flow -> do
      Printer $ tell "{"
      when hasComment $ Printer $ tell " "
    Block -> newLine >> putIndent
  let env' =
        PrintEnv
          { level = lvl'
          , context = ExpectFieldLabel objf.style NoComment
          }
  localState env' $ go hasComment ents
  case objf.style of
    Flow -> do
      Printer $ tell "}"
      newLine
    Block -> newLine
  where
    go hasComment ents = do
      lvl <- fromIntegral <$> gets (.level)
      let indent = Printer $ tell $ fromString (replicate lvl ' ')
          newl = Printer $ tell "\n"
      void $
        foldlM
          ( \ !isTail (lab, (mcomm, printer)) -> do
              when isTail $ case objf.style of
                Flow -> do
                  when hasComment indent
                  Printer $ tell ", "
                Block -> indent
              forM_ mcomm $
                T.lines >>> mapM_ \l -> do
                  Printer $ tell $ "# " <> TB.fromText l
                  newl
                  indent
              when (objf.style == Flow && isTail) $ Printer $ tell ", "
              Printer $ tell $ TB.fromText lab <> ": "
              let env'' =
                    PrintEnv
                      { level = fromIntegral lvl + 2
                      , context = ExpectFieldValue objf.style
                      }
              localState env'' printer
              if objf.style == Flow && not hasComment
                then Printer $ tell ", "
                else newl
              pure True
          )
          False
          ents

putArray :: BlockFormatter -> Codec 'Value i o -> V.Vector i -> Printer 'Value ()
putArray fmt p v = do
  i0 <- gets (.level)
  let env' =
        PrintEnv
          { level = i0
          , context = ExpectArrayElem {style = fmt.style, index = 0}
          }
  case fmt.style of
    Flow -> do
      Printer $ tell "["
      localState env' $ V.mapM_ (prettyValue p) v
      Printer $ tell "]"
    Block -> do
      localState env' $ V.mapM_ (prettyValue p) v

newLine :: Printer 'Value ()
newLine = Printer $ tell "\n"

putIndent :: Printer ctx ()
putIndent = Printer . tell . fromString . flip replicate ' ' . fromIntegral =<< gets (.level)

putLit :: TB.Builder -> Printer 'Value ()
putLit x =
  gets (.context) >>= \case
    TopLevel -> Printer $ tell $ x <> "\n"
    ExpectFieldValue _ -> Printer $ tell x
    ExpectArrayElem {..} -> do
      case style of
        Flow
          | index > 0 -> Printer $ tell $ ", " <> x
          | otherwise -> Printer $ tell x
        Block -> do
          putIndent
          Printer $ tell $ "- " <> x <> "\n"
      modify $ \env -> env {context = ExpectArrayElem {index = index + 1, ..}}

prettyJSON :: ValueFormatters -> J.Value -> Printer 'Value ()
prettyJSON _ J.Null = putNull
prettyJSON opts (J.Object dic) = putJSONObject' opts.object dic
prettyJSON opts (J.Array dic) = putJSONArray' opts.array dic
prettyJSON opts (J.String txt) = putText opts.text txt
prettyJSON opts (J.Number n) = case floatingOrInteger @Double n of
  Left {} -> putFloat opts.float n
  Right i -> putInteger i
prettyJSON _ (J.Bool b) = putBool b

putInteger :: Integer -> Printer 'Value ()
putInteger = putLit . fromString . show

putJSONArray' :: BlockFormatter -> J.Array -> Printer 'Value ()
putJSONArray' = undefined

putJSONObject' :: ObjectFormatter -> J.Object -> Printer 'Value ()
putJSONObject' = undefined
