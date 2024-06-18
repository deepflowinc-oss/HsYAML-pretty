{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.YAML.Pretty (
  Codec (..),
  genericCodec,
  parserOnly,
  orParse,
  asumParsers,
  (<!>),
  eitherCodec,
  Context (..),
  encode,
  encodeWith,
  fromNodeWith,
  fmapEither,
  fmapMaybe,
  decode1,
  decode1With,
  decode1Strict,
  decode1StrictWith,
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
  HasValueCodec (..),
) where

import Control.Applicative
import Control.Lens hiding (Context)
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
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable (foldl')
import Data.Generics.Labels ()
import Data.Int
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Pointed (Pointed (..))
import Data.Proxy (Proxy (..))
import Data.Scientific (FPFormat (..), Scientific, formatScientific, fromFloatDigits, toRealFloat)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
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
import GHC.Generics
import GHC.Generics qualified as G
import GHC.TypeLits (KnownSymbol, symbolVal)
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
    Codec context b output ->
    Codec context input output

divideCodec ::
  ((context == Product) ~ 'False) =>
  (input -> (a, b)) ->
  Codec context a output ->
  Codec context b output ->
  Codec context input output
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
encodeWith codec = writeEventsText . DL.toList . (DL.fromList [StreamStart, DocumentStart NoDirEndMarker] <>) . (<> DL.fromList [DocumentEnd False, StreamEnd]) . encodeWith_ codec

encodeWith_ :: Codec o a x -> a -> DEvStream
encodeWith_ PureCodec {} _ = mempty
encodeWith_ (FixedTextCodec sty t) () = DL.singleton $ textEvt sty t
encodeWith_ (Fail _) x = absurd x
encodeWith_ (AltCodec el _) (Left l) = encodeWith_ el l
encodeWith_ (AltCodec _ er) (Right r) = encodeWith_ er r
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
encodeWith_ (ProductCodec opt v) p =
  DL.singleton
    (SequenceStart Nothing untagged opt)
    <> encodeWith_ v p
    <> DL.singleton SequenceEnd
encodeWith_ (ElementCodec a) x = encodeWith_ a x
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
valueFromNode_ (Fail msg) x = Left (posOf x, msg)
valueFromNode_ (PureCodec a) _ = pure a
valueFromNode_ (AltCodec el er) v =
  case valueFromNode_ el v of
    Right x -> Right (Left x)
    Left {} -> valueFromNode_ (Right <$> er) v
valueFromNode_ (ViaJSON _) v = nodeToJSON v
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
valueFromNode_ (DivideCodec _ _ cr) v =
  valueFromNode_ cr v

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
objectFromNode_ pos (DivideCodec _ _ cr) dic =
  objectFromNode_ pos cr dic

class HasValueCodec a where
  valueCodec :: Codec Value a a

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

class GHasValueCodec f where
  gvalueCodec :: Codec Value (f ()) (f ())

class GHasProductCodec f where
  gprodCodec :: Codec Product (f ()) (f ())

instance GHasProductCodec U1 where
  gprodCodec = pure U1

instance
  ( m ~ 'Nothing
  , GHasValueCodec f
  ) =>
  GHasProductCodec (S1 ('MetaSel m x y z) f)
  where
  gprodCodec = elem $ dimap unM1 M1 gvalueCodec

instance
  (GHasProductCodec l, GHasProductCodec r) =>
  GHasProductCodec (l :*: r)
  where
  gprodCodec =
    (:*:)
      <$> lmap (\(l :*: _) -> l) (gprodCodec @l)
      <*> lmap (\(_ :*: r) -> r) (gprodCodec @r)

class GHasObjectCodec f where
  gobjCodec :: Codec Object (f ()) (f ())

instance
  {-# OVERLAPPING #-}
  ( m ~ 'Just sel
  , KnownSymbol sel
  , HasValueCodec a
  ) =>
  GHasObjectCodec (S1 ('MetaSel m x y z) (K1 i (Maybe a)))
  where
  gobjCodec =
    dimap (unK1 . unM1) (M1 . K1) $
      optionalField' (T.pack $ symbolVal @sel Proxy) Nothing True (valueCodec @a)

instance
  (GHasObjectCodec l, GHasObjectCodec r) =>
  GHasObjectCodec (l :*: r)
  where
  gobjCodec =
    (:*:)
      <$> lmap (\(l :*: _) -> l) (gobjCodec @l)
      <*> lmap (\(_ :*: r) -> r) (gobjCodec @r)

instance
  {-# OVERLAPPABLE #-}
  ( m ~ 'Just sel
  , KnownSymbol sel
  , HasValueCodec a
  ) =>
  GHasObjectCodec (S1 ('MetaSel m x y z) (K1 i a))
  where
  gobjCodec =
    dimap (unK1 . unM1) (M1 . K1) $
      requiredField' (T.pack $ symbolVal @sel Proxy) Nothing Nothing (valueCodec @a)

instance (HasValueCodec c) => GHasValueCodec (K1 i c) where
  gvalueCodec = dimap unK1 K1 valueCodec

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

type family GHasGoodContext pt where
  GHasGoodContext IsProd = GHasProductCodec
  GHasGoodContext IsObj = GHasObjectCodec

instance {-# OVERLAPPABLE #-} (GHasValueCodec f) => GHasValueCodec (M1 i c f) where
  gvalueCodec = dimap unM1 M1 gvalueCodec

instance
  ( GHasGoodContext (ProductType (l :*: r)) (l :*: r)
  , KnownProdType (ProductType (l :*: r))
  ) =>
  GHasValueCodec (l :*: r)
  where
  gvalueCodec = case sProdType @(ProductType (l :*: r)) of
    SIsProd -> prod gprodCodec
    SIsObj -> objectWith defaultObjectFormatter gobjCodec

instance {-# OVERLAPPING #-} (Constructor i) => GHasValueCodec (C1 i U1) where
  gvalueCodec =
    dimap (\(M1 U1) -> ()) (\() -> M1 U1) $
      literalText $
        T.pack $
          conName (undefined :: C1 i U1 ())

instance (GHasValueCodec l, GHasValueCodec r) => GHasValueCodec (l :+: r) where
  gvalueCodec =
    lmap (\case L1 x -> Left x; R1 y -> Right y) $
      L1 <$> gvalueCodec <!> R1 <$> gvalueCodec

instance
  (GHasValueCodec (Rep x), Generic x) =>
  HasValueCodec (Generically x)
  where
  valueCodec = dimap (\(Generically a) -> G.from a) (Generically . G.to) gvalueCodec

genericCodec :: (GHasValueCodec (Rep a), Generic a) => Codec 'Value a a
genericCodec = dimap Generically coerce valueCodec
