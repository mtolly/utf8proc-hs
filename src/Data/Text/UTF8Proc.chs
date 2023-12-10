{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Data.Text.UTF8Proc where

import           Data.ByteString   (packCString)
import qualified Data.ByteString   as B
import           Data.Coerce       (coerce)
import qualified Data.Text         as T
import qualified Data.Text.Foreign as F
import           Foreign
import           Foreign.C
import           System.IO.Unsafe  (unsafePerformIO)

#include "utf8proc.h"

utf8proc_VERSION_MAJOR, utf8proc_VERSION_MINOR, utf8proc_VERSION_PATCH :: CInt
utf8proc_VERSION_MAJOR = {#const UTF8PROC_VERSION_MAJOR #}
utf8proc_VERSION_MINOR = {#const UTF8PROC_VERSION_MINOR #}
utf8proc_VERSION_PATCH = {#const UTF8PROC_VERSION_PATCH #}

{#typedef utf8proc_int8_t   Int8     #}
{#typedef utf8proc_uint8_t  Word8    #}
{#typedef utf8proc_int16_t  Int16    #}
{#typedef utf8proc_uint16_t Word16   #}
{#typedef utf8proc_int32_t  Int32    #}
{#typedef utf8proc_uint32_t Word32   #}
{#typedef utf8proc_size_t   CSize    #}
{#typedef utf8proc_ssize_t  CPtrdiff #}
{#typedef utf8proc_bool     Bool     #}

{#enum utf8proc_option_t as Option {} #}
deriving instance Show Option

data Error
  = UTF8PROC_ERROR_NOMEM
  | UTF8PROC_ERROR_OVERFLOW
  | UTF8PROC_ERROR_INVALIDUTF8
  | UTF8PROC_ERROR_NOTASSIGNED
  | UTF8PROC_ERROR_INVALIDOPTS
  | ErrorUnknown CPtrdiff

errorCode :: Error -> CPtrdiff
errorCode err = case err of
  UTF8PROC_ERROR_NOMEM       -> {#const UTF8PROC_ERROR_NOMEM       #}
  UTF8PROC_ERROR_OVERFLOW    -> {#const UTF8PROC_ERROR_OVERFLOW    #}
  UTF8PROC_ERROR_INVALIDUTF8 -> {#const UTF8PROC_ERROR_INVALIDUTF8 #}
  UTF8PROC_ERROR_NOTASSIGNED -> {#const UTF8PROC_ERROR_NOTASSIGNED #}
  UTF8PROC_ERROR_INVALIDOPTS -> {#const UTF8PROC_ERROR_INVALIDOPTS #}
  ErrorUnknown x             -> x

{#pointer *utf8proc_property_t as CProperty newtype #}
deriving instance Storable CProperty
deriving instance Show CProperty

{#enum utf8proc_category_t as Category {} #}
deriving instance Show Category

{#enum utf8proc_bidi_class_t as BidiClass {} #}
deriving instance Show BidiClass

{#enum utf8proc_decomp_type_t as DecompType {} #}
deriving instance Show DecompType

{#enum utf8proc_boundclass_t as Boundclass {} #}
deriving instance Show Boundclass

{#enum utf8proc_indic_conjunct_break_t as IndicConjunctBreak {} #}
deriving instance Show IndicConjunctBreak

-- typedef utf8proc_int32_t (*utf8proc_custom_func)(utf8proc_int32_t codepoint, void *data);

-- UTF8PROC_DLLEXPORT extern const utf8proc_int8_t utf8proc_utf8class[256];

{#fun pure utf8proc_version {} -> `B.ByteString' packCString* #}
{#fun pure utf8proc_unicode_version {} -> `B.ByteString' packCString* #}
{#fun pure utf8proc_errmsg { errorCode `Error' } -> `B.ByteString' packCString* #}

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_iterate(const utf8proc_uint8_t *str, utf8proc_ssize_t strlen, utf8proc_int32_t *codepoint_ref);
-- UTF8PROC_DLLEXPORT utf8proc_bool utf8proc_codepoint_valid(utf8proc_int32_t codepoint);
-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_encode_char(utf8proc_int32_t codepoint, utf8proc_uint8_t *dst);

{#fun pure utf8proc_get_property { fromChar `Char' } -> `CProperty' #}

data Property = Property
  { p_category             :: Category
  , p_combining_class      :: Int16
  , p_bidi_class           :: Maybe BidiClass
  , p_decomp_type          :: Maybe DecompType
  , p_decomp_seqindex      :: Word16
  , p_casefold_seqindex    :: Word16
  , p_uppercase_seqindex   :: Word16
  , p_lowercase_seqindex   :: Word16
  , p_titlecase_seqindex   :: Word16
  , p_comb_index           :: Word16
  , p_bidi_mirrored        :: Bool
  , p_comp_exclusion       :: Bool
  , p_ignorable            :: Bool
  , p_control_boundary     :: Bool
  , p_charwidth            :: CUInt
  , p_pad                  :: CUInt
  , p_boundclass           :: Boundclass
  , p_indic_conjunct_break :: IndicConjunctBreak
  } deriving (Show)

{-# NOINLINE getProperty #-}
getProperty :: CProperty -> Property
getProperty cp = unsafePerformIO $ do
  let getMaybeEnum 0 = Nothing
      getMaybeEnum n = Just $ toEnum $ fromIntegral n
  a <- toEnum . fromIntegral <$> {#get utf8proc_property_t->category             #} cp
  b <- coerce                <$> {#get utf8proc_property_t->combining_class      #} cp
  c <- getMaybeEnum          <$> {#get utf8proc_property_t->bidi_class           #} cp
  d <- getMaybeEnum          <$> {#get utf8proc_property_t->decomp_type          #} cp
  e <- coerce                <$> {#get utf8proc_property_t->decomp_seqindex      #} cp
  f <- coerce                <$> {#get utf8proc_property_t->casefold_seqindex    #} cp
  g <- coerce                <$> {#get utf8proc_property_t->uppercase_seqindex   #} cp
  h <- coerce                <$> {#get utf8proc_property_t->lowercase_seqindex   #} cp
  i <- coerce                <$> {#get utf8proc_property_t->titlecase_seqindex   #} cp
  j <- coerce                <$> {#get utf8proc_property_t->comb_index           #} cp
  k <- toBool                <$> {#get utf8proc_property_t->bidi_mirrored        #} cp
  l <- toBool                <$> {#get utf8proc_property_t->comp_exclusion       #} cp
  m <- toBool                <$> {#get utf8proc_property_t->ignorable            #} cp
  n <- toBool                <$> {#get utf8proc_property_t->control_boundary     #} cp
  o <-                           {#get utf8proc_property_t->charwidth            #} cp
  p <-                           {#get utf8proc_property_t->pad                  #} cp
  q <- toEnum . fromIntegral <$> {#get utf8proc_property_t->boundclass           #} cp
  r <- toEnum . fromIntegral <$> {#get utf8proc_property_t->indic_conjunct_break #} cp
  return Property
    { p_category             = a
    , p_combining_class      = b
    , p_bidi_class           = c
    , p_decomp_type          = d
    , p_decomp_seqindex      = e
    , p_casefold_seqindex    = f
    , p_uppercase_seqindex   = g
    , p_lowercase_seqindex   = h
    , p_titlecase_seqindex   = i
    , p_comb_index           = j
    , p_bidi_mirrored        = k
    , p_comp_exclusion       = l
    , p_ignorable            = m
    , p_control_boundary     = n
    , p_charwidth            = o
    , p_pad                  = p
    , p_boundclass           = q
    , p_indic_conjunct_break = r
    }

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_decompose_char(
--   utf8proc_int32_t codepoint, utf8proc_int32_t *dst, utf8proc_ssize_t bufsize,
--   utf8proc_option_t options, int *last_boundclass
-- );

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_decompose(
--   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen,
--   utf8proc_int32_t *buffer, utf8proc_ssize_t bufsize, utf8proc_option_t options
-- );

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_decompose_custom(
--   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen,
--   utf8proc_int32_t *buffer, utf8proc_ssize_t bufsize, utf8proc_option_t options,
--   utf8proc_custom_func custom_func, void *custom_data
-- );

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_normalize_utf32(utf8proc_int32_t *buffer, utf8proc_ssize_t length, utf8proc_option_t options);

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_reencode(utf8proc_int32_t *buffer, utf8proc_ssize_t length, utf8proc_option_t options);

{#fun utf8proc_grapheme_break_stateful { fromChar `Char', fromChar `Char', id `Ptr Int32' } -> `Bool' #}
{#fun pure utf8proc_grapheme_break { fromChar `Char', fromChar `Char' } -> `Bool' #}

{-# NOINLINE breakGraphemes #-}
breakGraphemes :: T.Text -> [T.Text]
breakGraphemes txt = unsafePerformIO $ with 0 $ \p -> let
  go graphemeStart !graphemeSize t = case T.uncons t of
    Nothing -> return []
    Just (c, t') -> case T.uncons t' of
      Nothing -> return [graphemeStart]
      Just (c', _) -> do
        b <- utf8proc_grapheme_break_stateful c c' p
        if b
          then let
            grapheme = T.take (graphemeSize + 1) graphemeStart
            in (grapheme :) <$> go t' 0 t'
          else go graphemeStart (graphemeSize + 1) t'
  in go txt 0 txt

{#fun pure utf8proc_tolower { fromChar `Char' } -> `Char' toChar #}
{#fun pure utf8proc_toupper { fromChar `Char' } -> `Char' toChar #}
{#fun pure utf8proc_totitle { fromChar `Char' } -> `Char' toChar #}

fromChar :: Char -> Int32
fromChar = fromIntegral . fromEnum

toChar :: Int32 -> Char
toChar = toEnum . fromIntegral

{#fun pure utf8proc_islower { fromChar `Char' } -> `Bool' #}
{#fun pure utf8proc_isupper { fromChar `Char' } -> `Bool' #}
{#fun pure utf8proc_charwidth { fromChar `Char' } -> `CInt' #}
{#fun pure utf8proc_category { fromChar `Char' } -> `Category' #}
{#fun pure utf8proc_category_string { fromChar `Char' } -> `B.ByteString' packCString* #}

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_map(
--   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen, utf8proc_uint8_t **dstptr, utf8proc_option_t options
-- );

-- UTF8PROC_DLLEXPORT utf8proc_ssize_t utf8proc_map_custom(
--   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen, utf8proc_uint8_t **dstptr, utf8proc_option_t options,
--   utf8proc_custom_func custom_func, void *custom_data
-- );

{#fun pure utf8proc_NFD { utf8Ptr* `T.Text' } -> `T.Text' packAndFree* #}
{#fun pure utf8proc_NFC { utf8Ptr* `T.Text' } -> `T.Text' packAndFree* #}
{#fun pure utf8proc_NFKD { utf8Ptr* `T.Text' } -> `T.Text' packAndFree* #}
{#fun pure utf8proc_NFKC { utf8Ptr* `T.Text' } -> `T.Text' packAndFree* #}
{#fun pure utf8proc_NFKC_Casefold { utf8Ptr* `T.Text' } -> `T.Text' packAndFree* #}

utf8Ptr :: T.Text -> (Ptr Word8 -> IO a) -> IO a
utf8Ptr t f = F.useAsPtr t $ \p _ -> f p

packAndFree :: Ptr Word8 -> IO T.Text
packAndFree p = do
  t <- F.fromPtr0 p
  free p
  return t
