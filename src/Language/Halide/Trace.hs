{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Halide.Trace
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Trace
  ( TraceEvent (..)
  , TraceEventCode (..)
  , TraceLoadStoreContents (..)
  , peekTraceEvent
  , withTrace
  , setCustomTrace
  , traceStores
  , traceLoads
  , collectIterationOrder
  )
where

import Control.Concurrent.MVar
import Control.Exception (bracket, bracket_)
import Control.Monad (void)
import Data.ByteString (packCString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Foreign.Marshal (peekArray)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Dimension
import Language.Halide.Func
import Language.Halide.LoopLevel
import Language.Halide.Type
import Prelude hiding (min, tail)

data TraceEventCode
  = TraceLoad
  | TraceStore
  | TraceBeginRealization
  | TraceEndRealization
  | TraceProduce
  | TraceEndProduce
  | TraceConsume
  | TraceEndConsume
  | TraceBeginPipeline
  | TraceEndPipeline
  | TraceTag
  deriving stock (Show, Eq, Ord)

data TraceLoadStoreContents = TraceLoadStoreContents
  { value :: !(Ptr ())
  , valueType :: !HalideType
  , coordinates :: ![Int]
  }
  deriving stock (Show)

data TraceEvent = TraceEvent
  { func :: !Text
  , code :: !TraceEventCode
  , loadStoreContents :: !(Maybe TraceLoadStoreContents)
  }
  deriving stock (Show)

importHalide

instance Enum TraceEventCode where
  fromEnum =
    fromIntegral . \case
      TraceLoad -> [CU.pure| int { halide_trace_load } |]
      TraceStore -> [CU.pure| int { halide_trace_store } |]
      TraceBeginRealization -> [CU.pure| int { halide_trace_begin_realization } |]
      TraceEndRealization -> [CU.pure| int { halide_trace_end_realization } |]
      TraceProduce -> [CU.pure| int { halide_trace_produce } |]
      TraceEndProduce -> [CU.pure| int { halide_trace_end_produce } |]
      TraceConsume -> [CU.pure| int { halide_trace_consume } |]
      TraceEndConsume -> [CU.pure| int { halide_trace_end_consume } |]
      TraceBeginPipeline -> [CU.pure| int { halide_trace_begin_pipeline } |]
      TraceEndPipeline -> [CU.pure| int { halide_trace_end_pipeline } |]
      TraceTag -> [CU.pure| int { halide_trace_tag } |]
  toEnum k
    | fromIntegral k == [CU.pure| int { halide_trace_load } |] = TraceLoad
    | fromIntegral k == [CU.pure| int { halide_trace_store } |] = TraceStore
    | fromIntegral k == [CU.pure| int { halide_trace_begin_realization } |] = TraceBeginRealization
    | fromIntegral k == [CU.pure| int { halide_trace_end_realization } |] = TraceEndRealization
    | fromIntegral k == [CU.pure| int { halide_trace_produce } |] = TraceProduce
    | fromIntegral k == [CU.pure| int { halide_trace_end_produce } |] = TraceEndProduce
    | fromIntegral k == [CU.pure| int { halide_trace_consume } |] = TraceConsume
    | fromIntegral k == [CU.pure| int { halide_trace_end_consume } |] = TraceEndConsume
    | fromIntegral k == [CU.pure| int { halide_trace_begin_pipeline } |] = TraceBeginPipeline
    | fromIntegral k == [CU.pure| int { halide_trace_end_pipeline } |] = TraceEndPipeline
    | fromIntegral k == [CU.pure| int { halide_trace_tag } |] = TraceTag
    | otherwise = error $ "invalid TraceEventCode: " <> show k

peekTraceLoadStoreContents :: Ptr TraceEvent -> IO TraceLoadStoreContents
peekTraceLoadStoreContents p = do
  v <- [CU.exp| void* { $(const halide_trace_event_t* p)->value } |]
  tp <- peek =<< [CU.exp| const halide_type_t* { &$(const halide_trace_event_t* p)->type } |]
  n <- fromIntegral <$> [CU.exp| int { $(const halide_trace_event_t* p)->dimensions } |]
  cs <- peekArray n =<< [CU.exp| const int32_t* { $(const halide_trace_event_t* p)->coordinates } |]
  pure $ TraceLoadStoreContents v tp (fromIntegral <$> cs)

peekTraceEvent :: Ptr TraceEvent -> IO TraceEvent
peekTraceEvent p = do
  f <-
    fmap decodeUtf8 $
      packCString
        =<< [CU.exp| const char* { $(const halide_trace_event_t* p)->func } |]
  c <- toEnum . fromIntegral <$> [CU.exp| int { $(const halide_trace_event_t* p)->event } |]
  contents <-
    case c of
      TraceLoad -> Just <$> peekTraceLoadStoreContents p
      TraceStore -> Just <$> peekTraceLoadStoreContents p
      _ -> pure Nothing
  pure $ TraceEvent f c contents

withTrace
  :: (TraceEvent -> IO ()) -> (FunPtr (Ptr CxxUserContext -> Ptr TraceEvent -> IO Int32) -> IO a) -> IO a
withTrace customTrace = bracket allocate destroy
  where
    allocate = do
      $(C.mkFunPtr [t|Ptr CxxUserContext -> Ptr TraceEvent -> IO Int32|]) $ \_ p ->
        peekTraceEvent p >>= customTrace >> pure 0
    destroy = freeHaskellFunPtr

setCustomTrace
  :: (KnownNat n, IsHalideType a)
  => (TraceEvent -> IO ())
  -> Func t n a
  -> IO b
  -> IO b
setCustomTrace customTrace f action =
  withTrace customTrace $ \tracePtr ->
    bracket_ (set tracePtr) unset action
  where
    set tracePtr =
      withFunc f $ \f' ->
        [CU.block| void {
          auto& func = *$(Halide::Func* f');
          func.jit_handlers().custom_trace = $(int32_t (*tracePtr)(Halide::JITUserContext*, const halide_trace_event_t*));
        } |]
    unset =
      withFunc f $ \f' ->
        [CU.block| void {
          auto& func = *$(Halide::Func* f');
          func.jit_handlers().custom_trace = nullptr;
        } |]

traceStores :: (KnownNat n, IsHalideType a) => Func t n a -> IO (Func t n a)
traceStores f = do
  withFunc f $ \f' ->
    [CU.exp| void { $(Halide::Func* f')->trace_stores() } |]
  pure f

traceLoads :: (KnownNat n, IsHalideType a) => Func t n a -> IO (Func t n a)
traceLoads f = do
  withFunc f $ \f' ->
    [CU.exp| void { $(Halide::Func* f')->trace_loads() } |]
  pure f

collectIterationOrder
  :: (KnownNat n, IsHalideType a)
  => (TraceEventCode -> Bool)
  -> Func t n a
  -> IO b
  -> IO ([[Int]], b)
collectIterationOrder cond f action = do
  -- case c of
  --   TraceLoad -> void (traceLoads f)
  --   TraceStore -> void (traceStores f)
  --   _ -> pure ()
  m <- newMVar []
  let tracer (TraceEvent _ c' (Just payload))
        | cond c' = modifyMVar_ m $ pure . (payload.coordinates :)
      tracer _ = pure ()
  setCustomTrace tracer f $ do
    r <- action
    cs <- readMVar m
    pure (reverse cs, r)
