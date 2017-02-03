module Main (main) where
import           Network.Libre.TLS.FFI.Internal

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr

import           Data.ByteString                (packCStringLen)

import           Control.Monad                  (when)

main :: IO ()
main = do
  ret          <- tls_init_c
  when (ret /= 0) $ error "tls_init_c"

  confPtr      <- tls_config_new_c
  when (confPtr == nullPtr) $
    error "tls_config_new"

  fconfPtr     <- newForeignPtr tls_config_free_c_fp confPtr
  tlsPtr       <- allocate_fresh_tls_client_context_c
  when (tlsPtr == nullPtr) $
    error "allocate_fresh_tls_client_context_c"

  ftlsPtr      <- newForeignPtr tls_free_c_fp tlsPtr
  ret          <- withForeignPtr ftlsPtr $ \tlsPtr ->
                  withForeignPtr fconfPtr $ \confPtr ->
                  tls_configure_c tlsPtr confPtr
  when (ret /= 0) $
    error "tls_configure_c"

  ret          <- withCString "axman6.duckdns.org" $ \host ->
                  withCString "443" $ \port ->
                  withForeignPtr ftlsPtr $ \tlsPtr ->
                  tls_connect_c tlsPtr host port
  when (ret /= 0) $
    errorWith "tls_connect_c" ftlsPtr
  addForeignPtrFinalizer tls_close_c_fp ftlsPtr

  withCStringLen getString $ \(cstr,len) ->
    withForeignPtr ftlsPtr $ \tlsPtr -> do
    retlen <- tls_write_c tlsPtr (castPtr cstr) (fromIntegral len)
    when (retlen < fromIntegral len) $
      errorWith "tlw_write" ftlsPtr

  fbuf <- mallocForeignPtrBytes bufSize
  bs <- withForeignPtr fbuf $ \buf ->
    withForeignPtr ftlsPtr $ \tlsPtr -> do
    retlen <- tls_read_c tlsPtr (castPtr buf) (fromIntegral bufSize)
    when (retlen < 0) $
      errorWith "tls_read_c" ftlsPtr
    packCStringLen (buf,bufSize)

  print bs

  return ()

getString = "GET / HTTP/1.1\r\nHost: axman6.duckdns.org\r\n\r\n"
bufSize = 1024

errorWith :: String -> ForeignPtr LibTLSContext -> IO ()
errorWith fname ctx = do
  str <- withForeignPtr ctx $ \ctxp -> do
    cstr <- tls_error_c ctxp
    peekCString cstr
  putStrLn $ "Error: " ++ str
  error fname

