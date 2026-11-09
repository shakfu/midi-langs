-- The MicroHs REPL behind the mhs-midi plugin (mhs_plugin_runtime.c).
-- Flags are set up as in MicroHs.Main; the loop then serves requests from the
-- plugin instead of reading a terminal. Requests, one per mhs_plugin_next:
--   e<code>  evaluate code, as one REPL entry
--   r<us>    let voices run up to script time <us> microseconds
--   k        kill every thread but this one (the voices)
--   c        clear all definitions
-- NULL quits. Script time is the runtime clock (see plugin/runtime/extra.c),
-- so a voice's threadDelay waits in script time.
module PluginRepl(main) where
import qualified Prelude(); import MHSPrelude
import Control.Concurrent(threadDelay)
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import Foreign.C.String(CString, peekCString)
import Foreign.Ptr(nullPtr)
import System.Environment
import System.IO
import MicroHs.Compile(getPaths)
import MicroHs.Flags
import MicroHs.Interactive(I, startIState, reload, oneline, command)
import MicroHs.Lex(readInt)
import MicroHs.List(splitColonPath)
import MicroHs.Main(decodeArgs, readConfig, findAPackage, expandPath, mHSPKG)
import MicroHs.StateIO

foreign import ccall "mhs_plugin_next"      c_next      :: IO CString
foreign import ccall "mhs_plugin_done"      c_done      :: IO ()
foreign import ccall "mhs_plugin_clock_us"  c_clock_us  :: IO Int
foreign import ccall "mhs_plugin_set_clock_us" c_set_clock_us :: Int -> IO ()
foreign import ccall "mhs_plugin_kill_others" c_kill_others :: IO ()

main :: IO ()
main = do
  (mhsDir, srcs, mpkg) <- getPaths
  args <- getArgs
  let (cflags, _, _) = decodeArgs defaultFlags{ mhsdir = mhsDir } [] args
  conf <- readConfig cflags
  let paths = splitColonPath $ fromMaybe ('$':mHSPKG) $ lookup "mhs" conf >>= lookup "packageDbPath"
      (eflags, mdls, _) = decodeArgs defaultFlags{ pkgPaths = paths, srcPaths = srcs, mhsdir = mhsDir } [] args
  paths' <- nub . filter (not . null) . splitColonPath <$> expandPath mpkg (intercalate ":" $ pkgPaths eflags)
  let flags = eflags{ config = conf, pkgPaths = paths' }
  preload' <- mapM (findAPackage flags) (preload flags)
  st <- startIState flags{ preload = preload' } mdls "plugin"
  _ <- runStateIO serve st
  return ()

serve :: I ()
serve = do
  reload
  liftIO c_done
  let loop = do
        p <- liftIO c_next
        if p == nullPtr then return () else do
          req <- liftIO $ peekCString p
          -- An exception that escapes, such as Stop while compiling, ends the
          -- request and keeps the definitions; the loop must survive it.
          r <- trySIO (request req)
          either (\ e -> liftIO $ putStrLn $ "*** Exception: " ++ show e) return r
          liftIO $ hFlush stdout
          liftIO c_done
          loop
  loop

request :: String -> I ()
request ('e':code) =
  case dropWhile isSpace code of
    ':':r -> do _ <- command r; return ()
    _     -> oneline code
request ('r':us) = liftIO $ catchAll $ do
  let target = readInt us
  now <- c_clock_us
  -- One microsecond past the target: threadDelay queues this thread ahead of
  -- voices due at the same time, which must run first.
  threadDelay (max 0 (target - now) + 1)
  c_set_clock_us target
request "k" = liftIO $ catchAll $ do
  c_kill_others
  threadDelay 0  -- the killed voices run their handlers and end
request "c" = do _ <- command "clear"; return ()
request r = liftIO $ putStrLn $ "mhs-midi plugin: bad request " ++ show r

-- Stop interrupts this thread with UserInterrupt; it ends the request.
catchAll :: IO () -> IO ()
catchAll io = catch io (\ (_ :: SomeException) -> return ())
