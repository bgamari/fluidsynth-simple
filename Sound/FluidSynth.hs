module Sound.FluidSynth
    ( -- * Settings
      Setting
    , SettingName
    , strSetting
    , intSetting
    , numSetting
      -- * Basic types
    , Note, note
    , Channel, channel
    , Velocity, velocity
      -- * Synthesizer
    , FluidSynth
    , newFluidSynth
    , noteOn
    , noteOff
      -- * Sound fonts
    , loadSoundFont
    , SoundFont
    , AssignPresets(..)
    ) where

import Bindings.FluidSynth
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad (when)

-- | A FluidSynth synthesizer
data FluidSynth = FluidSynth
    { fluidSettings    :: FluidSettings
    , fluidSynth       :: ForeignPtr C'fluid_synth_t
    , fluidAudioDriver :: Maybe (ForeignPtr C'fluid_audio_driver_t)
    }

-- | A configuration setting
data Setting = Setting SettingName SettingVal
             deriving (Show, Read, Eq, Ord)
type SettingName = String
data SettingVal = StrSetting String
                | IntSetting Int
                | NumSetting Double
                deriving (Show, Read, Eq, Ord)

-- | Create a string-valued setting
strSetting :: SettingName -> String -> Setting
strSetting k v = Setting k (StrSetting v)

-- | Create an integer-valued setting
intSetting :: SettingName -> Int -> Setting
intSetting k v = Setting k (IntSetting v)

-- | Create a floating-point-valued setting
numSetting :: SettingName -> Double -> Setting
numSetting k v = Setting k (NumSetting v)

newtype FluidSettings = FluidSettings {getFluidSettings :: ForeignPtr C'fluid_settings_t}

newFluidSettings :: [Setting] -> EitherT String IO FluidSettings
newFluidSettings settings = do
    s <- liftIO c'new_fluid_settings
    let setSetting' :: CString -> SettingVal -> IO CInt
        setSetting' kPtr (StrSetting v) = withCString v $ \vPtr ->
            c'fluid_settings_setstr s kPtr vPtr
        setSetting' kPtr (IntSetting v) =
            c'fluid_settings_setint s kPtr (fromIntegral v)
        setSetting' kPtr (NumSetting v) =
            c'fluid_settings_setnum s kPtr (realToFrac v)

        setSetting :: Setting -> EitherT String IO ()
        setSetting (Setting k v) = do
            ret <- liftIO $ withCString k $ \kPtr -> setSetting' kPtr v
            when (ret == 0) $ left $ "Failed to set setting "++k

    mapM_ setSetting settings
    liftIO $ FluidSettings <$> newForeignPtr p'delete_fluid_settings s

setVerbose :: IO ()
setVerbose = do
    let go level = c'fluid_set_log_function level (castFunPtrToPtr p'fluid_default_log_function) nullPtr
    liftIO $ mapM_ go [c'FLUID_PANIC, c'FLUID_ERR, c'FLUID_WARN, c'FLUID_DBG]

-- | Create a new synthesizer and audio driver
newFluidSynth :: [Setting] -> EitherT String IO FluidSynth
newFluidSynth settings = do
    s <- newFluidSettings settings
    let withS = withForeignPtr (getFluidSettings s)
    synth <- liftIO $ withS $ \sPtr -> c'new_fluid_synth sPtr
    synthFPtr <- liftIO $ newForeignPtr p'delete_fluid_synth synth
    ad <- liftIO $ withS $ \sPtr -> c'new_fluid_audio_driver sPtr synth
    adFPtr <- liftIO $ newForeignPtr p'delete_fluid_audio_driver ad
    return $ FluidSynth s synthFPtr (Just adFPtr)

newtype Channel = Channel CInt
newtype Note = Note CInt
newtype Velocity = Velocity CInt

withSynth :: FluidSynth -> (Ptr C'fluid_synth_t -> IO a) -> IO a
withSynth synth = withForeignPtr (fluidSynth synth)

channel :: Int -> Channel
channel n = Channel (fromIntegral n)

note :: Int -> Note
note n
  | n >= 0 && n < 128 = Note (fromIntegral n)
  | otherwise         = error "Invalid note"

velocity :: Int -> Velocity
velocity n
  | n >= 0 && n < 128 = Velocity (fromIntegral n)
  | otherwise         = error "Invalid velocity"

-- | Send a note-on event
noteOn :: FluidSynth -> Channel -> Note -> Velocity -> EitherT String IO ()
noteOn synth (Channel ch) (Note note) (Velocity vel) =
    check "noteOn failed" $ withSynth synth $ \s->c'fluid_synth_noteon s ch note vel

-- | Send a note-off event
noteOff :: FluidSynth -> Channel -> Note -> EitherT String IO ()
noteOff synth (Channel ch) (Note note)=
    check "noteOff failed" $ withSynth synth $ \s->c'fluid_synth_noteoff s ch note

check :: String -> IO CInt -> EitherT String IO ()
check err action = do
    r <- liftIO action
    when (r /= c'FLUID_OK) $ left err

-- | Represents a loaded SoundFont
newtype SoundFont = SoundFont CInt

-- | Load a SoundFont
loadSoundFont :: FluidSynth
              -> FilePath                    -- ^ path of SoundFont file
              -> AssignPresets               -- ^ re-assign presets
              -> EitherT String IO SoundFont
loadSoundFont synth fname assignPresets = do
    res <- liftIO $ withCString fname $ \fnamePtr -> withSynth synth $ \s ->
      c'fluid_synth_sfload s fnamePtr $ case assignPresets of
                                         ReassignPresets -> 1
                                         otherwise       -> 0
    if res == c'FLUID_FAILED
       then left "sfload failed"
       else return (SoundFont res)

-- | Whether to re-assign channel presets when loading a SoundFont
data AssignPresets = ReassignPresets | PreservePresets
                   deriving (Show, Read, Eq, Ord, Bounded, Enum)
