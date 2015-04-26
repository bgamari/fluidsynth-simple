import Sound.FluidSynth
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

printEitherT :: EitherT String IO () -> IO ()
printEitherT action = runEitherT action >>= either print return

main = printEitherT $ do
    synth <- newFluidSynth [strSetting "audio.driver" "pulseaudio"]
    loadSoundFont synth "/usr/share/sounds/sf2/FluidR3_GM.sf2"
    noteOn synth (channel 0) (note 60) (velocity 100)
    liftIO $ threadDelay 1000000000
    noteOn synth (channel 0) (note 60) (velocity 100)
