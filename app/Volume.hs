{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Game.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Lens
import Data.Data
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Debug.Trace
import System.Random
import Control.Monad.Random

import Halive.Utils





{-

  Shapes:

  Keeping all the different shapes
  in a single lense, so we can pass them
  to the render function as a package,
  instead of one by one

-}

data Shapes u1 u2 = Shapes
  { _shpVoice  :: Shape u1
  , _shpMarker :: Shape u2 
  }
makeLenses ''Shapes





{-

  Voice: 

  The Voice structure is just a 'tube' that is 
  created in the fragment shader using the uv coordinates
  of a plane geometry

  Pose should always just be 0 , 0 , because the vox Points will

-}

data Voice = Voice
  { _voxPose        :: !(Pose GLfloat)
  , _voxPoints      :: ![V4 GLfloat]
  , _voxActive      :: !Int
  }
makeLenses ''Voice




{-

  World:

  This is where we keep the majority of our data. 
  If we pass the same world into our render function,
  We should get the same visual result *every* time!

-}


data World = World
  { _wldVoices        :: !(Map Int Voice)
  , _wldPlayer        :: !(Pose GLfloat)
  , _wldTime          :: !Float
  , _wldMarker        :: !(Pose GLfloat)
  }
makeLenses ''World



{-

  Uniforms:

  A Big list of uniforms we use across our programs
  

-}

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uViewProjection      :: UniformLocation (M44 GLfloat)
  , uNormalMatrix        :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)

  -- once we've got updated gamepal
  --, uPoints              :: UniformLocation [V4  GLfloat]
  , uPoint1              :: UniformLocation (V4  GLfloat)
  , uPoint2              :: UniformLocation (V4  GLfloat)
  , uPoint3              :: UniformLocation (V4  GLfloat)
  , uPoint4              :: UniformLocation (V4  GLfloat)
  , uPoint5              :: UniformLocation (V4  GLfloat)
  , uPoint6              :: UniformLocation (V4  GLfloat)
  , uPoint7              :: UniformLocation (V4  GLfloat)
  , uPoint8              :: UniformLocation (V4  GLfloat)
  , uTime                :: UniformLocation GLfloat
  } deriving (Data)





{-

  Main:

  Gets called at beginning of program. 
  Initializing everything here, and than calling our main game loop.

-}

enableDevices :: [GamePalDevices]
-- enableDevices = [UseOpenVR]
enableDevices = [UseOpenVR, UseHydra]

main :: IO ()
main = do

  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "Volume" NoGCPerFrame enableDevices


  -- Set up our marker resources
  markerProg   <- createShaderProgram "app/shaders/marker.vert" "app/shaders/marker.frag"
  markerGeo    <- icosahedronGeometry 0.1 2
  markerShape  <- makeShape markerGeo markerProg--markerGeo markerProg


    -- Set up our marker resources
  voiceProg   <- createShaderProgram "app/shaders/voice.vert" "app/shaders/voice.frag"
  voiceGeo    <- planeGeometry ( V2 1 1 ) ( V3 0 0 (-1) ) ( V3 0 1 0 ) ( V2 100 100 )
  voiceShape  <- makeShape voiceGeo voiceProg--markerGeo markerProg




  let shapes = Shapes{ _shpMarker = markerShape
                     , _shpVoice  = voiceShape
                     }


  {-

    Setting up some gl information

  -}
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE





  {-

    Building our default world.

    Because of the "!" s in the type declaration
    we need to declare all parts of the world, 
    or it be break in right away

  -}




  let world = World 
        { _wldVoices  = Map.fromList $ flip map [0] $ 
                          \i -> let something = Voice 
                                      { _voxPose  = Pose { _posPosition    = V3 0 0 0
                                                         , _posOrientation = Quaternion 0 (V3 0 1 0)
                                                         }
                                      , _voxPoints = flip map [0..10] $ 
                                                        \x -> getBasePoint4 ( x  )       
                                      , _voxActive = 1
                                      }
                                in (i, something)

        , _wldPlayer  = newPose {_posPosition = V3 0 0 5}
        , _wldMarker  = newPose {_posPosition = V3 3 2 3}
        , _wldTime    = 0
        }







  {-

    Main Game Loop!

  -}
  void . flip runStateT world . whileWindow gpWindow $ do


    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    time <- use wldTime

    {-

      Giving our marker a random position

    -}
    let x = sin( time * 5 ) * 10
        y = cos( time * 3 ) * 4
        z = cos( time * 4 ) * 5

    wldMarker .= newPose {_posPosition = V3 x y z }


    {-

      Updating the positions of the control points
      of the voices, so we get some drift!
  
    -}
      
    -- time <- use wldTime
    -- wldVoices . traverse . voxPoints . traverse . _x += ( sin time ) * 0.001
    
    {-voices <- use wldVoices
    forM_ ( zip [0..] ( Map.toList voices ) ) $ \( i , (objID, obj) ) -> do

      let voice = wldVoices . at objID . traverse
      let points = ( obj ^. voxPoints )
      forM_ ( zip [0..] ( points ) ) $ \( i , p ) -> do
        ( voice .  points ) !! i .= 1-}




    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer



    -- controls for debugging 
    shiftDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'LeftShift
    whenKeyPressed gpWindow Key'Z           $ liftIO $ putStrLn $ "oh" ++ show 5 ++ " yeah"


    viewMat <- viewMatrixFromPose <$> use wldPlayer


    -- Once we have set up all the neccesary information,
    -- Render away!
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shapes )










{-

  Render function.

  This should be totally pure! only take in world state, and feed back
  delicious pixels on the screen

-}



render :: (MonadIO m, MonadState World m) 
       => Shapes Uniforms Uniforms 
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shapes projection viewMat = do

  time <- use wldTime
  let markerShape = shapes ^. shpMarker
  let voiceShape = shapes ^. shpVoice

  markerPose <- use wldMarker
  let markerP = markerPose ^. posPosition





  {-

    Render the Marker

  -}

  useProgram (sProgram markerShape)

  withVAO (sVAO markerShape) $ do
    let model = mkTransformation ( Quaternion 0 (V3 0 1 0) ) markerP

    drawShape model projection viewMat markerShape

    voices <- use wldVoices
    forM_ ( zip [0..] ( Map.toList voices ) ) $ \( i , (objID, obj) ) -> do

      let model = mkTransformation (obj ^. voxPose . posOrientation) (obj ^. voxPose . posPosition)
          points = ( obj ^. voxPoints )


      forM_ ( zip [0..] ( points ) ) $ \( i , p ) -> do
        let model = mkTransformation ( Quaternion 0 (V3 0 1 0) ) (V3 (p ^. _x) (p ^. _y) (p ^. _z))

        drawShape model projection viewMat markerShape








  {-

    Render the voices

  -}

  useProgram (sProgram voiceShape)
  let Uniforms{..} = sUniforms voiceShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      markerPos = markerPos

  uniformV3 uCamera eyePos
  uniformF  uTime time


  withVAO (sVAO voiceShape) $ do

    voices <- use wldVoices
    forM_ ( zip [0..] ( Map.toList voices ) ) $ \( i , (objID, obj) ) -> do

      let model = mkTransformation (obj ^. voxPose . posOrientation) (obj ^. voxPose . posPosition)
          points = ( obj ^. voxPoints )

      -- once we've got updated gamepal
      -- uniformV4V uPoints points
      uniformV4 uPoint1 ( ( obj ^. voxPoints ) !! 0 ) 
      uniformV4 uPoint2 ( ( obj ^. voxPoints ) !! 1 ) 
      uniformV4 uPoint3 ( ( obj ^. voxPoints ) !! 2 ) 
      uniformV4 uPoint4 ( ( obj ^. voxPoints ) !! 3 )
      uniformV4 uPoint5 ( ( obj ^. voxPoints ) !! 4 )
      uniformV4 uPoint6 ( ( obj ^. voxPoints ) !! 5 )
      uniformV4 uPoint7 ( ( obj ^. voxPoints ) !! 6 )
      uniformV4 uPoint8 ( ( obj ^. voxPoints ) !! 7 )


      drawShape model projection viewMat voiceShape



{-

  Helper functions for drawing


-}

drawShape :: MonadIO m  => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawShape model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr




{-

  lil random function for positions

-}

getBasePoint i = position

  where x = sin( i * 10 ) * 6
        y = cos( i * 40 ) * 10
        z = sin( i * 100 ) * 4
        position = V3 x y z


getBasePoint4 i = position

  where x = sin(i * 0.6)
        y = cos(i * 0.2)
        z = sin(i * 0.3)
        position = V4 x y z (i+1)








 