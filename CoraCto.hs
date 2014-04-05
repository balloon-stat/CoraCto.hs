import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
import System.Exit
import System.Random
import Control.Monad
import Data.IORef
import Data.Array.IO

timerInterval = 40
width = 7
height = 16
droprate = 20
blsize = 0.066 :: GLfloat
data Block = NONE | WALL | RED | GREEN | PURPLE | BLUE | CYAN | YELLOW
    deriving(Show, Eq, Bounded, Enum)
data Key = KDrop | KCycle | KRight | KLeft
    deriving(Show, Eq, Ord, Enum, Ix)
type Field = IOArray (Int, Int) Block
type Chunk = IOArray (Int) (Int, Int, Block)
type Keystate = IOArray (Main.Key) (Bool, Int)

main = do
    view <- newField
    landed <- newField
    chunk <- newChunk
    keystate <- newKeystate
    frame <- newIORef 0
    isgameover <- newIORef False
    nextChunk chunk
    initialDisplayMode $= [RGBAMode, DoubleBuffered]
    initialWindowSize $= Size 640 480
    
    createWindow "CoraCto"
    displayCallback $= display view isgameover
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardProc keystate)
    addTimerCallback timerInterval $ timerProc (mainProc frame view landed chunk keystate isgameover)

    mainLoop

newField :: IO(Field)
newField = 
    newListArray ((0, 0), (width, height))
        [ if x == 0 || x == width || y == 0 then WALL else NONE
                          | x <- [0..width], y <- [0..height] ]

newChunk :: IO(Chunk)
newChunk = newArray (0, 2) (0, 0, RED)

newKeystate :: IO(Keystate)
newKeystate = newArray (KDrop, KLeft) (False, (-1))

nextChunk :: Chunk -> IO()
nextChunk chn = mapM_ writeRndJewel [0..2]
    where
        writeRndJewel y = rndColor >>= \rnd -> writeArray chn y (3, height - y, toEnum rnd)
        rndColor = getStdRandom $ randomR (fromEnum RED, fromEnum YELLOW)

display :: Field -> IORef Bool -> IO()
display view isgameover = do
    clearColor $= Color4 0.5 0.5 0.5 0.0
    clear [ColorBuffer]
    loadIdentity
    preservingMatrix $ renderField view
    isover <- readIORef isgameover 
    lineWidth $= 4.0
    lineSmooth $= Enabled
    when isover $ preservingMatrix renderGameOver
    swapBuffers

renderGameOver = do
    color (Color3 0.1 0.1 0.1 :: Color3 GLfloat)
    renderRect (-0.49::GLdouble) (-0.09) 1.12 0.25
    color (Color3 0.4 0.4 0.4 :: Color3 GLfloat)
    renderRect (-0.51::GLdouble) (-0.07) 1.12 0.25
    scale (0.001::GLdouble) 0.001 0.001
    w <- stringWidth MonoRoman "Stroke font"
    translate (Vector3 (-0.4*(fromIntegral w)) 0 0 ::Vector3 GLfloat)
    color (Color3 1.0 1.0 0.2 :: Color3 GLfloat)
    renderString MonoRoman "Game Over!" 
    translate (Vector3 (-0.925*(fromIntegral w)) 5 0 ::Vector3 GLfloat)
    color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
    renderString MonoRoman "Game Over!" 

renderField :: Field -> IO()
renderField view =
    mapM_ render [(x, y, toPos x width / 15.0 * 8.0, toPos y (height - 3) - 0.03)
                    | x <- [0..width], y <- [0..height-3] ]
    where
        render (x, y, px, py) = readArray view (x, y) >>= renderBlock (px, py)
        toPos index max = fromIntegral index / fromIntegral max - 0.5

renderBlock (px, py) block =
    (color $ getColor block) >> renderRect px py blsize blsize

renderRect px py w h = renderPrimitive Quads $
    mapM_ (vertex . uncurry Vertex2)
        [   (px,     py    ),
            (px,     py + h),
            (px + w, py + h),
            (px + w, py    )    ]

getColor :: Block -> Color3 GLfloat
getColor block
    | block == NONE   = Color3 1.0 1.0 1.0
    | block == WALL   = Color3 0.0 0.0 0.0
    | block == RED    = Color3 1.0 0.0 0.0
    | block == GREEN  = Color3 0.0 1.0 0.0
    | block == PURPLE = Color3 1.0 0.0 1.0
    | block == BLUE   = Color3 0.0 0.0 1.0
    | block == CYAN   = Color3 0.0 1.0 1.0
    | block == YELLOW = Color3 1.0 1.0 0.0

timerProc act = do
    act
    addTimerCallback timerInterval $ timerProc act
    
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
    lookAt (Vertex3 0.0 0.0 1.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    matrixMode $= Modelview 0

keyboardProc keystate ch state _ _
    | ch == Char 'h' = modifyDownkey isdown keystate KLeft
    | ch == Char 'j' = modifyDownkey isdown keystate KDrop
    | ch == Char 'k' = modifyDownkey isdown keystate KCycle
    | ch == Char 'l' = modifyDownkey isdown keystate KRight
    | ch == Char 'q' = exitWith ExitSuccess
    | otherwise      = return ()
    where
        isdown = state == Down

modifyDownkey True keystate key = do
    (keydown, count) <- readArray keystate key
    case keydown of
        True -> return ()
        False -> writeArray keystate key (True, 0)
modifyDownkey False keystate key = writeArray keystate key (False, -1)

operation :: Field -> Chunk -> Keystate -> IO()
operation landed chn keystate = do
    forM [KDrop .. KLeft] (isPress keystate) >>= keyOnProces landed chn
    forM [KDrop .. KLeft] (isRepeat keystate) >>= keyOnProces landed chn
    forM_ [KDrop .. KLeft] $ modifyKeycount keystate

isPress :: Keystate -> Main.Key -> IO(Main.Key, Bool)
isPress keystate key = readArray keystate key >>= \(_, count) -> return (key, count == 0)
isRepeat keystate key = readArray keystate key >>= \(_, count) ->
    return (key, count > 5 && mod count 3 == 0)

keyOnProces :: Field -> Chunk -> [(Main.Key, Bool)] -> IO()
keyOnProces landed chn ((key, False):xs) = keyOnProces landed chn xs
keyOnProces landed chn ((key, True):xs) =  keyOnProc key landed chn
keyOnProces landed chn [] = return ()

keyOnProc :: Main.Key -> Field -> Chunk -> IO()
keyOnProc key
    | key == KDrop  = opDrop
    | key == KRight = opRight
    | key == KLeft  = opLeft
    | key == KCycle = opCycle
    | otherwise     = \f c -> return ()

modifyKeycount keystate key = do
    (keydown, count) <- readArray keystate key
    case keydown of
        True  -> writeArray keystate key (True, count + 1)
        False -> return ()

mainProc :: IORef Int -> Field -> Field -> Chunk -> Keystate -> IORef Bool -> IO ()
mainProc frame view landed chn keystate isgameover = do
    isover <- readIORef isgameover
    canChunk <- canDropChunk landed chn
    canJewel <- canDropJewel landed
    (_, chunky, _) <- readArray chn 0
    unless (isover || canJewel) $ operation landed chn keystate
    frm <- get frame
    when (frm `mod` droprate == 0 && not isover) $
        if | canJewel         -> dropJewel landed
           | chunky == height -> do isvanish <- vanishJewel landed
                                    unless isvanish $ dropChunk landed chn
           | canChunk         -> dropChunk landed chn
           | otherwise        -> do
                     landChunk landed chn
                     isvanish <- vanishJewel landed
                     isover <- isGameOver landed
                     if | not isvanish && isover -> writeIORef isgameover isover
                        | otherwise ->  nextChunk chn
    copyField view landed
    overWrite view chn
    display view isgameover
    frame $~ (+1)

isGameOver landed = do
    check  [(x, y) | x <- [1..width - 1], y <- [(height - 2) .. height] ]
    where
        check [] = return False
        check (p:ps) = do
            block <- readArray landed p
            if | block == NONE -> check ps
               | otherwise     -> return True
            
copyField view landed = 
    mapM_ copy [(x, y) | x <- [0..width], y <- [0..height] ]
    where
        copy pos = readArray landed pos >>= writeArray view pos

overWrite view chn = mapM_ over [0..2]
    where
        over i = readArray chn i >>= \(x, y, b) -> writeArray view (x, y) b

landChunk landed chn =
    mapM_ landJewel [0..2]
    where
        landJewel i = do (x, y, b) <- readArray chn i
                         writeArray landed (x, y) b

vanishJewel landed = do
    mark <- vanish [(x, y, d) | x <- [1..width - 1], y <- [1..height - 3],
                            d <- [(0, 1), (1, 0), (1, 1), (1, -1)] ]
    forM_ mark (\pos -> writeArray landed pos NONE)
    return $ length mark > 0
    where
        vanish [] = return []
        vanish ((x, y, d) : xyds) = do
            jewel <- readArray landed (x, y)
            if jewel /= NONE then do vs <- vanish' (x, y, d)
                                     vss <- vanish xyds
                                     return (vs ++ vss)
                             else vanish xyds
        vanish' (x, y, (dx, dy)) = do
            jewel <- readArray landed (x, y)
            nextj <- readArray landed (x + dx, y + dy)
            if jewel == nextj
                then marking (x, y, (dx, dy)) 2
                else return []
        marking (x, y, (dx, dy)) n = do
            jewel <- readArray landed (x, y)
            nextj <- readArray landed (x + n * dx, y + n * dy)
            if jewel == nextj
                then marking (x, y, (dx, dy)) (n + 1) 
                else if n > 2 then return [(x + n * dx, y + n * dy) | n <- [0 .. n - 1] ]
                              else return []
        
dropChunk :: Field -> Chunk -> IO()
dropChunk = moveChunk (0, (-1))

dropJewel :: Field -> IO()
dropJewel landed =
    drop [(x, y) | x <- [1..width - 1], y <- [1..height - 3] ]
    where
        drop :: [(Int, Int)] -> IO()
        drop [] = return ()
        drop (p@(x, y):ps) = do
            block <- readArray landed p
            if block == NONE then drop ps
                             else do under <- readArray landed (x, y - 1)
                                     if under == NONE then write (x, y) >> drop ps
                                                      else drop ps
        write p@(x, y) = do
            block <- readArray landed p
            writeArray landed (x, y - 1) block
            writeArray landed (x, y) NONE

moveChunk :: (Int, Int) -> Field -> Chunk -> IO ()
moveChunk (dx, dy) landed chn = do
    (x, y, _) <- readArray chn 2
    block <- readArray landed (x + dx, y + dy)
    mapM_ (drop $ block == NONE) [0..2]
    where
        drop True i = do
            (x, y, b) <- readArray chn i
            writeArray chn i (x + dx, y + dy, b)
        drop False i = return ()

canDropChunk landed chn = do
    (x, y, _) <- readArray chn 2
    block <- readArray landed (x, y - 1)
    return $ block == NONE

canDropJewel landed =
    check [(x, y) | x <- [1..width - 1], y <- [1..height - 3] ]
    where
        check [] = return False
        check (p@(x, y):ps) = do
            block <- readArray landed p
            if block == NONE then check ps
                             else do under <- readArray landed (x, y - 1)
                                     if under == NONE then return True
                                                      else check ps

opCycle l chn = do
    (x, y, b) <- readArray chn 0
    mapM_ modify [1, 2]
    writeArray chn 2 (x, y - 2, b)
    where
        modify i = readArray chn i >>= \(x, y, b) -> writeArray chn (i - 1) (x, y + 1, b)

opRight = moveChunk (1, 0)
opLeft = moveChunk ((-1), 0)
opDrop = \landed chn -> replicateM_ 2 ( moveChunk (0, (-1)) landed chn)
