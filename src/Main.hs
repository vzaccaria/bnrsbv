{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Main where

import           Data.Array   (Array, Ix (..), array, (!), (//))
import           Data.SBV
import           GHC.Generics (Generic)

-- | All of the following pretty much taken from SBV Examples
type Address = SWord32

type Value = SWord32

-- | Convenient synonym for symbolic machine bits.
type Bit = SBool

-- | Register bank
type Registers = Array Register Value

type Memory = Model Word32 Word32        -- Model defined later

type Model = SFunArray

data ARM = ARM
    { memory    :: Memory
    , registers :: Registers
    , stackP    :: Address
    } deriving (Generic,Mergeable)

type Extract a = ARM -> a

type Program = ARM -> ARM

type Instruction = Program -> Program

getReg :: Register -> Extract Value
getReg r m = registers m ! r

-- | Set the value of a given register
setReg
    :: Register -> Value -> Program
setReg r v m =
    m
    { registers = registers m // [(r, v)]
    }

loadImmReg :: Register -> Value -> Instruction
loadImmReg r v k = k . setReg r v

-- | Read memory
peek
    :: Address -> Extract Value
peek a m = readArray (memory m) a

-- | Write to memory
poke
    :: Address -> Value -> Program
poke a v m =
    m
    { memory = writeArray (memory m) a v
    }

data Register
    = R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | SB
    | SL
    | FP
    | IP
    | LR
    deriving (Eq,Ord,Ix,Bounded,Enum)

pushReg :: Register -> Instruction
pushReg r k m =
    let regVal = getReg r m
    in k . poke (stackP m) regVal $ m

-- | The 'end' combinator "stops" our program, providing the final continuation
-- that does nothing.
end
    :: Program
end = id

program :: SWord32 -> Program
program v = start
  where
    start = loadImmReg R1 v $ pushReg R1 end

runProgram :: SWord32 -> Address -> ARM -> SWord32
runProgram v a m = vFinal
  where
    mFinal = program v m
    vFinal = peek a mFinal

initARM :: Memory -> Address -> Value -> ARM
initARM m a v =
    ARM
    { memory = m
    , registers = array
          (minBound, maxBound)
          [ (R0, v)
          , (R1, v)
          , (R2, v)
          , (R3, v)
          , (R4, v)
          , (R5, v)
          , (R6, v)
          , (R7, v)
          , (R8, v)
          , (SB, v)
          , (SL, v)
          , (FP, v)
          , (IP, v)
          , (LR, v)]
    , stackP = a
    }

programIsCorrect :: Memory -> Address -> SWord32 -> SBool
programIsCorrect m a v = runProgram v a m0 .== v
  where
    m0 = initARM m 0 a

theorem =
    prove $
    do v <- sWord32 "x"
       return $ programIsCorrect mem 0 v
  where
    mem = mkSFunArray (const 0)

--   push_w {r2, r4, r5, r6, r7, r8, sb, sl, fp, ip, lr}
--   ldm_w r1, {r4, r5, r6, r7}
--   ldm_w r0!, {r8, sb, sl, fp}
--   mov_w lr, r0
--   subw ip, pc, #0x774
--   eor_w r4, r4, r8
--   eor_w r5, r5, sb
main
    :: IO ()
main = do
    putStrLn "hello world"
