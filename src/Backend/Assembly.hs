module Backend.Assembly where
import Backend.TAC.TAC
import Backend.TAC.Monad
import Backend.FlowGraph.DSatur
import Data.Maybe
import qualified AST

returnReg astType = Id $ Reg "$v0" astType

tacToAssembly :: Instruction -> String
tacToAssembly (ThreeAddressCode Assign (Just x) (Just y@Constant{}) Nothing)        = assemblyInst "li" (Just x) (Just y) Nothing
tacToAssembly (ThreeAddressCode Assign (Just x) (Just y) Nothing)                   = assemblyInst "add" (Just x) (Just y) (Just zeroReg)

-- TODO use floating point arithmetic if necessary
tacToAssembly (ThreeAddressCode Add (Just x) (Just y) (Just z@Constant{}))          = assemblyInst "addi" (Just x) (Just y) (Just z)
tacToAssembly (ThreeAddressCode Add (Just x) (Just y@Constant{}) (Just z))            = assemblyInst "addi" (Just x) (Just z) (Just y)
tacToAssembly (ThreeAddressCode Add (Just x) (Just y) (Just z))                     = assemblyInst "add" (Just x) (Just y) (Just z)

tacToAssembly (ThreeAddressCode Minus (Just x) (Just y) Nothing)         = assemblyInst "sub" (Just x) (Just zeroReg) (Just y)
tacToAssembly (ThreeAddressCode Sub (Just x) (Just y) (Just z))          = assemblyInst "sub" (Just x) (Just y) (Just z)

-- TODO use floating point arithmetic if necessary
tacToAssembly (ThreeAddressCode Mult (Just x) (Just y) (Just z))         = assemblyInst "mult" (Just y) (Just z) Nothing ++ "\n" ++ assemblyInst "mflo" (Just x) Nothing Nothing
tacToAssembly (ThreeAddressCode Div (Just x) (Just y) (Just z))          = assemblyInst "div" (Just y) (Just z) Nothing ++ "\n" ++ assemblyInst "mflo" (Just x) Nothing Nothing

-- TODO: Cast between int and float
tacToAssembly (ThreeAddressCode (Cast _ toType) (Just x) (Just y) _)     = ""

tacToAssembly (ThreeAddressCode GoTo Nothing Nothing (Just label))       = assemblyInst "j" (Just label) Nothing Nothing
tacToAssembly (ThreeAddressCode If Nothing (Just b) (Just label))        = assemblyInst "bne" (Just b) (Just zeroReg) (Just label)
tacToAssembly (ThreeAddressCode IfFalse Nothing (Just b) (Just label))   = assemblyInst "beq" (Just b) (Just zeroReg) (Just label)
tacToAssembly (ThreeAddressCode Eq (Just x) (Just y) (Just label))       = assemblyInst "beq" (Just x) (Just y) (Just label)
tacToAssembly (ThreeAddressCode Neq (Just x) (Just y) (Just label))      = assemblyInst "bne" (Just x) (Just y) (Just label)
tacToAssembly (ThreeAddressCode Lez (Just x) Nothing (Just label))       = assemblyInst "blez" (Just x) Nothing (Just label)

tacToAssembly (ThreeAddressCode Get r@(Just reg@(Id Reg{})) v1@(Just addr) offset@(Just i@Constant{})) =
    tacToMoveInstruction "l" r offset v1

-- transform `$x = $i($y)` into `$x = $y + $i    $x = 0($x)`
tacToAssembly (ThreeAddressCode Get r@(Just reg@(Id Reg{})) v1@(Just addr@(Id Reg{})) offset@(Just i)) =
    assemblyInst "add" r v1 offset ++ "\n" ++
    tacToMoveInstruction "l" r (Just zeroConstant) r

tacToAssembly (ThreeAddressCode Get v@(Just var) v1@(Just addr) offset@(Just i)) =
    tacToMoveInstruction "s" v offset v1

-- TODO: En el tac, los $i($addr) cambiarlos por 0($i + $addr)
tacToAssembly (ThreeAddressCode Set v1@(Just addr) offset@(Just i@Constant{}) r@(Just feg)) =
    tacToMoveInstruction "s" r offset v1

tacToAssembly (ThreeAddressCode Get (Just x) (Just y) (Just i))          = error $ "Get " ++ show x ++ " := " ++ show y ++ "[" ++ show i ++ "]"
tacToAssembly (ThreeAddressCode Set (Just x) (Just i) (Just y))          = error $ "Set " ++ show x ++ "[" ++ show i ++ "] := " ++ show y

tacToAssembly (ThreeAddressCode NewLabel Nothing (Just label) Nothing)   = show label ++ ":"

tacToAssembly (ThreeAddressCode Call Nothing (Just label) (Just _))      = assemblyInst "jal" (Just label) Nothing Nothing
tacToAssembly (ThreeAddressCode Call (Just t) (Just label) (Just _))         =
    assemblyInst "jal" (Just label) Nothing Nothing ++ "\n" ++
    -- TODO: Use floating point arithmetic if necessary
    assemblyInst "add" (Just t) (Just $ returnReg (getType t)) (Just zeroReg)

tacToAssembly (ThreeAddressCode Return Nothing Nothing Nothing)          = "\tjr $ra" 
tacToAssembly (ThreeAddressCode Return Nothing (Just t) Nothing)         =
    -- TODO: Use floating point arithmetic if necessary
    assemblyInst "add" (Just $ returnReg (getType t)) (Just t) (Just zeroReg) ++
    "\n\tjr $ra" 

tacToAssembly (ThreeAddressCode Sbrk (Just t) (Just sz) Nothing) =
    syscall 9 sz Nothing ++ "\n" ++
    assemblyInst "add" (Just t) (Just $ returnReg (AST.Simple "quarter")) (Just zeroReg)

tacToAssembly (ThreeAddressCode Print Nothing (Just x) Nothing)
    -- Print integer
    | getType x `elem` map AST.Simple ["half", "whole", "quarter", "eighth"] =
        syscall 1 x Nothing
    -- Print float
    | getType x == AST.Simple "32th" =
        syscall 2 x Nothing
    -- Print double
    | getType x == AST.Simple "64th" =
        syscall 3 x Nothing
    -- Print string
    | otherwise =
        syscall 4 x Nothing

tacToAssembly (ThreeAddressCode Read Nothing (Just x) maybeSize)
    -- Read integer
    | getType x `elem` map AST.Simple ["half", "whole", "quarter", "eighth"] =
        syscall 5 x Nothing
    -- Read float
    | getType x == AST.Simple "32th" =
        syscall 6 x Nothing
    -- Read double
    | getType x == AST.Simple "64th" =
        syscall 7 x Nothing
    -- Read string
    | otherwise =
        syscall 8 x maybeSize

tacToAssembly (ThreeAddressCode Entry Nothing Nothing Nothing)           = ""
tacToAssembly (ThreeAddressCode Exit Nothing Nothing Nothing)            = ""

tacToAssembly (ThreeAddressCode Load r@(Just reg) v1@(Just x) Nothing) =
    tacToMoveInstruction "l" r v1 Nothing

-- TODO: transform `$x = $i($y)` into `$x = $y + $i    $x = 0($x)`
tacToAssembly (ThreeAddressCode Load r@(Just reg) v1@(Just addr) offset@(Just _)) =
    tacToMoveInstruction "l" r offset v1

tacToAssembly (ThreeAddressCode Store r@(Just reg) v1@(Just x) Nothing) =
    tacToMoveInstruction "s" r v1 Nothing

-- TODO: transform `$i($y) = $x` into `$x = $y + $i    $0($x) = $x`
tacToAssembly (ThreeAddressCode Store r@(Just reg) v1@(Just addr) offset@(Just _)) =
    tacToMoveInstruction "s" r offset v1

moveInstructions = ["lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw"]

tacToMoveInstruction :: String -> Maybe Value -> Maybe Value -> Maybe Value -> String
tacToMoveInstruction move (Just reg) mayVal2 mayVal3
    | getType reg `elem` [AST.Simple "whole", AST.Simple "half"] =
        assemblyInst (move ++ "b") (Just reg) mayVal2 mayVal3
    | getType reg == AST.Simple "quarter" = 
        assemblyInst (move ++ "h") (Just reg) mayVal2 mayVal3
    | getType reg == AST.Simple "eighth" =
        assemblyInst (move ++ "w") (Just reg) mayVal2 mayVal3
    | otherwise =
        assemblyInst (move ++ "w") (Just reg) mayVal2 mayVal3

assemblyInst :: String -> Maybe Value -> Maybe Value -> Maybe Value -> String
assemblyInst op mayVal1 mayVal2 mayVal3
    | op `elem` moveInstructions =
        "\t" ++ op ++ " " ++ justMaybeValue mayVal1 ++ " " ++ justMaybeValue mayVal2 ++ if isJust mayVal3 then "(" ++ justMaybeValue mayVal3 ++ ")" else ""
    | otherwise =    
        "\t" ++ op ++ " " ++ justMaybeValue mayVal1 ++ " " ++ justMaybeValue mayVal2 ++ " " ++ justMaybeValue mayVal3

syscall :: Int -> Value -> Maybe Value -> String
syscall v0 a0 maybeA1=
    assemblyInst "li" (Just $ returnReg (AST.Simple "quarter")) (Just $ toEighthConstant v0) Nothing ++ "\n" ++
    assemblyInst "add" (Just $ Id $ Reg "$a0" (AST.Simple "quarter")) (Just a0) (Just zeroReg) ++ "\n" ++
    if isJust maybeA1 then assemblyInst "add" (Just $ Id $ Reg "$a1" (AST.Simple "quarter")) maybeA1 (Just zeroReg) ++ "\n" else "" ++
    assemblyInst "syscall" Nothing Nothing Nothing

justMaybeValue = maybe "" show

generateAssembly :: [Instruction] -> String
generateAssembly tac = unlines $ map tacToAssembly tac