package miniplc0java.instruction;

public enum InstructionType {
    nop, pushI, pushF, pop, popN, dup, locA, argA, globA, load8, load16, load32, load64,
    store8, store16, store32, store64, alloc, free, stackAlloc, addI, subI, mulI,
    divI, addF, subF, mulF, divF, divU, shl, and, or, xor, not, cmpI, cmpU, cmpF,
    negI, negF, itof, ftoi, shrl, setLt, setGt, brforward,brbackward, br, brFalse, brTrue, call, ret,
    callName, scanI, scanC, scanF, printI, printC, printF, printS, printLn, shr, panic
}
