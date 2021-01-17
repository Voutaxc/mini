package miniplc0java.instruction;

public class Instruction {

    private InstructionType type;
    boolean hasNum;
    
    boolean is_u32;
    boolean is_d64;
    boolean needRelocation;
    private int num_u32;
    private long num_u64;
    private double num_d64;

    public Instruction(InstructionType type){
        this.type = type;
        this.hasNum = false;
        this.needRelocation = false;
    }
    public Instruction(InstructionType type, boolean is_u32, long num){
        this.type = type;
        this.hasNum = true;
        this.is_d64 = false;
        this.needRelocation = false;
        if(is_u32)
            this.num_u32 = (int)num;
        else
            this.num_u64 = num;
    }
    public Instruction(InstructionType type, double num){
        this.needRelocation = false;
        this.type = type;
        this.hasNum = true;
        this.is_u32 = false;
        this.is_d64 = true;
        this.num_d64 = num;
        
    }
    //getter and setter
    public InstructionType getType() {
        return type;
    }

    public void setType(InstructionType type) {
        this.type = type;
    }

    public boolean hasNum() {
        return hasNum;
    }

    public boolean is_u32() {
        return is_u32;
    }

    public int getNum_u32() {
        return num_u32;
    }

    public void setNum_32(int num) {
        this.num_u32 = num;
    }
 
    public long getNum_u64() {
        return num_u64;
    }

    public void setNum_64(int num) {
        this.num_u64 = num;
    }

    public double getNum_d64(){
        return this.num_d64;
    }
    
    @Override
    public String toString() {
        switch (this.type) {
            case nop: return "nop";
            case not: return "not";
            case pushI: {
                if (hasNum()) {
                    if (is_u32) {
                        return "push " + num_u32;
                    }
                    else return "push " + num_u64;
                }
                else return "push";
            }
            case pushF: {
                if (hasNum()) return "push " + num_d64;
                else return "push";
            }
            case pop: return "pop";
            case popN: {
                if (hasNum()) return "popN " + num_u32;
                else return "popN";
            }
            case locA: {
                if (hasNum()) return "locA " + num_u32;
                else return "locA";
            }
            case argA: {
                if (hasNum()) return "argA " + num_u32;
                else return "argA";
            }
            case globA: {
                if (hasNum()) return "globA " + num_u32;
                else return "globA";
            }
            case load64: return "load64";
            case store64: return "store64";
            case stackAlloc: {
                if (hasNum()) return "stackAlloc " + num_u32;
                else return "stackAlloc";
            }
            case addI: return "addI";
            case addF: return "addF";
            case subI: return "subI";
            case subF: return "subF";
            case mulI: return "mulI";
            case mulF: return "mulF";
            case divI: return "divI";
            case divF: return "divF";
            case cmpI: return "cmpI";
            case cmpF: return "cmpF";
            case negI: return "negI";
            case negF: return "negF";
            case itof: return "itof";
            case ftoi: return "ftoi";
            case setLt: return "setLt";
            case setGt: return "setGt";
            case br: {
                if (hasNum()) return "br " + num_u32;
                else return "br";
            }
            case brTrue: {
                if (hasNum()) return "brTrue " + num_u32;
                else return "brTrue";
            }
            case call: {
                if (hasNum()) return "call " + num_u32;
                else return "call";
            }
            case ret: return "ret";
            case callName: {
                if (hasNum()) return "callName " + num_u32;
                else return "callName";
            }
            default:
                return "panic";
        }
    }

    public boolean isNeedRelocation() {
        return needRelocation;
    }

    public void setNeedRelocation(boolean needRelocation) {
        this.needRelocation = needRelocation;
    }
}
