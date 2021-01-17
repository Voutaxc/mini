package miniplc0java.symbol;

import miniplc0java.instruction.*;
public class SymbolFn extends Symbol{

    private DataType retType;
    

    private int param_slot;
    private int loc_slot;
    private int ret_slot;

    /**the instruction list of the function*/
    public InstructionList instructionList;

    public SymbolFn(int index_global, int index_local, int index_param) {
        super(index_global, index_local, index_param);
        this.retType = DataType.VOID;
        this.instructionList = new InstructionList();
    }

    public SymbolFn(int index_global, int index_local, int index_param, int param_slot, int loc_slot, int ret_slot, DataType retType){
        super(index_global, index_local, index_param);
        this.retType = DataType.VOID;
        this.instructionList = new InstructionList();
        this.param_slot = param_slot;
        this.loc_slot = loc_slot;
        this.ret_slot = ret_slot;
        this.retType = retType;
    }

    public DataType getRetType() {
        return retType;
    }

    public void setRetType(DataType retType) {
        this.retType = retType;
    }

    public int getParam_slot() {
        return param_slot;
    }

    public void setParam_slot(int param_slot) {
        this.param_slot = param_slot;
    }

    public int getLoc_slot() {
        return loc_slot;
    }

    public void setLoc_slot(int loc_slot) {
        this.loc_slot = loc_slot;
    }

    public int getRet_slot() {
        return ret_slot;
    }

    public void setRet_slot(int ret_slot) {
        this.ret_slot = ret_slot;
    }

    public InstructionList getInstructionList() {
        return instructionList;
    }

    public void setInstructionList(InstructionList instructionList) {
        this.instructionList = instructionList;
    }

    
    @Override
    public String toString(){
        return "index_global: " + getIndex_global() +
        " index_local: " +  getIndex_local() + 
        " index_prarm: " + getIndex_param() +
        " rettype:" + retType + 
        " param_slot:" + param_slot + 
        " loc_slot:" + loc_slot +
        " ret_slot" + ret_slot +
        "\n" + instructionList.toString();

    }

}
