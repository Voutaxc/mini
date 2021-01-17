package miniplc0java.symbol;

public class SymbolVar extends Symbol{
    
    private boolean isGlobal;
    private boolean isConstant;
    private boolean isParam;
    
    private DataType dataType;
    
    public SymbolVar(int index_global, int index_local, int index_param){
        super(index_global, index_local, index_param);
        this.isConstant = this.isGlobal = this.isParam = false;
        dataType = DataType.VOID;
    }

    @Override
    public String toString(){
        return "is Constant: " + isConstant + " is Global:" + isGlobal + 
        " is Param:" + isParam +
        " datatype:" + dataType + "\n";
    }

    public boolean isConstant() {
        return isConstant;
    }

    public void setConstant(boolean isConstant) {
        this.isConstant = isConstant;
    }

    public boolean isGlobal() {
        return isGlobal;
    }

    public void setGlobal(boolean isGlobal) {
        this.isGlobal = isGlobal;
    }

    public DataType getDataType() {
        return dataType;
    }

    public void setDataType(DataType dataType) {
        this.dataType = dataType;
    }

    public boolean isParam() {
        return isParam;
    }

    public void setParam(boolean isParam) {
        this.isParam = isParam;
    }

}
