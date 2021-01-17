package miniplc0java.symbol;

public abstract class Symbol {
    private int index_global;
    private int index_local;
    private int index_param;
    Symbol(int index_global, int index_local, int index_param){
        this.index_global = index_global;
        this.index_local = index_local;
        this.index_param = index_param;
    }

    public int getIndex_global() {
        return index_global;
    }

    public void setIndex_global(int index_global) {
        this.index_global = index_global;
    }

    public int getIndex_local() {
        return index_local;
    }

    public void setIndex_local(int index_local) {
        this.index_local = index_local;
    }

    public int getIndex_param() {
        return index_param;
    }

    public void setIndex_param(int index_param) {
        this.index_param = index_param;
    }
    
}
