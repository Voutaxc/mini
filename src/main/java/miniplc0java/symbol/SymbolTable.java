package miniplc0java.symbol;

import java.util.LinkedHashMap;
import java.util.Map;

public class SymbolTable {
    
    private LinkedHashMap<String, Symbol> symbolTable = new LinkedHashMap<>();

    /** set after initialization time */
    private int fatherBlockId;
    /** set at initialization time, can not edit*/
    private int level;

    public SymbolTable(int level){
        this.level = level;
    }

    public int getFatherBlockId() {
        return fatherBlockId;
    }

    public void setFatherBlockId(int fatherBlockId) {
        this.fatherBlockId = fatherBlockId;
    }

    /**get a symbol (key:name) from the symbol table, return null if not found*/
    public Symbol get(String name){
        return symbolTable.get(name);
    }

    /** Inserts a symbol (key:name) into the symbol table, overrides it if it already exists */
    public void put(String name, Symbol symbol){
        symbolTable.put(name, symbol);
    }

    public LinkedHashMap<String, Symbol> getSymbolTable(){
        return this.symbolTable;
    }
    /**return the size of the symbol table */
    public int size(){
        return symbolTable.size();
    }

    public int getLevel() {
        return level;
    }
    
    public void cat(SymbolTable cat){
        for (Map.Entry<String, Symbol> entry: cat.getSymbolTable().entrySet())
            this.symbolTable.put(entry.getKey(), entry.getValue());
    }

    public void clear(){
        this.symbolTable.clear();
    }

    @Override
    public String toString(){
        StringBuilder s = new StringBuilder("");
        s.append("father: " + fatherBlockId + "\n");
        if(symbolTable.isEmpty()){
            s.append("the table is empty\n");
        }
        else
            for (Map.Entry<String, Symbol> entry: symbolTable.entrySet())
                s.append("name: " + entry.getKey() + ", " + entry.getValue().toString());
        
                return s.toString() + "\n";
    }
}
