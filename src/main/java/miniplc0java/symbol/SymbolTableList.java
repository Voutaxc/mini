package miniplc0java.symbol;

import java.util.ArrayList;
import java.util.List;

public class SymbolTableList {
    private List<SymbolTable> symbolTableList = new ArrayList<>();
    
    /**Initializes the Stack symbol table set*/
    public SymbolTableList(){

        SymbolTable fnSymbolTable = new SymbolTable(-1);
        fnSymbolTable.setFatherBlockId(-1);

        SymbolTable stringSymbolTable = new SymbolTable(-1);
            stringSymbolTable.setFatherBlockId(-1);

        SymbolTable globalSymbolTable = new SymbolTable(0);
        globalSymbolTable.setFatherBlockId(1);

        SymbolFn start = new SymbolFn(0, -1, -1);
        start.setParam_slot(0);
        start.setRet_slot(0);
        start.setLoc_slot(0);
        
        fnSymbolTable.put("_start", start);

        symbolTableList.add(fnSymbolTable);
        symbolTableList.add(stringSymbolTable);
        symbolTableList.add(globalSymbolTable);
    }
    
    public int size(){
        return symbolTableList.size();
    }

    /** add a new local var table*/
    public void addSymbolTable(int level){
        
        SymbolTable add = new SymbolTable(level);

        SymbolTable last = symbolTableList.get(symbolTableList.size() - 1);
        
        if(last.getLevel() == add.getLevel())
            add.setFatherBlockId(last.getFatherBlockId());
        else
            add.setFatherBlockId(symbolTableList.size() - 1);
        symbolTableList.add(add);
    }

    /**when a block ends, pop the local var table from the symbol table list */
    public void popSymbolTable(){
        if(symbolTableList.size() <= 2){
            System.out.println("Should not pop the global var table or the function table");
        }
        symbolTableList.remove(symbolTableList.size() - 1);
    }

    /**get the function table */
    public SymbolTable getFunctionSymbolTable(){
        return symbolTableList.get(0);
    }

    /**get the current local var table */
    public SymbolTable getCurrentSymbolTable(){
        return symbolTableList.get(symbolTableList.size() - 1);
    }

    /**get the constant String symbol */
    public SymbolTable getStringSymbolTable(){
        return symbolTableList.get(1);
    }
      
    /**get the global var table */
    public SymbolTable getGlobalSymbolTable(){
        return symbolTableList.get(2);
    }

    /** get the function's local var table */
    public SymbolTable getLocalSymbolTable(SymbolFn fn){
        if(fn == null)return null;
        return symbolTableList.get(fn.getIndex_local());
    }

    /** search for the symbol*/
    public Symbol searchSymbol(String name){
        Symbol ret = null;
        SymbolTable t = getCurrentSymbolTable();
        do{    
            ret = t.get(name);
            if(t.getFatherBlockId() == -1)break;
            t = symbolTableList.get(t.getFatherBlockId());
        }while(ret == null);
        return ret;
    }

    public Symbol searchDuplicatedSymbolVar(String name){
        SymbolTable t = getCurrentSymbolTable();
        return t.get(name);
    }

    /**search for the symbolfn */
    public SymbolFn searchSymbolFn(String name){
        SymbolTable t = getFunctionSymbolTable();
        Symbol fn = t.get(name);
        if(fn == null)
            return null;
        else
            return ((SymbolFn)fn);
    }

    /**search for the String */
    public SymbolVar searchStringSymbol(String name){
        SymbolTable t = getStringSymbolTable();
        SymbolVar string = (SymbolVar)t.get(name);
        if(string == null)
            return null;
        else
            return string;
    }

    public void print(){

        for(int i = 0; i < symbolTableList.size(); i++){
            String id;
            if(i == 0)id = "Function table";
            else if (i == 1)id = "String table";
            else if (i == 2)id = "Global Var table";
            else id = String.valueOf(i);
            System.out.print(id + "：\n" + symbolTableList.get(i));
        }
    }
}
