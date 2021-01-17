package miniplc0java;
import miniplc0java.instruction.*;
import miniplc0java.symbol.*;

import miniplc0java.analyser.*;


import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;

import java.util.List;
import java.util.Map;

public class OutPutter {
    private List<Byte> byteList = new ArrayList<>();
    private Analyser analyser;
    private boolean debug = false;
    public List<Byte> getBinaryList() {
        /* magic、version */
        byteList.addAll(byteTrans(4, 0x72303b3e));
        byteList.addAll(byteTrans(4, 1));
        if(debug)System.out.println("");
        int preGlobalSymbolTableSize = analyser.symbolTableList.getGlobalSymbolTable().size();
        int preFunctionSymbolTableSize = analyser.symbolTableList.getFunctionSymbolTable().size();
        SymbolTable globalTable = analyser.symbolTableList.getGlobalSymbolTable();
        globalTable.cat(analyser.symbolTableList.getFunctionSymbolTable());
        globalTable.cat(analyser.symbolTableList.getStringSymbolTable());
        if(debug)System.out.print("\nglobal symbol size:");
        // count
        byteList.addAll(byteTrans(4, globalTable.size()));
        int i = 0;
        for (Map.Entry<String, Symbol> entry : globalTable.getSymbolTable().entrySet()) {
            String name = entry.getKey();
            Symbol sym = entry.getValue();
            if(debug)System.out.print("\n" + i + "\nis Const:");
            // isConst
            if (i >= preGlobalSymbolTableSize || sym instanceof SymbolVar && ((SymbolVar) sym).isConstant())
                byteList.addAll(byteTrans(1, 1));
            else
                byteList.addAll(byteTrans(1, 0));
                
            if (i >= preGlobalSymbolTableSize) {
                if(debug)System.out.print("\nsize:");
                byteList.addAll(byteTrans(4, name.length()));
                if(debug)System.out.print("\nvalue:");
                byteList.addAll(byteTrans(name));
            } 
            else {
                if(debug)System.out.print("\nsize:");
                byteList.addAll(byteTrans(4, 8));
                if(debug)System.out.print("\nvalue:");
                byteList.addAll(byteTrans(8, 0));
            }
            if(debug)System.out.println("");
            i++;
        }
        if(debug)System.out.println("\nfunction num:");
        /** function */

        SymbolTable functionTable = analyser.symbolTableList.getFunctionSymbolTable();
        // count
        byteList.addAll(byteTrans(4, functionTable.size()));
        if(debug)System.out.println("");
        i = 0;
        for (Map.Entry<String, Symbol> entry : functionTable.getSymbolTable().entrySet()) {
            if(debug)System.out.println( i + ":");
            SymbolFn fn = (SymbolFn) entry.getValue();
            byteList.addAll(byteTrans(4, fn.getIndex_global()));
            if(debug)System.out.print("\nret_slot:");
            byteList.addAll(byteTrans(4, fn.getRet_slot()));
            if(debug)System.out.print("param_slot:");
            byteList.addAll(byteTrans(4, fn.getParam_slot()));
            if(debug)System.out.print("loc_slot:");
            byteList.addAll(byteTrans(4, fn.getLoc_slot()));
            if(debug)System.out.println("\ninstructionList size:");
            byteList.addAll(byteTrans(4, fn.getInstructionList().size()));
            if(debug)System.out.println("\ninstructions:");
            for (Instruction ins : fn.getInstructionList().getInstructionList()) {
                // instruction
                if(debug)System.out.print("\nins type:");
                byteList.addAll(byteInstruction(ins.getType()));
                if(debug)System.out.print("\nins num:");
                if(ins.isNeedRelocation()){
                    if(ins.getType() == InstructionType.callName){
                        byteList.addAll(byteTrans(4, ins.getNum_u32() + preGlobalSymbolTableSize));
                    }
                    else if(ins.getType() == InstructionType.pushI){
                        byteList.addAll(byteTrans(8, (long)(ins.getNum_u64() + preGlobalSymbolTableSize + preFunctionSymbolTableSize)));
                    }
                }
                
                else if (ins.hasNum()) {
                    if (ins.getType() == InstructionType.pushI) {
                        byteList.addAll(byteTrans(8, ins.getNum_u64()));
                    } else if (ins.getType() == InstructionType.pushF)
                        byteList.addAll(byteTrans(8, ins.getNum_d64()));
                    else
                        byteList.addAll(byteTrans(4, ins.getNum_u32()));
                }

            }
        }
        return byteList;
    }

    public OutPutter(Analyser analyser) {
        this.analyser = analyser;
    }

    public List<Byte> getByteList() {
        return byteList;
    }

    public void print(PrintStream output) throws IOException {
        int a[] = new int[8];
        byte[] bytes = new byte[byteList.size()];
        for(int k = 0; k < byteList.size(); k++){
            for(int i = 7; i >= 0; i--){
                a[i] = (byteList.get(k) >> i) & 0x0001;
            }
            bytes[k] = byteList.get(k);
            //if(debug)System.out.print("" + a[7] + a[6] + " " + a[5] + a[4] + " " + a[3] + a[2] + " " + a[1] + a[0] + "(" + byteList.get(k) + ")" + "\n");
        }
        output.write(bytes);
    }


    public List<Byte> byteTrans(int length, long num){
        if(debug)System.out.print(num + "(8) ");
        ArrayList<Byte> bytes = new ArrayList<>();
        int start = 8*(length-1);
        for(int i = 0; i < length; i++){
            bytes.add((byte)((num >> (start - i*8)) & 0xFF));
        }

        return bytes;
    }

    public List<Byte> byteTrans(int length, double num){
        if(debug)System.out.print(num + "(8) ");
        long val = Double.doubleToRawLongBits(num);
        ArrayList<Byte> bytes = new ArrayList<>();
        int start = 8*(length-1);
        for(int i = 0; i < length; i++){
            bytes.add((byte)((val >> (start - i*8)) & 0xFF));
        }

        return bytes;
    }

    public List<Byte> byteTrans(int length, int num){
        if(debug)System.out.print(num + "(4) ");
        ArrayList<Byte> bytes = new ArrayList<>();
        int start = 8*(length-1);
        for(int i = 0; i < length; i++){
            bytes.add((byte)((num >> (start - i*8)) & 0xFF));
        }

        return bytes;
    }

    public List<Byte> byteTrans(String str) {
        if(debug)System.out.print(" " + str);
        List<Byte> str_array = new ArrayList<>();

        for (int i = 0; i < str.length(); i ++){
            char ch = str.charAt(i);
            str_array.add((byte)(ch & 0xFF));
        }

        return str_array;
    }

    public List<Byte> byteInstruction(InstructionType opt) {
        if(opt == InstructionType.nop)
            return byteTrans(1, 0x00);
        else if(opt == InstructionType.pushI || opt == InstructionType.pushF)
            return byteTrans(1,0x01);
        else if(opt == InstructionType.pop)
            return byteTrans(1, 0x02);
        else if(opt == InstructionType.popN)
            return byteTrans(1, 0x03);
        else if(opt == InstructionType.dup)
            return byteTrans(1, 0x04);
        else if(opt == InstructionType.locA)
            return byteTrans(1, 0x0a);
        else if(opt == InstructionType.argA)
            return byteTrans(1, 0x0b);
        else if(opt == InstructionType.globA)
            return byteTrans(1, 0x0c);
        else if(opt == InstructionType.load8)
            return byteTrans(1, 0x10);
        else if(opt == InstructionType.load16)
            return byteTrans(1, 0x11);
        else if(opt == InstructionType.load32)
            return byteTrans(1, 0x12);
        else if(opt == InstructionType.load64)
            return byteTrans(1, 0x13);
        else if(opt == InstructionType.store8)
            return byteTrans(1, 0x14);
        else if(opt == InstructionType.store16)
            return byteTrans(1, 0x15);
        else if(opt == InstructionType.store32)
            return byteTrans(1, 0x16);
        else if(opt == InstructionType.store64)
            return byteTrans(1, 0x17);
        else if(opt == InstructionType.alloc)
            return byteTrans(1, 0x18);
        else if(opt == InstructionType.free)
            return byteTrans(1, 0x19);
        else if(opt == InstructionType.stackAlloc)
            return byteTrans(1, 0x1a);
        else if(opt == InstructionType.addI)
            return byteTrans(1, 0x20);
        else if(opt == InstructionType.subI)
            return byteTrans(1, 0x21);
        else if(opt == InstructionType.mulI)
            return byteTrans(1, 0x22);
        else if(opt == InstructionType.divI)
            return byteTrans(1, 0x23);
        else if(opt == InstructionType.addF)
            return byteTrans(1, 0x24);
        else if(opt == InstructionType.subF)
            return byteTrans(1, 0x25);
        else if(opt == InstructionType.mulF)
            return byteTrans(1, 0x26);
        else if(opt == InstructionType.divF)
            return byteTrans(1, 0x27);
        else if(opt == InstructionType.divU)
            return byteTrans(1, 0x28);
        else if(opt == InstructionType.shl)
            return byteTrans(1, 0x29);
        else if(opt == InstructionType.shr)
            return byteTrans(1, 0x2a);
        else if(opt == InstructionType.and)
            return byteTrans(1, 0x2b);
        else if(opt == InstructionType.or)
            return byteTrans(1, 0x2c);
        else if(opt == InstructionType.xor)
            return byteTrans(1, 0x2d);
        else if(opt == InstructionType.not)
            return byteTrans(1, 0x2e);
        else if(opt == InstructionType.cmpI)
            return byteTrans(1, 0x30);
        else if(opt == InstructionType.cmpU)
            return byteTrans(1, 0x31);
        else if(opt == InstructionType.cmpF)
            return byteTrans(1, 0x32);
        else if(opt == InstructionType.negI)
            return byteTrans(1, 0x34);
        else if(opt == InstructionType.negF)
            return byteTrans(1, 0x35);
        else if(opt == InstructionType.itof)
            return byteTrans(1, 0x36);
        else if(opt == InstructionType.ftoi)
            return byteTrans(1, 0x37);
        else if(opt == InstructionType.shrl)
            return byteTrans(1, 0x38);
        else if(opt == InstructionType.setLt)
            return byteTrans(1, 0x39);
        else if(opt == InstructionType.setGt)
            return byteTrans(1, 0x3a);
        else if(opt == InstructionType.br)
            return byteTrans(1, 0x41);
        else if(opt == InstructionType.brFalse)
            return byteTrans(1, 0x42);
        else if(opt == InstructionType.brTrue)
            return byteTrans(1, 0x43);
        else if(opt == InstructionType.call)
            return byteTrans(1, 0x48);
        else if(opt == InstructionType.ret)
            return byteTrans(1, 0x49);
        else if(opt == InstructionType.callName)
            return byteTrans(1, 0x4a);
        else if(opt == InstructionType.scanI)
            return byteTrans(1, 0x50);
        else if(opt == InstructionType.scanC)
            return byteTrans(1, 0x51);
        else if(opt == InstructionType.scanF)
            return byteTrans(1, 0x52);
        else if(opt == InstructionType.printI)
            return byteTrans(1, 0x54);
        else if(opt == InstructionType.printC)
            return byteTrans(1, 0x55);
        else if(opt == InstructionType.printF)
            return byteTrans(1, 0x56);
        else if(opt == InstructionType.printS)
            return byteTrans(1, 0x57);
        else if(opt == InstructionType.printLn)
            return byteTrans(1, 0x58);
        else if(opt == InstructionType.panic)
            return byteTrans(1, 0xfe);
        return null;
    }
}
