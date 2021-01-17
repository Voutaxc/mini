package miniplc0java.instruction;

import java.util.ArrayList;
import java.util.List;

public class InstructionList {
    private List<Instruction> instructionList = new ArrayList<>();

    public void add(Instruction instruction){
        instructionList.add(instruction);
    }

    public Instruction get(int index){
        return this.instructionList.get(index);
    }

    public void pop(){
        if(this.instructionList.isEmpty())return;
        this.instructionList.remove(this.instructionList.size() - 1);
    }

    public int size(){
        return instructionList.size();
    }

    public void print(){
        for(Instruction it : this.instructionList){
            System.out.println(it.toString());
        }
        System.out.println("");
    }

    public List<Instruction> getInstructionList() {
        return instructionList;
    }

    @Override
    public String toString(){
        StringBuilder s = new StringBuilder("");
        for(int i = 0; i<instructionList.size(); i++){
            s.append(instructionList.get(i) + "\n");
        }
        s.append("\n");
        return s.toString();

    }
    
    
}
