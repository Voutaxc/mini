package miniplc0java.tokenizer;

import java.util.ArrayList;
import java.util.Optional;
import java.util.Scanner;

import miniplc0java.util.Pos;


public class StringIter {

    ArrayList<String> linesBuffer = new ArrayList<>();

    Scanner scanner;
 
    Pos ptrNext = new Pos(0, 0);

    Pos ptr = new Pos(0, 0);

    boolean initialized = false;

    Optional<Character> peeked = Optional.empty();

    public StringIter(Scanner scanner) {
        this.scanner = scanner;
    }
    public StringIter(String s) {
        this.linesBuffer.add(s);
        initialized = true;
    }
    public void readAll() {
        if (initialized) {
            return;
        }
        while (scanner.hasNext()) {
            linesBuffer.add(scanner.nextLine() + '\n');
        }
        // todo:check read \n?
        initialized = true;
    }


    public Pos nextPos() {
        if (ptr.row >= linesBuffer.size()) {
            throw new Error("advance after EOF");
        }
        if (ptr.col == linesBuffer.get(ptr.row).length() - 1) {
            return new Pos(ptr.row + 1, 0);
        }
        return new Pos(ptr.row, ptr.col + 1);
    }

    public Pos currentPos() {
        return ptr;
    }

    public Pos previousPos() {
        if (ptr.row == 0 && ptr.col == 0) {
            throw new Error("previous position from beginning");
        }
        if (ptr.col == 0) {
            return new Pos(ptr.row - 1, linesBuffer.get(ptr.row - 1).length() - 1);
        }
        return new Pos(ptr.row, ptr.col - 1);
    }


    public char nextChar() {
        if (this.peeked.isPresent()) {
            char ch = this.peeked.get();
            this.peeked = Optional.empty();
            this.ptr = ptrNext;
            return ch;
        } else {
            char ch = this.getNextChar();
            this.ptr = ptrNext;
            return ch;
        }
    }

    private char getNextChar() {
        if (isEOF()) {
            return 0;
        }
        char result = linesBuffer.get(ptrNext.row).charAt(ptrNext.col);
        ptrNext = nextPos();
        return result;
    }

    public char peekChar() {
        if (peeked.isPresent()) {
            return peeked.get();
        } else {
            char ch = getNextChar();
            this.peeked = Optional.of(ch);
            return ch;
        }
    }

    public Boolean isEOF() {
        return ptr.row >= linesBuffer.size();
    }


    public void unreadLast() {
        ptr = previousPos();
    }

}
