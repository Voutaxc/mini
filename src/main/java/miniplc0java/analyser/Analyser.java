package miniplc0java.analyser;


import java.util.Map;

import miniplc0java.error.*;
import miniplc0java.instruction.*;
import miniplc0java.tokenizer.*;
import miniplc0java.util.Pos;
import miniplc0java.symbol.*;

public final class Analyser {

    Tokenizer tokenizer;    
    Token peekedToken = null;
    public SymbolTableList symbolTableList;
    int level;
    int exprnum;
    int loc_slot, param_slot, ret_slot;
    SymbolFn func;
    SymbolTable funcParam;

    int breakins, continueins;

    boolean hasRet;

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        this.level = 0;
        this.symbolTableList = new SymbolTableList();
        this.func = (SymbolFn)symbolTableList.getFunctionSymbolTable().get("_start");
        this.funcParam = new SymbolTable(0);
        this.exprnum = 0;
        this.hasRet = false;
    }

    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    private Token next() throws TokenizeError {
        exprnum++;
        if (peekedToken != null) {
            var token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }


    private Token expect(TokenType tt) throws CompileError {
        exprnum++;
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * program -> decl_stmt* function*
     * @throws CompileError
     */
    public void analyseProgram() throws CompileError{


        /**mark the end of file */
        boolean eof = true;
        while(eof)
        {
            switch(peek().getTokenType()){
        
            case LET_KW:
            case CONST_KW:
                analyseDeclareStatement();
                break;
            case FN_KW:
                analyseFunction();
                break;
            default:
                if(peek().getTokenType() == TokenType.EOF)
                    eof = false;
                else
                    throw new AnalyzeError(ErrorCode.ExpectedToken, peek().getStartPos());   
            }
        }


        SymbolFn main = symbolTableList.searchSymbolFn("main");
        if(main == null)
            throw new AnalyzeError(ErrorCode.NullMainFunction, new Pos(0, 0));

        InstructionList start = symbolTableList.searchSymbolFn("_start").getInstructionList();
        start.add(new Instruction(InstructionType.stackAlloc, true, main.getRet_slot()));
        start.add(new Instruction(InstructionType.call, true, main.getIndex_global()));
        if(main.getRet_slot() != 0)
            start.add(new Instruction(InstructionType.popN, true, main.getRet_slot()));

       symbolTableList.print();
             
    }

    public void analyseFunction() throws CompileError{
        expect(TokenType.FN_KW);
        
        //init
        func = new SymbolFn(symbolTableList.getFunctionSymbolTable().size(), symbolTableList.size(), -1);
        funcParam = new SymbolTable(0);
        ret_slot = loc_slot = param_slot = 0;
        hasRet = false;
        breakins = continueins = -1;

        Token name, type;
        
        name = expect(TokenType.IDENT);
        if(name.getDataType() != DataType.NONE)
            throw new AnalyzeError(ErrorCode.InvalidType, name.getStartPos());
        if(symbolTableList.searchSymbolFn(name.getValueString()) != null)
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, name.getStartPos());

        expect(TokenType.L_PAREN);

        if(peek().getTokenType() != TokenType.R_PAREN){
            analyseFunctionParam();
            while(peek().getTokenType() == TokenType.COMMA){
                next();
                analyseFunctionParam();
            }
        }

        func.setParam_slot(param_slot);

        expect(TokenType.R_PAREN);
        expect(TokenType.ARROW);

        type = expect(TokenType.IDENT);
        if(type.getDataType() == DataType.NONE)
            throw new AnalyzeError(ErrorCode.InvalidType, type.getStartPos());
        func.setRetType(type.getDataType());
        
        if(type.getDataType() == DataType.VOID)
            ret_slot = 0;
        else
            ret_slot = 1;
        func.setRet_slot(ret_slot);

        symbolTableList.getFunctionSymbolTable().put(name.getValueString(), func);

        analyseStatementSeq();
        
        func.setLoc_slot(loc_slot);

        if(!hasRet && func.getRetType() == DataType.VOID)
            func.instructionList.add(new Instruction(InstructionType.ret));
        else if(!hasRet)
            throw new AnalyzeError(ErrorCode.NotAllRoutesReturn, name.getStartPos());

        func = (SymbolFn)symbolTableList.getFunctionSymbolTable().get("_start");
    }

    private void analyseFunctionParam() throws CompileError{
        Token name, type;
        SymbolVar var = new SymbolVar(-1, -1, param_slot);

        if(peek().getTokenType() == TokenType.CONST_KW){
            var.setConstant(true);
            next();
        }

        name = expect(TokenType.IDENT);
        if(name.getDataType() == DataType.UINT || name.getDataType() == DataType.DOUBLE)
            throw new AnalyzeError(ErrorCode.InvalidType, name.getStartPos());

        expect(TokenType.COLON);

        type = expect(TokenType.IDENT);
        if(type.getDataType() == DataType.NONE || name.getDataType() == DataType.VOID)
            throw new AnalyzeError(ErrorCode.InvalidType, type.getStartPos());
        var.setDataType(type.getDataType());

        var.setParam(true);
        funcParam.put(name.getValueString(), var);
        param_slot ++;
    }

    private void analyseStatement() throws CompileError{
        
        switch(peek().getTokenType()){
        case LET_KW:
        case CONST_KW:
            analyseDeclareStatement();
            break;
        case IF_KW:
            analyseIfStatement();
            break;
        case WHILE_KW:
            analyseWhileStatement();
            break;
        case RETURN_KW:
            analyseReturnStatement();
            break;
        case SEMICOLON:
            next();
            break;
        case L_BRACE:
            analyseStatementSeq();
            break;
        case BREAK_KW:
            analyseBreakStatement();
            break;
        case CONTINUE_KW:
            analyseContinueStatement();
            break;
        default:
            if(!analyseA())
                func.instructionList.add(new Instruction(InstructionType.popN, true, 1));
            expect(TokenType.SEMICOLON);
        }
    }

    private void analyseStatementSeq() throws CompileError{
        expect(TokenType.L_BRACE);
        level++;
        symbolTableList.addSymbolTable(level);

        symbolTableList.getLocalSymbolTable(func).cat(funcParam);
        funcParam.clear();
        boolean isStatement = true;
        while(isStatement){
            switch(peek().getTokenType()){
            case R_BRACE:
                isStatement = false;
                break;
            default:
                analyseStatement();
            }
        }
        expect(TokenType.R_BRACE);
        level--;
        if(level != 0)symbolTableList.popSymbolTable();
    }

    private void analyseIfStatement() throws CompileError{

        boolean temp = hasRet;
        boolean ifhasRet = true;
        hasRet = false;

        expect(TokenType.IF_KW);
        analyseB();
        
        func.instructionList.add(new Instruction(InstructionType.brTrue, true, 1));
        func.instructionList.add(new Instruction(InstructionType.br, true, 0));
        int ifins = func.instructionList.size();

        analyseStatementSeq();
        func.instructionList.add(new Instruction(InstructionType.br, true, 0));
        func.instructionList.get(ifins - 1).setNum_32(func.instructionList.size() - ifins);

        if(!hasRet)ifhasRet = false;
        hasRet = false;
        if(peek().getTokenType() == TokenType.ELSE_KW){
            int elseins = func.instructionList.size();

            next();

            if(peek().getTokenType() == TokenType.IF_KW)
                analyseIfStatement(); 
            else
                analyseStatementSeq();
            
            func.instructionList.get(elseins - 1).setNum_32(func.instructionList.size() - elseins);   
        }
        if(!hasRet)ifhasRet = false;
        
        hasRet = temp || ifhasRet;
    }
    
    private void analyseWhileStatement() throws CompileError{
        boolean temp = hasRet;
        int tempbreakins = breakins;
        int tempcontinueins = continueins;
        expect(TokenType.WHILE_KW);

        continueins = func.instructionList.size();
        analyseB();

        func.instructionList.add(new Instruction(InstructionType.brTrue, true, 1));
        breakins = func.instructionList.size();
        func.instructionList.add(new Instruction(InstructionType.br, true, 0));
        int whileins = func.instructionList.size();

        analyseStatementSeq();

        func.instructionList.add(new Instruction(InstructionType.br, true, 0));
        func.instructionList.get(func.instructionList.size() - 1).setNum_32(continueins - func.instructionList.size());
        func.instructionList.get(whileins - 1).setNum_32(func.instructionList.size() - whileins);
        breakins = tempbreakins;
        continueins = tempcontinueins;
        hasRet = temp;
    }
    
    private void analyseReturnStatement() throws CompileError{
        DataType retType;
        hasRet = true;
        Token ret = expect(TokenType.RETURN_KW);
        if(peek().getTokenType() != TokenType.SEMICOLON){
            func.instructionList.add(new Instruction(InstructionType.argA, 0));
            retType = analyseB();
            func.instructionList.add(new Instruction(InstructionType.store64));
        }
        else{
            retType = DataType.VOID;
            next();
        }
        if(retType != func.getRetType())
            throw new AnalyzeError(ErrorCode.InvalidReturn, ret.getStartPos());
        func.instructionList.add(new Instruction(InstructionType.ret));
    }

    private void analyseDeclareStatement() throws CompileError{

        if(peek().getTokenType() != TokenType.LET_KW && peek().getTokenType() != TokenType.CONST_KW)
            throw new AnalyzeError(ErrorCode.ExpectedToken, peek().getStartPos());

        Token name, type;
        
        SymbolVar var = new SymbolVar(symbolTableList.getGlobalSymbolTable().size(), loc_slot, param_slot);

        if(next().getTokenType() == TokenType.LET_KW)
            var.setConstant(false);
        else
            var.setConstant(true);        

        var.setGlobal(level == 0);

        name = expect(TokenType.IDENT);
        if(name.getDataType() == DataType.UINT || name.getDataType() == DataType.DOUBLE)
            throw new AnalyzeError(ErrorCode.InvalidType, name.getStartPos());

        expect(TokenType.COLON);

        type = expect(TokenType.IDENT);
        if(type.getDataType() == DataType.NONE || type.getDataType() == DataType.VOID)
            throw new AnalyzeError(ErrorCode.InvalidType, type.getStartPos());
        var.setDataType(type.getDataType());
        
        if(symbolTableList.searchDuplicatedSymbolVar(name.getValueString()) != null)
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, name.getStartPos());

        if(peek().getTokenType() != TokenType.ASSIGN && var.isConstant())
            throw new AnalyzeError(ErrorCode.ConstantNeedValue, peek().getStartPos());

        if(level == 0)
            symbolTableList.getGlobalSymbolTable().put(name.getValueString(), var);
        else{
            symbolTableList.getCurrentSymbolTable().put(name.getValueString(), var);
            loc_slot ++;
        }

        if(peek().getTokenType() == TokenType.ASSIGN){
            next();
            
            if(level == 0)
                func.instructionList.add(new Instruction(InstructionType.globA, true, var.getIndex_global()));
            else
                func.instructionList.add(new Instruction(InstructionType.locA, true, var.getIndex_local()));
            if(var.getDataType() != analyseB())
                throw new AnalyzeError(ErrorCode.TypeMismatch, name.getStartPos());
            func.instructionList.add(new Instruction(InstructionType.store64));
        }

        expect(TokenType.SEMICOLON);
    }
    
    /**analyse continue statement */
    private void analyseContinueStatement() throws CompileError{
        expect(TokenType.CONTINUE_KW);
        if(continueins == -1)throw new AnalyzeError(ErrorCode.NoContinueContext, peek().getStartPos());
        func.instructionList.add(new Instruction(InstructionType.br, true, 0));
        func.instructionList.get(func.instructionList.size() - 1).setNum_32(continueins - func.instructionList.size());
        expect(TokenType.SEMICOLON);
    }

    /**analyse break statement */
    private void analyseBreakStatement() throws CompileError{
        expect(TokenType.BREAK_KW);
        if(continueins == -1)throw new AnalyzeError(ErrorCode.NoBreakContext, peek().getStartPos());
        func.instructionList.add(new Instruction(InstructionType.br, true, 0));
        func.instructionList.get(func.instructionList.size() - 1).setNum_32(breakins - func.instructionList.size());
        expect(TokenType.SEMICOLON);
    }

    private boolean analyseA() throws CompileError {
        exprnum = 0;
        Token peeked = peek();
        
        DataType retType = analyseB();

        if (peek().getTokenType() == TokenType.ASSIGN) {
            if(peeked.getTokenType() != TokenType.IDENT || exprnum != 1)
                throw new AnalyzeError(ErrorCode.ExpectedToken, peek().getStartPos());
            
            func.instructionList.pop();

            Symbol sym = symbolTableList.searchSymbol(peeked.getValueString());
            
            if(sym == null || sym instanceof SymbolFn)
                throw new AnalyzeError(ErrorCode.NotDeclared, peeked.getStartPos());
            
            SymbolVar var = (SymbolVar)sym;
            
            if(var.isConstant())
                throw new AnalyzeError(ErrorCode.AssignToConstant, peek().getStartPos());
            next();

            if(var.getDataType() != analyseB())
                throw new AnalyzeError(ErrorCode.TypeMismatch, peeked.getStartPos());
            func.instructionList.add(new Instruction(InstructionType.store64));
            return true;
        }
        if(retType == DataType.UINT || retType == DataType.DOUBLE)
            return false;
        return true;
    }

    private DataType analyseB() throws CompileError {
        
        DataType left = analyseC();
        DataType right;
        Token oper;
        switch (peek().getTokenType()) {
        case EQ:case NEQ:case LT:case GT:case LE:case GE:
            oper = next();
            right = analyseC();
            break;
        default:
            return left;
        }
        if(left != right)
            throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());

        switch(left){
            case UINT:
                func.instructionList.add(new Instruction(InstructionType.cmpI));
                break;
            case DOUBLE:
                func.instructionList.add(new Instruction(InstructionType.cmpF));
                break;
            default:
                throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());
                
        }
       
        switch (oper.getTokenType()){
        case EQ:
            func.instructionList.add(new Instruction(InstructionType.not));
            break;
        case NEQ:
            break;
        case LT:
            func.instructionList.add(new Instruction(InstructionType.setLt));
            break;
        case GT:
            func.instructionList.add(new Instruction(InstructionType.setGt));
            break;
        case LE:
            func.instructionList.add(new Instruction(InstructionType.setGt));
            func.instructionList.add(new Instruction(InstructionType.not));
            break;
        case GE:
            func.instructionList.add(new Instruction(InstructionType.setLt));
            func.instructionList.add(new Instruction(InstructionType.not));
            break;

        default:
        }
        return left;
    }

    private DataType analyseC() throws CompileError {
        DataType left = analyseD();
        DataType right;
        Token oper;
        while (peek().getTokenType() == TokenType.PLUS || peek().getTokenType() == TokenType.MINUS) {
            oper = next();
            right = analyseD();

            if(left != right)
                throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());
            
            switch(right){
            case DOUBLE:
                if(oper.getTokenType() == TokenType.PLUS)
                    func.instructionList.add(new Instruction(InstructionType.addF));
                else
                    func.instructionList.add(new Instruction(InstructionType.subF));
                break;
            case UINT:
                if(oper.getTokenType() == TokenType.PLUS)
                    func.instructionList.add(new Instruction(InstructionType.addI));
                else
                    func.instructionList.add(new Instruction(InstructionType.subI));
                break;
            default:
                throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());
            }

        }
        return left;
    }

    private DataType analyseD() throws CompileError {
        DataType left = analyseE();
        DataType right;
        Token oper;
        while (peek().getTokenType() == TokenType.MUL || peek().getTokenType() == TokenType.DIV) {
            oper = next();
            right = analyseE();

            if(left != right)
                throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());
            
            switch(right){
            case DOUBLE:
                if(oper.getTokenType() == TokenType.MUL)
                    func.instructionList.add(new Instruction(InstructionType.mulF));
                else
                    func.instructionList.add(new Instruction(InstructionType.divF));
                break;
            case UINT:
                if(oper.getTokenType() == TokenType.MUL)
                    func.instructionList.add(new Instruction(InstructionType.mulI));
                else
                    func.instructionList.add(new Instruction(InstructionType.divI));
                break;
            default:
                throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());
            }

        }
        return left;
    }

    private DataType analyseE() throws CompileError {
        
        DataType left = analyseF();
        Token trans;

        if(left == DataType.NONE)
            throw new AnalyzeError(ErrorCode.TypeMismatch, peek().getStartPos());
        
        while (peek().getTokenType() == TokenType.AS_KW) {
            //get 'as'
            next();

            //get trans type
            trans = peek();
            switch(trans.getDataType()){
                case UINT:
                    if(left == DataType.DOUBLE)
                        func.instructionList.add(new Instruction(InstructionType.ftoi));
                        left = DataType.UINT;
                    break;
                case DOUBLE:
                    if(left == DataType.UINT)
                        func.instructionList.add(new Instruction(InstructionType.itof));
                        left = DataType.DOUBLE;
                    break;
                default:
                    throw new AnalyzeError(ErrorCode.ExpectedToken, trans.getStartPos());
            }
            //get the trans type
            next();
        }
        return left;
    }

    private DataType analyseF() throws CompileError {
        DataType right;
        boolean flag = false;
        Token oper = peek();
        while (peek().getTokenType() == TokenType.MINUS) {
            next();
            flag = !flag;
        }

        right = analyseG();
        if(flag){
            switch(right){
                
                case UINT:
                    func.instructionList.add(new Instruction(InstructionType.negI));
                    break;
                case DOUBLE:
                    func.instructionList.add(new Instruction(InstructionType.negF));
                    break;
                default:
                    throw new AnalyzeError(ErrorCode.TypeMismatch, oper.getStartPos());
            }
        }   
        return right;
    }

    private DataType analyseG() throws CompileError {
        
        Token token = peek();
        switch (peek().getTokenType()) {
            case UINT_LITERAL:
                next();
                long int64 = (long)(token.getValue());
                func.instructionList.add(new Instruction(InstructionType.pushI, false, int64));
                return DataType.UINT;
            case STRING_LITERAL:
                next();
                // a new string
                SymbolVar string = (SymbolVar)symbolTableList.getStringSymbolTable().get(token.getValueString());
                if(string == null){
                    string = new SymbolVar(symbolTableList.getStringSymbolTable().size(), -1 ,-1);
                    string.setIndex_global(symbolTableList.getStringSymbolTable().size());
                    symbolTableList.getStringSymbolTable().put(token.getValueString(), string);
                }
                Instruction ins = new Instruction(InstructionType.pushI, false, (long)(string.getIndex_global()));
                ins.setNeedRelocation(true);
                func.instructionList.add(ins);
                
                return DataType.UINT;
            case DOUBLE_LITERAL:
                next();
                double double64 = (double)(token.getValue());
                func.instructionList.add(new Instruction(InstructionType.pushF, double64));
                return DataType.DOUBLE;
            case CHAR_LITERAL:
                next();
                int ch = ((int)(char)token.getValue()) ;
                func.instructionList.add(new Instruction(InstructionType.pushI, false, (long)ch));
                return DataType.UINT;
            case L_PAREN:
                next();
                DataType type = analyseB();
                expect(TokenType.R_PAREN);
                return type;
            case IDENT:
                
                Token ident = next();

                //function call
                if (peek().getTokenType() == TokenType.L_PAREN) {
                    //get '('
                    next();

                    //not declared
                    SymbolFn fn = symbolTableList.searchSymbolFn(ident.getValueString());
                    if(fn == null)
                        throw new AnalyzeError(ErrorCode.NotDeclared, ident.getStartPos());
                    //call _start is invalid
                    if(fn.getIndex_global() == 0)
                        throw new AnalyzeError(ErrorCode.InvalidCall, ident.getStartPos());

                    //stackAlloc
                    func.instructionList.add(new Instruction(InstructionType.stackAlloc, true, fn.getRet_slot()));
                    
                    //param matching
                    boolean flag = false;
                    for (Map.Entry<String, Symbol> entry: symbolTableList.getLocalSymbolTable(fn).getSymbolTable().entrySet()){
                        SymbolVar var = (SymbolVar)entry.getValue();
                        
                        if(!var.isParam())break;
                        if(flag)
                            expect(TokenType.COMMA);
                        flag = true;
                        if((var.getDataType()) != analyseB())
                            throw new AnalyzeError(ErrorCode.InvalidParam, peek().getStartPos());
                    }
                    expect(TokenType.R_PAREN);
                    if(fn.getIndex_global() <=8){
                        ins = new Instruction(InstructionType.callName, true, fn.getIndex_global());
                        ins.setNeedRelocation(true);
                        func.instructionList.add(ins);
                    }
                        
                    else
                        func.instructionList.add(new Instruction(InstructionType.call, true, fn.getIndex_global()));
                    return fn.getRetType();
                }
                else{
                    Symbol sym = symbolTableList.searchSymbol(ident.getValueString());
                    if(sym == null || sym instanceof SymbolFn)
                        throw new AnalyzeError(ErrorCode.NotDeclared, ident.getStartPos());
                        
                    SymbolVar var = (SymbolVar)sym;

                    if(var.isGlobal())
                        func.instructionList.add(new Instruction(InstructionType.globA, true, var.getIndex_global()));
                    else if(!var.isParam())
                        func.instructionList.add(new Instruction(InstructionType.locA, true, var.getIndex_local()));
                    else
                        func.instructionList.add(new Instruction(InstructionType.argA, true, var.getIndex_param() + 
                        (func.getRetType() == DataType.VOID ? 0 : 1) ));
                    
                    func.instructionList.add(new Instruction(InstructionType.load64));
                    return var.getDataType();
                }
            default:
                throw new AnalyzeError(ErrorCode.ExpectedToken, token.getStartPos());
        }
    }

	public Tokenizer getTokenizer() {
		return tokenizer;
	}

	public void setTokenizer(Tokenizer tokenizer) {
		this.tokenizer = tokenizer;
	}


}