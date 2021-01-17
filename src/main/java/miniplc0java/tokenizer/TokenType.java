package miniplc0java.tokenizer;

public enum TokenType {
    /** ordinal = 0*/
    None,
    
    

    
    /** 'let' ordinal = 1*/
    LET_KW,
    /** 'as' ordinal = 2*/
    AS_KW,
    /** 'while' ordinal = 3*/
    WHILE_KW,
    /** 'if' ordinal = 4*/
    IF_KW,
    /** 'else' ordinal = 5*/
    ELSE_KW,
    /** 'return' ordinal = 6*/
    RETURN_KW,
    /** 'break' ordinal = 7*/
    BREAK_KW,
    /** 'continue' ordinal = 8*/
    CONTINUE_KW,
    /** 'const' ordinal = 9*/
    CONST_KW,
    /** 'fn' ordinal = 10*/
    FN_KW,
    

    /** ordinal = 11*/
    UINT_LITERAL,
    /**ordinal = 12*/
    STRING_LITERAL,
    /**ordinal = 13*/
    DOUBLE_LITERAL,
    /**ordinal = 14*/
    CHAR_LITERAL,
    /**ordinal = 15*/
    VOID_LITERAL,
    

    IDENT,
    

    /** '+' */
    PLUS,
    /** '-' */
    MINUS,
    /** '*' */
    MUL,
    /** '/' */
    DIV,
    /** '=' */
    ASSIGN,
    /** "==" */
    EQ,
    /** "!=" */
    NEQ,
    /** '< */
    LT,
    /** '>' */
    GT,
    /** "<=" */
    LE,
    /** ">=" */
    GE,
    /** '(' */
    L_PAREN,
    /** ')' */
    R_PAREN,
    /** {' */
    L_BRACE,
    /** '}' */
    R_BRACE,
    /** "->" */
    ARROW,
    /** ',' */
    COMMA,
    /** ':' */
    COLON,
    /** ';' */
    SEMICOLON,
    
    

    COMMENT,
    
    

    EOF;

    @Override
    public String toString() {
        switch (this) {
            case None:
                return "NullToken";
                

            case FN_KW:
                return "fn";
            case LET_KW:
                return "let";
            case CONST_KW:
                return "const";
            case AS_KW:
                return "as";
            case WHILE_KW:
                return "while";
            case IF_KW:
                return "if";
            case ELSE_KW:
                return "else";
            case RETURN_KW:
                return "return";
            case BREAK_KW:
                return "break";
            case CONTINUE_KW:
                return "continue";
                
                

            case UINT_LITERAL:
            	return "UnsignedInteger";
            case STRING_LITERAL:
            	return "String";
            case DOUBLE_LITERAL:
            	return "Double";
            case CHAR_LITERAL:
            	return "Char";
            
            	

            case IDENT:
            	return "Identifier";
            	
            	

            case PLUS:
            	return "PlusSign";
            case MINUS:
            	return "MinusSign";
            case MUL:
            	return "MultipleSign";
            case DIV:
            	return "DivisionSign";
            case ASSIGN:
            	return "AssignmentSign";
            case EQ:
            	return "EqualSign";
            case NEQ:
            	return "UnEqualSign";
            case LT:
            	return "LessThanSign";
            case GT:
            	return "GreaterThanSign";
            case LE:
            	return "LessOrEqualSign";
            case GE:
            	return "GreaterOrEqualSign";
            case L_PAREN:
            	return "LeftParenthesis";
            case R_PAREN:
            	return "RightParenthesis";
            case L_BRACE:
            	return "LeftBrace";
            case R_BRACE:
            	return "RightBrace";
            case ARROW:
            	return "ArrowSign";
            case COMMA:
            	return "Comma";
            case COLON:
            	return "Colon";
            case SEMICOLON:
            	return "Semicolon";
            	
            	

            case COMMENT:
            	return "Comment";
            	
            	

            case EOF:
            	return "EndOfFile";
            	
            default:
                return "InvalidToken";
        }
    }
}
