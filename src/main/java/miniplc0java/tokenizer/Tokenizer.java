package miniplc0java.tokenizer;
import miniplc0java.util.Pos;
import miniplc0java.error.TokenizeError;
import miniplc0java.error.ErrorCode;

public class Tokenizer {

    private StringIter it;

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    public Token nextToken() throws TokenizeError {
        it.readAll();


        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }
        char peek = it.peekChar();

        if (Character.isDigit(peek)) {
            return lexUIntOrDouble();
        }

        else if(peek == '\"') {
        	return lexString();
        }

        else if(peek == '\'') {
        	return lexChar();
        }

        else if (Character.isAlphabetic(peek)) {
            return lexIdentOrKeyword();
        } 

        else {
			Token token = lexOperatorOrUnknownOrComment();
			if(token.getTokenType() == TokenType.COMMENT)
				return nextToken();
			else return token;
        }
    }

    private Token lexUIntOrDouble() throws TokenizeError {
        
    	Pos tempBegin = new Pos(it.currentPos().row, it.currentPos().col);
    	long tempInt = 0;
    	while(Character.isDigit(it.peekChar())) {
    		tempInt = tempInt * 10 + it.nextChar() - 48;
    	}
    	if(it.peekChar()!='.')
    		return new Token(TokenType.UINT_LITERAL, tempInt ,tempBegin ,it.currentPos());
    	

    	it.nextChar();
    	double tempDouble = (double)tempInt;
    	double k = 1.0 / 10;
    	while(Character.isDigit(it.peekChar())) {
    		tempDouble += k * (it.nextChar() - 48);
    		
    		k /= 10;
    	}

    	if(it.peekChar() == 'e' || it.peekChar() == 'E') {
    		it.nextChar();
    		char tempSign = '+';

    		if(it.peekChar() == '+' || it.peekChar() == '-') {
    			tempSign = it.nextChar();
    		}else {

    		}

    		if(!Character.isDigit(it.peekChar()))
    			throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
    		tempInt = 0;
    		while(Character.isDigit(it.peekChar())) {
        		tempInt = tempInt * 10 + it.nextChar() - 48;
        	}

    		while(tempInt != 0) {
    			if(tempSign == '+')tempDouble *= 10;
    			else tempDouble /= 10;
    			tempInt--;
    		}
    	}
    	else {

    	}
    	return new Token(TokenType.DOUBLE_LITERAL, tempDouble, tempBegin, it.currentPos());
    }
    

    private Token lexString() throws TokenizeError {
    	Pos tempBegin = new Pos(it.currentPos().row, it.currentPos().col);
    	StringBuilder tempString = new StringBuilder("");

    	it.nextChar();
    	while(it.peekChar() != '\"') {

    		if(it.peekChar() == '\n')throw new TokenizeError(ErrorCode.ExpectedToken, tempBegin);
    		

    		else if(it.peekChar() == '\\') {
    			it.nextChar();
    			char tempchar = it.peekChar();
    			if(tempchar == '\\' || tempchar == '\"' || tempchar == '\'' || tempchar == 'n'
    					|| tempchar == 'r' | tempchar == 't') {
    				if(tempchar == 'n')tempchar = '\n';
    				if(tempchar == 'r')tempchar = '\r';
    				if(tempchar == 't')tempchar = '\t';
    				tempString.append(tempchar);
    				it.nextChar();
    			}else {

    				throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
    			}
    		}
    		else {

    			tempString.append(it.nextChar());
    		}
    	}
    	it.nextChar();
    	
    	return new Token(TokenType.STRING_LITERAL, tempString.toString(), tempBegin, it.currentPos());
    }
    

    private Token lexChar() throws TokenizeError{
    	Pos tempBegin = new Pos(it.currentPos().row, it.currentPos().col);
    	char tempChar ='\0';

    	it.nextChar();

    	if(it.peekChar() == '\\') {
    		it.nextChar();
    		char tempchar = it.peekChar();
    		
			if(tempchar == '\\' || tempchar == '\"' || tempchar == '\'' || tempchar == 'n'
					|| tempchar == 'r' | tempchar == 't') {
				tempChar = it.nextChar();
				if(tempChar == 'n')tempChar = '\n';
				if(tempChar == 'r')tempChar = '\r';
				if(tempChar == 't')tempChar = '\t';
			}else {

				throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
			}
		}

		else if(it.peekChar() == '\''){
			throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
		}

    	else {
    		tempChar = it.nextChar();
    	}

    	if(it.peekChar() != '\'')
    		throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
    	it.nextChar();
    	return new Token(TokenType.CHAR_LITERAL, tempChar, tempBegin, it.currentPos());
    }
    
    private Token lexComment() throws TokenizeError{
    	Pos tempBegin = new Pos(it.currentPos().row, it.currentPos().col);
    	StringBuilder tempStringBuilder = new StringBuilder("");
    	if(it.peekChar() != '/') {
    		throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
    	}

    	it.nextChar();
    	while(it.peekChar() != '\n') {
    		if(it.isEOF()) {
    			break;
    		}
    		else {
    			tempStringBuilder.append(it.nextChar());
    		}	
    	}
    	return new Token(TokenType.COMMENT, tempStringBuilder.toString(), tempBegin, it.currentPos());
    }
    
    
    private Token lexIdentOrKeyword() throws TokenizeError {
    	
    	Pos tempBegin = new Pos(it.currentPos().row, it.currentPos().col);
    	StringBuilder tempStringBuilder = new StringBuilder("");
    	
    	
    	while(Character.isLetterOrDigit(it.peekChar()) || 	it.peekChar() == '_') {
    		tempStringBuilder.append(it.nextChar());
    	}
    	String tempString = tempStringBuilder.toString();
    	switch(tempString) {
    		case "fn":
				return new Token(TokenType.FN_KW, tempString, tempBegin, it.currentPos());
    		case "let":
				return new Token(TokenType.LET_KW, tempString, tempBegin, it.currentPos());
    		case "const":
				return new Token(TokenType.CONST_KW, tempString, tempBegin, it.currentPos());
    		case "as":
				return new Token(TokenType.AS_KW, tempString, tempBegin, it.currentPos());
    		case "while":
				return new Token(TokenType.WHILE_KW, tempString, tempBegin, it.currentPos());
    		case "if":
				return new Token(TokenType.IF_KW, tempString, tempBegin, it.currentPos());
    		case "else":
				return new Token(TokenType.ELSE_KW, tempString, tempBegin, it.currentPos());
    		case "return":
				return new Token(TokenType.RETURN_KW, tempString, tempBegin, it.currentPos());
    		case "break":
				return new Token(TokenType.BREAK_KW, tempString, tempBegin, it.currentPos());
    		case "continue":
    			return new Token(TokenType.CONTINUE_KW, tempString, tempBegin, it.currentPos());
    		default:
				return new Token(TokenType.IDENT, tempString, tempBegin, it.currentPos());
    	}
    
    }

    private Token lexOperatorOrUnknownOrComment() throws TokenizeError {
    	Pos tempBegin = new Pos(it.currentPos().row, it.currentPos().col);
    	switch (it.nextChar()) {
	        case '+':
	            return new Token(TokenType.PLUS, "+", tempBegin, it.currentPos());
	
	        case '-':
	        	if(it.peekChar() == '>') {
	        		it.nextChar();
	        		return new Token(TokenType.ARROW, "->", tempBegin, it.currentPos());
	        	}
	            return new Token(TokenType.MINUS, "-", tempBegin, it.currentPos());
	
	        case '*':
	            return new Token(TokenType.MUL, "*", tempBegin, it.currentPos());
			case '/':
				if(it.peekChar() == '/'){
					return lexComment();
				}
	        	return new Token(TokenType.DIV, "/", tempBegin, it.currentPos());
	        	
	        case '=':
	        	if(it.peekChar() == '=') {
	        		it.nextChar();
	        		return new Token(TokenType.EQ, "==", tempBegin, it.currentPos());
	        	}
	            return new Token(TokenType.ASSIGN, "=", tempBegin, it.currentPos());
	        
	        case '!':
	        	if(it.peekChar() == '=') {
	        		it.nextChar();
	        		return new Token(TokenType.NEQ, "!=", tempBegin, it.currentPos());
	        	}
	        	else throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
	        
	        case '<':
	        	if(it.peekChar() == '=') {
	        		it.nextChar();
	        		return new Token(TokenType.LE, "<=", tempBegin, it.currentPos());
	        	}
	            return new Token(TokenType.LT, "<", tempBegin, it.currentPos());
	            
	        case '>':
	        	if(it.peekChar() == '=') {
	        		it.nextChar();
	        		return new Token(TokenType.GE, ">=", tempBegin, it.currentPos());
	        	}
	            return new Token(TokenType.GT, ">", tempBegin, it.currentPos());
	            
	        case '(':
	            return new Token(TokenType.L_PAREN, "(", tempBegin, it.currentPos());
	
	        case ')':
	        	return new Token(TokenType.R_PAREN, ")", tempBegin, it.currentPos());
	        
	        case '{':
	            return new Token(TokenType.L_BRACE, "{", tempBegin, it.currentPos());
	
	        case '}':
	        	return new Token(TokenType.R_BRACE, "}", tempBegin, it.currentPos());
	        	
	        case ',':
	        	return new Token(TokenType.COMMA, ",", tempBegin, it.currentPos());
	        
	        case ':':
	        	return new Token(TokenType.COLON, ":", tempBegin, it.currentPos());
	        
	        case ';':
	        	return new Token(TokenType.SEMICOLON, ';', tempBegin, it.currentPos());
	        	
	        default:
	            throw new TokenizeError(ErrorCode.InvalidInput, tempBegin);
    	}
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }
}
