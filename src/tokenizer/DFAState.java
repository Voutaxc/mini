package tokenizer;

public enum DFAState {
    INITIAL_STATE,
    UNSIGNED_INTEGER_STATE,
    PLUS_SIGN_STATE,
    MINUS_SIGN_STATE,
    DIVISION_SIGN_STATE,
    MULTIPLICATION_SIGN_STATE,
    IDENTIFIER_STATE,
    EQUAL_SIGN_STATE,
    SEMICOLON_STATE,
    LEFTBRACKET_STATE,
    RIGHTBRACKET_STATE
}
