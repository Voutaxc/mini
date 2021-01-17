package miniplc0java.error;

public enum ErrorCode {
    NoError, // Should be only used internally.
    StreamError, EOF, InvalidInput, InvalidIdentifier, IncompleteStatement, IntegerOverflow, ConflictReference, // int32_t overflow.
    NoBegin, NoEnd, NeedIdentifier, ConstantNeedValue, NoSemicolon, InvalidVariableDeclaration, IncompleteExpression,
    NotDeclared, AssignToConstant, DuplicateDeclaration, TypeMismatch, NotInitialized, InvalidAssignment, InvalidCall, InvalidPrint, InvalidType, ExpectedToken,
    InvalidDeclaration, InvalidExpression, InvalidParam, InvalidReturn, NullStatement, NullMainFunction, InvalidStatement, NoBreakContext, NoContinueContext, NotAllRoutesReturn,
}
