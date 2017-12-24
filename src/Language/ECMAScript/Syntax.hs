{-# LANGUAGE OverloadedStrings #-}

module Language.ECMAScript.Syntax
    ( Identifier (..)
    , Literal (..)
    , Program (..)
    , Statement (..)
    , Block (..)
    , SwitchCase (..)
    , CatchClause (..)
    , VariableDeclaration (..)
    , VariableDeclarationKind (..)
    , VariableDeclarator (..)
    , Expression (..)
    , Property (..)
    , PropertyKind (..)
    , UnaryOperator (..)
    , UpdateOperator (..)
    , PrePostFix (..)
    , BinaryOperator (..)
    , AssignmentOperator (..)
    , LogicalOperator (..)
    , Member (..)
    , Pattern (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Or
import           Data.Text        (Text)

estree :: Text -> [Pair] -> Value
estree t props = object $ props ++
    [ "type" .= t
    , "loc" .= Null ]

orToPairs :: (ToJSON a, ToJSON b) => Text -> Text -> Or a b -> [Pair]
orToPairs fstKey sndKey (Fst a) =
    [ fstKey .= a
    , sndKey .= Null ]
orToPairs fstKey sndKey (Snd b) =
    [ fstKey .= Null
    , sndKey .= b ]
orToPairs fstKey sndKey (Both a b) =
    [ fstKey .= a
    , sndKey .= b ]

(.=|) :: (ToJSON a, ToJSON b) => Text -> Either a b -> Pair
k .=| Left a = k .= a
k .=| Right b = k .= b
infixr 8 .=|

(.=?|) :: (ToJSON a, ToJSON b) => Text -> Maybe (Either a b) -> Pair
k .=?| Nothing = k .= Null
k .=?| Just x = k .=| x
infixr 8 .=?|

data Identifier
    = Identifier String

instance ToJSON Identifier where
    toJSON (Identifier name) = estree "Identifier"
        [ "name" .= name ]

data Literal
    = StringLiteral String
    | BooleanLiteral Bool
    | NullLiteral
    | NumberLiteral Double
    | RegExpLiteral String String

instance ToJSON Literal where
    toJSON literal = estree "Literal" $
        case literal of
            StringLiteral s ->
                [ "value" .= s ]
            BooleanLiteral b ->
                [ "value" .= b ]
            NullLiteral ->
                [ "value" .= Null ]
            NumberLiteral n ->
                [ "value" .= n ]
            RegExpLiteral pattern flags ->
                [ "value" .= Null
                , "regex" .= object
                    [ "pattern" .= pattern
                    , "flags" .= flags ] ]

data Program
    = Program [Statement]

instance ToJSON Program where
    toJSON (Program body) = estree "Program"
        [ "body" .= body ]

data Statement
    = ExpressionStatement Expression
    | DirectiveStatement String
    | BlockStatement Block
    | EmptyStatement
    | DebuggerStatement
    | WithStatement Expression Statement
    | ReturnStatement (Maybe Expression)
    | LabeledStatement Identifier Statement
    | BreakStatement (Maybe Identifier)
    | ContinueStatement (Maybe Identifier)
    | IfStatement Expression Statement (Maybe Statement)
    | SwitchStatement Expression [SwitchCase]
    | ThrowStatement Expression
    | TryStatement [Statement] (Or CatchClause Block)
    | WhileStatement Expression Statement
    | DoWhileStatement Statement Expression
    | ForStatement (Maybe (Either VariableDeclaration Expression))
        (Maybe Expression) (Maybe Expression) Statement
    | ForInStatement (Either VariableDeclaration Pattern) Expression Statement
    | FunctionDeclaration Identifier [Pattern] Block
    | VariableDeclarationStatement VariableDeclaration

instance ToJSON Statement where
    toJSON (ExpressionStatement expression) = estree "ExpressionStatement"
        [ "expression" .= expression ]
    toJSON (DirectiveStatement directive) = estree "ExpressionStatement"
        [ "expression" .= StringLiteral directive
        , "directive" .= directive ]
    toJSON (BlockStatement block) = toJSON block
    toJSON EmptyStatement = estree "EmptyStatement" []
    toJSON DebuggerStatement = estree "DebuggerStatement" []
    toJSON (WithStatement expression body) = estree "WithStatement"
        [ "object" .= expression
        , "body" .= body ]
    toJSON (ReturnStatement argument) = estree "ReturnStatement"
        [ "argument" .= argument ]
    toJSON (LabeledStatement label body) = estree "LabeledStatement"
        [ "label" .= label
        , "body" .= body ]
    toJSON (BreakStatement label) = estree "BreakStatement"
        [ "label" .= label ]
    toJSON (ContinueStatement label) = estree "ContinueStatement"
        [ "label" .= label ]
    toJSON (IfStatement test consequent alternate) = estree "IfStatement"
        [ "test" .= test
        , "consequent" .= consequent
        , "alternate" .= alternate ]
    toJSON (SwitchStatement discriminant cases) = estree "SwitchStatement"
        [ "discriminant" .= discriminant
        , "cases" .= cases ]
    toJSON (ThrowStatement argument) = estree "ThrowStatement"
        [ "argument" .= argument ]
    toJSON (TryStatement block handlerOrFinalizer) = estree "TryStatement" $
        [ "block" .= block ]
        ++ orToPairs "handler" "finalizer" handlerOrFinalizer
    toJSON (WhileStatement test body) = estree "WhileStatement"
        [ "test" .= test
        , "body" .= body ]
    toJSON (DoWhileStatement body test) = estree "DoWhileStatement"
        [ "body" .= body
        , "test" .= test ]
    toJSON (ForStatement initializer test update body) = estree "ForStatement"
        [ "init" .=?| initializer
        , "test" .= test
        , "update" .= update
        , "body" .= body ]
    toJSON (ForInStatement left right body) = estree "ForInStatement"
        [ "left" .=| left
        , "right" .= right
        , "body" .= body ]
    toJSON (FunctionDeclaration name params body) = estree "FunctionDeclaration"
        [ "id" .= name
        , "params" .= params
        , "body" .= body ]
    toJSON (VariableDeclarationStatement variableDeclaration) =
        toJSON variableDeclaration

data Block
    = Block [Statement]

instance ToJSON Block where
    toJSON (Block body) = estree "BlockStatement"
        [ "body" .= body ]

data SwitchCase
    = SwitchCase (Maybe Expression) [Statement]

instance ToJSON SwitchCase where
    toJSON (SwitchCase test consequent) = estree "SwitchCase"
        [ "test" .= test
        , "consequent" .= consequent ]

data CatchClause
    = CatchClause Pattern Block

instance ToJSON CatchClause where
    toJSON (CatchClause param body) = estree "CatchClause"
        [ "param" .= param
        , "body" .= body ]

data VariableDeclaration
    = VariableDeclaration VariableDeclarationKind [VariableDeclarator]

instance ToJSON VariableDeclaration where
    toJSON (VariableDeclaration kind declarations) =
        estree "VariableDeclaration"
            [ "declarations" .= declarations
            , "kind" .= kind ]

data VariableDeclarationKind
    = VarVariableDeclaration

instance ToJSON VariableDeclarationKind where
    toJSON VarVariableDeclaration = "var"

data VariableDeclarator
    = VariableDeclarator Pattern (Maybe Expression)

instance ToJSON VariableDeclarator where
    toJSON (VariableDeclarator name value) = estree "VariableDeclarator"
        [ "id" .= name
        , "init" .= value ]

data Expression
    = IdentifierExpression Identifier
    | LiteralExpression Literal
    | ThisExpression
    | ArrayExpression [Maybe Expression]
    | ObjectExpression [Property]
    | FunctionExpression (Maybe Identifier) [Pattern] Block
    | UnaryExpression UnaryOperator Expression
    | UpdateExpression UpdateOperator PrePostFix Expression
    | BinaryExpression BinaryOperator Expression Expression
    | AssignmentExpression AssignmentOperator
        (Either Pattern Expression) Expression
    | LogicalExpression LogicalOperator Expression Expression
    | MemberExpression Member
    | ConditionalExpression Expression Expression Expression
    | CallExpression Expression [Expression]
    | NewExpression Expression [Expression]
    | SequenceExpression [Expression]

instance ToJSON Expression where
    toJSON (IdentifierExpression identifier) = toJSON identifier
    toJSON (LiteralExpression literal) = toJSON literal
    toJSON ThisExpression = estree "ThisExpression" []
    toJSON (ArrayExpression elements) = estree "ArrayExpression"
        [ "elements" .= elements ]
    toJSON (ObjectExpression properties) = estree "ObjectExpression"
        [ "properties" .= properties ]
    toJSON (FunctionExpression name params body) = estree "FunctionExpression"
        [ "id" .= name
        , "params" .= params
        , "body" .= body ]
    toJSON (UnaryExpression operator argument) = estree "UnaryExpression"
        [ "operator" .= operator
        , "argument" .= argument
        , "prefix" .= True ]
    toJSON (UpdateExpression operator prefix argument) =
        estree "UpdateExpression"
            [ "operator" .= operator
            , "argument" .= argument
            , "prefix" .= case prefix of
                Prefix  -> True
                Postfix -> False ]
    toJSON (BinaryExpression operator left right) = estree "BinaryExpression"
        [ "operator" .= operator
        , "left" .= left
        , "right" .= right ]
    toJSON (AssignmentExpression operator left right) =
        estree "AssignmentExpression"
            [ "operator" .= operator
            , "left" .=| left
            , "right" .= right ]
    toJSON (LogicalExpression operator left right) = estree "LogicalExpression"
        [ "operator" .= operator
        , "left" .= left
        , "right" .= right ]
    toJSON (MemberExpression member) = toJSON member
    toJSON (ConditionalExpression test consequent alternate) =
        estree "ConditionalExpression"
            [ "test" .= test
            , "alternate" .= alternate
            , "consequent" .= consequent ]
    toJSON (CallExpression callee arguments) = estree "CallExpression"
        [ "callee" .= callee
        , "arguments" .= arguments ]
    toJSON (NewExpression callee arguments) = estree "NewExpression"
        [ "callee" .= callee
        , "arguments" .= arguments ]
    toJSON (SequenceExpression expressions) = estree "SequenceExpression"
        [ "expressions" .= expressions ]

data Property
    = Property PropertyKind (Either Literal Identifier) Expression

instance ToJSON Property where
    toJSON (Property kind key value) = estree "Property"
        [ "key" .=| key
        , "value" .= value
        , "kind" .= kind ]

data PropertyKind
    = InitProperty
    | GetProperty
    | SetProperty

instance ToJSON PropertyKind where
    toJSON InitProperty = "init"
    toJSON GetProperty  = "get"
    toJSON SetProperty  = "set"

data UnaryOperator
    = UnaryNegationOperator
    | UnaryPlusOperator
    | LogicalNotOperator
    | BitwiseNotOperator
    | TypeofOperator
    | VoidOperator
    | DeleteOperator

instance ToJSON UnaryOperator where
    toJSON UnaryNegationOperator = "-"
    toJSON UnaryPlusOperator     = "+"
    toJSON LogicalNotOperator    = "!"
    toJSON BitwiseNotOperator    = "~"
    toJSON TypeofOperator        = "typeof"
    toJSON VoidOperator          = "void"
    toJSON DeleteOperator        = "delete"

data UpdateOperator
    = IncrementOperator
    | DecrementOperator

instance ToJSON UpdateOperator where
    toJSON IncrementOperator = "++"
    toJSON DecrementOperator = "--"

data PrePostFix
    = Prefix
    | Postfix

data BinaryOperator
    = EqualOperator
    | NotEqualOperator
    | StrictEqualOperator
    | StrictNotEqualOperator
    | LessThanOperator
    | LessThanOrEqualOperator
    | GreaterThanOperator
    | GreaterThanOrEqualOperator
    | LeftShiftOperator
    | RightShiftOperator
    | UnsignedRightShiftOperator
    | AdditionOperator
    | SubtractionOperator
    | MultiplicationOperator
    | DivisionOperator
    | RemainderOperator
    | BitwiseOrOperator
    | BitwiseXorOperator
    | BitwiseAndOperator
    | InOperator
    | InstanceofOperator

instance ToJSON BinaryOperator where
    toJSON EqualOperator              = "=="
    toJSON NotEqualOperator           = "!="
    toJSON StrictEqualOperator        = "==="
    toJSON StrictNotEqualOperator     = "!=="
    toJSON LessThanOperator           = "<"
    toJSON LessThanOrEqualOperator    = "<="
    toJSON GreaterThanOperator        = ">"
    toJSON GreaterThanOrEqualOperator = ">="
    toJSON LeftShiftOperator          = "<<"
    toJSON RightShiftOperator         = ">>"
    toJSON UnsignedRightShiftOperator = ">>>"
    toJSON AdditionOperator           = "+"
    toJSON SubtractionOperator        = "-"
    toJSON MultiplicationOperator     = "*"
    toJSON DivisionOperator           = "/"
    toJSON RemainderOperator          = "%"
    toJSON BitwiseOrOperator          = "|"
    toJSON BitwiseXorOperator         = "^"
    toJSON BitwiseAndOperator         = "&"
    toJSON InOperator                 = "in"
    toJSON InstanceofOperator         = "instanceof"

data AssignmentOperator
    = AssignmentOperator
    | AdditionAssignmentOperator
    | SubtractionAssignmentOperator
    | MultiplicationAssignmentOperator
    | DivisionAssignmentOperator
    | RemainderAssignmentOperator
    | LeftShiftAssignmentOperator
    | RightShiftAssignmentOperator
    | UnsignedRightShiftAssignmentOperator
    | BitwiseOrAssignmentOperator
    | BitwiseXorAssignmentOperator
    | BitwiseAndAssignmentOperator

instance ToJSON AssignmentOperator where
    toJSON AssignmentOperator                   = "="
    toJSON AdditionAssignmentOperator           = "+="
    toJSON SubtractionAssignmentOperator        = "-="
    toJSON MultiplicationAssignmentOperator     = "*="
    toJSON DivisionAssignmentOperator           = "/="
    toJSON RemainderAssignmentOperator          = "%="
    toJSON LeftShiftAssignmentOperator          = "<<="
    toJSON RightShiftAssignmentOperator         = ">>="
    toJSON UnsignedRightShiftAssignmentOperator = ">>>="
    toJSON BitwiseOrAssignmentOperator          = "|="
    toJSON BitwiseXorAssignmentOperator         = "^="
    toJSON BitwiseAndAssignmentOperator         = "&="

data LogicalOperator
    = LogicalOrOperator
    | LogicalAndOperator

instance ToJSON LogicalOperator where
    toJSON LogicalOrOperator  = "||"
    toJSON LogicalAndOperator = "&&"

data Member
    = Member Expression (Either Expression Identifier)

instance ToJSON Member where
    toJSON (Member obj property) = estree "MemberExpression"
        [ "object" .= obj
        , "property" .=| property
        , "computed" .= case property of
            Left _  -> True
            Right _ -> False ]

data Pattern
    = IdentifierPattern Identifier
    | MemberPattern Member

instance ToJSON Pattern where
    toJSON (IdentifierPattern identifier) = toJSON identifier
    toJSON (MemberPattern member)         = toJSON member
