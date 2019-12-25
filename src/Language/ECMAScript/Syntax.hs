{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ECMAScript syntax tree.
-- Based off the [estree spec](https://github.com/estree/estree),
-- with some modifications to make it valid/idiomatic Haskell.
-- All nodes have 'ToJSON' instances so that they can be converted into the
-- corresponding estree JSON node.
-- Newest supported ECMAScript standard is ES2015.
module Language.ECMAScript.Syntax
    ( Either' (..)
    , Identifier
    , mkIdentifier
    , Literal (..)
    , Program (..)
    , SourceType (..)
    , Statement (..)
    , Block (..)
    , SwitchCase (..)
    , CatchClause (..)
    , Declaration (..)
    , VariableDeclaration (..)
    , VariableDeclarationKind (..)
    , VariableDeclarator (..)
    , ClassBody (..)
    , MethodDefinition (..)
    , MethodDefinitionKind (..)
    , Super (..)
    , SpreadElement (..)
    , Function (..)
    , Expression (..)
    , TemplateLiteral (..)
    , TemplateElement (..)
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
    , AssignmentProperty (..)
    , ModuleDeclaration (..)
    , ImportSpecifier (..)
    , ImportDefaultSpecifier (..)
    , ImportNamespaceSpecifier (..)
    , ExportSpecifier (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Or
import           Data.Text                          (Text)

import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Generate.ConvertErr
import           Language.Dtfpl.M
import           Language.Dtfpl.M.Util
import           Language.ECMAScript.Syntax.Verify

-- | Adds @type@ and @loc@ properties to make the node a valid estree node.
-- @type@ is set to the given string while @loc@ is always null.
estree :: Text -> [Pair] -> Value
estree t props = object $ props ++
    [ "type" .= t
    , "loc" .= Null ]

-- | Convert an 'Or' to two pairs.
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

-- | Identical to 'Either', but has a custom 'ToJSON' instance, since the
-- instance for 'Either' is already defined in "Data.Aeson".
data Either' a b = Left' a | Right' b

isLeft' :: Either' a b -> Bool
isLeft' (Left' _)  = True
isLeft' (Right' _) = False

isRight' :: Either' a b -> Bool
isRight' (Left' _)  = False
isRight' (Right' _) = True

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
    toJSON (Left' a)  = toJSON a
    toJSON (Right' b) = toJSON b

newtype Identifier = Identifier String

-- | Create an 'Identifier'.
-- Checks if the given string is valid in debug mode only.
mkIdentifier :: (MEnv m, MError m) => String -> m Identifier
mkIdentifier s = do
    debugErrIf (not $ isValidIdentifier s) $
        InternalConvertErr $ InternalInvalidTargetASTErr $
            errQuote s ++ " is not a valid ECMAScript identifier"
    pure $ Identifier s

instance ToJSON Identifier where
    toJSON (Identifier name) =
        estree "Identifier"
            [ "name" .= name ]

data Literal
    = StringLiteral String
    | BooleanLiteral Bool
    | NullLiteral
    | NumberLiteral Double
    | RegExpLiteral String String

instance ToJSON Literal where
    toJSON literal =
        estree "Literal" $
            case literal of
                StringLiteral s ->
                    [ "value" .= s ]
                BooleanLiteral b ->
                    [ "value" .= b ]
                NullLiteral ->
                    [ "value" .= Null ]
                NumberLiteral n ->
                    [ "value" .= n ]
                RegExpLiteral pat flags ->
                    [ "value" .= Null
                    , "regex" .= object
                        [ "pattern" .= pat
                        , "flags" .= flags ] ]

data Program = Program SourceType [Either' Statement ModuleDeclaration]

instance ToJSON Program where
    toJSON (Program sourceType body) =
        estree "Program"
            [ "sourceType" .= sourceType
            , "body" .= body ]

data SourceType
    = ScriptSourceType
    | ModuleSourceType

instance ToJSON SourceType where
    toJSON ScriptSourceType = "script"
    toJSON ModuleSourceType = "module"

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
    | ForStatement (Maybe (Either' VariableDeclaration Expression))
        (Maybe Expression) (Maybe Expression) Statement
    | ForInStatement (Either' VariableDeclaration Pattern) Expression Statement
    | ForOfStatement (Either' VariableDeclaration Pattern) Expression Statement
    | DeclarationStatement Declaration

instance ToJSON Statement where
    toJSON (ExpressionStatement expression) =
        estree "ExpressionStatement"
            [ "expression" .= expression ]
    toJSON (DirectiveStatement directive) =
        estree "ExpressionStatement"
            [ "expression" .= StringLiteral directive
            , "directive" .= directive ]
    toJSON (BlockStatement block) = toJSON block
    toJSON EmptyStatement =
        estree "EmptyStatement" []
    toJSON DebuggerStatement =
        estree "DebuggerStatement" []
    toJSON (WithStatement expression body) =
        estree "WithStatement"
            [ "object" .= expression
            , "body" .= body ]
    toJSON (ReturnStatement argument) =
        estree "ReturnStatement"
            [ "argument" .= argument ]
    toJSON (LabeledStatement label body) =
        estree "LabeledStatement"
            [ "label" .= label
            , "body" .= body ]
    toJSON (BreakStatement label) =
        estree "BreakStatement"
            [ "label" .= label ]
    toJSON (ContinueStatement label) =
        estree "ContinueStatement"
            [ "label" .= label ]
    toJSON (IfStatement test consequent alternate) =
        estree "IfStatement"
            [ "test" .= test
            , "consequent" .= consequent
            , "alternate" .= alternate ]
    toJSON (SwitchStatement discriminant cases) =
        estree "SwitchStatement"
            [ "discriminant" .= discriminant
            , "cases" .= cases ]
    toJSON (ThrowStatement argument) =
        estree "ThrowStatement"
            [ "argument" .= argument ]
    toJSON (TryStatement block handlerOrFinalizer) =
        estree "TryStatement" $
            [ "block" .= block ]
            ++ orToPairs "handler" "finalizer" handlerOrFinalizer
    toJSON (WhileStatement test body) =
        estree "WhileStatement"
            [ "test" .= test
            , "body" .= body ]
    toJSON (DoWhileStatement body test) =
        estree "DoWhileStatement"
            [ "body" .= body
            , "test" .= test ]
    toJSON (ForStatement initializer test update body) =
        estree "ForStatement"
            [ "init" .= initializer
            , "test" .= test
            , "update" .= update
            , "body" .= body ]
    toJSON (ForInStatement left right body) =
        estree "ForInStatement"
            [ "left" .= left
            , "right" .= right
            , "body" .= body ]
    toJSON (ForOfStatement left right body) =
        estree "ForOfStatement"
            [ "left" .= left
            , "right" .= right
            , "body" .= body ]
    toJSON (DeclarationStatement declaration) = toJSON declaration

newtype Block = Block [Statement]

instance ToJSON Block where
    toJSON (Block body) =
        estree "BlockStatement"
            [ "body" .= body ]

data SwitchCase
    = SwitchCase (Maybe Expression) [Statement]

instance ToJSON SwitchCase where
    toJSON (SwitchCase test consequent) =
        estree "SwitchCase"
            [ "test" .= test
            , "consequent" .= consequent ]

data CatchClause
    = CatchClause Pattern Block

instance ToJSON CatchClause where
    toJSON (CatchClause param body) =
        estree "CatchClause"
            [ "param" .= param
            , "body" .= body ]

data Declaration
    = FunctionDeclaration Bool Identifier [Pattern] Block
    | VariableDeclarationDeclaration VariableDeclaration
    | ClassDeclaration Identifier (Maybe Expression) ClassBody

instance ToJSON Declaration where
    toJSON (FunctionDeclaration generator name params body) =
        estree "FunctionDeclaration"
            [ "generator" .= generator
            , "id" .= name
            , "params" .= params
            , "body" .= body ]
    toJSON (VariableDeclarationDeclaration variableDeclaration) =
        toJSON variableDeclaration
    toJSON (ClassDeclaration name superClass body) =
        estree "ClassDeclaration"
            [ "id" .= name
            , "superClass" .= superClass
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
    | LetVariableDeclaration
    | ConstVariableDeclaration

instance ToJSON VariableDeclarationKind where
    toJSON VarVariableDeclaration   = "var"
    toJSON LetVariableDeclaration   = "let"
    toJSON ConstVariableDeclaration = "const"

data VariableDeclarator
    = VariableDeclarator Pattern (Maybe Expression)

instance ToJSON VariableDeclarator where
    toJSON (VariableDeclarator name value) =
        estree "VariableDeclarator"
            [ "id" .= name
            , "init" .= value ]

newtype ClassBody = ClassBody [MethodDefinition]

instance ToJSON ClassBody where
    toJSON (ClassBody body) =
        estree "ClassBody"
            [ "body" .= body ]

data MethodDefinition
    = MethodDefinition Bool MethodDefinitionKind
        (Either' Expression Identifier) Function

instance ToJSON MethodDefinition where
    toJSON (MethodDefinition static kind key value) =
        estree "MethodDefinition"
            [ "key" .= key
            , "value" .= value
            , "kind" .= kind
            , "computed" .= isLeft' key
            , "static" .= static ]

data MethodDefinitionKind
    = ConstructorMethodDefinition
    | MethodMethodDefinition
    | GetMethodDefinition
    | SetMethodDefinition

instance ToJSON MethodDefinitionKind where
    toJSON ConstructorMethodDefinition = "constructor"
    toJSON MethodMethodDefinition      = "method"
    toJSON GetMethodDefinition         = "get"
    toJSON SetMethodDefinition         = "set"

data Super
    = Super

instance ToJSON Super where
    toJSON Super =
        estree "Super" []

data SpreadElement
    = SpreadElement Expression

instance ToJSON SpreadElement where
    toJSON (SpreadElement argument) =
        estree "SpreadElement"
            [ "argument" .= argument ]

data Function = Function Bool (Maybe Identifier) [Pattern] Block

instance ToJSON Function where
    toJSON (Function generator name params body) =
        estree "FunctionExpression"
            [ "generator" .= generator
            , "id" .= name
            , "params" .= params
            , "body" .= body ]

data Expression
    = IdentifierExpression Identifier
    | LiteralExpression Literal
    | TemplateLiteralExpression TemplateLiteral
    | TaggedTemplateExpression Expression TemplateLiteral
    | ThisExpression
    | ArrayExpression [Maybe (Either' Expression SpreadElement)]
    | ObjectExpression [Property]
    | FunctionExpression Function
    | ArrowFunctionExpression [Pattern] (Either' Block Expression)
    | UnaryExpression UnaryOperator Expression
    | UpdateExpression UpdateOperator PrePostFix Expression
    | BinaryExpression BinaryOperator Expression Expression
    | AssignmentExpression AssignmentOperator Pattern Expression
    | LogicalExpression LogicalOperator Expression Expression
    | MemberExpression Member
    | ConditionalExpression Expression Expression Expression
    | CallExpression (Either' Expression Super)
        [Either' Expression SpreadElement]
    | NewExpression Expression [Either' Expression SpreadElement]
    | SequenceExpression [Expression]
    | YieldExpression Bool (Maybe Expression)
    | ClassExpression (Maybe Identifier) (Maybe Expression) ClassBody
    | MetaProperty
    | PassthruExpression Value

instance ToJSON Expression where
    toJSON (IdentifierExpression identifier) = toJSON identifier
    toJSON (LiteralExpression literal) = toJSON literal
    toJSON (TemplateLiteralExpression templateLiteral) = toJSON templateLiteral
    toJSON (TaggedTemplateExpression tag quasi) =
        estree "TaggedTemplateExpression"
            [ "tag" .= tag
            , "quasi" .= quasi ]
    toJSON ThisExpression =
        estree "ThisExpression" []
    toJSON (ArrayExpression elements) =
        estree "ArrayExpression"
            [ "elements" .= elements ]
    toJSON (ObjectExpression properties) =
        estree "ObjectExpression"
            [ "properties" .= properties ]
    toJSON (FunctionExpression function) = toJSON function
    toJSON (ArrowFunctionExpression params body) =
        estree "ArrowFunctionExpression"
            [ "generator" .= False
            , "id" .= Null
            , "params" .= params
            , "body" .= body
            , "expression" .= isRight' body ]
    toJSON (UnaryExpression operator argument) =
        estree "UnaryExpression"
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
    toJSON (BinaryExpression operator left right) =
        estree "BinaryExpression"
            [ "operator" .= operator
            , "left" .= left
            , "right" .= right ]
    toJSON (AssignmentExpression operator left right) =
        estree "AssignmentExpression"
            [ "operator" .= operator
            , "left" .= left
            , "right" .= right ]
    toJSON (LogicalExpression operator left right) =
        estree "LogicalExpression"
            [ "operator" .= operator
            , "left" .= left
            , "right" .= right ]
    toJSON (MemberExpression member) = toJSON member
    toJSON (ConditionalExpression test consequent alternate) =
        estree "ConditionalExpression"
            [ "test" .= test
            , "alternate" .= alternate
            , "consequent" .= consequent ]
    toJSON (CallExpression callee arguments) =
        estree "CallExpression"
            [ "callee" .= callee
            , "arguments" .= arguments ]
    toJSON (NewExpression callee arguments) =
        estree "NewExpression"
            [ "callee" .= callee
            , "arguments" .= arguments ]
    toJSON (SequenceExpression expressions) =
        estree "SequenceExpression"
            [ "expressions" .= expressions ]
    toJSON (YieldExpression delegate argument) =
        estree "YieldExpression"
            [ "argument" .= argument
            , "delegate" .= delegate ]
    toJSON (ClassExpression name superClass body) =
        estree "ClassExpression"
            [ "id" .= name
            , "superClass" .= superClass
            , "body" .= body ]
    toJSON MetaProperty =
        estree "MetaProperty"
            [ "meta" .= ("new" :: Text)
            , "property" .= ("target" :: Text) ]
    toJSON (PassthruExpression value) = value

data TemplateLiteral
    = TemplateLiteral [TemplateElement] [Expression]

instance ToJSON TemplateLiteral where
    toJSON (TemplateLiteral quasis expressions) =
        estree "TemplateLiteral"
            [ "quasis" .= quasis
            , "expressions" .= expressions ]

newtype TemplateElement = TemplateElement String

instance ToJSON TemplateElement where
    toJSON (TemplateElement raw) =
        estree "TemplateElement"
            [ "tail" .= False -- not actually sure what this is for
            , "value" .= object
                [ "cooked" .= raw -- cooked doesn't matter
                , "raw" .= raw ] ]

data Property
    = Property PropertyKey Expression
    | ShorthandProperty Identifier
    | MethodProperty PropertyKind PropertyKey Function

type PropertyKey = Either' Expression (Either' Literal Identifier)

instance ToJSON Property where
    toJSON (Property key value) =
        estree "Property"
            [ "key" .= key
            , "value" .= value
            , "kind" .= InitProperty
            , "method" .= False
            , "shorthand" .= False
            , "computed" .= isLeft' key ]
    toJSON (ShorthandProperty key) =
        estree "Property"
            [ "key" .= key
            , "value" .= key
            , "kind" .= InitProperty
            , "method" .= False
            , "shorthand" .= True
            , "computed" .= False ]
    toJSON (MethodProperty kind key value) =
        estree "Property"
            [ "key" .= key
            , "value" .= value
            , "kind" .= kind
            , "method" .= case kind of
                InitProperty -> True
                _            -> False
            , "shorthand" .= False
            , "computed" .= isLeft' key ]

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
    = Member (Either' Expression Super) (Either' Expression Identifier)

instance ToJSON Member where
    toJSON (Member obj property) =
        estree "MemberExpression"
            [ "object" .= obj
            , "property" .= property
            , "computed" .= isLeft' property ]

data Pattern
    = IdentifierPattern Identifier
    | MemberPattern Member
    | ObjectPattern [AssignmentProperty]
    | ArrayPattern [Maybe Pattern]
    | RestElement Pattern
    | AssignmentPattern Pattern Expression

instance ToJSON Pattern where
    toJSON (IdentifierPattern identifier) = toJSON identifier
    toJSON (MemberPattern member) = toJSON member
    toJSON (ObjectPattern properties) =
        estree "ObjectPattern"
            [ "properties" .= properties ]
    toJSON (ArrayPattern elements) =
        estree "ArrayPattern"
            [ "elements" .= elements ]
    toJSON (RestElement argument) =
        estree "RestElement"
            [ "argument" .= argument ]
    toJSON (AssignmentPattern left right) =
        estree "AssignmentPattern"
            [ "left" .= left
            , "right" .= right ]

data AssignmentProperty
    = AssignmentProperty PropertyKey Pattern
    | ShorthandAssignmentProperty Pattern

instance ToJSON AssignmentProperty where
    toJSON (AssignmentProperty key value) =
        estree "Property"
            [ "key" .= key
            , "value" .= value
            , "kind" .= InitProperty
            , "method" .= False
            , "shorthand" .= False
            , "computed" .= isLeft' key ]
    toJSON (ShorthandAssignmentProperty key) =
        estree "Property"
            [ "key" .= key
            , "value" .= key
            , "kind" .= InitProperty
            , "method" .= False
            , "shorthand" .= True
            , "computed" .= False ]

data ModuleDeclaration
    = ImportDeclaration [Either' ImportSpecifier
        (Either' ImportDefaultSpecifier ImportNamespaceSpecifier)] Literal
    | ExportNamedDeclarationDeclaration Declaration
    | ExportNamedSpecifiersDeclaration [ExportSpecifier] (Maybe Literal)
    | ExportDefaultDeclaration (Either' Declaration Expression)
    | ExportAllDeclaration Literal

instance ToJSON ModuleDeclaration where
    toJSON (ImportDeclaration specifiers source) =
        estree "ImportDeclaration"
            [ "specifiers" .= specifiers
            , "source" .= source ]
    toJSON (ExportNamedDeclarationDeclaration declaration) =
        estree "ExportNamedDeclaration"
            [ "declaration" .= declaration
            , "specifiers" .= ([] :: [ExportSpecifier])
            , "source" .= Null ]
    toJSON (ExportNamedSpecifiersDeclaration specifiers source) =
        estree "ExportNamedDeclaration"
            [ "declaration" .= Null
            , "specifiers" .= specifiers
            , "source" .= source ]
    toJSON (ExportDefaultDeclaration declaration) =
        estree "ExportDefaultDeclaration"
            [ "declaration" .= declaration ]
    toJSON (ExportAllDeclaration source) =
        estree "ExportAllDeclaration"
            [ "source" .= source ]

data ImportSpecifier
    = ImportSpecifier Identifier
    | AliasedImportSpecifier Identifier Identifier

instance ToJSON ImportSpecifier where
    toJSON (ImportSpecifier imported) =
        estree "ImportSpecifier"
            [ "imported" .= imported
            , "local" .= imported ]
    toJSON (AliasedImportSpecifier imported local) =
        estree "ImportSpecifier"
            [ "imported" .= imported
            , "local" .= local ]

newtype ImportDefaultSpecifier = ImportDefaultSpecifier Identifier

instance ToJSON ImportDefaultSpecifier where
    toJSON (ImportDefaultSpecifier local) =
        estree "ImportDefaultSpecifier"
            [ "local" .= local ]

newtype ImportNamespaceSpecifier = ImportNamespaceSpecifier Identifier

instance ToJSON ImportNamespaceSpecifier where
    toJSON (ImportNamespaceSpecifier local) =
        estree "ImportNamespaceSpecifier"
            [ "local" .= local ]

data ExportSpecifier
    = ExportSpecifier Identifier
    | AliasedExportSpecifier Identifier Identifier

instance ToJSON ExportSpecifier where
    toJSON (ExportSpecifier local) =
        estree "ExportSpecifier"
            [ "local" .= local
            , "exported" .= local ]
    toJSON (AliasedExportSpecifier local exported) =
        estree "ExportSpecifier"
            [ "local" .= local
            , "exported" .= exported ]
