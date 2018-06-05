
export type Arguments = Array<(SpreadElement | Expression)>;
export type IdentifierList = Array<IdentifierName>;

export type Identifier = string;
export type IdentifierName = string;
export type Label = string;

export enum VariableDeclarationKind {
    Var = "var",
    Let = "let",
    Const = "const"
};

export enum CompoundAssignmentOperator {
    AddAssign = "+=",
    SubAssign = "-=",
    MulAssign = "*=",
    DivAssign = "/=",
    ModAssign = "%=",
    PowAssign = "**=",
    ShlAssign = "<<=",
    ShrAssign = ">>=",
    SarAssign = ">>>=",
    BitOrAssign = "|=",
    BitXorAssign = "^=",
    BitAndAssign = "&="
}

export enum BinaryOperator {
    Comma         = ",",
    LogicalOr     = "||",
    LogicalAnd    = "&&",
    BitOr         = "|",
    BitXor        = "^",
    BitAnd        = "&",
    Equal         = "==",
    NotEqual      = "!=",
    StrictEqual   = "===",
    StrictNotEqual= "!==",
    Less          = "<",
    LessEqual     = "<=",
    Greater       = ">",
    GreaterEqual  = ">=",
    In            = "in",
    Instanceof    = "instanceof",
    Shl           = "<<",
    Shr           = ">>",
    Sar           = ">>>",
    Add           = "+",
    Sub           = "-",
    Mul           = "*",
    Div           = "/",
    Mod           = "%",
    Pow           = "**",
}

export enum UnaryOperator {
    Pos       = "+",
    Neg       = "-",
    LogicalNot = "!",
    BitNot    = "~",
    Typeof    = "typeof",
    Void      = "void",
    Delete    = "delete"
}

export enum UpdateOperator {
    Incr = "++",
    Decr = "--"
}

//
// Deferred assertions
//

export class AssertedBlockScope {
    // checked eagerly during transformation
    readonly lexicallyDeclaredNames: IdentifierList;

    // checked lazily as inner functions are invoked
    readonly capturedNames: IdentifierList;
    readonly hasDirectEval: boolean;

    constructor(params: {lexicallyDeclaredNames: IdentifierList,
                         capturedNames: IdentifierList,
                         hasDirectEval: boolean})
    {
        this.lexicallyDeclaredNames = params.lexicallyDeclaredNames;
        this.capturedNames = params.capturedNames;
        this.hasDirectEval = params.hasDirectEval;
    }
}

export class AssertedVarScope {
    // checked eagerly during transformation
    readonly lexicallyDeclaredNames: IdentifierList;
    readonly varDeclaredNames: IdentifierList;

    // checked lazily as inner functions are invoked
    readonly capturedNames: IdentifierList;
    readonly hasDirectEval: boolean;

    constructor(params: {lexicallyDeclaredNames: IdentifierList,
                         varDeclaredNames: IdentifierList,
                         capturedNames: IdentifierList,
                         hasDirectEval: boolean})
    {
        this.lexicallyDeclaredNames = params.lexicallyDeclaredNames;
        this.varDeclaredNames = params.varDeclaredNames;
        this.capturedNames = params.capturedNames;
        this.hasDirectEval = params.hasDirectEval;
    }
}

export class AssertedParameterScope {
    // checked eagerly during transformation
    readonly parameterNames: IdentifierList;

    // checked lazily as inner functions are invoked
    readonly capturedNames: IdentifierList;
    readonly hasDirectEval: boolean;

    constructor(params: {parameterNames: IdentifierList,
                         capturedNames: IdentifierList,
                         hasDirectEval: boolean})
    {
        this.parameterNames = params.parameterNames;
        this.capturedNames = params.capturedNames;
        this.hasDirectEval = params.hasDirectEval;
    }
}

//
// Nodes
//

export abstract class BaseNode {
    readonly type_: string;

    constructor() {
        this.type_ = this.constructor.name as string;
    }
}

export type Program = (Script | Module);

export type IterationStatement =
    (DoWhileStatement |
     ForInStatement   |
     ForOfStatement   |
     ForStatement     |
     WhileStatement);

export type Statement =
    (Block               |
     BreakStatement      |
     ContinueStatement   |
     ClassDeclaration    |
     DebuggerStatement   |
     EmptyStatement      |
     ExpressionStatement |
     FunctionDeclaration |
     IfStatement         |
     IterationStatement  |
     LabelledStatement   |
     ReturnStatement     |
     SwitchStatement     |
     SwitchStatementWithDefault |
     ThrowStatement      |
     TryCatchStatement   |
     TryFinallyStatement |
     VariableDeclaration |
     WithStatement);

export type Literal =
    (LiteralBooleanExpression  |
     LiteralInfinityExpression |
     LiteralNullExpression     |
     LiteralNumericExpression  |
     LiteralStringExpression);

export type Expression =
    (Literal                    |
     LiteralRegExpExpression    |
     ArrayExpression            |
     ArrowExpression            |
     AssignmentExpression       |
     BinaryExpression           |
     CallExpression             |
     CompoundAssignmentExpression |
     ComputedMemberExpression   |
     ConditionalExpression      |
     ClassExpression            |
     FunctionExpression         |
     IdentifierExpression       |
     NewExpression              |
     NewTargetExpression        |
     ObjectExpression           |
     UnaryExpression            |
     StaticMemberExpression     |
     TemplateExpression         |
     ThisExpression             |
     UpdateExpression           |
     YieldExpression            |
     YieldStarExpression        |
     AwaitExpression)

export type PropertyName =
    (ComputedPropertyName |
     LiteralPropertyName);

export type MethodDefinition = (Method | Getter | Setter);

export type ObjectProperty =
    (MethodDefinition   |
     DataProperty       |
     ShorthandProperty);

export type ExportDeclaration =
    (ExportAllFrom  |
     ExportFrom     |
     ExportLocals   |
     ExportDefault  |
     Export);

export type ImportDeclaration = (ImportNamespace | Import);

export type FunctionDeclaration =
    (EagerFunctionDeclaration |
     SkippableFunctionDeclaration);

export type FunctionExpression =
    (EagerFunctionExpression |
     SkippableFunctionExpression);

export type Method = (EagerMethod | SkippableMethod);

export type Getter = (EagerGetter | SkippableGetter);

export type Setter = (EagerSetter | SkippableSetter);

export type ArrowExpression =
    (EagerArrowExpression |
     SkippableArrowExpression);


//
// Bindings
//

export class BindingIdentifier extends BaseNode {
    readonly name: Identifier;

    constructor(params: {name: Identifier}) {
        super();
        this.name = params.name;
    }
}

export type BindingPattern = (ObjectBinding | ArrayBinding);
export type Binding = (BindingPattern | BindingIdentifier);

export type SimpleAssignmentTarget =
    (AssignmentTargetIdentifier     |
     ComputedMemberAssignmentTarget |
     StaticMemberAssignmentTarget);

export type AssignmentTargetPattern =
    (ObjectAssignmentTarget |
     ArrayAssignmentTarget);

// `DestructuringAssignmentTarget`
export type AssignmentTarget =
    (AssignmentTargetPattern |
     SimpleAssignmentTarget);

// `FormalParameter`
export type Parameter = (Binding | BindingWithInitializer);

export class BindingWithInitializer extends BaseNode {
    readonly binding: Binding;
    readonly init: Expression;

    constructor(params: {binding: Binding, init: Expression}) {
        super();
        this.binding = params.binding;
        this.init = params.init;
    }
}

export class AssignmentTargetIdentifier extends BaseNode {
    readonly name: Identifier;

    constructor(params: {name: Identifier}) {
        super();
        this.name = params.name;
    }
}

export class ComputedMemberAssignmentTarget extends BaseNode {
    // The object whose property is being assigned.
    readonly object_: (Expression | Super);
    // The expression resolving to the name of the property to be accessed.
    readonly expression: Expression;

    constructor(params: {object_: (Expression|Super),
                         expression: Expression})
    {
        super();
        this.object_ = params.object_;
        this.expression = params.expression;
    }
}

export class StaticMemberAssignmentTarget extends BaseNode {
    // The object whose property is being assigned.
    readonly object_: (Expression | Super);
    // The name of the property to be accessed.
    readonly property: IdentifierName;

    constructor(params: {object_: (Expression | Super),
                         property: IdentifierName})
    {
        super();
        this.object_ = params.object_;
        this.property = params.property;
    }
}

// `ArrayBindingPattern`
export class ArrayBinding extends BaseNode {
    // The elements of the array pattern; a null value represents an elision.
    readonly elements: Array<(Binding | BindingWithInitializer | null)>;
    readonly rest: Binding | null;
}

// `SingleNameBinding`
export class BindingPropertyIdentifier extends BaseNode {
    readonly binding: BindingIdentifier;
    readonly init: Expression | null;
}

// `BindingProperty :: PropertyName : BindingElement`
export class BindingPropertyProperty extends BaseNode {
    readonly name: PropertyName;
    readonly binding: (Binding | BindingWithInitializer);
}

export type BindingProperty =
    (BindingPropertyIdentifier |
     BindingPropertyProperty);

export class ObjectBinding extends BaseNode {
    readonly properties: Array<BindingProperty>;
}

// This interface represents the case where the initializer is present in
// `AssignmentElement :: DestructuringAssignmentTarget Initializer_opt`.
export class AssignmentTargetWithInitializer extends BaseNode {
  readonly binding: AssignmentTarget;
  readonly init: Expression;
}

// `ArrayAssignmentPattern`
export class ArrayAssignmentTarget extends BaseNode {
    // The elements of the array pattern; a null value represents an elision.
    readonly elements: Array<(AssignmentTarget |
                              AssignmentTargetWithInitializer |
                              null)>;
    readonly rest: AssignmentTarget | null;
}

// `AssignmentProperty :: IdentifierReference Initializer_opt`
export class AssignmentTargetPropertyIdentifier extends BaseNode {
    readonly binding: AssignmentTargetIdentifier;
    readonly init: Expression | null;
}

// `AssignmentProperty :: PropertyName : Node`
export class AssignmentTargetPropertyProperty extends BaseNode {
    readonly name: PropertyName;
    readonly binding: (AssignmentTarget | AssignmentTargetWithInitializer);
}

export type AssignmentTargetProperty =
    (AssignmentTargetPropertyIdentifier |
     AssignmentTargetPropertyProperty);


// `ObjectAssignmentPattern`
export class ObjectAssignmentTarget extends BaseNode {
    readonly properties: Array<AssignmentTargetProperty>;
}


// classes

export class ClassExpression extends BaseNode {
    readonly name: BindingIdentifier | null;
    readonly super_: Expression | null;
    readonly elements: Array<ClassElement>;
}

export class ClassDeclaration extends BaseNode {
    readonly name: BindingIdentifier;
    readonly super_: Expression | null;
    readonly elements: Array<ClassElement>;
}

export class ClassElement extends BaseNode {
    // True iff `IsStatic` of ClassElement is true.
    readonly isStatic: boolean;
    readonly method: MethodDefinition;
}


// modules

export class Module extends BaseNode {
    readonly scope: AssertedVarScope | null;
    readonly directives: Array<Directive>;
    readonly items: Array<(ImportDeclaration | ExportDeclaration | Statement)>;
}

// An `ImportDeclaration` not including a namespace import.
export class Import extends BaseNode {
    readonly moduleSpecifier: string;
    // `ImportedDefaultBinding`, if present.
    readonly defaultBinding: BindingIdentifier | null;
    readonly namedImports: Array<ImportSpecifier>;
}

// An `ImportDeclaration` including a namespace import.
export class ImportNamespace extends BaseNode {
    readonly moduleSpecifier: string;
    // `ImportedDefaultBinding`, if present.
    readonly defaultBinding:  BindingIdentifier | null;
    readonly namespaceBinding: BindingIdentifier;
}

export class ImportSpecifier extends BaseNode {
    // The `IdentifierName` in the production
    //  `ImportSpecifier :: IdentifierName as ImportedBinding`;
    // absent if this specifier represents the production
    // `ImportSpecifier :: ImportedBinding`.
    readonly name: IdentifierName | null;
    readonly binding: BindingIdentifier;
}

// `export * FromClause;`
export class ExportAllFrom extends BaseNode {
    readonly moduleSpecifier: string;
}

// `export ExportClause FromClause;`
export class ExportFrom extends BaseNode {
    readonly namedExports: Array<ExportFromSpecifier>;
    readonly moduleSpecifier: string;
}

// `export ExportClause;`
export class ExportLocals extends BaseNode {
    readonly namedExports: Array<ExportLocalSpecifier>;
}

// `export VariableStatement`, `export Declaration`
export class Export extends BaseNode {
    readonly declaration: (FunctionDeclaration |
                           ClassDeclaration |
                           VariableDeclaration);
}

// `export default HoistableDeclaration`,
// `export default ClassDeclaration`,
// `export default AssignmentExpression`
export class ExportDefault extends BaseNode {
    readonly body: (FunctionDeclaration | ClassDeclaration | Expression);
}

// `ExportSpecifier`, as part of an `ExportFrom`.
export class ExportFromSpecifier extends BaseNode {
    // The only `IdentifierName in `ExportSpecifier :: IdentifierName`,
    // or the first in `ExportSpecifier :: IdentifierName as IdentifierName`.
    readonly name: IdentifierName;
    // The second `IdentifierName` in
    // `ExportSpecifier :: IdentifierName as IdentifierName`,
    // if that is the production represented.
    readonly exportedName: IdentifierName | null;
}

// `ExportSpecifier`, as part of an `ExportLocals`.
export class ExportLocalSpecifier extends BaseNode {
    // The only `IdentifierName in `ExportSpecifier :: IdentifierName`,
    // or the first in `ExportSpecifier :: IdentifierName as IdentifierName`.
    readonly name: IdentifierExpression;
    // The second `IdentifierName` in
    // `ExportSpecifier :: IdentifierName as IdentifierName`, if present.
    readonly exportedName: IdentifierName | null;
}


// property definition

// `MethodDefinition :: PropertyName ( UniqueFormalParameters ) { FunctionBody }`,
// `GeneratorMethod :: * PropertyName ( UniqueFormalParameters ) { GeneratorBody }`,
// `AsyncMethod :: async PropertyName ( UniqueFormalParameters ) { AsyncFunctionBody }`
export class EagerMethod extends BaseNode {
    // True for `AsyncMethod`, false otherwise.
    readonly isAsync: boolean;
    // True for `GeneratorMethod`, false otherwise.
    readonly isGenerator: boolean;
    readonly name: PropertyName;
    readonly parameterScope: AssertedParameterScope | null;
    readonly bodyScope: AssertedVarScope | null;
    // The `UniqueFormalParameters`.
    readonly params: FormalParameters;
    readonly body: FunctionBody;
}

/* [Skippable] */
export class SkippableMethod extends BaseNode {
    readonly skipped: EagerMethod;
}

// `get PropertyName ( ) { FunctionBody }`
export class EagerGetter extends BaseNode {
    readonly name: PropertyName;
    readonly bodyScope: AssertedVarScope | null;
    readonly body: FunctionBody;
}

/* [Skippable] */
export class SkippableGetter extends BaseNode {
    readonly skipped: EagerGetter;
}

// `set PropertyName ( PropertySetParameterList ) { FunctionBody }`
export class EagerSetter extends BaseNode {
    readonly name: PropertyName;
    readonly parameterScope: AssertedParameterScope | null;
    readonly bodyScope: AssertedVarScope | null;
    // The `PropertySetParameterList`.
    readonly param: Parameter;
    readonly body: FunctionBody;
}

/* [Skippable] */
export class SkippableSetter extends BaseNode {
    readonly skipped: EagerSetter;
}

// `PropertyDefinition :: PropertyName : AssignmentExpression`
export class DataProperty extends BaseNode {
    readonly name: PropertyName;
    // The `AssignmentExpression`.
    readonly expression: Expression;

    constructor(params: {name: PropertyName, expression: Expression}) {
        super();
        this.name = params.name;
        this.expression = params.expression;
    }
}

// `PropertyDefinition :: IdentifierReference`
export class ShorthandProperty extends BaseNode {
    // The `IdentifierReference`.
    readonly name: IdentifierExpression;
}

export class ComputedPropertyName extends BaseNode {
    readonly expression: Expression;
}

// `LiteralPropertyName`
export class LiteralPropertyName extends BaseNode {
    readonly value: string;

    constructor(params: {value: string}) {
        super();
        this.value = params.value;
    }
}


// literals

// `BooleanLiteral`
export class LiteralBooleanExpression extends BaseNode {
    readonly value: boolean;

    constructor(params: {value: boolean}) {
        super();
        this.value = params.value;
    }
}

// A `NumericLiteral` for which the Number value of its MV is positive infinity.
export class LiteralInfinityExpression extends BaseNode { };

// `NullLiteral`
export class LiteralNullExpression extends BaseNode { };

// `NumericLiteral`
export class LiteralNumericExpression extends BaseNode {
    readonly value: number;

    constructor(params: {value: number}) {
        super();
        this.value = params.value;
    }
}

// `RegularExpressionLiteral`
export class LiteralRegExpExpression extends BaseNode {
    readonly pattern: string;
    readonly flags: string;

    constructor(params: {pattern: string, flags: string}) {
        super();
        this.pattern = params.pattern;
        this.flags = params.flags;
    }
}

// `StringLiteral`
export class LiteralStringExpression extends BaseNode {
    readonly value: string;

    constructor(params: {value: string}) {
        super();
        this.value = params.value;
    }
}


// other expressions

// `ArrayLiteral`
export type ArrayElement = (SpreadElement | Expression | null);
export class ArrayExpression extends BaseNode {
    // The elements of the array literal; a null value represents an elision.
    readonly elements: Array<ArrayElement>;

    constructor(params: {elements:Array<ArrayElement>}) {
        super();
        this.elements = params.elements;
    }
}

// `ArrowFunction`,
// `AsyncArrowFunction`
export class EagerArrowExpression extends BaseNode {
    // True for `AsyncArrowFunction`, false otherwise.
    readonly isAsync: boolean;
    readonly parameterScope: AssertedParameterScope | null;
    readonly bodyScope: AssertedVarScope | null;
    readonly params: FormalParameters;
    readonly body: (FunctionBody | Expression);
}

/* [Skippable] */
export class SkippableArrowExpression extends BaseNode {
    readonly skipped: EagerArrowExpression;
}

// `AssignmentExpression :: LeftHandSideExpression = AssignmentExpression`
export class AssignmentExpression extends BaseNode {
    // The `LeftHandSideExpression`.
    readonly binding: AssignmentTarget;
    // The `AssignmentExpression` following the `=`.
    readonly expression: Expression;

    constructor(params: {binding: AssignmentTarget,
                         expression: Expression})
    {
        super();
        this.binding = params.binding;
        this.expression = params.expression;
    }
}

// `ExponentiationExpression`,
// `MultiplicativeExpression`,
// `AdditiveExpression`,
// `ShiftExpression`,
// `RelationalExpression`,
// `EqualityExpression`,
// `BitwiseANDExpression`,
// `BitwiseXORExpression`,
// `BitwiseORExpression`,
// `LogicalANDExpression`,
// `LogicalORExpression`
export class BinaryExpression extends BaseNode {
    readonly operator: BinaryOperator;
    // The expression before the operator.
    readonly left: Expression;
    // The expression after the operator.
    readonly right: Expression;

    constructor(params: {operator: BinaryOperator,
                         right: Expression,
                         left: Expression})
    {
        super();
        this.operator = params.operator;
        this.left = params.left;
        this.right = params.right;
    }
}

export class CallExpression extends BaseNode {
    readonly callee: (Expression | Super);
    readonly arguments_: Arguments;

    constructor(params: {callee:(Expression | Super),
                         arguments_: Arguments})
    {
        super();
        this.callee = params.callee;
        this.arguments_ = params.arguments_;
    }
}

// `AssignmentExpression :: LeftHandSideExpression AssignmentOperator AssignmentExpression`
export class CompoundAssignmentExpression extends BaseNode {
    readonly operator: CompoundAssignmentOperator;
    // The `LeftHandSideExpression`.
    readonly binding: SimpleAssignmentTarget;
    // The `AssignmentExpression`.
    readonly expression: Expression;

    constructor(params: {operator: CompoundAssignmentOperator,
                         binding: SimpleAssignmentTarget,
                         expression: Expression})
    {
        super();
        this.operator = params.operator;
        this.binding = params.binding;
        this.expression = params.expression;
    }
}

export class ComputedMemberExpression extends BaseNode {
    // The object whose property is being accessed.
    readonly object_: (Expression | Super);
    // The expression resolving to the name of the property to be accessed.
    readonly expression: Expression;

    constructor(params: {object_: (Expression|Super),
                         expression: Expression})
    {
        super();
        this.object_ = params.object_;
        this.expression = params.expression;
    }
}

// `ConditionalExpression :: LogicalORExpression ? AssignmentExpression : AssignmentExpression`
export class ConditionalExpression extends BaseNode {
    // The `LogicalORExpression`.
    readonly test: Expression;
    // The first `AssignmentExpression`.
    readonly consequent: Expression;
    // The second `AssignmentExpression`.
    readonly alternate: Expression;

    constructor(params: {test: Expression,
                         consequent: Expression,
                         alternate: Expression})
    {
        super();
        this.test = params.test;
        this.consequent = params.consequent;
        this.alternate = params.alternate;
    }
}

// `FunctionExpression`,
// `GeneratorExpression`,
// `AsyncFunctionExpression`,
export class EagerFunctionExpression extends BaseNode {
    readonly isAsync: boolean;
    readonly isGenerator: boolean;
    readonly name: BindingIdentifier | null;
    readonly parameterScope: AssertedParameterScope | null;
    readonly bodyScope: AssertedVarScope | null;
    readonly params: FormalParameters;
    readonly body: FunctionBody;

    constructor(params: {isAsync: boolean;
                         isGenerator: boolean;
                         name: BindingIdentifier;
                         parameterScope: AssertedParameterScope | null;
                         bodyScope: AssertedVarScope | null;
                         params: FormalParameters;
                         body: FunctionBody})
    {
        super();
        this.isAsync = params.isAsync;
        this.isGenerator = params.isGenerator;
        this.name = params.name;
        this.parameterScope = params.parameterScope;
        this.bodyScope = params.bodyScope;
        this.params = params.params;
        this.body = params.body;
    }
}

/* [Skippable] */
export class SkippableFunctionExpression extends BaseNode {
    readonly skipped: EagerFunctionExpression;
}

// `IdentifierReference`
export class IdentifierExpression extends BaseNode {
    readonly name: Identifier;

    constructor(params: {name: Identifier}) {
        super();
        this.name = params.name;
    }
}

export class NewExpression extends BaseNode {
    readonly callee: Expression;
    readonly arguments_: Arguments;

    constructor(params: {callee: Expression, arguments_: Arguments}) {
        super();
        this.callee = params.callee;
        this.arguments_ = params.arguments_;
    }
}

export class NewTargetExpression extends BaseNode { };

export class ObjectExpression extends BaseNode {
    readonly properties: Array<ObjectProperty>;

    constructor(params: {properties: Array<ObjectProperty>}) {
        super();
        this.properties = params.properties;
    }
}

export class UnaryExpression extends BaseNode {
    readonly operator: UnaryOperator;
    readonly operand: Expression;

    constructor(params: {operator: UnaryOperator,
                         operand: Expression})
    {
        super();
        this.operator = params.operator;
        this.operand = params.operand;
    }
}

export class StaticMemberExpression extends BaseNode {
    // The object whose property is being accessed.
    readonly object_: (Expression | Super);
    // The name of the property to be accessed.
    readonly property: IdentifierName;

    constructor(params: {object_: (Expression | Super),
                         property: IdentifierName}) {
        super();
        this.object_ = params.object_;
        this.property = params.property;
    }
}

// `TemplateLiteral`,
// `MemberExpression :: MemberExpression TemplateLiteral`,
// `CallExpression : CallExpression TemplateLiteral`
export class TemplateExpression extends BaseNode {
    // The second `MemberExpression` or `CallExpression`, if present.
    readonly tag: Expression | null;
    // The contents of the template. This list must be alternating
    // TemplateElements and Expressions, beginning and ending with
    // TemplateElement.
    readonly elements: Array<(Expression | TemplateElement)>;
}

// `PrimaryExpression :: this`
export class ThisExpression extends BaseNode { };

// `UpdateExpression :: LeftHandSideExpression ++`,
// `UpdateExpression :: LeftHandSideExpression --`,
// `UpdateExpression :: ++ LeftHandSideExpression`,
// `UpdateExpression :: -- LeftHandSideExpression`
export class UpdateExpression extends BaseNode {
    // True for `UpdateExpression :: ++ LeftHandSideExpression` and
    // `UpdateExpression :: -- LeftHandSideExpression`, false otherwise.
    readonly isPrefix: boolean;
    readonly operator: UpdateOperator;
    readonly operand: SimpleAssignmentTarget;

    constructor(params: {isPrefix: boolean,
                         operator: UpdateOperator,
                         operand: SimpleAssignmentTarget})
    {
        super();
        this.isPrefix = params.isPrefix;
        this.operator = params.operator;
        this.operand = params.operand;
    }
}

// `YieldExpression :: yield`,
// `YieldExpression :: yield AssignmentExpression`
export class YieldExpression extends BaseNode {
    // The `AssignmentExpression`, if present.
    readonly expression: Expression | null;
}

// `YieldExpression :: yield * AssignmentExpression`
export class YieldStarExpression extends BaseNode {
    readonly expression: Expression;
}

export class AwaitExpression extends BaseNode {
    readonly expression: Expression;
}


// other statements

export class BreakStatement extends BaseNode {
    readonly label: Label | null;

    constructor(params: {label: Label|null}) {
        super();
        this.label = params.label;
    }
}

export class ContinueStatement extends BaseNode {
    readonly label: Label | null;

    constructor(params: {label: Label|null}) {
        super();
        this.label = params.label;
    }
}

export class DebuggerStatement extends BaseNode { };

export class DoWhileStatement extends BaseNode {
    readonly test: Expression;
    readonly body: Statement;

    constructor(params: {test: Expression, body: Statement}) {
        super();
        this.test = params.test;
        this.body = params.body;
    }
}

export class EmptyStatement extends BaseNode { };

export class ExpressionStatement extends BaseNode {
    readonly expression: Expression;

    constructor(params: {expression: Expression}) {
        super();
        this.expression = params.expression;
    }
}

export class ForInOfBinding extends BaseNode {
    readonly kind: VariableDeclarationKind;
    readonly binding: Binding;

    constructor(params: {kind: VariableDeclarationKind,
                         binding: Binding})
    {
        super();
        this.kind = params.kind;
        this.binding = params.binding;
    }
}

// `for ( LeftHandSideExpression in Expression ) Statement`,
// `for ( var ForBinding in Expression ) Statement`,
// `for ( ForDeclaration in Expression ) Statement`,
// `for ( var BindingIdentifier Initializer in Expression ) Statement`
export class ForInStatement extends BaseNode {
    // The expression or declaration before `in`.
    readonly left: (ForInOfBinding | AssignmentTarget);
    // The expression after `in`.
    readonly right: Expression;
    readonly body: Statement;

    constructor(params: {left: (ForInOfBinding | AssignmentTarget),
                         right: Expression,
                         body: Statement}) {
        super();
        this.left = params.left;
        this.right = params.right;
        this.body = params.body;
    }
}

// `for ( LeftHandSideExpression of Expression ) Statement`,
// `for ( var ForBinding of Expression ) Statement`,
// `for ( ForDeclaration of Expression ) Statement`
export class ForOfStatement extends BaseNode {
    // The expression or declaration before `of`.
    readonly left: (ForInOfBinding | AssignmentTarget);
    // The expression after `of`.
    readonly right: Expression;
    readonly body: Statement;
}

// `for ( Expression ; Expression ; Expression ) Statement`,
// `for ( var VariableDeclarationList ; Expression ; Expression ) Statement`
export class ForStatement extends BaseNode {
    // The expression or declaration before the first `;`, if present.
    readonly init: (VariableDeclaration | Expression | null);
    // The expression before the second `;`, if present
    readonly test: Expression | null;
    // The expression after the second `;`, if present
    readonly update: Expression | null;
    readonly body: Statement;

    constructor(params: {init: (VariableDeclaration | Expression | null),
                         test: Expression | null,
                         update: Expression | null,
                         body: Statement})
    {
        super();
        this.init = params.init;
        this.test = params.test;
        this.update = params.update;
        this.body = params.body;
    }
}

// `if ( Expression ) Statement`,
// `if ( Expression ) Statement else Statement`,
export class IfStatement extends BaseNode {
    readonly test: Expression;
    // The first `Statement`.
    readonly consequent: Statement;
    // The second `Statement`, if present.
    readonly alternate: Statement | null;

    constructor(params: {test: Expression,
                         consequent: Statement,
                         alternate: Statement|null})
    {
        super();
        this.test = params.test;
        this.consequent = params.consequent;
        this.alternate = params.alternate;
    }
}

export class LabelledStatement extends BaseNode {
    readonly label: Label;
    readonly body: Statement;
}

export class ReturnStatement extends BaseNode {
    readonly expression: Expression | null;

    constructor(params: {expression: Expression | null}) {
        super();
        this.expression = this.expression;
    }
}

// A `SwitchStatement` whose `CaseBlock` is
//   `CaseBlock :: { CaseClauses }`.
export class SwitchStatement extends BaseNode {
    readonly discriminant: Expression;
    readonly cases: Array<SwitchCase>;

    constructor(params: {discriminant: Expression,
                         cases: Array<SwitchCase>})
    {
        super();
        this.discriminant = params.discriminant;
        this.cases = params.cases;
    }
}

// A `SwitchStatement` whose `CaseBlock` is
//   `CaseBlock :: { CaseClauses DefaultClause CaseClauses }`.
export class SwitchStatementWithDefault extends BaseNode {
    readonly discriminant: Expression;
    // The `CaseClauses` before the `DefaultClause`.
    readonly preDefaultCases: Array<SwitchCase>;
    // The `DefaultClause`.
    readonly defaultCase: SwitchDefault;
    // The `CaseClauses` after the `DefaultClause`.
    readonly postDefaultCases: Array<SwitchCase>;

    constructor(params: {discriminant: Expression,
                         preDefaultCases: Array<SwitchCase>,
                         defaultCase: SwitchDefault,
                         postDefaultCases: Array<SwitchCase>})
    {
        super();
        this.discriminant = params.discriminant;
        this.preDefaultCases = params.preDefaultCases;
        this.defaultCase = params.defaultCase;
        this.postDefaultCases = params.postDefaultCases;
    }
}

export class ThrowStatement extends BaseNode {
    readonly expression: Expression;

    constructor(params: {expression: Expression}) {
        super();
        this.expression = params.expression;
    }
}

// `TryStatement :: try Block Catch`
export class TryCatchStatement extends BaseNode {
    readonly body: Block;
    readonly catchClause: CatchClause;

    constructor(params: {body: Block, catchClause: CatchClause}) {
        super();
        this.body = params.body;
        this.catchClause = params.catchClause;
    }
}

// `TryStatement :: try Block Finally`,
// `TryStatement :: try Block Catch Finally`
export class TryFinallyStatement extends BaseNode {
    // The `Block`.
    readonly body: Block;
    // The `Catch`, if present.
    readonly catchClause: CatchClause | null;
    // The `Finally`.
    readonly finalizer: Block;
}

export class WhileStatement extends BaseNode {
    readonly test: Expression;
    readonly body: Statement;

    constructor(params: {test: Expression, body: Statement}) {
        super();
        this.test = params.test;
        this.body = params.body;
    }
}

export class WithStatement extends BaseNode {
    readonly object_: Expression;
    readonly body: Statement;
}


// other nodes

export class Block extends BaseNode {
    readonly scope: AssertedBlockScope | null;
    readonly statements: Array<Statement>;

    constructor(params: {scope: AssertedBlockScope,
                         statements: Array<Statement>})
    {
        super();
        this.scope = params.scope;
        this.statements = params.statements;
    }
}

// `Catch`
export class CatchClause extends BaseNode {
    // `AssertedParameterScope` is used for catch bindings so the declared names
    // are checked using BoundNames.
    readonly bindingScope: AssertedParameterScope | null;
    readonly binding: Binding;
    readonly body: Block;

    constructor(params: {bindingScope: AssertedParameterScope|null,
                         binding: Binding,
                         body: Block})
    {
        super();
        this.bindingScope = params.bindingScope;
        this.binding = params.binding;
        this.body = params.body;
    }
}

// An item in a `DirectivePrologue`
export class Directive extends BaseNode {
    readonly rawValue: string;

    constructor(params: {rawValue:string}) {
        super();
        this.rawValue = params.rawValue;
    }
}

export class FormalParameters extends BaseNode {
    readonly items: Array<Parameter>;
    readonly rest: Binding | null;

    constructor(params: {items: Array<Parameter>,
                         rest: Binding | null})
    {
        super();
        this.items = params.items;
        this.rest = params.rest;
    }
}

export class FunctionBody extends BaseNode {
    readonly directives: Array<Directive>;
    readonly statements: Array<Statement>;

    constructor(params: {directives: Array<Directive>,
                         statements: Array<Statement>})
    {
        super();
        this.directives = params.directives;
        this.statements = params.statements;
    }
}



// `FunctionDeclaration`,
// `GeneratorDeclaration`,
// `AsyncFunctionDeclaration`
export class EagerFunctionDeclaration extends BaseNode {
    readonly isAsync: boolean;
    readonly isGenerator: boolean;
    readonly name: BindingIdentifier;
    readonly parameterScope: AssertedParameterScope | null;
    readonly bodyScope: AssertedVarScope | null;
    readonly params: FormalParameters;
    readonly body: FunctionBody;

    constructor(params: {isAsync: boolean;
                         isGenerator: boolean;
                         name: BindingIdentifier;
                         parameterScope: AssertedParameterScope | null;
                         bodyScope: AssertedVarScope | null;
                         params: FormalParameters;
                         body: FunctionBody})
    {
        super();
        this.isAsync = params.isAsync;
        this.isGenerator = params.isGenerator;
        this.name = params.name;
        this.parameterScope = params.parameterScope;
        this.bodyScope = params.bodyScope;
        this.params = params.params;
        this.body = params.body;
    }
}

/* [Skippable] */
export class SkippableFunctionDeclaration extends BaseNode {
    readonly skipped: EagerFunctionDeclaration;
}

export class Script extends BaseNode {
    readonly scope: AssertedVarScope | null;
    readonly directives: Array<Directive>;
    readonly statements: Array<Statement>;

    constructor(params: {scope:AssertedVarScope|null,
                         directives: Array<Directive>,
                         statements: Array<Statement>})
    {
        super();
        this.scope = params.scope;
        this.directives = params.directives;
        this.statements = params.statements;
    }
}

export class SpreadElement extends BaseNode {
    readonly expression: Expression;
}

// `super`
export class Super extends BaseNode { };

// `CaseClause`
export class SwitchCase extends BaseNode {
    readonly test: Expression;
    readonly consequent: Array<Statement>;

    constructor(params: {test: Expression,
                         consequent: Array<Statement>})
    {
        super();
        this.test = params.test;
        this.consequent = params.consequent;
    }
}

// `DefaultClause`
export class SwitchDefault extends BaseNode {
    readonly consequent: Array<Statement>;

    constructor(params: {consequent: Array<Statement>}) {
        super();
        this.consequent = params.consequent;
    }
}

// `TemplateCharacters`
export class TemplateElement extends BaseNode {
    readonly rawValue: string;
}

export class VariableDeclaration extends BaseNode {
    readonly kind: VariableDeclarationKind;
    /* [NonEmpty] */
    readonly declarators: Array<VariableDeclarator>;

    constructor(params: {kind: VariableDeclarationKind,
                         declarators: Array<VariableDeclarator>})
    {
        super();
        this.kind = params.kind;
        this.declarators = params.declarators;
    }
}

export class VariableDeclarator extends BaseNode {
    readonly binding: Binding;
    readonly init: Expression | null;


    constructor(params: {binding: Binding,
                         init: (Expression | null)})
    {
        super();
        this.binding = params.binding;
        this.init = params.init;
    }
}
