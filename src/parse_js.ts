
import * as assert from 'assert';
import * as shift from 'shift-parser';

import * as S from './schema';

class MatchError extends Error {
    readonly matchType: string;
    readonly unrecognizedValue: any;

    constructor(matchType, unrecognizedValue) {
        super(`MatchError(${matchType}) - ${unrecognizedValue}`);
        this.matchType = matchType;
        this.unrecognizedValue = unrecognizedValue;
    }
}

function assertNodeType(node: any, typeStr: string) {
    assert.equal(node.type, typeStr, `Node type ${node.type} != ${typeStr}`);
}
function assertType(val: any, typeStr: string, nullable: boolean = false) {
    const chkStr: string = nullable ? `${typeStr}???` : typeStr;
    if (nullable && val === null) { return; }
    assert.equal(typeof(val), typeStr, `Type ${typeof val} != ${chkStr}`);
}
function assertIsArray(val: any) {
    return Array.isArray(val);
}
function propNames(obj: any) {
    return Object.getOwnPropertyNames(obj);
}
function summarizeNode(obj: any): any {
    const result = {};
    for (let name in obj) {
        const n = (name === 'type') ? '<<<TYPE>>>' : name;
        if (Array.isArray(obj[name])) {
            result[n] = '<<<ARRAY>>>';
        } else if ((typeof(obj[name]) === 'object') && (obj[name] !== null)) {
            if ('type' in obj[name]) {
                result[n] = `<<<TYPE:${obj[name].type}>>>`;
            } else {
                result[n] = '<<<OBJECT>>>';
            }
        } else {
            result[n] = obj[name];
        }
    }
    return result;
}
function nodeShortSummary(obj: any): string {
    return `${obj.type}(${propNames(obj)})`;
}

type ScopeNameLocation = (S.VariableDeclarationKind | 'parameter');

abstract class BaseScope {
    capturedNames: Array<string>;
    hasDirectEval: boolean;

    nameMap: Map<string, ScopeNameLocation>;
    captureSet: Set<string>;

    constructor() {
        this.capturedNames = new Array();
        this.hasDirectEval = false;

        this.nameMap = new Map();
        this.captureSet = new Set();
    }

    protected addName(name: string, location: ScopeNameLocation) {
        this.nameMap.set(name, location);
    }

    doesBindName(name: string): boolean {
        return this.nameMap.get(name) !== undefined;
    }

    /** Try to capture the use of the given name with this scope.
     * Return whether successful. */
    findOrCaptureUse(name: string, capture: boolean): boolean {
        if (this.doesBindName(name)) {
            if (capture && !this.captureSet.has(name)) {
                this.captureSet.add(name);
                this.capturedNames.push(name);
            }
            return true;
        }
        return false;
    }
}

class BlockScope extends BaseScope {
    lexicallyDeclaredNames: Array<string>;

    constructor() {
        super();
        this.lexicallyDeclaredNames = new Array();
    }

    addLetName(name: string) {
        super.addName(name, S.VariableDeclarationKind.Let);
        this.lexicallyDeclaredNames.push(name);
    }
    addConstName(name: string) {
        super.addName(name, S.VariableDeclarationKind.Const);
        this.lexicallyDeclaredNames.push(name);
    }

    extractBlockScope(): S.AssertedBlockScope {
        return new S.AssertedBlockScope({
            lexicallyDeclaredNames: this.lexicallyDeclaredNames,
            capturedNames: this.capturedNames,
            hasDirectEval: this.hasDirectEval});
    }
}

class VarScope extends BaseScope {
    lexicallyDeclaredNames: Array<string>;
    varDeclaredNames: Array<string>;

    constructor() {
        super();
        this.lexicallyDeclaredNames = new Array();
        this.varDeclaredNames = new Array();
    }

    addLetName(name: string) {
        super.addName(name, S.VariableDeclarationKind.Let);
        this.lexicallyDeclaredNames.push(name);
    }
    addConstName(name: string) {
        super.addName(name, S.VariableDeclarationKind.Const);
        this.lexicallyDeclaredNames.push(name);
    }
    addVarName(name: string) {
        super.addName(name, S.VariableDeclarationKind.Var);
        this.varDeclaredNames.push(name);
    }

    extractVarScope(): S.AssertedVarScope {
        return new S.AssertedVarScope({
            lexicallyDeclaredNames: this.lexicallyDeclaredNames,
            varDeclaredNames: this.varDeclaredNames,
            capturedNames: this.capturedNames,
            hasDirectEval: this.hasDirectEval});
    }
}

class ParameterScope extends BaseScope {
    parameterNames: Array<string>;

    constructor() {
        super();
        this.parameterNames = new Array();
    }

    addParameterName(name: string) {
        super.addName(name, 'parameter');
        this.parameterNames.push(name);
    }

    extractParameterScope(): S.AssertedParameterScope {
        return new S.AssertedParameterScope({
            parameterNames: this.parameterNames,
            capturedNames: this.capturedNames,
            hasDirectEval: this.hasDirectEval});
    }
}

enum ScopeBindMode {
    None = "none",
    Var = "var",
    Let = "let",
    Const = "const",
    Parameter = "parameter"
}

class Context {
    scopeStack: Array<BaseScope>;
    bindStack: Array<ScopeBindMode>;

    constructor() {
        this.scopeStack = new Array();
        this.bindStack = new Array();
    }

    atTopScope(): boolean {
        return this.scopeStack.length === 0;
    }

    enterVarScope<T>(f: (VarScope) => T): T {
        const varScope = new VarScope();
        return this.enterScope<VarScope, T>(varScope, f);
    }
    enterBlockScope<T>(f: (BlockScope) => T): T {
        const blockScope = new BlockScope();
        return this.enterScope<BlockScope, T>(blockScope, f);
    }
    enterParameterScope<T>(f: (ParameterScope) => T): T {
        const paramScope = new ParameterScope();
        return this.enterScope<ParameterScope, T>(paramScope, f);
    }
    private enterScope<S extends BaseScope, T>(scope: S, f: (S) => T): T {
        this.scopeStack.push(scope);
        const result = f(scope);
        this.scopeStack.pop();
        return result;
    }

    bindDeclKind<T>(kind: S.VariableDeclarationKind, f: () => T): T {
        switch (kind) {
          case S.VariableDeclarationKind.Var:
            return this.bindVars(f);
          case S.VariableDeclarationKind.Let:
            return this.bindLets(f);
          case S.VariableDeclarationKind.Const:
            return this.bindConsts(f);
          default:
            throw new Error(`Invalid VariableDeclarationKind ${kind}`);
        }
    }
    bindVars<T>(f: () => T): T {
        return this.bind<T>(ScopeBindMode.Var, f);
    }
    bindLets<T>(f: () => T): T {
        return this.bind<T>(ScopeBindMode.Let, f);
    }
    bindConsts<T>(f: () => T): T {
        return this.bind<T>(ScopeBindMode.Const, f);
    }
    bindParameters<T>(f: () => T): T {
        return this.bind<T>(ScopeBindMode.Parameter, f);
    }
    private bind<T>(mode: ScopeBindMode, f: () => T): T {
        this.bindStack.push(mode);
        const result = f();
        this.bindStack.pop();
        return result;
    }

    noteBoundName(name: S.Identifier) {
        assert(this.bindStack.length > 0);
        const bindMode = this.bindStack[this.bindStack.length - 1];
        switch (bindMode) {
          case ScopeBindMode.Var:       return this.noteBoundVar(name);
          case ScopeBindMode.Let:       return this.noteBoundLet(name);
          case ScopeBindMode.Const:     return this.noteBoundConst(name);
          case ScopeBindMode.Parameter: return this.noteBoundParameter(name);
          default: throw new Error(`Invalid scope bind mode: ${bindMode}`);
        }
    }

    private noteBoundVar(name: S.Identifier) {
        const found = this.eachScope(scope => {
            if (scope instanceof VarScope) {
                (scope as VarScope).addVarName(name as string);
                return true;
            }
        });
        assert(found === true);
    }
    private noteBoundLet(name: S.Identifier) {
        const found = this.eachScope(scope => {
            if (scope instanceof BlockScope) {
                (scope as BlockScope).addLetName(name as string);
                return true;
            }
            if (scope instanceof VarScope) {
                (scope as VarScope).addLetName(name as string);
                return true;
            }
        });
        assert(found === true);
    }
    private noteBoundConst(name: S.Identifier) {
        const found = this.eachScope(scope => {
            if (scope instanceof BlockScope) {
                (scope as BlockScope).addConstName(name as string);
                return true;
            }
            if (scope instanceof VarScope) {
                (scope as VarScope).addConstName(name as string);
                return true;
            }
        });
        assert(found === true);
    }
    private noteBoundParameter(name: S.Identifier) {
        const found = this.eachScope(scope => {
            if (scope instanceof ParameterScope) {
                (scope as ParameterScope).addParameterName(name as string);
                return true;
            }
        });
        assert(found === true);
    }
    private eachScope<T>(f: (BaseScope) => T|undefined): T|undefined {
        const len = this.scopeStack.length;
        for (var i = 0; i < len; i++) {
            const scope = this.scopeStack[len - (i+1)];
            const r = f(scope);
            if (r !== undefined) {
                return r;
            }
        }
        return undefined;
    }

    /* Note the use of a name in the current scope context.
     * Return the scope that binds the name (after marking it
     * at used if necessary), or return null if the reference
     * is free.
     */
    noteUseName(name: string): BaseScope|null {
        let capture: boolean = false;

        const foundScope = this.eachScope((scope: BaseScope) => {
            if (scope.findOrCaptureUse(name, capture)) {
                return scope;
            }

            // If we're crossing a VarScope boundary, which corresponds
            // to function scope, set flag to capture names as we're
            // entering a caller function's scope.
            if (!capture && (scope instanceof VarScope)) {
                capture = true;
            }
        });
        return foundScope || null;
    }
}

export function importScript(data: string): any {
    const json: any = shift.parseScript(data);
    if (json.type !== 'Script') {
        throw new Error('Not a script');
    }
    const importer: Importer = new Importer();
    const script: S.Script = importer.liftScript(json);

    const sr: StringRegistry = importer.strings;
    const strings = sr.stringsInFrequencyOrder();
    let stLength = 0;
    strings.forEach((s, i) => {
        const f = sr.frequencyOf(s);
        console.log(`String [${i}] \`${s}\` - ${f}`);
        stLength += (s.length + 1);
    });
    console.log(`String table length: ${stLength}`);
    return {};
}

class StringRegistry {
    // Table mapping all strings that are used to
    // number of uses.
    stringTable: Map<string, number>;

    constructor() {
        this.stringTable = new Map();
    }

    noteString(s: string) {
        const count = this.stringTable.get(s);
        const next = (count !== undefined) ? (count as number) + 1
                                           : 1;
        this.stringTable.set(s, next);
    }

    // Return an array of all the strings ordered by use.
    stringsInFrequencyOrder(): Array<string> {
        const st = this.stringTable;
        const array: Array<string> = new Array();
        for (let s of st.keys()) {
            array.push(s);
        }
        // Sort, with highest frequencies showing up first.
        array.sort((a: string, b: string) => (st.get(b) - st.get(a)));

        return array;
    }

    frequencyOf(s: string) {
        assert(this.stringTable.has(s));
        return this.stringTable.get(s);
    }
}

class Importer {
    readonly cx: Context;
    readonly strings: StringRegistry;

    constructor() {
        this.cx = new Context();
        this.strings = new StringRegistry();
    }

    //
    // Top level
    //

    liftScript(json: any): S.Script {
        assertNodeType(json, 'Script');
        assert(this.cx.atTopScope());

        const directives = (json.directives as Array<any>).map(
            d => this.liftDirective(d));

        return this.cx.enterVarScope((ss: VarScope) => {
            const statements = (json.statements as Array<any>).map(
                s => this.liftStatement(s));

            const scope = ss.extractVarScope();
            return new S.Script({scope, directives, statements});
        });
    }

    liftDirective(json: any): S.Directive {
        assertNodeType(json, 'Directive');
        assertType(json.rawValue, 'string');

        return new S.Directive({rawValue: json.rawValue as string});
    }

    //
    // Statements
    //

    liftStatement(json: any): S.Statement {
        switch (json.type as string) {
          case 'ExpressionStatement':
            return this.liftExpressionStatement(json);
          case 'VariableDeclarationStatement':
            return this.liftVariableDeclarationStatement(json);
          case 'FunctionDeclaration':
            return this.liftFunctionDeclaration(json);
          case 'IfStatement':
            return this.liftIfStatement(json);
          case 'WhileStatement':
            return this.liftWhileStatement(json);
          case 'DoWhileStatement':
            return this.liftDoWhileStatement(json);
          case 'BlockStatement':
            return this.liftBlockStatement(json);
          case 'ReturnStatement':
            return this.liftReturnStatement(json);
          case 'ForInStatement':
            return this.liftForInStatement(json);
          case 'ForStatement':
            return this.liftForStatement(json);
          case 'BreakStatement':
            return this.liftBreakStatement(json);
          case 'ContinueStatement':
            return this.liftContinueStatement(json);
          case 'TryCatchStatement':
            return this.liftTryCatchStatement(json);
          case 'TryFinallyStatement':
            return this.liftTryFinallyStatement(json);
          case 'ThrowStatement':
            return this.liftThrowStatement(json);
          case 'SwitchStatement':
            return this.liftSwitchStatement(json);
          case 'SwitchStatementWithDefault':
            return this.liftSwitchStatementWithDefault(json);
          case 'LabeledStatement': /* WHAT? */
            return this.liftLabeledStatement(json);
          case 'EmptyStatement':
            return this.liftEmptyStatement(json);
          default:
            throw new MatchError('Statement', json.type);
        }
    }
    liftExpressionStatement(json: any): S.ExpressionStatement {
        assertNodeType(json, 'ExpressionStatement');

        const expression = this.liftExpression(json.expression);
        return new S.ExpressionStatement({expression});
    }
    liftVariableDeclarationStatement(json: any): S.VariableDeclaration {
        assertNodeType(json, 'VariableDeclarationStatement');
        return this.liftVariableDeclaration(json.declaration);
    }
    liftVariableDeclaration(json: any): S.VariableDeclaration {
        assertNodeType(json, 'VariableDeclaration');

        const kind = this.liftVariableDeclarationKind(json.kind as string);
        const declarators = json.declarators.map(d => {
            return this.liftVariableDeclarator(d)
        });

        return new S.VariableDeclaration({kind, declarators});
    }
    liftVariableDeclarationKind(kind: string): S.VariableDeclarationKind {
        switch (kind) {
          case 'var': return S.VariableDeclarationKind.Var;
          case 'let': return S.VariableDeclarationKind.Let;
          case 'const': return S.VariableDeclarationKind.Const;
          default: throw new MatchError('VariableDeclarationKind', kind);
        }
    }
    liftVariableDeclarator(json: any): S.VariableDeclarator {
        assertNodeType(json, 'VariableDeclarator');

        // Lift the expression before the binding because
        // the expression does not capture the bound variable name.
        const init = (('init' in json) && (json.init !== null))
                            ? this.liftExpression(json.init)
                            : null;
        const binding = this.cx.bindVars(() => {
            return this.liftBinding(json.binding);
        });
        return new S.VariableDeclarator({binding, init});
    }
    liftBinding(json: any): S.Binding {
        const binding = this.tryLiftBinding(json);
        if (binding === null) {
            throw new MatchError('Binding', json.type);
        }
        return binding;
    }
    tryLiftBinding(json: any): S.Binding|null {
        switch (json.type) {
          case 'BindingIdentifier':
            return this.liftBindingIdentifier(json);
          case 'BindingPattern':
            throw new Error('BindingPattern not handled yet.');
          default:
            return null;
        }
    }
    liftBindingIdentifier(json: any): S.BindingIdentifier {
        assertNodeType(json, 'BindingIdentifier');
        assertType(json.name, 'string');

        const name = json.name as S.Identifier;
        this.cx.noteBoundName(name);
        this.strings.noteString(name);
        return new S.BindingIdentifier({name});
    }

    liftFunctionDeclaration(json: any): S.FunctionDeclaration {
        assertNodeType(json, 'FunctionDeclaration');
        assertType(json.isGenerator, 'boolean');

        const isAsync = false;
        const isGenerator = json.isGenerator as boolean;
        return this.cx.enterParameterScope(ps => {
            const name = this.cx.bindParameters(() => {
                return this.liftBindingIdentifier(json.name);
            });
            const params = this.liftFormalParameters(json.params);
            const parameterScope = ps.extractParameterScope();

            return this.cx.enterVarScope(bs => {
                const body = this.liftFunctionBody(json.body);
                const bodyScope = bs.extractVarScope();

                // TODO: Emit an SkippableFunctionDeclaration when appropriate.
                return new S.EagerFunctionDeclaration({
                    isAsync, isGenerator, name,
                    parameterScope, params,
                    bodyScope, body
                });
            });
        });
    }

    liftFormalParameters(json: any): S.FormalParameters {
        assertNodeType(json, 'FormalParameters');
        return this.cx.bindParameters(() => {
            const items = json.items.map(i => this.liftParameter(i))
            const rest: (S.Binding | null) =
                json.rest !== null ?
                    this.liftBinding(json.rest)
                  : null;
            return new S.FormalParameters({items, rest});
        });
    }
    liftParameter(json: any): S.Parameter {
        // Try to lift a binding
        let binding = this.tryLiftBinding(json);
        if (binding !== null) {
            return binding;
        }

        // TODO: handle other parameter options (BindingWithInitializer)
        throw new MatchError('Parameter', json.type);
    }

    liftFunctionBody(json: any): S.FunctionBody {
        assertNodeType(json, 'FunctionBody');

        const directives = json.directives.map(d => this.liftDirective(d));
        const statements = json.statements.map(s => this.liftStatement(s));
        return new S.FunctionBody({directives, statements});
    }

    liftIfStatement(json: any): S.IfStatement {
        assertNodeType(json, 'IfStatement');

        const test = this.liftExpression(json.test);
        const consequent = this.liftStatement(json.consequent);
        const alternate =
            json.alternate !== null ?
                this.liftStatement(json.alternate)
              : null;

        return new S.IfStatement({test, consequent, alternate});
    }
    liftWhileStatement(json: any): S.WhileStatement {
        assertNodeType(json, 'WhileStatement');

        const test = this.liftExpression(json.test);
        const body = this.liftStatement(json.body);

        return new S.WhileStatement({test, body});
    }
    liftDoWhileStatement(json: any): S.DoWhileStatement {
        assertNodeType(json, 'DoWhileStatement');

        const test = this.liftExpression(json.test);
        const body = this.liftStatement(json.body);

        return new S.DoWhileStatement({test, body});
    }

    liftBlockStatement(json: any): S.Block {
        assertNodeType(json, 'BlockStatement');
        return this.liftBlock(json.block);
    }
    liftBlock(json: any): S.Block {
        assertNodeType(json, 'Block');

        return this.cx.enterBlockScope(s => {
            const statements = json.statements.map(s => this.liftStatement(s));
            const scope = s.extractBlockScope();
            return new S.Block({scope, statements});
        });
    }
    liftReturnStatement(json: any): S.ReturnStatement {
        assertNodeType(json, 'ReturnStatement');

        const expression =
            json.expression !== null ?
                this.liftExpression(json.expression)
              : null;

        return new S.ReturnStatement({expression});
    }
    liftForInStatement(json: any): S.ForInStatement {
        assertNodeType(json, 'ForInStatement');

        return this.cx.enterBlockScope((vs: VarScope) => {
            // Lift the expression before the binding so
            // the expression gets scoped before the variable
            // gets bound.
            const right = this.liftExpression(json.right);
            const left = this.liftForInStatementLeft(json.left);
            const body = this.liftStatement(json.body);

            return new S.ForInStatement({left, right, body});
        });
    }
    liftForInStatementLeft(json: any)
      : (S.ForInOfBinding | S.AssignmentTarget)
    {
        const result = this.tryLiftAssignmentTarget(json);
        if (result !== null) {
            return result;
        }

        if (json.type == 'VariableDeclaration') {
            const kind = this.liftVariableDeclarationKind(json.kind as string);
            if (json.declarators.length != 1) {
                throw new Error('Invalid ForIn with multiple declarations:' +
                                ` ${json.declarators.length}.`);
            }
            const decl = json.declarators[0];
            if (decl.type !== 'VariableDeclarator') {
                throw new Error('Expected VariableDeclarator in ForIn,' +
                                ` but got: ${decl.type}.`);
            }

            const binding = this.cx.bindDeclKind(kind, () => {
                return this.liftBinding(decl.binding);
            });
            return new S.ForInOfBinding({kind, binding});
        }

        throw new MatchError('ForInStatementLeft', json.type);
    }

    liftForStatement(json: any): S.ForStatement {
        assertNodeType(json, 'ForStatement');

        return this.cx.enterBlockScope(bs => {
            const init = this.liftForStatementInit(json.init);
            const test = json.test !== null ? this.liftExpression(json.test)
                                            : null;
            const update = json.update !== null ? this.liftExpression(json.update)
                                            : null;
            const body = this.liftStatement(json.body);

            return new S.ForStatement({init, test, update, body});
        });
    }
    liftForStatementInit(json: any)
      : (S.VariableDeclaration | S.Expression | null)
    {
        if (json === null) {
            return null;
        }

        if (json.type === 'VariableDeclaration') {
            return this.liftVariableDeclaration(json);
        }

        const expr = this.tryLiftExpression(json);
        if (expr !== null) {
            return expr;
        }

        throw new MatchError('ForStatementInit', json.type);
    }
    liftBreakStatement(json: any): S.BreakStatement {
        assertNodeType(json, 'BreakStatement');
        assertType(json.label, 'string', /* nullable = */ true);

        const label = json.label as (S.Label|null);
        if (label !== null) {
            // Note the label string.
            this.strings.noteString(label);
        }

        return new S.BreakStatement({label});
    }
    liftContinueStatement(json: any): S.ContinueStatement {
        assertNodeType(json, 'ContinueStatement');
        assertType(json.label, 'string', /* nullable = */ true);

        const label = json.label as (S.Label|null);
        if (label !== null) {
            // Note the label string.
            this.strings.noteString(label);
        }
        return new S.ContinueStatement({label});
    }
    liftTryCatchStatement(json: any): S.TryCatchStatement {
        assertNodeType(json, 'TryCatchStatement');

        const body = this.liftBlock(json.body);
        const catchClause = this.liftCatchClause(json.catchClause);

        return new S.TryCatchStatement({body, catchClause});
    }
    liftCatchClause(json: any): S.CatchClause {
        assertNodeType(json, 'CatchClause');

        return this.cx.enterParameterScope(bs => {
            const binding = this.cx.bindParameters(() => {
                return this.liftBindingIdentifier(json.binding);
            });
            const body = this.liftBlock(json.body);
            const bindingScope = bs.extractParameterScope();
            return new S.CatchClause({bindingScope, binding, body});
        });
    }

    liftTryFinallyStatement(json: any): S.TryFinallyStatement {
        assertNodeType(json, 'TryFinallyStatement');
        return summarizeNode(json);
    }
    liftThrowStatement(json: any): S.ThrowStatement {
        assertNodeType(json, 'ThrowStatement');

        const expression = this.liftExpression(json.expression);

        return new S.ThrowStatement({expression});
    }
    liftSwitchStatement(json: any): S.SwitchStatement {
        assertNodeType(json, 'SwitchStatement');

        const discriminant = this.liftExpression(json.discriminant);
        const cases = json.cases.map(c => this.liftSwitchCase(c));
        return new S.SwitchStatement({discriminant, cases});
    }
    liftSwitchStatementWithDefault(json: any): S.SwitchStatementWithDefault {
        assertNodeType(json, 'SwitchStatementWithDefault');

        const discriminant = this.liftExpression(json.discriminant);
        const preDefaultCases = json.preDefaultCases.map(c => {
            return this.liftSwitchCase(c);
        });
        const defaultCase = this.liftSwitchDefault(json.defaultCase);
        const postDefaultCases = json.preDefaultCases.map(c => {
            return this.liftSwitchCase(c);
        });

        return new S.SwitchStatementWithDefault({
            discriminant, preDefaultCases, defaultCase, postDefaultCases
        });
    }
    liftSwitchCase(json: any): S.SwitchCase {
        assertNodeType(json, 'SwitchCase');

        const test = this.liftExpression(json.test);
        const consequent = json.consequent.map(c => this.liftStatement(c));

        return new S.SwitchCase({test, consequent});
    }
    liftSwitchDefault(json: any): S.SwitchDefault {
        assertNodeType(json, 'SwitchDefault');

        const consequent = json.consequent.map(c => this.liftStatement(c));

        return new S.SwitchDefault({consequent});
    }

    liftLabeledStatement(json: any): S.LabelledStatement {
        assertNodeType(json, 'LabeledStatement');
        return summarizeNode(json);
    }
    liftEmptyStatement(json: any): S.EmptyStatement {
        assertNodeType(json, 'EmptyStatement');
        return summarizeNode(json);
    }

    liftExpression(json: any): S.Expression {
        const expr = this.tryLiftExpression(json);
        if (expr !== null) {
            return expr;
        }
        throw new MatchError('Expression', json.type);
    }
    liftExpressionOrSuper(json: any): S.Expression {
        const expr = this.tryLiftExpression(json);
        if (expr !== null) {
            return expr;
        }
        // TODO: Handle 'Super'.
        throw new MatchError('ExpressionOrSuper', json.type);
    }
    tryLiftExpression(json: any): S.Expression|null {
        switch (json.type as string) {
          case 'CallExpression':
            return this.liftCallExpression(json);
          case 'StaticMemberExpression':
            return this.liftStaticMemberExpression(json);
          case 'IdentifierExpression':
            return this.liftIdentifierExpression(json);
          case 'LiteralStringExpression':
            return this.liftLiteralStringExpression(json);
          case 'LiteralBooleanExpression':
            return this.liftLiteralBooleanExpression(json);
          case 'ObjectExpression':
            return this.liftObjectExpression(json);
          case 'ArrayExpression':
            return this.liftArrayExpression(json);
          case 'FunctionExpression':
            return this.liftFunctionExpression(json);
          case 'AssignmentExpression':
            return this.liftAssignmentExpression(json);
          case 'LiteralNullExpression':
            return this.liftLiteralNullExpression(json);
          case 'UnaryExpression':
            return this.liftUnaryExpression(json);
          case 'BinaryExpression':
            return this.liftBinaryExpression(json);
          case 'ComputedMemberExpression':
            return this.liftComputedMemberExpression(json);
          case 'LiteralNumericExpression':
            return this.liftLiteralNumericExpression(json);
          case 'LiteralRegExpExpression':
            return this.liftLiteralRegExpExpression(json);
          case 'CompoundAssignmentExpression':
            return this.liftCompoundAssignmentExpression(json);
          case 'UpdateExpression':
            return this.liftUpdateExpression(json);
          case 'NewExpression':
            return this.liftNewExpression(json);
          case 'ThisExpression':
            return this.liftThisExpression(json);
          case 'ConditionalExpression':
            return this.liftConditionalExpression(json);
          default:
            return null;
        }
    }
    liftCallExpression(json: any): S.CallExpression {
        assertNodeType(json, 'CallExpression');

        // TODO: Check for |super| in callee.
        const callee = this.liftExpression(json.callee);
        const arguments_ = (json.arguments as Array<any>).map(
            s => this.liftExpression(s));
        return new S.CallExpression({callee, arguments_});
    }
    liftStaticMemberExpression(json: any): S.StaticMemberExpression {
        assertNodeType(json, 'StaticMemberExpression');
        assertType(json.property, 'string');

        // TODO: Check for |super| in object_.
        const object_ = this.liftExpression(json.object);
        const property = json.property;
        return new S.StaticMemberExpression({object_, property});
    }
    liftIdentifierExpression(json: any): S.IdentifierExpression {
        assertNodeType(json, 'IdentifierExpression');
        assertType(json.name, 'string');

        const name = json.name as S.Identifier;

        // Note the use of the identifier.
        this.cx.noteUseName(name);
        this.strings.noteString(name);

        return new S.IdentifierExpression({name});
    }
    liftLiteralStringExpression(json: any): S.LiteralStringExpression {
        assertNodeType(json, 'LiteralStringExpression');
        assertType(json.value, 'string');

        const value = json.value as string;

        return new S.LiteralStringExpression({value});
    }
    liftLiteralBooleanExpression(json: any): S.LiteralBooleanExpression {
        assertNodeType(json, 'LiteralBooleanExpression');
        assertType(json.value, 'boolean');

        const value = json.value as boolean;

        return new S.LiteralBooleanExpression({value});
    }
    liftObjectExpression(json: any): S.ObjectExpression {
        assertNodeType(json, 'ObjectExpression');

        const properties = (json.properties as Array<any>).map(p => {
            return this.liftObjectProperty(p);
        });

        return new S.ObjectExpression({properties});
    }

    liftObjectProperty(json: any): S.ObjectProperty {
        switch (json.type as string) {
          case 'DataProperty':
            return this.liftDataProperty(json);
          case 'MethodDefinition':
          case 'ShorthandProperty':
          default:
            throw new MatchError('ObjectProperty', json.type);
        }
    }

    liftDataProperty(json: any): S.DataProperty {
        assertNodeType(json, 'DataProperty');

        const name = this.liftPropertyName(json.name);
        const expression = this.liftExpression(json.expression);

        return new S.DataProperty({name, expression});
    }

    liftArrayExpression(json: any): S.ArrayExpression {
        assertNodeType(json, 'ArrayExpression');

        const elements = json.elements.map(e => this.liftArrayElement(e));

        return new S.ArrayExpression({elements});
    }
    liftArrayElement(json: any): S.ArrayElement {
        if (json === null) {
            return null;
        }

        const expr = this.tryLiftExpression(json);
        if (expr !== null) {
            return expr;
        }

        if (json.type === 'SpreadElement') {
            return summarizeNode(json);
        }

        throw new MatchError('ArrayElement', json.type);
    }
    liftFunctionExpression(json: any): S.FunctionExpression {
        assertNodeType(json, 'FunctionExpression');
        assertType(json.isGenerator, 'boolean');

        const isAsync = false;
        const isGenerator = json.isGenerator as boolean;

        return this.cx.enterParameterScope(ps => {
            const name = this.cx.bindParameters(() => {
                return json.name !== null ?
                    this.liftBindingIdentifier(json.name)
                  : null;
            });
            const params = this.liftFormalParameters(json.params);
            return this.cx.enterVarScope(bs => {
                const body = this.liftFunctionBody(json.body);
                const parameterScope = ps.extractParameterScope();
                const bodyScope = bs.extractVarScope();

                // TODO: Emit a SkippableFunctionExpression when appropriate.
                return new S.EagerFunctionExpression({
                    isAsync, isGenerator, name,
                    parameterScope, params,
                    bodyScope, body
                });
            });
        });
    }
    liftAssignmentExpression(json: any): S.AssignmentExpression {
        assertNodeType(json, 'AssignmentExpression');

        const binding = this.liftAssignmentTarget(json.binding);
        const expression = this.liftExpression(json.expression);

        return new S.AssignmentExpression({binding, expression});
    }
    liftAssignmentTarget(json: any): S.AssignmentTarget {
        const result = this.tryLiftAssignmentTarget(json);
        if (result !== null) {
            return result;
        }
        throw new MatchError('AssignmentTarget', json.type);
    }
    tryLiftAssignmentTarget(json: any): S.AssignmentTarget | null {
        const simple = this.tryLiftSimpleAssignmentTarget(json);
        if (simple !== null) {
            return simple;
        }
        return null;
    }
    liftSimpleAssignmentTarget(json: any): S.SimpleAssignmentTarget {
        const target = this.tryLiftSimpleAssignmentTarget(json);
        if (target !== null) {
            return target;
        }
        throw new MatchError('SimpleAssignmentTarget', json.type);
    }
    tryLiftSimpleAssignmentTarget(json: any): S.SimpleAssignmentTarget|null {
        switch (json.type as string) {
          case 'AssignmentTargetIdentifier':
            return this.liftAssignmentTargetIdentifier(json);
          case 'StaticMemberAssignmentTarget':
            return this.liftStaticMemberAssignmentTarget(json);
          case 'ComputedMemberAssignmentTarget':
            return this.liftComputedMemberAssignmentTarget(json);
          default:
            return null;
        }
    }
    liftAssignmentTargetIdentifier(json: any): S.AssignmentTargetIdentifier {
        assertNodeType(json, 'AssignmentTargetIdentifier');
        assertType(json.name, 'string');

        const name = json.name as S.Identifier;

        // Note the use of the identifier.
        this.cx.noteUseName(name);
        this.strings.noteString(name);

        return new S.AssignmentTargetIdentifier({name});
    }
    liftStaticMemberAssignmentTarget(json: any)
      : S.StaticMemberAssignmentTarget
    {
        assertNodeType(json, 'StaticMemberAssignmentTarget');
        assertType(json.property, 'string');

        const object_ = this.liftExpressionOrSuper(json.object);
        const property = json.property as S.IdentifierName;

        // Note the string.
        this.strings.noteString(property);

        return new S.StaticMemberAssignmentTarget({object_, property});
    }

    liftComputedMemberAssignmentTarget(json: any)
      : S.ComputedMemberAssignmentTarget
    {
        assertNodeType(json, 'ComputedMemberAssignmentTarget');

        const object_ = this.liftExpressionOrSuper(json.object);
        const expression = this.liftExpression(json.expression);

        return new S.ComputedMemberAssignmentTarget({object_, expression});
    }

    liftLiteralNullExpression(json: any): S.LiteralNullExpression {
        assertNodeType(json, 'LiteralNullExpression');
        return new S.LiteralNullExpression();
    }
    liftUnaryExpression(json: any): S.UnaryExpression {
        assertNodeType(json, 'UnaryExpression');
        assertType(json.operator, 'string');

        const operator = json.operator as S.UnaryOperator;
        const operand = this.liftExpression(json.operand);

        return new S.UnaryExpression({operator, operand});
    }
    liftBinaryExpression(json: any): S.BinaryExpression {
        assertNodeType(json, 'BinaryExpression');
        assertType(json.operator, 'string');

        const operator = json.operator as S.BinaryOperator;
        const left = this.liftExpression(json.left);
        const right = this.liftExpression(json.right);

        return new S.BinaryExpression({operator, left, right});
    }
    liftComputedMemberExpression(json: any): S.ComputedMemberExpression {
        assertNodeType(json, 'ComputedMemberExpression');

        const object_ = this.liftExpressionOrSuper(json.object);
        const expression = this.liftExpression(json.expression);

        return new S.ComputedMemberExpression({object_, expression});
    }
    liftLiteralNumericExpression(json: any): S.LiteralNumericExpression {
        assertNodeType(json, 'LiteralNumericExpression');
        assertType(json.value, 'number');

        const value = json.value as number;
        return new S.LiteralNumericExpression({value});
    }
    liftLiteralRegExpExpression(json: any): S.LiteralRegExpExpression {
        assertNodeType(json, 'LiteralRegExpExpression');
        assertType(json.pattern, 'string');
        assertType(json.global, 'boolean');
        assertType(json.ignoreCase, 'boolean');
        assertType(json.multiLine, 'boolean');
        assertType(json.unicode, 'boolean');
        assertType(json.sticky, 'boolean');

        const pattern = json.pattern as string;

        const flagArray: Array<string> = [];
        if (json.global) { flagArray.push('g'); }
        if (json.ignoreCase) { flagArray.push('i'); }
        if (json.multiLine) { flagArray.push('m'); }
        if (json.unicode) { flagArray.push('u'); }
        if (json.sticky) { flagArray.push('y'); }
        const flags = flagArray.join();

        return new S.LiteralRegExpExpression({pattern, flags});
    }
    liftCompoundAssignmentExpression(json: any)
      : S.CompoundAssignmentExpression
    {
        assertNodeType(json, 'CompoundAssignmentExpression');
        assertType(json.operator, 'string');

        const operator = json.operator as S.CompoundAssignmentOperator;
        const binding = this.liftSimpleAssignmentTarget(json.binding);
        const expression = this.liftExpression(json.expression);

        return new S.CompoundAssignmentExpression({
            operator, binding, expression
        });
    }
    liftUpdateExpression(json: any): S.UpdateExpression {
        assertNodeType(json, 'UpdateExpression');
        assertType(json.isPrefix, 'boolean');
        assertType(json.operator, 'string');

        const isPrefix = json.isPrefix as boolean;
        const operator = json.operator as S.UpdateOperator;
        const operand = this.liftSimpleAssignmentTarget(json.operand);

        return new S.UpdateExpression({isPrefix, operator, operand});
    }
    liftNewExpression(json: any): S.NewExpression {
        assertNodeType(json, 'NewExpression');

        const callee = this.liftExpression(json.callee);
        const arguments_ = json.arguments.map(s => this.liftExpression(s));

        return new S.NewExpression({callee, arguments_});
    }
    liftThisExpression(json: any): S.ThisExpression {
        assertNodeType(json, 'ThisExpression');
        return new S.ThisExpression();
    }
    liftConditionalExpression(json: any): S.ConditionalExpression {
        assertNodeType(json, 'ConditionalExpression');

        const test = this.liftExpression(json.test);
        const consequent = this.liftExpression(json.consequent);
        const alternate = this.liftExpression(json.alternate);

        return new S.ConditionalExpression({test, consequent, alternate});
    }

    liftPropertyName(json: any): S.PropertyName {
        switch (json.type as string) {
          case 'StaticPropertyName':
            return this.liftStaticPropertyName(json);
          default:
            throw new MatchError('PropertyName', json.type);
        }
    }

    liftStaticPropertyName(json: any): S.LiteralPropertyName {
        assertNodeType(json, 'StaticPropertyName');
        assertType(json.value, 'string');

        const value = json.value as string;

        return new S.LiteralPropertyName({value});
    }
}
