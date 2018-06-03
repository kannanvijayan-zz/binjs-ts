
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


class Context {
    curScope: Scope;

    constructor() {
        this.curScope = null;
    }

    atTopScope(): boolean {
        return this.curScope === null;
    }

    pushScope(): Scope {
        this.curScope = new Scope(this.curScope);
        return this.curScope;
    }

    popScope(scope: Scope) {
        assert(this.curScope !== null);
        assert(this.curScope === scope);
        this.curScope = this.curScope.parent;
    }
}

type BareVarScopeObject = {
    lexicallyDeclaredNames: Array<string>,
    varDeclaredNames: Array<string>,
    capturedNames: Array<string>,
    hasDirectEval: boolean
};

type ScopeNameLocation = (S.VariableDeclarationKind | 'parameter');

class Scope {
    parent: Scope | null;
    lexicallyDeclaredNames: Array<string>;
    varDeclaredNames: Array<string>;
    parameterNames: Array<string>;
    capturedNames: Array<string>;
    hasDirectEval: boolean;

    nameMap: Map<string, ScopeNameLocation>;
    captureSet: Set<string>;

    constructor(parent: Scope|null) {
        this.parent = parent;
        this.lexicallyDeclaredNames = new Array();
        this.varDeclaredNames = new Array();
        this.capturedNames = new Array();
        this.parameterNames = new Array();
        this.hasDirectEval = false;

        this.nameMap = new Map();
        this.captureSet = new Set();
    }

    extractVarScope(): S.AssertedVarScope {
        assert(this.parameterNames.length == 0);

        return new S.AssertedVarScope({
            lexicallyDeclaredNames: this.lexicallyDeclaredNames,
            varDeclaredNames: this.varDeclaredNames,
            capturedNames: this.capturedNames,
            hasDirectEval: this.hasDirectEval});
    }
    extractBlockScope(): S.AssertedBlockScope {
        assert(this.parameterNames.length == 0);
        assert(this.varDeclaredNames.length == 0);

        return new S.AssertedBlockScope({
            lexicallyDeclaredNames: this.lexicallyDeclaredNames,
            capturedNames: this.capturedNames,
            hasDirectEval: this.hasDirectEval});
    }
    extractParameterScope(): S.AssertedParameterScope {
        assert(this.varDeclaredNames.length == 0);
        assert(this.lexicallyDeclaredNames.length == 0);

        return new S.AssertedParameterScope({
            parameterNames: this.parameterNames,
            capturedNames: this.capturedNames,
            hasDirectEval: this.hasDirectEval});
    }

    addName(name: string, location: ScopeNameLocation) {
        assert(!this.nameMap.has(name));
        assert(!this.captureSet.has(name));
        this.nameMap.set(name, location);
        switch (location) {
          case 'parameter':
            this.parameterNames.push(name);
            break;
          case S.VariableDeclarationKind.Var:
            this.varDeclaredNames.push(name);
            break;
          case S.VariableDeclarationKind.Let:
          case S.VariableDeclarationKind.Const:
            this.lexicallyDeclaredNames.push(name);
            break;
          default:
            throw new MatchError('NameLocation', location);
        }
    }

    useName(name: string) {
        if (this.nameMap.has(name)) {
            return 0;
        }
        if (this.parent == null) {
            return -1;
        }
        return this.parent.useNameInChildScope(name, 1);
    }
    private useNameInChildScope(name: string, depth: number) {
        if (this.nameMap.has(name)) {
            if (!this.captureSet.has(name)) {
                this.captureSet.add(name);
                this.capturedNames.push(name);
            }
            return depth;
        }
        if (this.parent == null) {
            return -1;
        }
        return this.parent.useNameInChildScope(name, depth + 1);
    }
}

export function importScript(data: string): any {
    const json: any = shift.parseScript(data);
    if (json.type !== 'Script') {
        throw new Error('Not a script');
    }
    const importer = new Importer();
    return importer.liftScript(json);
}

class Importer {
    readonly cx: Context;

    constructor() {
        this.cx = new Context();
    }

    //
    // Top level
    //

    liftScript(json: any): S.Script {
        assertNodeType(json, 'Script');
        assert(this.cx.atTopScope());

        const ss = this.cx.pushScope();

        const directives = (json.directives as Array<any>).map(
            d => this.liftDirective(d));
        const statements = (json.statements as Array<any>).map(
            s => this.liftStatement(s));
        
        const scope = ss.extractVarScope();
        this.cx.popScope(ss);

        return new S.Script({scope, directives, statements});
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
          case 'BlockStatement':
            return this.liftBlockStatement(json);
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

        const binding = this.liftBinding(json.binding);
        const init = (('init' in json) && (json.init !== null))
                            ? this.liftExpression(json.init)
                            : null;
        return new S.VariableDeclarator({binding, init});
    }
    tryLiftBinding(json: any): S.Binding|null {
        switch (json.type) {
          case 'BindingIdentifier':
            return this.liftBindingIdentifier(json);
          case 'BindingPattern':
            throw new MatchError('Binding', json.type);
          default:
            return null;
        }
    }
    liftBinding(json: any): S.Binding {
        const binding = this.tryLiftBinding(json);
        if (binding === null) {
            throw new MatchError('Binding', json.type);
        }
        return binding;
    }
    liftBindingIdentifier(json: any): S.BindingIdentifier {
        assertNodeType(json, 'BindingIdentifier');
        assertType(json.name, 'string');

        const name = json.name as S.Identifier;
        return new S.BindingIdentifier({name});
    }

    liftFunctionDeclaration(json: any): S.FunctionDeclaration {
        assertNodeType(json, 'FunctionDeclaration');
        assertType(json.isGenerator, 'boolean');

        const isAsync = false;
        const isGenerator = json.isGenerator as boolean;
        const name = this.liftBindingIdentifier(json.name);

        const ps = this.cx.pushScope();
        const params = this.liftFormalParameters(json.params);

        const bs = this.cx.pushScope();
        const body = this.liftFunctionBody(json.body);

        this.cx.popScope(bs);
        this.cx.popScope(ps);

        const parameterScope = ps.extractParameterScope();
        const bodyScope = bs.extractVarScope();

        // TODO: Emit an LazyFunctionDeclaration when appropriate.
        return new S.EagerFunctionDeclaration({
            isAsync, isGenerator, name,
            parameterScope, params,
            bodyScope, body
        });
    }

    liftFormalParameters(json: any): S.FormalParameters {
        assertNodeType(json, 'FormalParameters');
        const items = json.items.map(i => this.liftParameter(i))
        const rest: (S.Binding | null) =
            json.rest !== null ?
                this.liftBinding(json.rest)
              : null;
        return new S.FormalParameters({items, rest});
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
        const statements = json.directives.map(s => this.liftStatement(s));
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

    liftBlockStatement(json: any): S.Block {
        assertNodeType(json, 'BlockStatement');
        return this.liftBlock(json.block);
    }
    liftBlock(json: any): S.Block {
        assertNodeType(json, 'Block');
        const s = this.cx.pushScope();
        const statements = json.statements.map(s => this.liftStatement(s));
        this.cx.popScope(s);
        const scope = s.extractBlockScope();

        return new S.Block({scope, statements});
    }

    liftExpression(json: any): S.Expression {
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
          case 'LiteralNullExpression':
            return this.liftLiteralNullExpression(json);
          default:
            throw new MatchError('Expression', json.type);
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
        return summarizeNode(json);
    }
    liftFunctionExpression(json: any): S.FunctionExpression {
        assertNodeType(json, 'FunctionExpression');
        return summarizeNode(json);
    }
    liftLiteralNullExpression(json: any): S.LiteralNullExpression {
        assertNodeType(json, 'LiteralNullExpression');
        return summarizeNode(json);
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
