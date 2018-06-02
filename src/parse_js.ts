
import * as assert from 'assert';
import * as shift from 'shift-parser';

import * as S from './schema';

function assertNodeType(node: any, typeStr: string) {
    assert.equal(node.type, typeStr, `Node type ${node.type} != ${typeStr}`);
}
function assertType(val: any, typeStr: string, nullable: boolean = false) {
    const chkStr: string = nullable ? `${typeStr}???` : typeStr;
    if (nullable && val === null) { return; }
    assert.equal(typeof(val), typeStr, `Type ${typeof val} != ${chkStr}`);
}
function assertArray(val: any) {
    return Array.isArray(val);
}
function propNames(obj: any) {
    return Object.getOwnPropertyNames(obj);
}
function summarizeNode(obj: any): any {
    const result = {};
    for (let name in obj) {
        const n = (name === 'type') ? '[TYPE]' : name;
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
        this.curScope = new Scope(null);
    }
}

enum VarScopeKind { Dynamic, Lexical };

type BareScopeObject = {
    lexicallyDeclaredNames: Array<string>,
    varDeclaredNames: Array<string>,
    capturedNames: Array<string>,
    hasDirectEval: boolean
};

class Scope {
    parent: Scope | null;
    lexicallyDeclaredNames: Array<string>;
    varDeclaredNames: Array<string>;
    capturedNames: Array<string>;
    hasDirectEval: boolean;

    nameMap: Map<string, VarScopeKind>;
    captureSet: Set<string>;

    constructor(parent: Scope|null) {
        this.parent = null;
        this.lexicallyDeclaredNames = new Array();
        this.varDeclaredNames = new Array();
        this.capturedNames = new Array();
        this.hasDirectEval = false;

        this.nameMap = new Map();
        this.captureSet = new Set();
    }

    extractVarScope(): BareScopeObject {
        return {lexicallyDeclaredNames: this.lexicallyDeclaredNames,
                varDeclaredNames: this.varDeclaredNames,
                capturedNames: this.capturedNames,
                hasDirectEval: this.hasDirectEval};
    }

    addName(name: string, kind: VarScopeKind) {
        assert(!this.nameMap.has(name));
        assert(!this.captureSet.has(name));
        this.nameMap.set(name, kind);
        if (kind === VarScopeKind.Dynamic) {
            this.varDeclaredNames.push(name);
        } else {
            this.lexicallyDeclaredNames.push(name);
        }
    }

    checkUse(name: string) {
        if (this.nameMap.has(name)) {
            return 0;
        }
        if (this.parent == null) {
            return -1;
        }
        return this.parent.checkUseInChildScope(name, 1);
    }
    private checkUseInChildScope(name: string, depth: number) {
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
        return this.parent.checkUseInChildScope(name, depth + 1);
    }
}

export function importScript(data: string): any {
    const json: any = shift.parseScript(data);
    if (json.type !== 'Script') {
        throw new Error("Not a script");
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

        const directives = (json.directives as Array<any>).map(
            d => this.liftDirective(d));
        const statements = (json.statements as Array<any>).map(
            s => this.liftStatement(s));
        
        const scope = new S.AssertedVarScope(
                            this.cx.curScope.extractVarScope());

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
          case "ExpressionStatement":
            return this.liftExpressionStatement(json);
          default:
            return `EXPRESSION: ${nodeShortSummary(json)}` as any;
            // throw new Error(`Unhandled statement type: ${json.type}.`);
        }
    }
    liftExpressionStatement(json: any): S.ExpressionStatement {
        assertNodeType(json, 'ExpressionStatement');

        const expression = this.liftExpression(json.expression);
        return new S.ExpressionStatement({expression});
    }

    liftExpression(json: any): S.Expression {
        switch (json.type as string) {
          case "CallExpression":
            return this.liftCallExpression(json);
          case "StaticMemberExpression":
            return this.liftStaticMemberExpression(json);
          case "IdentifierExpression":
            return this.liftIdentifierExpression(json);
          default:
            return `EXPRESSION: ${json.type}(${propNames(json)})` as any;
            // throw new Error(`Unhandled statement type: ${json.type}.`);
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

        const name = json.name as string;

        return new S.IdentifierExpression({name});
    }
}
