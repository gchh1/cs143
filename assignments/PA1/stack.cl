(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)


class StackNode {
    value : String;           
    next  : StackNode;        

    init(v : String, n : StackNode) : StackNode {
        {
            value <- v;
            next  <- n;
            self;
        }
    };

    getValue() : String{ value };
    getNext()  : StackNode { next };
    setNext(n : StackNode) : Object { { next <- n; self; } };
};

class Stack inherits IO {
    -- Attributes
    top : StackNode;

    -- Methods
    pop() : String {
        let top_value : String <- if is_empty() then "" else top.getValue() fi in {
            if not is_empty() then 
                top <- top.getNext()
            else self fi;
            top_value;
        }
    };
    push(item : String) : Object { 
        {
            top <- (new StackNode).init(item, top);
            self;
        }
    };
    is_empty() : Bool { 
        isvoid top
    };
    print() : Object { 
        let cur : StackNode <- top in 
            while not isvoid cur loop {
                out_string(cur.getValue());
                out_string("\n");
                cur <- cur.getNext();
            } pool
        
    };

};

class Command {
    execute(stack : Stack) : Object { abort() };
    should_quit() : Bool { false };
};

class PushCommand inherits Command {
    value : String;

    init(val : String) : SELF_TYPE {
        {
            value <- val;
            self;
        }
    };

    execute(stack : Stack) : Object {
        stack.push(value)
    };
};

class DCommand inherits Command {
    execute(stack : Stack) : Object {
        {
            stack.print();
        }
    };
};

class XCommand inherits Command {
    execute(stack : Stack) : Object { self };
    should_quit() : Bool { true };
};

class ECommand inherits Command {
    a2i : A2I <- new A2I;
    execute(stack : Stack) : Object {
        if stack.is_empty() then self else
            let top_val : String <- stack.pop() in
                if top_val = "s" then
                    let val1 : String <- stack.pop(),
                        val2 : String <- stack.pop()
                    in {
                        stack.push(val1);
                        stack.push(val2);
                    }
                else if top_val = "+" then
                    let val1 : Int <- a2i.a2i(stack.pop()),
                        val2 : Int <- a2i.a2i(stack.pop())
                    in
                        stack.push(a2i.i2a(val1 + val2))
                else
                    stack.push(top_val)
                fi fi
        fi
    };
};

class Main inherits IO {
    stack : Stack <- new Stack;

    prompt() : String {
        {
            out_string("> ");
            in_string();
        }
    };

    parse_command(ch : String) : Command {
        if ch = "d" then new DCommand
        else if ch = "x" then new XCommand
        else if ch = "e" then new ECommand
        else (new PushCommand).init(ch) 
        fi fi fi
    };

    main() : Object {
        let exit : Bool <- false in
        while not exit loop {
            let line : String <- prompt(),
                cmd : Command <- parse_command(line)   
            in {
                cmd.execute(stack);
                exit <- cmd.should_quit();
            };
        } pool
    };

};
