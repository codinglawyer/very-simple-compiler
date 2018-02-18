type token =
  | OpenParen
  | CloseParen
  | Number(string)
  | String(string)
  | Name(string);

let stringToCharList = string => {
  let rec exp = (index, acc) =>
    switch (index < 0) {
    | true => acc
    | _ => exp(index - 1, [string.[index], ...acc])
    };
  exp(String.length(string) - 1, []);
};

/*
 @inpt - a list of chars containing the remaining data to process
 @current - a param of type option token with the current type of token being processed
 @tokens - he accumulator with the list of tokens that will be sent as output
 */
let tokenizer = input => {
  let rec transform = (inpt, current, tokens) =>
    switch inpt {
    | [] => List.rev(tokens)
    | _ =>
      /* char that is being processed*/
      let head = List.hd(inpt);
      /* remainder*/
      let tail = List.tl(inpt);
      /* partially applied transform*/
      let next = transform(tail);
      switch (head, current) {
      /* current is None*/
      | ('(', None) => next(None, [String.make(1, head), ...tokens])
      | (')', None) => next(None, [String.make(1, head), ...tokens])
      | (' ' | '\t' | '\r' | '\n', None) => next(None, tokens)
      | ('"', None) => next(Some(String("")), tokens)
      | ('a'..'z', None) => next(Some(Name(String.make(1, head))), tokens)
      | ('0'..'9', None) => next(Some(Number(String.make(1, head))), tokens)
      /* current is String*/
      | ('"', Some(String(current))) => next(None, [current, ...tokens])
      | (head, Some(String(current))) =>
        next(Some(String(current ++ String.make(1, head))), tokens)
      /* current is Name*/
      | (' ', Some(Name(current))) => next(None, [current, ...tokens])
      | (')', Some(Name(current))) =>
        next(None, [String.make(1, head), current, ...tokens])
      | ('a'..'z', Some(Name(current))) =>
        next(Some(Name(current ++ String.make(1, head))), tokens)
      /* current is Number*/
      | (' ', Some(Number(current))) => next(None, [current, ...tokens])
      | (')', Some(Number(current))) =>
        next(None, [String.make(1, head), current, ...tokens])
      | ('0'..'9', Some(Number(current))) =>
        next(Some(Number(current ++ String.make(1, head))), tokens)
      /* cover the rest of the cases*/
      | (_, _) => next(None, tokens)
      };
    };
  transform(stringToCharList(input), None, []);
};

/* tokenizer("(add 2 (subtract 4 2))"); */
let tokens = tokenizer("(add 2 (subtract 4 3))");

Js.log(Array.of_list(tokens));



/* AST nodes */
type numberLiteralNode = {
  type_: string,
  value: string
};

type callExpressionNode = {
  type_: string,
  name: string,
  params: list(numberLiteralNode)
  /* params: string */
};

type node =
  | NumberLiteralNode(numberLiteralNode)
  | CallExpressionNode(callExpressionNode);

type callExpressions = {
  type_: string,
  name: string,
  params: list(node)
};

type ast = {
  type_: string,
  body: list(callExpressions)
};

let parser = tokens => {
  let rec func = (input, current, ast) => {
    let head = List.hd(input);
    let tail = List.tl(input);
    Js.log(head);
    Js.log(ast);
    /* Js.log(tail); */
    switch head {
    | "(" => func(tail, None, ast)
    | ")" => func(tail, None, ast)
    | "add" =>
      func(
        tail,
        None,
        {...ast, body: [{type_: "CallExpression", name: "add", params: []}]}
      )
    | "2" =>
      func(
        tail,
        None,
        {
          ...ast,
          body: [
            {
              type_: "CallExpression",
              name: "add",
              params: [
                NumberLiteralNode({type_: "NumberLiteral", value: head})
              ]
            }
          ]
        }
      )
    | "subtract" =>
      func(
        tail,
        None,
        {
          ...ast,
          body: [
            {
              type_: "CallExpression",
              name: "add",
              params: [
                NumberLiteralNode({type_: "NumberLiteral", value: "2"}),
                CallExpressionNode({
                  type_: "CallExpression",
                  name: head,
                  params: []
                })
              ]
            }
          ]
        }
      )
    | "4" =>
      func(
        tail,
        None,
        {
          ...ast,
          body: [
            {
              type_: "CallExpression",
              name: "add",
              params: [
                NumberLiteralNode({type_: "NumberLiteral", value: "2"}),
                CallExpressionNode({
                  type_: "CallExpression",
                  name: "subtract",
                  params: [{type_: "NumberLiteral", value: head}]
                })
              ]
            }
          ]
        }
      )
    | "3" =>
      func(
        tail,
        None,
        {
          ...ast,
          body: [
            {
              type_: "CallExpression",
              name: "add",
              params: [
                NumberLiteralNode({type_: "NumberLiteral", value: "2"}),
                CallExpressionNode({
                  type_: "CallExpression",
                  name: "subtract",
                  params: [
                    {type_: "NumberLiteral", value: "4"},
                    {type_: "NumberLiteral", value: head}
                  ]
                })
              ]
            }
          ]
        }
      )
    | _ => Js.log(head)
    };
  };
  func(tokens, None, {type_: "Program", body: []});
};

parser(tokens);


/* old tokenizer version*/
/* let tokenizer = input => {
     let rec transform = (expression, currentIndex, tokens) =>
       if (currentIndex !== 0) {
         let char = expression.[currentIndex];
         let token =
           switch char {
           | '(' => {type_: "paren", value: String.make(1, char)}
           | ')' => {type_: "paren", value: String.make(1, char)}
           | ' ' => {type_: "space", value: String.make(0, char)}
           | 'a'..'z' => {type_: "name", value: String.make(1, char)}
           | '0'..'9' => {type_: "number", value: String.make(1, char)}
           };
         Js.log(token.type_);
         let tok = [token, ...tokens];
         Js.log(Array.of_list(tok));
         transform(input, currentIndex - 1, tok);
       };
     Js.log(transform(input, String.length(input) - 1, []));
   }; */
/* [@bs.deriving jsConverter]
   type something = {
     foo: int,
     bar: float
   };

   let x = {
     foo: 10,
     bar: 0.01
   };

   Js.log(somethingToJs(x)); */