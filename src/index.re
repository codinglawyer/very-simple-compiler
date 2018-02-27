type token =
  | OpenParen
  | CloseParen
  | Number(string)
  | String(string)
  | Name(string)
  | CallExpression;

let stringToCharList = (string: string) => {
  let rec exp = (index, acc) =>
    switch (index < 0) {
    | true => acc
    | _ => exp(index - 1, [string.[index], ...acc])
    };
  exp(String.length(string) - 1, []);
};

/*
 @remainingChars - a list of chars containing the remaining data to process
 @previousToken - a param of type option token with the current type of token being processed
 @tokens - he accumulator with the list of tokens that will be sent as output
 */
let tokenizer = (input: string) => {
  let rec transform =
          (~remainingChars: list(char), ~previousToken: option(token), ~tokens: list(string)) =>
    /* recursively call the function until all the tokens are processed*/
    remainingChars === [] ?
      List.rev(tokens) :
      {
        /* char that is being processed*/
        let head = List.hd(remainingChars);
        /* remainder*/
        let tail = List.tl(remainingChars);
        /* partially applied transform*/
        let next = transform(~remainingChars=tail);
        switch (head: char, previousToken: option(token)) {
        /* previousToken is None*/
        | ('(', None) =>
          next(~previousToken=None, ~tokens=[String.make(1, head), ...tokens])
        | (')', None) =>
          next(~previousToken=None, ~tokens=[String.make(1, head), ...tokens])
        | (' ' | '\t' | '\r' | '\n', None) => next(~previousToken=None, ~tokens)
        | ('"', None) => next(~previousToken=Some(String("")), ~tokens)
        | ('a'..'z', None) =>
          next(~previousToken=Some(Name(String.make(1, head))), ~tokens)
        | ('0'..'9', None) =>
          next(~previousToken=Some(Number(String.make(1, head))), ~tokens)
        /* previousToken is String*/
        | ('"', Some(String(previousToken))) =>
          next(~previousToken=None, ~tokens=[previousToken, ...tokens])
        | (head, Some(String(previousToken))) =>
          next(~previousToken=Some(String(previousToken ++ String.make(1, head))), ~tokens)
        /* previousToken is Name*/
        | (' ', Some(Name(previousToken))) =>
          next(~previousToken=None, ~tokens=[previousToken, ...tokens])
        | (')', Some(Name(previousToken))) =>
          next(
            ~previousToken=None,
            ~tokens=[String.make(1, head), previousToken, ...tokens]
          )
        | ('a'..'z', Some(Name(previousToken))) =>
          next(~previousToken=Some(Name(previousToken ++ String.make(1, head))), ~tokens)
        /* previousToken is Number*/
        | (' ', Some(Number(previousToken))) =>
          next(~previousToken=None, ~tokens=[previousToken, ...tokens])
        | (')', Some(Number(previousToken))) =>
          next(
            ~previousToken=None,
            ~tokens=[String.make(1, head), previousToken, ...tokens]
          )
        | ('0'..'9', Some(Number(previousToken))) =>
          next(~previousToken=Some(Number(previousToken ++ String.make(1, head))), ~tokens)
        /* cover the rest of the cases*/
        | (_, _) => next(~previousToken=None, ~tokens)
        };
      };
  transform(~remainingChars=stringToCharList(input), ~previousToken=None, ~tokens=[]);
};

/*
   How AST should look like

   {type_: "Program", body: [
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
 ]}; */

/* AST nodes */
type literalNode = {
  type_: string,
  value: string
};

type callExpressionNode = {
  type_: option(string),
  value: option(string),
  name: option(string),
  params: option(list(callExpressionNode))
};

type ast = {
  type_: string,
  body: list(callExpressionNode)
};

let numberRegex = [%bs.re "/^\\d+$/"];

let parser = (tokens: list(string)) => {
  let rec func = (~remainingTokens: list(string), ~previousNode: option(token), ~ast) =>
    remainingTokens === [] ?
      ast :
      {
        let head = List.hd(remainingTokens);
        let tail = List.tl(remainingTokens);
        /* Js.log(head); */
        /* Js.log(remainingTokens); */
        switch (head: string, previousNode: option(token)) {
        | ("(", None | Some(CallExpression)) =>
          func(~remainingTokens=tail, ~previousNode=Some(OpenParen), ~ast=ast)
        | (")", Some(CallExpression)) => func(~remainingTokens=tail, ~previousNode=Some(CloseParen), ~ast=ast)
        | ("add", Some(OpenParen)) => [
            {
              type_: Some("CallExpression"),
              name: Some("add"),
              value: None,
              params: Some(func(~remainingTokens=tail, ~previousNode=Some(CallExpression), ~ast=[]))
            }
          ]
        | (_numberRegex, Some(CallExpression)) =>
          func(
            ~remainingTokens=tail,
            ~previousNode=Some(CallExpression),
            ~ast=[
              {
                type_: Some("NumberLiteral"),
                value: Some(head),
                params: None,
                name: None
              },
              ...ast
            ]
          )
        | ("subtract", Some(OpenParen)) => [
            {
              type_: Some("CallExpression"),
              name: Some(head),
              params: Some(func(~remainingTokens=tail, ~previousNode=Some(CallExpression), ~ast=[])),
              value: None
            },
            ...ast
          ]
        | (_, _) => func(~remainingTokens=tail, ~previousNode=None, ~ast=ast)
        };
      };
  let ast = {type_: "Program", body: func(~remainingTokens=tokens, ~previousNode=None, ~ast=[])};
  Js.log(ast);
};

let lispExpression = "(add 2 (subtract 4 3))";

let compiler = lispExpression |> tokenizer |> parser;

Js.log(compiler);
