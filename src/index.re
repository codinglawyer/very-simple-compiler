let lisp = "(add 2 (subtract 4 2))";

type token = {
  type_: string,
  value: string
};

/* lexical analysis with tokenizer */
/* [{type: "number"}, {value: "3"}] */
let tokenizer = input => {
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
};

tokenizer(lisp);