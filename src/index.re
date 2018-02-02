let lisp = "(add 2 (subtract 4 2))";

/* lexical analysis with tokenizer */
/* [{type: "number"}, {value: "3"}] */
let tokenizer = input => {
  let rec transform = (expression, currentIndex, tokens) =>
    if (currentIndex !== 0) {
      let char = expression.[currentIndex];
      let token =
        switch char {
        | '(' => String.make(1, char)
        | ')' => String.make(1, char)
        | ' ' => String.make(0, char)
        | 'a'..'z' => String.make(1, char)
        | '0'..'9' => String.make(1, char)
        };
      let token = [token, ...tokens];
      Js.log(Array.of_list(token));
      transform(input, currentIndex - 1, token);
    };
  Js.log(transform(input, String.length(input) - 1, []));
};

tokenizer(lisp);