# fp-2024
<command> ::= <add_command> | <list_command> | <find_command> | <remove_command>

<add_command> ::= "add book" <book_details>

<list_command> ::= "list books"

<find_command> ::= "find book" <book_details>

<remove_command> ::= "remove book" <book_details>


<book_details> ::= <title> "," <author> "," <year>

<title> ::= <string>

<author> ::= <string>

<year> ::= <number>

<string> ::= <letter> | <letter> <string>

<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<number> ::= "0" | "1" | ... | "9"