# fp-2024
```
<command> ::= <add_command> | <list_command> | <find_command> | <remove_command>

<add_command> ::= "add book" <book_details> | <book_details> <add_command>

<list_command> ::= "list books" | <list_command>

<find_command> ::= "find book" <book_details> | <book_details> <find_command>

<remove_command> ::= "remove book" <book_details> | <book_details> <remove_command>

<book_details> ::= <title> "," <author> "," <year>

<title> ::= <string>

<author> ::= <string>

<year> ::= <number> | <number> <year>

<string> ::= <letter> | <letter> <string>

<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<number> ::= "0" | "1" | ... | "9"

```
