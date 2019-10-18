# MONKERS

<p align="center">A monkey language interpreter implementation, from the book <a href="https://interpreterbook.com/">Writing an interpreter in Go</a></p>

<p align="center">
  <img width="300" src="https://monkeylang.org/images/logo.png">
</p>



## An overview of what you'll find

  #### Integers: `0; 42; 947;`

  #### Booleans: `true | false`

  #### Strings `"someString";`

  #### Functions
```
let add = fn(x, y) { return x + y; };
let three = add(1, 2);
```

  #### Arrays:
```
let arr = [1, true, fn (x) { return x; }];
let one = arr[0];
```

  #### Hashes:
```
let add = fn (x, y) = { return x + y; };
let some_hash = {"foo" + "bar": 2, true: add(40, 2)};
let two = some_hash["foobar"];
let meaning_of_life = some_hash[true];
```

  ### Builtin functions

  - `len(X)`: Returns the length of X, can be applied on `string`, `array` or `hash`.
  - `first(X)`: Returns the first element of an array X, or `null` if it has no elements.
  - `last(X)`: Returns the last element of an array of X, or `null` if it has no elements.
  - `head(X)`: Returns a slice of an array X containing all but the last element, or an empty array if X had no elements.
  - `tail(X)`: Returns a slice of an array X containing all but the first element, or an empty array if X had no elements.
  - `push(X)`: Adds an element in the last position of an array X.


  ### Infix Operators

  - `*, /, +, -`: Regular math operators that can be used on `integer`s.

  - `+`: Other than it's previous use, it can also be used to concatenate `string`.
    ```
    let foobar = "foo" + "bar";
    ```

  - `[]`: Index operator can be applied to `array`s or `hash`es to get the element at the given index.
    ```
    let some_hash = {"foo": "bar"};
    let bar = some_hash["foo"];
    ```

    ```
    let numbers = [3, 2, 1];
    let one = numbers[2];
    ```

  ### Prefix Operators

  - `!`: Bang operator negates the RHS operand.
    ```
    let not_true = !true;
    let unchanged = !!true;
    ```


## The REPL

The project includes a REPL, which you can use to evaluate `monkey` code. You can launch said REPL by running `cargo run`.

```
> let some_hash = {"foo" + "bar": 2 * 2};
[foobar: 4]

> some_hash["foobar"]
4
```
