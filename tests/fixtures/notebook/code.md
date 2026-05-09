# Code blocks fixture

Two fenced code blocks so the copy-button injection has more than one
target — catches a future regression where the first one wires but the
loop terminates early.

```js
const greet = (name) => `Hello, ${name}!`;
console.log(greet("emanote"));
```

```sh
echo "second block"
```
