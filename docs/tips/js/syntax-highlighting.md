---
slug: syntax-highlighting
order: -1
---

# Syntax Highlighting

Emanote includes built-in syntax highlighting powered by [skylighting](https://github.com/jgm/skylighting), the same library used by Pandoc. Code blocks are highlighted at build time—no JavaScript required.

## How it Works

Code blocks are automatically tokenized during rendering. Each token gets a CSS class (like `kw` for keywords, `st` for strings, `co` for comments) and styled via CSS included in emanote's default theme.

### Example

```haskell
-- A simple factorial function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

```python
def fibonacci(n):
    """Generate fibonacci sequence up to n"""
    a, b = 0, 1
    while a < n:
        yield a
        a, b = b, a + b
```

```nix
{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    vim
    git
  ];
}
```

## Supported Languages

Skylighting supports [over 140 languages](https://github.com/jgm/skylighting/tree/master/skylighting-core/xml) including:

- Haskell, Python, JavaScript, TypeScript, Rust, Go
- Nix, Shell/Bash, YAML, JSON, TOML
- HTML, CSS, SQL, Markdown
- And many more...

## Customizing the Theme

The default theme is in `_emanote-static/skylighting.css`. To customize, create your own `_emanote-static/skylighting.css` in your notes directory to override the default.

Alternatively, add custom styles in your `index.yaml`:

```yaml
page:
  headHtml: |
    <style>
    /* Override keyword color */
    code span.kw { color: #ff79c6; font-weight: bold; }
    /* Override string color */
    code span.st { color: #f1fa8c; }
    </style>
```

### Token Classes

| Class | Token Type    | Example              |
| ----- | ------------- | -------------------- |
| `kw`  | Keyword       | `if`, `then`, `else` |
| `dt`  | Data Type     | `Int`, `String`      |
| `dv`  | Decimal Value | `42`, `100`          |
| `st`  | String        | `"hello"`            |
| `ch`  | Character     | `'a'`                |
| `co`  | Comment       | `-- comment`         |
| `fu`  | Function      | function names       |
| `op`  | Operator      | `+`, `-`, `*`        |
