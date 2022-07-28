# Search

**These are temporary private notes!** TODO: Cleanup and write actual docs

```bash
emanote gen result -L 'docs;default'

cd result

echo '[input]' > stork.toml
echo 'files = [' >> stork.toml
rg -g '!-/' '<title>' -A1 | grep html- | sed 's/\(.*.html\)-  *\(.*\)/  {path = "\1", url="\1", title = """\2"""},/' >> stork.toml
echo ']' >> stork.toml

stork build --input stork.toml --output stork.st

podman run --rm -it -v $PWD:/usr/share/nginx/html:Z -p 8080:80 nginx:alpine
```
