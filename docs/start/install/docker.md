# Docker image

The official Docker image, which is built in CI, can be used as follows:

```sh
cd /path/to/notebook
mkdir output.docker
docker run \
  -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 \
  --tmpfs /tmp \
  -v $PWD:/data \
  sridca/emanote emanote -L "/data" gen /data/output.docker
open ./output.docker/index.html
```

## Links

- https://hub.docker.com/r/sridca/emanote
