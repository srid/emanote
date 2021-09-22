# Docker image

The official Docker image, which is built in CI, can be used as follows:

```sh
cd /path/to/notebook
docker run -v $PWD:/data sridca/emanote emanote -L "/data" gen /data/output.docker
open ./output.docker/index.html
```

## Links

- https://hub.docker.com/r/sridca/emanote
