# Docker image

The official Docker image, which is built in CI, can be uses as follows:

```sh
cd /path/to/notebook
docker run -v $PWD:/data sridca/emanote emanote --layers "/data" gen /data/output.docker
open ./output.docker/index.html
```

## Links

- https://hub.docker.com/r/sridca/emanote
