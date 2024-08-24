# Layer system

Emanote's layer system allows you to "merge" multiple notebook directories and treat them as if they were a single notebook directory. The `-L` option in the command line accepts layers, and you can specify multiple of them with the leftmost taking the most precedence.

For example,

```sh
emanote -L ./docs1:./docs2 run
```

Internally, Emanote merges both `docs1` and `docs2` folders and treats them as a single directory. Thus, both `docs1` and `docs2` can contain the same file, and the one in `docs1` will take precedence.

## "Default" layer

Emanote *implicitly* includes what is known as the "default" layer. Its contents can be seen [here](https://github.com/srid/emanote/tree/master/emanote/default). This layer contains [[html-template]], [[yaml-config|index.yaml]] and other default assets, like the logo, favicon and fonts. When you run `emanote -L /your/notebook run`, your notebook is overlaid *on top of* this default layer. What this means, in effect, is that you override **just about any file** in the default layer, such the HTML content of [[html-template]], in your own notebook directory. As an example, see [`template/hooks`](https://github.com/srid/emanote/tree/master/docs/templates/hooks) of this documentation notbook.

## Merge semantics

The default merge semantic is to replace with the file on the right layer. For some file types, special merge semantic applies. For example, [[yaml-config|YAML files]] are merged by deep merge, not file-level replacement. This is what allows you to create `index.yaml` that overrides only a subset of the default configuration.

## Mount point

>[!tip] Composition
> To learn more about composition of multiple notebooks, see [here](https://github.com/srid/emanote/issues/494).

Layers can be mounted at a specific path. For example, if you want to mount `docs1` at `/D1` and `docs2` at `/D2`, you can do so with:

```sh
emanote -L ./docs1@D1;./docs2@D2 run
```

When two layers are mounted at distinct mount points it becomes impossible for there to be overlaps. This is useful to host sub-sites under a single site, such as in [this case](https://github.com/flake-parts/community.flake.parts).
