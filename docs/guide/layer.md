# Layer system

Emanote's layer system allows you to "merge" multiple notebooks and treat them as if they were a single notebook. The `-L` option in the command line accepts layers, and you can specify multiple of them with the right most taking the most precedence.

## "Default" layer

Emanote *implicitly* includes what is known as the "default" layer. Its contents can be seen [here](https://github.com/srid/emanote/tree/master/default). This layer contains [[html-template]], [[yaml-config|index.yaml]] and other default assets, like the logo, favicon and fonts. When you run `emanote -L /your/notebook run`, your notebook is overlaid *on top of* this default layer. What this means, in effect, is that you override **just about any file** in the default layer, such the HTML content of [[html-template]], in your own notebook directory. As an example, see [`template/hooks`](https://github.com/srid/emanote/tree/master/docs/templates/hooks) of this documentation notbook.

## Merge semantics

The default merge semantic is to replace with the file on the right layer. For some file types, special merge semantic applies. For example, [[yaml-config|YAML files]] are merged by deep merge, not file-level replacement. This is what allows you to create `index.yaml` that overrides only a subset of the default configuration.