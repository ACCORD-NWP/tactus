# Domain configuration

## Define new domains

The domain you use for your run needs to be defined in a toml file and placed in include/domains folder and few test domains are provided.

We recommend you use tools like domain_maker.py (EpyGraM) or [Domain Creation Tool](https://hirlam.github.io/HarmonieSystemDocumentation/dev/ExperimentConfiguration/ModelDomain/) (follow the link) to define your domain.

The parameters you can put into your domain configuration can be found in [config/domain](https://destination-earth-digital-twins.github.io/deode-prototype-docs/misc_section_in_doc_page.html#property-config-domain) section.

Some of the parameters are optional and derived automatically if not provided:

- **ilone** / **ilate**: If not provided will default to 11 gridpoints. 
- **nbzonl** / **nbzong**: If not provided will be derived automatically from resolution (xdx/xdy) but if domain very small, i.e. less than 250 gridpoints in either direction, then the parameter defaults to 8 gridpoints.

Finally you'll need to update _domain_ under section [include] in your main config file and point it to your new domain toml file.
