# Changelog

## [0.7.0](https://github.com/wasmerio/wasmer-toml/compare/wasmer-toml-v0.6.0...wasmer-toml-v0.7.0) (2023-07-20)


### ⚠ BREAKING CHANGES

* Manifest and Package are now #[non_exhaustive] and configurable via a builder API
* made ManifestError and ValidationError more strongly typed and descriptive
* Removed unnecessary Option wrappers from the Manifest type

### Features

* Added an "entrypoint" field to the "[package]" table (fixes [#15](https://github.com/wasmerio/wasmer-toml/issues/15)) ([d6bce6b](https://github.com/wasmerio/wasmer-toml/commit/d6bce6b620000dd156e3cc5a6aefa9c316c7c8ac))
* Added validation for duplicate commands and modules ([26f8f84](https://github.com/wasmerio/wasmer-toml/commit/26f8f84e168c01e30d5838b10b2eea10b457f57c))
* Added validation to check that the entrypoint is valid ([b9b677c](https://github.com/wasmerio/wasmer-toml/commit/b9b677cc461896cdc26246d32add2043b26ffd1e))
* made ManifestError and ValidationError more strongly typed and descriptive ([75040b8](https://github.com/wasmerio/wasmer-toml/commit/75040b8bb73a267024ae2f11aeda88387a56795e))
* Manifest and Package are now #[non_exhaustive] and configurable via a builder API ([2b99e5c](https://github.com/wasmerio/wasmer-toml/commit/2b99e5cc8a1f9c1e6aa1a9e6d9da05ca6a5cd998))
* Removed unnecessary Option wrappers from the Manifest type ([5307784](https://github.com/wasmerio/wasmer-toml/commit/53077842114d39b0d1ce8277c4158f669e641545))


### Miscellaneous Chores

* release 0.7.0 ([e855934](https://github.com/wasmerio/wasmer-toml/commit/e85593437f3d862b06659b105528199fbfcb1cbf))
