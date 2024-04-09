(function() {var implementors = {
"indexmap":[["impl&lt;K, V&gt; <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"indexmap/map/struct.Slice.html\" title=\"struct indexmap::map::Slice\">MapSlice</a>&lt;K, V&gt;<span class=\"where fmt-newline\">where\n    K: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a>,\n    V: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a>,</span>"],["impl&lt;K, V, S&gt; <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"indexmap/map/struct.IndexMap.html\" title=\"struct indexmap::map::IndexMap\">IndexMap</a>&lt;K, V, S&gt;<span class=\"where fmt-newline\">where\n    K: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.70.0/core/hash/trait.Hash.html\" title=\"trait core::hash::Hash\">Hash</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.70.0/core/cmp/trait.Eq.html\" title=\"trait core::cmp::Eq\">Eq</a>,\n    V: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a>,\n    S: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.70.0/core/hash/trait.BuildHasher.html\" title=\"trait core::hash::BuildHasher\">BuildHasher</a>,</span>"],["impl&lt;T, S&gt; <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"indexmap/set/struct.IndexSet.html\" title=\"struct indexmap::set::IndexSet\">IndexSet</a>&lt;T, S&gt;<span class=\"where fmt-newline\">where\n    T: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.70.0/core/hash/trait.Hash.html\" title=\"trait core::hash::Hash\">Hash</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.70.0/core/cmp/trait.Eq.html\" title=\"trait core::cmp::Eq\">Eq</a>,\n    S: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.70.0/core/hash/trait.BuildHasher.html\" title=\"trait core::hash::BuildHasher\">BuildHasher</a>,</span>"],["impl&lt;T&gt; <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"indexmap/set/struct.Slice.html\" title=\"struct indexmap::set::Slice\">SetSlice</a>&lt;T&gt;<span class=\"where fmt-newline\">where\n    T: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a>,</span>"]],
"semver":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"semver/struct.Version.html\" title=\"struct semver::Version\">Version</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"semver/struct.Comparator.html\" title=\"struct semver::Comparator\">Comparator</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"semver/struct.VersionReq.html\" title=\"struct semver::VersionReq\">VersionReq</a>"]],
"serde":[],
"serde_cbor":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"serde_cbor/enum.Value.html\" title=\"enum serde_cbor::Value\">Value</a>"],["impl&lt;T: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a>&gt; <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_cbor/tags/struct.Tagged.html\" title=\"struct serde_cbor::tags::Tagged\">Tagged</a>&lt;T&gt;"]],
"serde_json":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"serde_json/enum.Value.html\" title=\"enum serde_json::Value\">Value</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_json/struct.Map.html\" title=\"struct serde_json::Map\">Map</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/1.70.0/alloc/string/struct.String.html\" title=\"struct alloc::string::String\">String</a>, <a class=\"enum\" href=\"serde_json/enum.Value.html\" title=\"enum serde_json::Value\">Value</a>&gt;"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_json/value/struct.Number.html\" title=\"struct serde_json::value::Number\">Number</a>"]],
"serde_spanned":[["impl&lt;T: <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a>&gt; <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_spanned/struct.Spanned.html\" title=\"struct serde_spanned::Spanned\">Spanned</a>&lt;T&gt;"]],
"serde_yaml":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"serde_yaml/enum.Value.html\" title=\"enum serde_yaml::Value\">Value</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_yaml/value/struct.Number.html\" title=\"struct serde_yaml::value::Number\">Number</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_yaml/struct.Mapping.html\" title=\"struct serde_yaml::Mapping\">Mapping</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"serde_yaml/value/struct.TaggedValue.html\" title=\"struct serde_yaml::value::TaggedValue\">TaggedValue</a>"]],
"toml":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"toml/map/struct.Map.html\" title=\"struct toml::map::Map\">Map</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/1.70.0/alloc/string/struct.String.html\" title=\"struct alloc::string::String\">String</a>, <a class=\"enum\" href=\"toml/enum.Value.html\" title=\"enum toml::Value\">Value</a>&gt;"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"toml/enum.Value.html\" title=\"enum toml::Value\">Value</a>"]],
"toml_datetime":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"toml_datetime/struct.Datetime.html\" title=\"struct toml_datetime::Datetime\">Datetime</a>"]],
"toml_edit":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"toml_edit/struct.InternalString.html\" title=\"struct toml_edit::InternalString\">InternalString</a>"]],
"wasmer_toml":[["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"wasmer_toml/enum.Abi.html\" title=\"enum wasmer_toml::Abi\">Abi</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/rust/struct.Wasmer.html\" title=\"struct wasmer_toml::rust::Wasmer\">Wasmer</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.WitBindings.html\" title=\"struct wasmer_toml::WitBindings\">WitBindings</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.Manifest.html\" title=\"struct wasmer_toml::Manifest\">Manifest</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"wasmer_toml/enum.Command.html\" title=\"enum wasmer_toml::Command\">Command</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"wasmer_toml/enum.ModuleReference.html\" title=\"enum wasmer_toml::ModuleReference\">ModuleReference</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.FileCommandAnnotations.html\" title=\"struct wasmer_toml::FileCommandAnnotations\">FileCommandAnnotations</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.Package.html\" title=\"struct wasmer_toml::Package\">Package</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"wasmer_toml/enum.Bindings.html\" title=\"enum wasmer_toml::Bindings\">Bindings</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"wasmer_toml/enum.CommandAnnotations.html\" title=\"enum wasmer_toml::CommandAnnotations\">CommandAnnotations</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.CommandV2.html\" title=\"struct wasmer_toml::CommandV2\">CommandV2</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.WaiBindings.html\" title=\"struct wasmer_toml::WaiBindings\">WaiBindings</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.Module.html\" title=\"struct wasmer_toml::Module\">Module</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"enum\" href=\"wasmer_toml/enum.FileKind.html\" title=\"enum wasmer_toml::FileKind\">FileKind</a>"],["impl <a class=\"trait\" href=\"serde/ser/trait.Serialize.html\" title=\"trait serde::ser::Serialize\">Serialize</a> for <a class=\"struct\" href=\"wasmer_toml/struct.CommandV1.html\" title=\"struct wasmer_toml::CommandV1\">CommandV1</a>"]]
};if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()