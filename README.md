dds-rs
======

[![pipeline status][pipeline]][master]

[pipeline]: https://gitlab.com/mechaxl/dds-rs/badges/master/pipeline.svg
[master]: https://gitlab.com/mechaxl/dds-rs/commits/master

Introduction
------------

A library for decoding and encoding DirectDraw Surface files. Currently handles decoding some uncompressed DX9 formats,
as well as DXT1-5. Supports encoding in the A8R8G8B8 format. Support for cubemaps and volumes, as well as DX10 is
planned.


Project Layout
--------------

The `dds-rs` crate lives in the `dds-rs` subdirectory of this repo. There also
exist several utilities in `dds-utils` that may eventually get published to
crates.io. They also function as examples of how to use `dds-rs`.

Example input files can be found under `examples/`.


Examples
--------

```rust
extern crate dds;

use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use dds::DDS;

fn main() {
    let file = File::open(Path::new("foo.dds")).unwrap();
    let mut reader = BufReader::new(file);

    let dds = DDS::decode(&mut reader).unwrap();
}
```
