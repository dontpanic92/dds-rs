dds-rs
======

A library for decoding and encoding DirectDraw Surface files. Current
supported format matrix:

| Compression | Decoding | Encoding |
|:-----------:|:--------:|:--------:|
|    None     |    X     |    X     |
|    DXT1     |    X     |          |
|   DXT2/3    |    X     |          |
|   DXT4/5    |    X     |          |


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
