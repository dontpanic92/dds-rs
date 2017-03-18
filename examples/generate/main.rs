//! Generates a simple DDS file

extern crate clap;
extern crate dds;
extern crate image;

use std::fs::File;
use std::io::Write;

use dds::{DDS, Compression};


fn main() {
    let pixels = (0u32..32 * 32).map(|i| {
        let p = (255.0 * (i as f32).sin()) as u8;
        [p, p, p, 255]
    }).collect::<Vec<_>>();

    let bytes = DDS::encode(&pixels, (32, 32), Compression::None).unwrap();

    let mut file = File::create("samples/generated.dds").unwrap();
    file.write_all(&bytes[..]).unwrap();
}
