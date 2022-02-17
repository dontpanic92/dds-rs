//! Generates a simple DDS file

extern crate clap;
extern crate dds;
extern crate image;
extern crate rgb;

use dds::{Compression, DDS};
use rgb::RGBA;
use std::fs::File;
use std::io::Write;

fn main() {
    let pixels = (0u32..32 * 32)
        .map(|i| {
            let p = (255.0 * (i as f32).sin()) as u8;
            RGBA {
                r: p,
                g: p,
                b: p,
                a: 255,
            }
        })
        .collect::<Vec<_>>();

    let bytes = DDS::encode(&pixels, (32, 32), Compression::None).unwrap();

    let mut file = File::create("output/generated.dds").unwrap();
    file.write_all(&bytes[..]).unwrap();
}
