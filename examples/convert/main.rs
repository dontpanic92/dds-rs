//! Demonstrates usage of `dds-rs` by converting each mipmap
//! in a DDS file to a png

extern crate clap;
extern crate dds;
extern crate image;

use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use dds::DDS;

use clap::{Arg, App};


fn main() {
    let matches = App::new("convert")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .index(1))
        .get_matches();

    let filename = matches.value_of("INPUT").unwrap_or("examples/assets/ground.dds");

    let file = File::open(filename).expect("Couldn't find file!");
    let mut reader = BufReader::new(file);

    let dds = DDS::decode(&mut reader).unwrap();

    for (i, layer) in dds.layers.iter().enumerate() {
        let height = dds.header.height / 2u32.pow(i as u32);
        let width = dds.header.width / 2u32.pow(i as u32);

        let mut imgbuf = image::ImageBuffer::new(height, width);

        for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
            *pixel = image::Rgba(layer[(x + height * y) as usize]);
        }

        let path = format!("examples/output/test-{}.png", height);
        let ref mut fout = File::create(&Path::new(path.as_str())).unwrap();
        let _ = image::ImageRgba8(imgbuf).save(fout, image::PNG);
    }
}
