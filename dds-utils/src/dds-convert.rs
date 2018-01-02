//! Demonstrates usage of `dds-rs` by converting each mipmap
//! in a DDS file to a png

extern crate clap;
extern crate dds;
extern crate image;
extern crate rgb;

use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::process::exit;
use std::iter::Iterator;

use dds::DDS;

use clap::{Arg, App};
use rgb::ComponentSlice;
use image::Pixel;


fn main() {
    let matches = App::new("convert")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .index(1))
        .get_matches();

    let filename = matches.value_of("INPUT").unwrap_or("../examples/ground.dds");

    let file = File::open(filename).expect("Couldn't find file!");
    let mut reader = BufReader::new(file);

    let dds = match DDS::decode(&mut reader) {
        Ok(dds) => dds,
        Err(e) => {
            println!("Error encountered while converting image: {:?}", e);
            exit(1);
        }
    };

    for (i, layer) in dds.layers.iter().enumerate() {
        let height = dds.header.height / 2u32.pow(i as u32);
        let width = dds.header.width / 2u32.pow(i as u32);

        let mut imgbuf = image::ImageBuffer::new(width, height);

        for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
            *pixel = *image::Rgba::from_slice(layer[(x + width * y) as usize].as_slice());
        }

        let path = format!("output/test-{}.png", height);
        let fout = &mut File::create(&Path::new(path.as_str())).unwrap();
        image::ImageRgba8(imgbuf).save(fout, image::PNG).unwrap();
    }
}
