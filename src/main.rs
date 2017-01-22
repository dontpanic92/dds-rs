extern crate dds;
extern crate clap;
#[macro_use]
extern crate prettytable;

use std::fs::File;
use std::io::BufReader;
use std::str;

use dds::DDS;

use clap::{Arg, App};


fn main() {
    let matches = App::new("dds-info")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
            .index(1))
        .get_matches();

    let file = File::open(matches.value_of("INPUT").unwrap()).expect("Couldn't find file!");
    let mut reader = BufReader::new(file);

    let header = DDS::parse_header(&mut reader).unwrap();

    let raw = header.get_raw();
    let rgb_info = header.get_rgb_info();


    ptable!(
        ["Attribute", "Value"],
        ["Height", header.height],
        ["Width", header.width],
        ["Mipmaps", header.mipmap_count],
        ["Compression", header.compression],
        ["Pitch/Linear Size", raw.pitch_or_linear_size],
        ["Depth", raw.depth],
        ["Flags", format!("{:#010X}", raw.flags)],
        [" - PITCH", raw.flags & 0x8 != 0],
        [" - MIPMAPCOUNT", raw.flags & 0x20000 != 0],
        [" - LINEARSIZE", raw.flags & 0x80000 != 0],
        [" - DEPTH", raw.flags & 0x800000 != 0],
        ["Details", format!("{:#010X}\n{:#010X}", raw.caps, raw.caps2)],
        [" - COMPLEX", raw.caps & 0x8 != 0],
        [" - TEXTURE", raw.caps & 0x1000 != 0],
        [" - MIPMAP", raw.caps & 0x400000 != 0],
        [" - CUBEMAP", raw.caps2 & 0x200 != 0],
        [" - CUBEMAP_POSITIVEX", raw.caps2 & 0x400 != 0],
        [" - CUBEMAP_NEGATIVEX", raw.caps2 & 0x800 != 0],
        [" - CUBEMAP_POSITIVEY", raw.caps2 & 0x1000 != 0],
        [" - CUBEMAP_NEGATIVEY", raw.caps2 & 0x2000 != 0],
        [" - CUBEMAP_POSITIVEZ", raw.caps2 & 0x4000 != 0],
        [" - CUBEMAP_NEGATIVEZ", raw.caps2 & 0x8000 != 0],
        [" - VOLUME", raw.caps2 & 0x200_000 != 0],
        ["Pixel Format", header.pixel_format],
        ["Bits per pixel", rgb_info[0]],
        ["Red Mask", format!("{:#010X}", rgb_info[1])],
        ["Blue Mask", format!("{:#010X}", rgb_info[2])],
        ["Green Mask", format!("{:#010X}", rgb_info[3])],
        ["Alpha Mask", format!("{:#010X}", rgb_info[4])],
        ["Pixel Format Flags", format!("{:#010X}", raw.pixel_format.flags)],
        [" - ALPHAPIXELS", raw.pixel_format.flags & 0x1 != 0],
        [" - ALPHA", raw.pixel_format.flags & 0x2 != 0],
        [" - FOURCC", raw.pixel_format.flags & 0x4 != 0],
        [" - RGB", raw.pixel_format.flags & 0x40 != 0],
        [" - YUV", raw.pixel_format.flags & 0x200 != 0],
        [" - LUMINANCE", raw.pixel_format.flags & 0x20000 != 0]
    );
}
