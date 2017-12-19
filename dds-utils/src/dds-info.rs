extern crate dds;
extern crate clap;
#[macro_use]
extern crate prettytable;

use std::fs::File;
use std::io::{BufReader, Seek, SeekFrom};

use dds::DDS;

use clap::{Arg, App};
use prettytable::format;


fn main() {
    let matches = App::new("dds-info")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
            .index(1))
        .get_matches();

    let filename = matches.value_of("INPUT").unwrap_or("../assets/ground.dds");
    let file = File::open(filename).expect("Couldn't find file!");
    let mut reader = BufReader::new(file);

    let header = DDS::parse_header(&mut reader).unwrap();
    reader.seek(SeekFrom::Start(0)).expect("Couldn't seek to file beginning!");
    let raw = DDS::parse_header_raw(&mut reader).unwrap();

    let pitch_linear_col_name = match raw.flags & 0x8 == 0{
        false => "Pitch",
        true => "Linear Size",
    };

    let mut table = table!(
        ["Height", header.height],
        ["Width", header.width],
        ["Mipmaps", header.mipmap_count],
        ["Compression", header.compression],
        [pitch_linear_col_name, raw.pitch_or_linear_size],
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
        ["Bits per pixel", raw.pixel_format.rgb_bit_count],
        ["Red Mask", format!("{:#010X}", raw.pixel_format.red_bit_mask)],
        ["Blue Mask", format!("{:#010X}", raw.pixel_format.blue_bit_mask)],
        ["Green Mask", format!("{:#010X}", raw.pixel_format.green_bit_mask)],
        ["Alpha Mask", format!("{:#010X}", raw.pixel_format.alpha_bit_mask)],
        ["Pixel Format Flags", format!("{:#010X}", raw.pixel_format.flags)],
        [" - ALPHAPIXELS", raw.pixel_format.flags & 0x1 != 0],
        [" - ALPHA", raw.pixel_format.flags & 0x2 != 0],
        [" - FOURCC", raw.pixel_format.flags & 0x4 != 0],
        [" - RGB", raw.pixel_format.flags & 0x40 != 0],
        [" - YUV", raw.pixel_format.flags & 0x200 != 0],
        [" - LUMINANCE", raw.pixel_format.flags & 0x20000 != 0]
    );

    table.set_titles(row!["ATTRIBUTE", "VALUE"]);
    table.set_format(*format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
    table.printstd();
}
