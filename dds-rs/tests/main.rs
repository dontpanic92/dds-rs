extern crate dds;
extern crate image;
extern crate rgb;

use dds::{Compression, DDS};
use image::GenericImage;
use rgb::{ComponentBytes, FromSlice};
use std::fs::File;
use std::io::BufReader;
use std::io::Cursor;

fn compare_dds_to_png(dds_path: String, png_path: String) {
    let mut reader = BufReader::new(File::open(dds_path).unwrap());
    let dds = DDS::decode(&mut reader).unwrap();

    let img = image::open(png_path).unwrap();

    let width = img.width() as usize;
    for (x, y, pixel) in img.pixels() {
        assert_eq!(
            pixel.data[0],
            dds.layers[0][x as usize + y as usize * width].r
        );
        assert_eq!(
            pixel.data[1],
            dds.layers[0][x as usize + y as usize * width].g
        );
        assert_eq!(
            pixel.data[2],
            dds.layers[0][x as usize + y as usize * width].b
        );
        assert_eq!(
            pixel.data[3],
            dds.layers[0][x as usize + y as usize * width].a
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Read;

    #[test]
    fn test_encode_uncompressed() {
        let pixels = vec![0u8; 64];

        let bytes = DDS::encode(&pixels.as_rgba().into(), (4, 4), Compression::None).unwrap();

        let dds = DDS::decode(&mut Cursor::new(bytes)).unwrap();

        assert_eq!(dds.layers.len(), 1);
        assert_eq!(pixels, dds.layers[0].as_bytes());
    }

    #[test]
    fn test_encode_uncompressed_rectangular() {
        let pixels: Vec<_> = (0u8..128).collect();

        let bytes = DDS::encode(&pixels.as_rgba().into(), (4, 8), Compression::None).unwrap();

        let dds = DDS::decode(&mut Cursor::new(bytes)).unwrap();

        assert_eq!(dds.layers.len(), 1);
        assert_eq!(pixels, dds.layers[0].as_bytes());
    }

    #[test]
    fn test_dds_vs_png() {
        let filenames = [
            "dxt1",
            "dxt5",
            "qt/DXT1",
            "qt/DXT2",
            "qt/DXT3",
            "qt/DXT4",
            "qt/DXT5",
            "qt/A8R8G8B8",
            "qt/A8R8G8B8.2",
        ];

        for filename in filenames.iter() {
            compare_dds_to_png(
                format!("../examples/{}.dds", filename),
                format!("../examples/{}.png", filename),
            );
        }
    }
}
