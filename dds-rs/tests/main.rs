extern crate dds;
extern crate rgb;


use dds::{DDS, Compression};
use std::fs::File;
use std::io::Cursor;
use rgb::{ComponentBytes, FromSlice};


const DXT1_LAYERS: &[&[u8]] = &[
    &[
        240,   0,   0, 255,  240,   0,   0, 255,  240,   0,   0, 255,  240,   0,   0, 255,
        240,  82,  80, 255,  240,  82,  80, 255,  240,  82,  80, 255,  240,  82,  80, 255,
        240, 165, 160, 255,  240, 165, 160, 255,  240, 165, 160, 255,  240, 165, 160, 255,
        240, 248, 240, 255,  240, 248, 240, 255,  240, 248, 240, 255,  240, 248, 240, 255,
    ],
    &[
        240,  60,  56, 255,  240,  60,  56, 255,  240, 145, 141, 255,  240, 145, 141, 255,
        240, 188, 184, 255,  240, 188, 184, 255,  240, 145, 141, 255,  240, 145, 141, 255,
        240, 145, 141, 255,  240, 145, 141, 255,  240, 145, 141, 255,  240, 145, 141, 255,
        240, 145, 141, 255,  240, 145, 141, 255,  240, 145, 141, 255,  240, 145, 141, 255,
    ],
    &[
        242, 125, 120, 255,  245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,
        245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,
        245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,
        245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,  245, 154, 184, 255,
    ],
];


const DXT5_LAYERS: &[&[u8]] = &[
    &[
        240,   0,   0, 255, 240,   0,   0, 192, 240,   0,   0, 140, 240,   0,   0, 64,
        240,  82,  80, 255, 240,  82,  80, 192, 240,  82,  80, 140, 240,  82,  80, 64,
        240, 165, 160, 255, 240, 165, 160, 192, 240, 165, 160, 140, 240, 165, 160, 64,
        240, 248, 240, 255, 240, 248, 240, 192, 240, 248, 240, 140, 240, 248, 240, 64,
    ],
    &[
        240,  60,  56, 212, 240,  60,  56, 115, 240, 145, 141, 115, 240, 145, 141, 115,
        240, 188, 184, 212, 240, 188, 184, 115, 240, 145, 141, 115, 240, 145, 141, 115,
        240, 145, 141, 115, 240, 145, 141, 115, 240, 145, 141, 115, 240, 145, 141, 115,
        240, 145, 141, 115, 240, 145, 141, 115, 240, 145, 141, 115, 240, 145, 141, 115,
    ],
    &[
        242, 125, 120, 163, 245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163,
        245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163,
        245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163,
        245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163, 245, 154, 184, 163,
    ],
];


#[cfg(test)]
mod tests {
    use std::io::Read;
    use super::*;

    #[test]
    fn test_uncompressed() {
        let pixels = vec![0u8; 64];

        let bytes = DDS::encode(&pixels.as_rgba().into(), (4, 4), Compression::None).unwrap();

        let dds = DDS::decode(&mut Cursor::new(bytes)).unwrap();

        assert_eq!(dds.layers.len(), 1);
        assert_eq!(pixels, dds.layers[0].as_bytes());
    }

    #[test]
    fn test_uncompressed_rectangular() {
        let pixels: Vec<_> = (0u8..128).collect();

        let bytes = DDS::encode(&pixels.as_rgba().into(), (4, 8), Compression::None).unwrap();

        let dds = DDS::decode(&mut Cursor::new(bytes)).unwrap();

        assert_eq!(dds.layers.len(), 1);
        assert_eq!(pixels, dds.layers[0].as_bytes());
    }

    #[test]
    fn test_dxt1() {
        let mut buf = Vec::new();

        let mut file = File::open("../examples/dxt1.dds").expect("Couldn't find file!");

        file.read_to_end(&mut buf).unwrap();

        let dds = DDS::decode(&mut Cursor::new(buf.clone())).unwrap();

        assert_eq!(dds.layers.len(), 3);

        for i in 0..dds.layers.len() {
            assert_eq!(dds.layers[i], DXT1_LAYERS[i].as_rgba());
        }
    }

    #[test]
    fn test_dxt5() {
        let mut buf = Vec::new();

        let mut file = File::open("../examples/dxt5.dds").expect("Couldn't find file!");

        file.read_to_end(&mut buf).unwrap();

        let dds = DDS::decode(&mut Cursor::new(buf.clone())).unwrap();

        assert_eq!(dds.layers.len(), 3);

        for i in 0..dds.layers.len() {
            assert_eq!(dds.layers[i], DXT5_LAYERS[i].as_rgba());
        }
    }
}
