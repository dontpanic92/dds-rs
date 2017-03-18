extern crate dds;

use std::io::Cursor;

use dds::{DDS, Compression};


#[test]
fn test_encode_decode() {
    let pixels = vec![[1, 2, 3, 4]; 16];

    let bytes = DDS::encode(&pixels, (4, 4), Compression::None).unwrap();

    let dds = DDS::decode(&mut Cursor::new(bytes)).unwrap();

    assert_eq!(pixels, dds.layers[0]);
}
