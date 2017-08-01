#![feature(test)]

extern crate test;
extern crate dds;

use dds::DDS;
use std::fs::File;
use std::io::Cursor;
use test::Bencher;


#[cfg(test)]
mod tests {
    use std::io::Read;
    use super::*;

    #[bench]
    fn bench_decode(b: &mut Bencher) {
        let mut buf = Vec::new();
        let mut file = File::open("examples/assets/ground.dds").expect("Couldn't find file!");

        file.read_to_end(&mut buf).unwrap();

        b.iter(|| {
            DDS::decode(&mut Cursor::new(buf.clone())).unwrap()
        })
    }
}
