//! Handles decoding (and someday encoding) DirectDraw Surface files.
//!
//! # Examples
//!
//! ```rust
//! extern crate dds;
//!
//! use std::fs::File;
//! use std::io::BufReader;
//! use std::path::Path;
//!
//! use dds::DDS;
//!
//! fn main() {
//!     let file = File::open(Path::new("examples/assets/ground.dds")).unwrap();
//!     let mut reader = BufReader::new(file);
//!
//!     let dds = DDS::decode(&mut reader).unwrap();
//! }
//! ```

// General TODOs:
// - Handle Cubemaps/Volumes

#[macro_use]
extern crate serde_derive;
extern crate bincode;
extern crate serde;

use bincode::{serialize, deserialize, Infinite, ErrorKind};

use std::cmp;
use std::fmt;
use std::io;



/// Normalized internal RGBA pixel representation
pub type Pixel = [u8; 4];

/// Represents an error encountered while parsing a DDS file
#[derive(Debug)]
pub enum ParseError {
    IO(io::Error),
    Deserialize(Box<ErrorKind>),
    Parse(String),
}

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> ParseError {
        ParseError::IO(err)
    }
}

impl<'a> From<&'a str> for ParseError {
    fn from(err: &'a str) -> ParseError {
        ParseError::Parse(err.into())
    }
}

impl From<String> for ParseError {
    fn from(err: String) -> ParseError {
        ParseError::Parse(err.into())
    }
}

impl From<Box<ErrorKind>> for ParseError {
    fn from(err: Box<ErrorKind>) -> ParseError {
        ParseError::Deserialize(err)
    }
}

/// Represents an error encountered while encoding
/// a `Vec<Pixel>` into `Vec<u8>`.
#[derive(Debug)]
pub enum EncodeError {
    Encode(String),
}

impl<'a> From<&'a str> for EncodeError {
    fn from(err: &'a str) -> EncodeError {
        EncodeError::Encode(err.into())
    }
}

impl From<String> for EncodeError {
    fn from(err: String) -> EncodeError {
        EncodeError::Encode(err.into())
    }
}


/// Pixel information as represented in the DDS file
///
/// Direct translation of struct found here:
/// https://msdn.microsoft.com/en-us/library/bb943984.aspx
#[repr(C)]
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct RawPixelFormat {
    pub size: u32,
    pub flags: u32,
    pub four_cc: [u8; 4],
    pub rgb_bit_count: u32,
    pub red_bit_mask: u32,
    pub green_bit_mask: u32,
    pub blue_bit_mask: u32,
    pub alpha_bit_mask: u32,
}


/// Header as represented in the DDS file
///
/// Direct translation of struct found here:
/// https://msdn.microsoft.com/en-us/library/bb943982.aspx
#[repr(C)]
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct RawHeader {
    pub size: u32,
    pub flags: u32,
    pub height: u32,
    pub width: u32,
    pub pitch_or_linear_size: u32,
    pub depth: u32,
    pub mipmap_count: u32,
    pub reserved: [u32; 11],
    pub pixel_format: RawPixelFormat,
    pub caps: u32,
    pub caps2: u32,
    pub caps3: u32,
    pub caps4: u32,
    pub reserved2: u32,
}


/// Convenience enum for storing common pixel formats
///
/// See here for more information about the common formats:
/// https://msdn.microsoft.com/en-us/library/bb943991.aspx
#[derive(Debug)]
pub enum PixelFormat {
    A1R5G5B5,
    A2B10G10R10,
    A2R10G10B10,
    A4L4,
    A4R4G4B4,
    A8,
    A8B8G8R8,
    A8L8,
    A8R3G3B2,
    A8R8G8B8,
    G16R16,
    L16,
    L8,
    R5G6B5,
    R8G8B8,
    Unknown,
    X1R5G5B5,
    X4R4G4B4,
    X8B8G8R8,
    X8R8G8B8,
}

impl fmt::Display for PixelFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Represents the compression format of a DDS file,
/// aka the four-cc bytes.
#[derive(Debug)]
pub enum Compression {
    DXT1,
    DXT2,
    DXT3,
    DXT5,
    None,
    Other(u8, u8, u8, u8),
}

impl Compression {
    fn from_bytes(bytes: &[u8; 4]) -> Compression {
        match bytes {
            b"\x00\x00\x00\x00" => Compression::None,
            b"DXT1" => Compression::DXT1,
            b"DXT2" => Compression::DXT2,
            b"DXT3" => Compression::DXT3,
            b"DXT5" => Compression::DXT5,
            _ => Compression::Other(bytes[0], bytes[1], bytes[2], bytes[3]),
        }
    }
}

impl fmt::Display for Compression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Represents a parsed DDS header. Has several convenience attributes,
/// as well as a reference to the raw header.
#[derive(Debug)]
pub struct Header {
    /// Height of the main image
    pub height: u32,

    /// Width of the main image
    pub width: u32,

    /// How many levels of mipmaps there are
    pub mipmap_count: u32,

    /// Compression type used
    pub compression: Compression,

    /// The pixel format used
    pub pixel_format: PixelFormat,

    /// Raw header that byte-matches the DDS header format
    pub raw_header: RawHeader,
}

impl Header {
    // Returns layer sizes
    fn get_layer_sizes(&self) -> Vec<(usize, usize)> {
        // Files with only a single texture will often have
        // the mipmap count set to 0, so we force generating
        // at least a single level
        (0..cmp::max(self.mipmap_count, 1))
            .map(|i: u32| (
                (self.height / 2u32.pow(i)) as usize,
                (self.width / 2u32.pow(i)) as usize,
            ))
            .collect::<Vec<_>>()
    }

    /// Returns raw header
    pub fn get_raw(&self) -> &RawHeader {
        &self.raw_header
    }

    // Returns the number of bytes used per pixel
    fn get_pixel_bytes(&self) -> usize {
        self.raw_header.pixel_format.rgb_bit_count as usize / 8
    }
}


/// Represents a parsed DDS file
pub struct DDS {
    /// The parsed DDS header
    pub header: Header,

    /// Mipmap layers
    pub layers: Vec<Vec<Pixel>>,
}


impl DDS {
    // Parses some common pixel formats from the raw bit masks, for convenience
    fn parse_pixel_format(header: &RawHeader) -> PixelFormat {
        let p = &header.pixel_format;

        match (p.rgb_bit_count, p.red_bit_mask, p.green_bit_mask, p.blue_bit_mask, p.alpha_bit_mask) {
            (16,     0x7C00,      0x3E0,       0x1F,     0x8000) => PixelFormat::A1R5G5B5,
            (32,      0x3FF,    0xFFC00, 0x3FF00000, 0xC0000000) => PixelFormat::A2B10G10R10,
            (32, 0x3FF00000,    0xFFC00,      0x3FF, 0xC0000000) => PixelFormat::A2R10G10B10,
            ( 8,        0xF,        0x0,        0x0,       0xF0) => PixelFormat::A4L4,
            (16,      0xF00,       0xF0,        0xF,     0xF000) => PixelFormat::A4R4G4B4,
            ( 8,        0x0,        0x0,        0x0,       0xFF) => PixelFormat::A8,
            (32,       0xFF,     0xFF00,   0xFF0000, 0xFF000000) => PixelFormat::A8B8G8R8,
            (16,       0xFF,        0x0,        0x0,     0xFF00) => PixelFormat::A8L8,
            (16,       0xE0,       0x1C,        0x3,     0xFF00) => PixelFormat::A8R3G3B2,
            (32,   0xFF0000,     0xFF00,       0xFF, 0xFF000000) => PixelFormat::A8R8G8B8,
            (32,     0xFFFF, 0xFFFF0000,        0x0,        0x0) => PixelFormat::G16R16,
            (16,     0xFFFF,        0x0,        0x0,        0x0) => PixelFormat::L16,
            ( 8,       0xFF,        0x0,        0x0,        0x0) => PixelFormat::L8,
            (16,     0xF800,      0x7E0,       0x1F,        0x0) => PixelFormat::R5G6B5,
            (24,   0xFF0000,     0xFF00,       0xFF,        0x0) => PixelFormat::R8G8B8,
            (16,     0x7C00,      0x3E0,       0x1F,        0x0) => PixelFormat::X1R5G5B5,
            (16,      0xF00,       0xF0,        0xF,        0x0) => PixelFormat::X4R4G4B4,
            (32,       0xFF,     0xFF00,   0xFF0000,        0x0) => PixelFormat::X8B8G8R8,
            (32,   0xFF0000,     0xFF00,       0xFF,        0x0) => PixelFormat::X8R8G8B8,
            ( _,          _,          _,          _,          _) => PixelFormat::Unknown,
        }
    }

    /// Parses a `Header` object from a raw `u8` buffer.
    pub fn parse_header<R: io::Read>(buf: &mut R) -> Result<Header, ParseError> {
        let mut magic_buf = [0; 4];
        let mut header_buf = [0u8; 124];

        buf.read_exact(&mut magic_buf[..]).expect("Not enough bytes to read magic bytes!");

        // If the file doesn't start with `DDS `, abort decoding
        if &magic_buf != b"DDS " {
            return Err(format!("Expected the file to start with `DDS `, got `{}` instead.", String::from_utf8_lossy(&magic_buf)).into());
        }

        buf.read_exact(&mut header_buf[..]).expect("Not enough bytes to read header!");

        let raw_header: RawHeader = deserialize(&header_buf[..])?;

        Ok(Header {
            height: raw_header.height,
            width: raw_header.width,
            mipmap_count: raw_header.mipmap_count,
            compression: Compression::from_bytes(&raw_header.pixel_format.four_cc),
            pixel_format: DDS::parse_pixel_format(&raw_header),
            raw_header: raw_header,
        })
    }

    // Handles decoding an uncompressed buffer into a series of mipmap images
    fn decode_uncompressed(header: &Header, data_buf: &mut Vec<u8>) -> Vec<Vec<Pixel>> {
        let layer_sizes = header.get_layer_sizes();

        // Take the vec of layer sizes and map to a vec of layers
        layer_sizes
        .iter()
        .map(|&(h, w)| {
            data_buf
            // Remove the pixels we care about from the buffer
            .drain(..h * w * header.get_pixel_bytes())
            .collect::<Vec<_>>()
            // Chunk into groups of 3 or 4, then convert to normalized [u8; 4] RGBA format
            .chunks(header.get_pixel_bytes())
            .map(|p| {
                let pixel: u32 = p[0] as u32 + ((p[1] as u32) << 8) + ((p[2] as u32) << 16) + ((p[3] as u32) << 24);

                let red = header.raw_header.pixel_format.red_bit_mask;
                let green = header.raw_header.pixel_format.green_bit_mask;
                let blue = header.raw_header.pixel_format.blue_bit_mask;
                let alpha = header.raw_header.pixel_format.alpha_bit_mask;

                [
                    ((pixel & red) >> red.trailing_zeros()) as u8,
                    ((pixel & green) >> green.trailing_zeros()) as u8,
                    ((pixel & blue) >> blue.trailing_zeros()) as u8,
                    ((pixel & alpha) >> alpha.trailing_zeros()) as u8,
                ]
            })
            .collect()
        })
        .collect()
    }

    // Handles decoding a DXT1-compressed 64-bit buffer into 16 pixels
    fn bytes_to_pixels_dxt1(bytes: &[u8]) -> Vec<Pixel> {
        // Convert to `u32` to allow overflow for arithmetic below
        let color0 = (((bytes[1] as u16) << 8) + bytes[0] as u16) as u32;
        let color1 = (((bytes[3] as u16) << 8) + bytes[2] as u16) as u32;

        // Iterate through each pair of bits in each `code` byte to
        // determine the color for each pixel
        bytes[4..]
        .iter()
        .rev()
        .flat_map(|&code| {
            (0..4)
            .map(|i| {
                // Implements this lookup table for calculating pixel colors.
                // Used on a per channel basis, except for calculating
                // `color0 > color1`.
                //
                // code | color0 > color1 | color0 <= color1
                // -----------------------------------------
                //   0  |       c0        |       c0
                //   1  |       c1        |       c1
                //   2  | (2*c0 + c1) / 3 |  (c0 + c1) / 2
                //   3  | (c0 + 2*c1) / 3 |      black
                let lookup = |c0: u32, c1| -> u32 {
                    match (color0 > color1, (code >> (i * 2)) & 0x3) {
                        ( true, 0) => c0,
                        ( true, 1) => c1,
                        ( true, 2) => (2 * c0 + c1) / 3,
                        ( true, 3) => (c0 + 2 * c1) / 3,
                        (false, 0) => c0,
                        (false, 1) => c1,
                        (false, 2) => (c0 + c1) / 2,
                        (false, 3) => 0,
                        _ => unreachable!(),
                    }
                };

                let red0 = (color0 & 0xF800) >> 11;
                let red1 = (color1 & 0xF800) >> 11;
                let green0 = (color0 & 0x7E0) >> 5;
                let green1 = (color1 & 0x7E0) >> 5;
                let blue0 = color0 & 0x1F;
                let blue1 = color1 & 0x1F;

                // After colors have been calculated, inflate from 5/6-bit
                // to 8-bit by multipling by 8/4, respectively. No alpha
                // channel is used in DXT1 compression, so set to 100%
                // across the board.
                [
                    lookup(8 * red0, 8 * red1) as u8,
                    lookup(4 * green0, 4 * green1) as u8,
                    lookup(8 * blue0, 8 * blue1) as u8,
                    255
                ]
            })
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    }

    // Handles decoding a DXT3-compressed 128-bit buffer into 16 pixels
    fn bytes_to_pixels_dxt3(bytes: &[u8]) -> Vec<Pixel> {
        // Convert to `u32` to allow overflow for arithmetic below
        let color0 = (((bytes[9] as u16) << 8) + bytes[8] as u16) as u32;
        let color1 = (((bytes[11] as u16) << 8) + bytes[10] as u16) as u32;

        // Iterate through each pair of bits in each `code` byte to
        // determine the color for each pixel
        bytes[12..]
        .iter()
        .rev()
        .enumerate()
        .flat_map(|(i, &code)| {
            (0..4)
            .map(|j| {
                let lookup = |c0: u32, c1| -> u32 {
                    match (code >> (j * 2)) & 0x3 {
                        0 => c0,
                        1 => c1,
                        2 => (2 * c0 + c1) / 3,
                        3 => (c0 + 2 * c1) / 3,
                        _ => unreachable!(),
                    }
                };

                let alpha_nibble = (bytes[2 * (3 - i) + j / 2] >> (4 * (j % 2))) & 0xF;

                let red0 = (color0 & 0xF800) >> 11;
                let red1 = (color1 & 0xF800) >> 11;
                let green0 = (color0 & 0x7E0) >> 5;
                let green1 = (color1 & 0x7E0) >> 5;
                let blue0 = color0 & 0x1F;
                let blue1 = color1 & 0x1F;

                // After colors and alpha have been calculated, inflate from 4/5/6-bit
                // to 8-bit by multipling by 16/8/4, respectively.
                [
                    lookup(8 * red0, 8 * red1) as u8,
                    lookup(4 * green0, 4 * green1) as u8,
                    lookup(8 * blue0, 8 * blue1) as u8,
                    16 * alpha_nibble,
                ]
            })
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    }

    // Handles decoding a DXT5-compressed 128-bit buffer into 16 pixels
    fn bytes_to_pixels_dxt5(bytes: &[u8]) -> Vec<Pixel> {
        let color0 = (((bytes[9] as u16) << 8) + bytes[8] as u16) as u32;
        let color1 = (((bytes[11] as u16) << 8) + bytes[10] as u16) as u32;

        let alpha0 = bytes[0] as u32;
        let alpha1 = bytes[1] as u32;

        // Convert 6 u8's into a single 48 bit number, to make it easier to grab 3-bit
        // chunks out of them.
        let alpha_info = bytes[2..8]
        .iter()
        .enumerate()
        .fold(0u64, |memo, (i, &x)| memo + ((x as u64) << 8 * i) as u64);

        bytes[12..]
        .iter()
        .rev()
        .enumerate()
        .flat_map(|(i, &code)| {
            (0..4)
            .map(|j| {
                // Implements this lookup table for calculating pixel colors.
                // Used on a per channel basis, except for calculating
                // `color0 > color1`.
                //
                // code |      value      |
                // ------------------------
                //   0  |       c0        |
                //   1  |       c1        |
                //   2  | (2*c0 + c1) / 3 |
                //   3  | (c0 + 2*c1) / 3 |
                let lookup = |c0: u32, c1| -> u32 {
                    match (code >> (j * 2)) & 0x3 {
                        0 => c0,
                        1 => c1,
                        2 => (2 * c0 + c1) / 3,
                        3 => (c0 + 2 * c1) / 3,
                        _ => unreachable!(),
                    }
                };

                let red0 = (color0 & 0xF800) >> 11;
                let red1 = (color1 & 0xF800) >> 11;
                let green0 = (color0 & 0x7E0) >> 5;
                let green1 = (color1 & 0x7E0) >> 5;
                let blue0 = color0 & 0x1F;
                let blue1 = color1 & 0x1F;


                // Interpolate between two given alpha values based on the 3-bit lookup value
                // stored in `alpha_info`.
                let alpha = match (alpha0 > alpha1, (alpha_info >> (3 * (4 * (3 - i) + j))) & 0x07) {
                    (true, 0) => alpha0,
                    (true, 1) => alpha1,
                    (true, 2) => (6 * alpha0 + 1 * alpha1) / 7,
                    (true, 3) => (5 * alpha0 + 2 * alpha1) / 7,
                    (true, 4) => (4 * alpha0 + 3 * alpha1) / 7,
                    (true, 5) => (3 * alpha0 + 4 * alpha1) / 7,
                    (true, 6) => (2 * alpha0 + 5 * alpha1) / 7,
                    (true, 7) => (1 * alpha0 + 6 * alpha1) / 7,
                    (false, 0) => alpha0,
                    (false, 1) => alpha1,
                    (false, 2) => (4 * alpha0 + 1 * alpha1) / 5,
                    (false, 3) => (3 * alpha0 + 2 * alpha1) / 5,
                    (false, 4) => (2 * alpha0 + 3 * alpha1) / 5,
                    (false, 5) => (1 * alpha0 + 4 * alpha1) / 5,
                    (false, 6) => 0,
                    (false, 7) => 255,
                    t @ _ => unreachable!(format!("This value should not have occurred: `{:?}`", t)),
                };

                [
                    lookup(8 * red0, 8 * red1) as u8,
                    lookup(4 * green0, 4 * green1) as u8,
                    lookup(8 * blue0, 8 * blue1) as u8,
                    alpha as u8,
                ]
            })
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    }

    // Handles decoding a DXT1-5 compressed buffer into a series of mipmap images
    // TODO: Decode non-square images
    // TODO: Handle DXT1 1-bit alpha variant
    // TODO: Handle DXT2 and DXT4 images
    fn decode_dxt(header: &Header, data_buf: &mut Vec<u8>) -> Vec<Vec<Pixel>> {
        let layer_sizes = header.get_layer_sizes();

        // Take the vec of layer sizes and map to a vec of layers
        layer_sizes
        .iter()
        .map(|&(height, width)| {
            let h = cmp::max(height, 4);
            let w = cmp::max(width, 4);

            // DXT1 compression uses 64 bits per 16 pixels, while DXT3-5 use 128 bits.
            // Calculate how many total bytes to read out of the buffer for each layer
            // here, as well as how big each individual chunk size is.
            let (layer_bytes, chunk_size) = match header.compression {
                Compression::DXT1 => (h * w / 2, 8),
                _ => (h * w, 16),
            };

            data_buf
            // Remove the pixels we care about from the buffer
            .drain(..layer_bytes)
            .collect::<Vec<_>>()
             // Chunk into blocks of appropriate size
            .chunks(chunk_size)
            // Turn those blocks into 16 `[u8; 4]` pixels, and flatten into a
            // vec of pixels for the entire image. Follow here for the dirty details:
            // https://www.khronos.org/opengl/wiki/S3_Texture_Compression
            .flat_map(match header.compression {
                Compression::DXT1 => DDS::bytes_to_pixels_dxt1,
                Compression::DXT2 | Compression::DXT3 => DDS::bytes_to_pixels_dxt3,
                Compression::DXT5 => DDS::bytes_to_pixels_dxt5,
                _ => unreachable!(format!("This function cannot handle `{:?}` images", header.compression)),
            })
            .collect::<Vec<_>>()
            // Since the 16 byte pixel blocks are actually 4x4 texels, group image
            // into chunks of four rows each, and then transpose into a row of texels.
            .chunks(4 * w)
            .flat_map(|p| {
                let mut pixels = Vec::new();

                for i in (0..4).rev() {
                    for j in 0..w / 4 {
                        pixels.push(p[(i + j * 4) * 4 + 0]);
                        pixels.push(p[(i + j * 4) * 4 + 1]);
                        pixels.push(p[(i + j * 4) * 4 + 2]);
                        pixels.push(p[(i + j * 4) * 4 + 3]);
                    }
                }

                pixels
            })
            // And that's it!
            .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
    }

    /// Decodes a buffer into a header and a series of mipmap images.
    /// Handles uncompressed and DXT1-5 compressed images.
    pub fn decode<R: io::Read>(buf: &mut R) -> Result<DDS, ParseError> {
        let header = DDS::parse_header(buf)?;

        let mut data_buf = Vec::new();
        buf.read_to_end(&mut data_buf)?;

        let layers = match header.compression {
            Compression::None => {
                DDS::decode_uncompressed(&header, &mut data_buf)
            }
            Compression::DXT1 | Compression::DXT3 | Compression::DXT5 => {
                DDS::decode_dxt(&header, &mut data_buf)
            }
            _ => {
                let compression_type = String::from_utf8_lossy(&header.raw_header.pixel_format.four_cc[..]);
                return Err(format!("The compression type `{}` is unsupported!", compression_type).into());
            }
        };

        Ok(DDS {
            header: header,
            layers: layers,
        })
    }

    /// Encodes a series of Pixels as a bunch of bytes, suitable for writing to disk, etc.
    /// Currently only supports uncompressed RGBA images
    pub fn encode(pixels: &Vec<Pixel>, size: (u32, u32), compression: Compression) -> Result<Vec<u8>, EncodeError> {
        // Make sure we don't have more/less data than claimed
        if size.0 * size.1 < pixels.len() as u32 {
            return Err(format!("Found {} extra bytes!", pixels.len() as u32 - size.0 * size.1).into());
        }

        if size.0 * size.1 > pixels.len() as u32 {
            return Err(format!("Need {} extra bytes!", size.0 * size.1 - pixels.len() as u32).into());
        }

        // TODO: Remove this limitation
        if size.0 != size.1 {
            return Err("Non-square images not yet supported!".into());
        }

        match compression {
            Compression::None => {
                // Start with magic number
                let mut data = b"DDS ".to_vec();

                // Add header to buffer
                data.extend(serialize(&RawHeader {
                    size: size.0 * size.1 * 4,
                    flags: 0,
                    height: size.0,
                    width: size.1,
                    pitch_or_linear_size: 0,
                    depth: 0,
                    mipmap_count: 0,
                    reserved: [0; 11],
                    pixel_format: RawPixelFormat {
                        size: 0,
                        flags: 0x41,
                        four_cc: [0; 4],
                        rgb_bit_count: 32,
                        red_bit_mask: 0xFF,
                        green_bit_mask: 0xFF00,
                        blue_bit_mask: 0xFF0000,
                        alpha_bit_mask: 0xFF000000,
                    },
                    caps: 0,
                    caps2: 0,
                    caps3: 0,
                    caps4: 0,
                    reserved2: 0,
                }, Infinite).unwrap());

                // Add layers to buffer
                data.extend(pixels.iter().flat_map(|p| p).collect::<Vec<_>>());

                Ok(data)
            }
            _ => Err(format!("Cannot encode {:?}!", compression).into())
        }

    }
}
