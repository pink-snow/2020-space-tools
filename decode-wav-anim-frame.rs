#!/usr/bin/env run-cargo-script

// This code is based on the code contributed by Discord user @aaaa1.
// https://raw.githubusercontent.com/zaitsev85/message-from-space/master/source/decode-wav.rs
// https://message-from-space.readthedocs.io/en/latest/radio-transmission-recording.html

// MIT License
// 
// Copyright (c) 2020 Pegovka Observatory
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

//! ```cargo
//! [dependencies]
//! png = "0.16"
//! hound = "3.4"
//! ```

fn main() {
    let input = std::env::args().collect::<Vec<_>>()[1].clone();
    let fname = std::env::args().collect::<Vec<_>>()[2].clone();
    let arg0 = std::env::args().collect::<Vec<_>>()[3].parse::<f64>().unwrap();
    let arg1 = std::env::args().collect::<Vec<_>>()[4].parse::<f64>().unwrap();

    let r = hound::WavReader::open(input).unwrap();
    let spec = r.spec();
    let samples: Vec<i16> = r.into_samples().map(Result::unwrap).collect();

    let freq = 600;
    let step = 2.0 * std::f32::consts::PI * freq as f32 / spec.sample_rate as f32;
    let xys: Vec<(f32, f32)> = samples.iter().copied().enumerate().map(|(i, s)| {
        let s = s as f32;
        let a = i as f32 * step;
        (a.cos() * s, a.sin() * s)
    }).collect();

    let mut axyz = vec![(0.0, 0.0)];
    for (x, y) in xys {
        let last = *axyz.last().unwrap();
        axyz.push((last.0 + x, last.1 + y));
    }

    let mut ds: Vec<f32> = axyz.iter().zip(axyz.iter().skip(1000)).map(|(xy1, xy2)| {
        let dx = xy1.0 - xy2.0;
        let dy = xy1.1 - xy2.1;
        dx * dx + dy * dy
    }).collect();
    let max = *ds.iter().max_by(|x, y| x.partial_cmp(y).unwrap()).unwrap();
    ds.iter_mut().for_each(|x| *x /= max);

    let width = 24usize;
    let height = 49usize;
    let scale = 8usize;

    let w = std::fs::File::create(fname).unwrap();
    let w = std::io::BufWriter::new(w);
    let mut encoder = png::Encoder::new(w, (width * scale) as u32, (height * scale) as u32);
    encoder.set_color(png::ColorType::Grayscale);
    encoder.set_depth(png::BitDepth::Eight);
    let mut w = encoder.write_header().unwrap();

    let mut data = vec![0u8; width * height * scale * scale];

    for (i, cell) in data.iter_mut().enumerate() {
        let ix = i % scale;
        let nx = i / scale % width;
        let iy = i / scale / width % scale;
        let ny = i / scale / width / scale;

        let p =
            (ix + iy * scale) +
            (nx + ny * width) * scale * scale;

        let pp = p as f64 / 20. / (scale * scale) as f64 * arg0 + arg1;

        let ppp = (pp * spec.sample_rate as f64) as usize;

        *cell = (ds.get(ppp).copied().unwrap_or(0.0) * 255.0) as u8;
    }
    w.write_image_data(&data).unwrap();
}
