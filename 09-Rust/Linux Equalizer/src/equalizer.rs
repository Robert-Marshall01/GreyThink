use serde::{Deserialize, Serialize};

/// Represents a single frequency band in the equalizer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Band {
    pub frequency: f32,
    pub gain: f32, // in dB
    pub q: f32,    // Quality factor (bandwidth)
}

/// The main equalizer state containing all bands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EqualizerState {
    pub bands: Vec<Band>,
    pub enabled: bool,
    pub master_gain: f32,
    
    // Direct access for LADSPA control mapping
    #[serde(skip)]
    cached_values: bool,
}

impl EqualizerState {
    pub fn new() -> Self {
        Self {
            bands: vec![
                Band { frequency: 32.0, gain: 0.0, q: 1.0 },
                Band { frequency: 64.0, gain: 0.0, q: 1.0 },
                Band { frequency: 125.0, gain: 0.0, q: 1.0 },
                Band { frequency: 250.0, gain: 0.0, q: 1.0 },
                Band { frequency: 500.0, gain: 0.0, q: 1.0 },
                Band { frequency: 1000.0, gain: 0.0, q: 1.0 },
                Band { frequency: 2000.0, gain: 0.0, q: 1.0 },
                Band { frequency: 4000.0, gain: 0.0, q: 1.0 },
                Band { frequency: 8000.0, gain: 0.0, q: 1.0 },
                Band { frequency: 16000.0, gain: 0.0, q: 1.0 },
            ],
            enabled: true,
            master_gain: 0.0,
            cached_values: false,
        }
    }

    pub fn set_band_gain(&mut self, index: usize, gain: f32) {
        if index < self.bands.len() {
            self.bands[index].gain = gain.clamp(-12.0, 12.0);
        }
    }

    pub fn reset(&mut self) {
        for band in &mut self.bands {
            band.gain = 0.0;
        }
        self.master_gain = 0.0;
    }

    // Direct band accessors for LADSPA control string generation
    pub fn band_32hz(&self) -> f32 { self.bands.get(0).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_64hz(&self) -> f32 { self.bands.get(1).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_125hz(&self) -> f32 { self.bands.get(2).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_250hz(&self) -> f32 { self.bands.get(3).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_500hz(&self) -> f32 { self.bands.get(4).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_1khz(&self) -> f32 { self.bands.get(5).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_2khz(&self) -> f32 { self.bands.get(6).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_4khz(&self) -> f32 { self.bands.get(7).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_8khz(&self) -> f32 { self.bands.get(8).map(|b| b.gain).unwrap_or(0.0) }
    pub fn band_16khz(&self) -> f32 { self.bands.get(9).map(|b| b.gain).unwrap_or(0.0) }
}

/// Biquad filter implementation for each band
/// Uses Direct Form II Transposed structure for numerical stability
pub struct BiquadFilter {
    // Feedforward coefficients (numerator)
    b0: f32,
    b1: f32,
    b2: f32,
    // Feedback coefficients (denominator, normalized so a0=1)
    a1: f32,
    a2: f32,
    // State variables
    z1: f32,
    z2: f32,
}

impl BiquadFilter {
    pub fn new_peaking_eq(sample_rate: f32, frequency: f32, gain_db: f32, q: f32) -> Self {
        let mut filter = Self {
            b0: 1.0, b1: 0.0, b2: 0.0,
            a1: 0.0, a2: 0.0,
            z1: 0.0, z2: 0.0,
        };
        filter.calculate_coefficients(sample_rate, frequency, gain_db, q);
        filter
    }
    
    fn calculate_coefficients(&mut self, sample_rate: f32, frequency: f32, gain_db: f32, q: f32) {
        // Using RBJ Audio EQ Cookbook formulas for peaking EQ
        let a = 10_f32.powf(gain_db / 40.0); // Amplitude
        let omega = 2.0 * std::f32::consts::PI * frequency / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        // Numerator coefficients (b)
        let b0 = 1.0 + alpha * a;
        let b1 = -2.0 * cos_omega;
        let b2 = 1.0 - alpha * a;
        
        // Denominator coefficients (a) - a0 will be normalized to 1
        let a0 = 1.0 + alpha / a;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha / a;

        // Normalize by a0 (so denominator a0 = 1)
        self.b0 = b0 / a0;
        self.b1 = b1 / a0;
        self.b2 = b2 / a0;
        self.a1 = a1 / a0;  // Store as-is, NOT negated
        self.a2 = a2 / a0;  // Store as-is, NOT negated
    }
    
    /// Update filter coefficients WITHOUT resetting state
    pub fn update_coefficients(&mut self, sample_rate: f32, frequency: f32, gain_db: f32, q: f32) {
        self.calculate_coefficients(sample_rate, frequency, gain_db, q);
        // Do NOT reset z1 and z2 - preserve filter state!
    }

    pub fn process(&mut self, input: f32) -> f32 {
        // Direct Form II Transposed:
        // y[n] = b0*x[n] + z1
        // z1 = b1*x[n] - a1*y[n] + z2
        // z2 = b2*x[n] - a2*y[n]
        let output = self.b0 * input + self.z1;
        self.z1 = self.b1 * input - self.a1 * output + self.z2;
        self.z2 = self.b2 * input - self.a2 * output;
        output
    }

    pub fn reset(&mut self) {
        self.z1 = 0.0;
        self.z2 = 0.0;
    }
}

/// Process audio samples through the equalizer
pub struct Equalizer {
    filters_left: Vec<BiquadFilter>,
    filters_right: Vec<BiquadFilter>,
    sample_rate: f32,
    master_gain_db: f32,
}

impl Equalizer {
    pub fn new(sample_rate: f32, state: &EqualizerState) -> Self {
        let filters_left = state
            .bands
            .iter()
            .map(|band| BiquadFilter::new_peaking_eq(sample_rate, band.frequency, band.gain, band.q))
            .collect();

        let filters_right = state
            .bands
            .iter()
            .map(|band| BiquadFilter::new_peaking_eq(sample_rate, band.frequency, band.gain, band.q))
            .collect();

        Self {
            filters_left,
            filters_right,
            sample_rate,
            master_gain_db: state.master_gain,
        }
    }

    pub fn update_from_state(&mut self, state: &EqualizerState) {
        // Only update filter coefficients, DON'T recreate filters!
        // This preserves filter state and prevents audio glitches
        for (i, band) in state.bands.iter().enumerate() {
            if i < self.filters_left.len() {
                self.filters_left[i].update_coefficients(self.sample_rate, band.frequency, band.gain, band.q);
                self.filters_right[i].update_coefficients(self.sample_rate, band.frequency, band.gain, band.q);
            }
        }
        self.master_gain_db = state.master_gain;
    }

    pub fn process_stereo(&mut self, left: f32, right: f32) -> (f32, f32) {
        let mut left_out = left;
        let mut right_out = right;

        for i in 0..self.filters_left.len() {
            left_out = self.filters_left[i].process(left_out);
            right_out = self.filters_right[i].process(right_out);
        }

        // Apply master gain (convert dB to linear)
        let master_gain_linear = 10_f32.powf(self.master_gain_db / 20.0);
        left_out *= master_gain_linear;
        right_out *= master_gain_linear;

        (left_out, right_out)
    }
}
