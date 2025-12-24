// Quick test to verify the biquad filter math
fn main() {
    let sample_rate = 48000.0_f32;
    let frequency = 1000.0_f32;
    let gain_db = 0.0_f32; // Flat response
    let q = 1.0_f32;
    
    // Calculate coefficients using the same formula as in equalizer.rs
    let a = 10_f32.powf(gain_db / 40.0);
    let omega = 2.0 * std::f32::consts::PI * frequency / sample_rate;
    let sin_omega = omega.sin();
    let cos_omega = omega.cos();
    let alpha = sin_omega / (2.0 * q);

    let b0 = 1.0 + alpha * a;
    let b1 = -2.0 * cos_omega;
    let b2 = 1.0 - alpha * a;
    let a0 = 1.0 + alpha / a;
    let a1 = -2.0 * cos_omega;
    let a2 = 1.0 - alpha / a;

    // Normalized coefficients
    let c_a0 = b0 / a0;
    let c_a1 = b1 / a0;
    let c_a2 = b2 / a0;
    let c_b1 = -a1 / a0;  // Note the negation
    let c_b2 = -a2 / a0;  // Note the negation

    println!("Biquad coefficients for flat EQ (0 dB at 1kHz):");
    println!("  a0 (gain): {}", c_a0);
    println!("  a1:        {}", c_a1);
    println!("  a2:        {}", c_a2);
    println!("  b1:        {}", c_b1);
    println!("  b2:        {}", c_b2);
    println!("");
    
    // For flat EQ, with 0 dB gain, a=1, so the filter should be unity:
    // When a=1: b0=1+alpha, b2=1-alpha, a0=1+alpha, a2=1-alpha
    // So b0/a0 = 1 (unity), b2/a0 = (1-alpha)/(1+alpha), etc.
    println!("At DC (z=1), the gain should be 1.0:");
    let dc_gain = (c_a0 + c_a1 + c_a2) / (1.0 - c_b1 - c_b2);
    println!("  DC gain: {}", dc_gain);
    
    // Test the filter with a simple sine wave
    println!("");
    println!("Testing with 1kHz sine wave input:");
    
    let mut z1 = 0.0_f32;
    let mut z2 = 0.0_f32;
    let mut input_sum = 0.0_f32;
    let mut output_sum = 0.0_f32;
    
    for i in 0..480 {  // 10ms at 48kHz
        let input = (2.0 * std::f32::consts::PI * 1000.0 * (i as f32) / 48000.0).sin();
        
        // Process through biquad
        let output = input * c_a0 + z1;
        z1 = input * c_a1 + z2 - c_b1 * output;
        z2 = input * c_a2 - c_b2 * output;
        
        input_sum += input.abs();
        output_sum += output.abs();
    }
    
    println!("  Input RMS: {:.4}", input_sum / 480.0);
    println!("  Output RMS: {:.4}", output_sum / 480.0);
    println!("  Ratio: {:.4}", output_sum / input_sum);
    
    // Test master gain
    let master_gain_db = 0.0_f32 + 3.0; // +3 dB boost
    let master_gain_linear = 10_f32.powf(master_gain_db / 20.0);
    println!("");
    println!("Master gain (+3 dB boost):");
    println!("  Linear gain: {:.4}", master_gain_linear);
}
