// Lightweight tests for math helpers in math_rpg.cpp
// This test file temporarily renames main in math_rpg.cpp when included
#include <iostream>
#include <cmath>
#include <string>

// Rename main in the included file so we can call its helpers directly
#define main math_rpg_main_for_test
#include "../math_rpg.cpp"
#undef main

using namespace std;

int main() {
    int failed = 0;
    double v;

    // Test parse_number_with_pi basic cases
    if (!parse_number_with_pi("pi", v) || fabs(v - PI) > 1e-9) {
        cerr << "FAIL: parse 'pi' -> expected PI, got " << v << "\n";
        failed++;
    }
    if (!parse_number_with_pi("3pi/2", v) || fabs(v - 3.0*PI/2.0) > 1e-9) {
        cerr << "FAIL: parse '3pi/2' -> expected 3pi/2, got " << v << "\n";
        failed++;
    }
    if (!parse_number_with_pi("2.5", v) || fabs(v - 2.5) > 1e-9) {
        cerr << "FAIL: parse '2.5' -> expected 2.5, got " << v << "\n";
        failed++;
    }

    // Edge cases: negative pi, unicode pi, fraction before pi
    if (!parse_number_with_pi("-pi", v) || fabs(v + PI) > 1e-9) {
        cerr << "FAIL: parse '-pi' -> expected -PI, got " << v << "\n";
        failed++;
    }
    // unicode pi U+03C0 (UTF-8 0xCF 0x80) — make sure parser handles it
    string u = "\xCF\x80"; // UTF-8 sequence for 'π'
    double uv = 0.0;
    if (!parse_number_with_pi(u, uv) || fabs(uv - PI) > 1e-6) {
        cerr << "FAIL: parse unicode pi -> expected PI, got " << uv << "\n";
        failed++;
    }
    if (!parse_number_with_pi("3/2pi", v) || fabs(v - 3.0*PI/2.0) > 1e-9) {
        cerr << "FAIL: parse '3/2pi' -> expected 3pi/2, got " << v << "\n";
        failed++;
    }


    // Test rad_to_string for a few canonical angles
    string s1 = rad_to_string(PI/2.0, 6);
    if (s1.find("pi/2") == string::npos && s1.find("π/2") == string::npos) {
        cerr << "FAIL: rad_to_string(pi/2) -> expected containing 'pi/2' or 'π/2', got '" << s1 << "'\n";
        failed++;
    }

    string s2 = rad_to_string(PI, 6);
    if (s2.find("pi") == string::npos && s2.find("π") == string::npos) {
        cerr << "FAIL: rad_to_string(pi) -> expected containing 'pi' or 'π', got '" << s2 << "'\n";
        failed++;
    }

    // Sanity: rad_to_string of non-fraction returns decimal with 'rad'
    string s3 = rad_to_string(0.123456, 4);
    if (s3.find("rad") == string::npos) {
        cerr << "FAIL: rad_to_string(decimal) -> expected 'rad' suffix, got '" << s3 << "'\n";
        failed++;
    }

    if (failed == 0) cout << "ALL TESTS PASSED\n";
    else cout << failed << " test(s) failed\n";
    return (failed == 0) ? 0 : 1;
}
