// Math RPG - Trigonometry quiz
// SPDX-License-Identifier: MIT
// Copyright (c) 2025 Robert Marshall
// Player starts with 3000 HP. Each stick in the bar = 100 HP (30 sticks total).
// Wrong answers cost 1400 HP * multiplier between 0.5 and 2.0 (700 - 2800 HP).
// HP bar uses ceil to display sticks (always round up to the next 100).
// Bar color: green >50%, yellow <=50% and >20%, red <=20%.

#include <iostream>
#include <cmath>
#include <random>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <limits>
#include <iomanip>
#include <sstream>
#include <chrono>
#include <cstdlib>
using namespace std;

const int MAX_HP = 3000;
const int STICK_HP = 100;
const int TOTAL_STICKS = MAX_HP / STICK_HP; // 30

const double PI = acos(-1.0);

// ANSI color codes (may work on modern Windows terminals)
const string COLOR_RESET = "\x1b[0m";
const string COLOR_GREEN = "\x1b[32m";
const string COLOR_YELLOW = "\x1b[33m";
const string COLOR_RED = "\x1b[31m";

// Utility: clamp
template<typename T>
T clamp_t(T v, T lo, T hi) { return v < lo ? lo : (v > hi ? hi : v); }

// Format a radian value to a short string with units (e.g. "0.7854 rad").
string rad_to_string(double r, int prec=4) {
	// Try to represent as a rational multiple of PI when possible.
	const double x = r / PI;
	const double tol = 1e-8;
	// Common denominators to try (covers typical unit circle fractions)
	int dens[] = {1,2,3,4,6,8,12,24};
	for (int d : dens) {
		double numd = round(x * d);
		double approx = numd / d;
		if (fabs(x - approx) < tol) {
			long long num = (long long) llround(numd);
			long long den = d;
			if (num == 0) return string("0 rad");
			long long g = std::gcd(std::llabs(num), den);
			num /= g; den /= g;
			// format numerator/pi
			ostringstream oss;
			if (den == 1) {
				if (num == 1) oss << "π";
				else if (num == -1) oss << "-π";
				else oss << num << "π";
			} else {
				// show as (num)π/den
				oss << num << "π/" << den;
			}
			oss << " rad";
			return oss.str();
		}
	}
	// fallback: decimal
	ostringstream oss;
	oss << fixed << setprecision(prec) << r;
	return oss.str() + " rad";
}

// Parse a numeric token that may contain 'pi' (or unicode pi) and simple fractions.
// Examples accepted: "3pi/2", "pi/4", "-pi", "3*pi/2", "3/2pi", "2.356"
bool parse_number_with_pi(const string &raw, double &out_val) {
	string s = raw;
	// trim spaces
	auto ltrim = [](string &str){ size_t p = str.find_first_not_of(" \t\n\r"); if (p!=string::npos) str.erase(0,p); else str.clear(); };
	auto rtrim = [](string &str){ size_t p = str.find_last_not_of(" \t\n\r"); if (p!=string::npos) str.erase(p+1); else str.clear(); };
	ltrim(s); rtrim(s);
	if (s.size() == 0) return false;
	// replace unicode pi with ascii "pi"
	for (char &c : s) if ((unsigned char)c == 0xCF || (unsigned char)c == 0xCE) { /* noop for multibyte start */ }
	// simple replacement for common utf8 sequence of pi: 0xCF 0x80 -> "pi"
	// but easier: replace any occurrence of the Greek small letter pi (U+03C0) encoded in UTF-8
	// by searching for byte sequence \xCF\x80
	string utfpi;
	utfpi.push_back((char)0xCF); utfpi.push_back((char)0x80);
	size_t pos = 0;
	while ((pos = s.find(utfpi, pos)) != string::npos) {
		s.replace(pos, utfpi.size(), "pi");
		pos += 2;
	}
	// lower-case
	string low = s;
	transform(low.begin(), low.end(), low.begin(), ::tolower);
	s = low;
	// remove '*' characters
	s.erase(remove(s.begin(), s.end(), '*'), s.end());

	// If no "pi" present, try stod
	size_t ip = s.find("pi");
	if (ip == string::npos) {
		try {
			out_val = stod(s);
			return true;
		} catch(...) {
			return false;
		}
	}

	// split around "pi"
	string before = s.substr(0, ip);
	string after = s.substr(ip + 2);

	// parse numerator multiplier from before; it may be empty, "-", or a fraction like "3/2"
	double multiplier = 1.0;
	if (!before.empty()) {
		// remove whitespace
		before.erase(remove_if(before.begin(), before.end(), ::isspace), before.end());
		if (before == "+" || before == "") multiplier = 1.0;
		else if (before == "-") multiplier = -1.0;
		else {
			size_t slash = before.find('/');
			try {
				if (slash != string::npos) {
					double n = stod(before.substr(0, slash));
					double d = stod(before.substr(slash+1));
					multiplier = n / d;
				} else {
					multiplier = stod(before);
				}
			} catch(...) { return false; }
		}
	}

	// parse denominator from after, if present as "/den" (e.g., "pi/2")
	double denom = 1.0;
	if (!after.empty()) {
		// remove whitespace
		after.erase(remove_if(after.begin(), after.end(), ::isspace), after.end());
		if (after.size() > 0 && after[0] == '/') {
			string denstr = after.substr(1);
			try { denom = stod(denstr); } catch(...) { return false; }
		} else {
			// unexpected trailing text after pi
			try { denom = stod(after); if (denom != 0) denom = 1.0/denom; else return false; } catch(...) { return false; }
		}
	}

	out_val = multiplier * PI / denom;
	return true;
}

void display_hp_bar(int hp) {
	double ratio = double(hp) / double(MAX_HP);
	string color = COLOR_GREEN;
	if (ratio > 0.5) color = COLOR_GREEN;
	else if (ratio > 0.2) color = COLOR_YELLOW;
	else color = COLOR_RED;

	int sticks = (hp <= 0) ? 0 : ( (hp + STICK_HP - 1) / STICK_HP ); // ceil
	sticks = clamp_t(sticks, 0, TOTAL_STICKS);

	string filled(sticks, 'I');
	string empty(TOTAL_STICKS - sticks, '-');

	cout << "HP [" << color << filled << empty << COLOR_RESET << "] " << hp << " / " << MAX_HP << "\n";
}

// Small answer container that supports either a single real answer or a
// complex-style answer (real + imaginary / or real+angle for trig form).
struct QuestionAnswer {
	string q;
	bool complex; // true => answer is two values (real, imag) or (r, theta)
	double real;  // if complex==false this is the correct real answer
	double imag;  // if complex==true this is the second component
	// answerFormat: 0 = single value; 1 = pair x,y; 2 = pair magnitude,angle (r,theta in radians);
	// 3 = complex number (real + imag i)
	int answerFormat;
	QuestionAnswer(): q(""), complex(false), real(numeric_limits<double>::quiet_NaN()), imag(0.0) {}
};

// Generate a trig question. Returns a QuestionAnswer which may contain a real
// answer or a complex-style answer (two numbers).
QuestionAnswer generate_question(mt19937 &rng) {
	// Angles commonly used on the unit circle
	vector<int> angles = {0,30,45,60,90,120,135,150,180,210,225,240,270,300,315,330};

	// Question categories:
	// 0 = basic (sin/cos/tan), 1 = reciprocal identities (sec/csc/cot),
	// 2 = ratio identities (tan = sin/cos, cot = cos/sin),
	// 3 = even/odd identities, 4 = cofunction identities, 5 = Pythagorean identity,
	// 6 = sum/difference of angles, 7 = double-angle, 8 = power-reducing,
	// 9 = half-angle, 10 = product-to-sum, 11 = sum-to-product
	// 12 = Law of Sines (AAS/ASA/SSA/AAA), 13 = Law of Cosines (SSS/SAS), 14 = Area (SSS/SAS)
	uniform_int_distribution<int> catDist(0,21);
	int cat = catDist(rng);

	uniform_int_distribution<int> angleDist(0, (int)angles.size()-1);
	int idx = angleDist(rng);
	int ang = angles[idx];
	double rad = ang * PI / 180.0;

	string q;
	double correct = numeric_limits<double>::quiet_NaN();
	bool isComplexAnswer = false;
	double complex_real = 0.0, complex_imag = 0.0;
	int complex_format = 3; // default for two-value answers: 3 = complex number (real + imag i)

	if (cat == 0) {
		// basic trig
		uniform_int_distribution<int> funcDist(0,2); // sin, cos, tan
		int func = funcDist(rng);
		if (func == 0) {
			correct = sin(rad);
			q = string("Compute sin(") + rad_to_string(rad) + "). Give a decimal (e.g. 0.5): ";
		} else if (func == 1) {
			correct = cos(rad);
			q = string("Compute cos(") + rad_to_string(rad) + "). Give a decimal (e.g. 0.5): ";
		} else {
			// avoid tan at 90/270
			if (ang % 180 == 90) {
				ang = angles[(idx + 1) % angles.size()];
				rad = ang * PI / 180.0;
			}
			correct = tan(rad);
			q = string("Compute tan(") + rad_to_string(rad) + "). Give a decimal (or large value): ";
		}

	} else if (cat == 1) {
		// reciprocal identities: sec = 1/cos, csc = 1/sin, cot = 1/tan
		uniform_int_distribution<int> recipDist(0,2);
		int r = recipDist(rng);
		if (r == 0) {
			// sec
			// avoid cos = 0
			if (ang % 180 == 90) { ang = angles[(idx + 1) % angles.size()]; rad = ang * PI / 180.0; }
			double cosv = cos(rad);
			correct = 1.0 / cosv;
			q = string("Compute sec(") + rad_to_string(rad) + ") using the reciprocal identity sec(theta)=1/cos(theta). Give a decimal: ";
		} else if (r == 1) {
			// csc
			if (ang % 180 == 0) { ang = angles[(idx + 1) % angles.size()]; rad = ang * PI / 180.0; }
			double sinv = sin(rad);
			correct = 1.0 / sinv;
			q = string("Compute csc(") + rad_to_string(rad) + ") using csc(theta)=1/sin(theta). Give a decimal: ";
		} else {
			// cot = 1/tan ; avoid tan undefined
			if (ang % 180 == 90) { ang = angles[(idx + 1) % angles.size()]; rad = ang * PI / 180.0; }
			double tanv = tan(rad);
			correct = 1.0 / tanv;
			q = string("Compute cot(") + rad_to_string(rad) + ") using cot(theta)=1/tan(theta). Give a decimal: ";
		}

	} else if (cat == 2) {
		// ratio identities: tan = sin/cos, cot = cos/sin
		uniform_int_distribution<int> ratioDist(0,1);
		int r = ratioDist(rng);
		if (r == 0) {
			if (ang % 180 == 90) { ang = angles[(idx + 1) % angles.size()]; rad = ang * PI / 180.0; }
			double sinv = sin(rad), cosv = cos(rad);
			correct = sinv / cosv;
			q = string("Use the ratio identity tan(theta)=sin(theta)/cos(theta). Compute tan(") + rad_to_string(rad) + "): ";
		} else {
			if (ang % 180 == 0) { ang = angles[(idx + 1) % angles.size()]; rad = ang * PI / 180.0; }
			double sinv = sin(rad), cosv = cos(rad);
			correct = cosv / sinv;
			q = string("Use the ratio identity cot(theta)=cos(theta)/sin(theta). Compute cot(") + rad_to_string(rad) + "): ";
		}

	} else if (cat == 3) {
		// even/odd identities: sin(-θ) = -sin(θ), cos(-θ) = cos(θ), tan(-θ) = -tan(θ)
		uniform_int_distribution<int> fo(0,2); // 0 sin,1 cos,2 tan
		int f = fo(rng);
		int nang = -ang; // negative angle
		double nrad = nang * PI / 180.0;
		if (f == 0) {
			correct = sin(nrad); // should equal -sin(rad)
			q = string("Using even/odd identities, compute sin(") + rad_to_string(nrad) + ". Hint: sin(-theta) = -sin(theta). Give a decimal: ";
		} else if (f == 1) {
			correct = cos(nrad);
			q = string("Using even/odd identities, compute cos(") + rad_to_string(nrad) + ". Hint: cos(-theta) = cos(theta). Give a decimal: ";
		} else {
			// tan may be undefined
			if (ang % 180 == 90) { ang = angles[(idx + 1) % angles.size()]; nang = -ang; nrad = nang * PI / 180.0; }
			correct = tan(nrad);
			q = string("Using even/odd identities, compute tan(") + rad_to_string(nrad) + ". Hint: tan(-theta) = -tan(theta). Give a decimal: ";
		}

	} else if (cat == 4) {
		// cofunction identities: sin(theta) = cos(90 - theta), etc.
		uniform_int_distribution<int> cf(0,2); // 0 sin/cos,1 cos/sin,2 tan/cot
		int c = cf(rng);
		if (c == 0) {
			int theta = ang % 90; // pick smaller angle for clarity
			double thrad = theta * PI / 180.0;
			double val = sin(thrad);
			correct = val;
			q = string("Use the cofunction identity sin(theta)=cos(90-theta). Compute sin(") + rad_to_string(thrad) + string(") by relating it to cos(") + rad_to_string((90-theta)*PI/180.0) + ". Give a decimal: ";
		} else if (c == 1) {
			int theta = ang % 90;
			double thrad = theta * PI / 180.0;
			double val = cos(thrad);
			correct = val;
			q = string("Use the cofunction identity cos(theta)=sin(90-theta). Compute cos(") + rad_to_string(thrad) + string(") by relating it to sin(") + rad_to_string((90-theta)*PI/180.0) + ". Give a decimal: ";
		} else {
			int theta = ang % 90;
			double thrad = theta * PI / 180.0;
			double val = tan(thrad);
			correct = val;
			q = string("Use the cofunction identity tan(theta)=cot(90-theta). Compute tan(") + rad_to_string(thrad) + string(") by relating it to cot(") + rad_to_string((90-theta)*PI/180.0) + ". Give a decimal: ";
		}

	} else if (cat == 5) {
		// Pythagorean identity: sin^2 + cos^2 = 1. Given one value, compute the other.
		// Choose a rational-like sine value from a set that yields neat square roots.
		vector<pair<double,double>> triples = {
			{3.0/5.0, 4.0/5.0}, {4.0/5.0, 3.0/5.0}, {5.0/13.0, 12.0/13.0}, {12.0/13.0, 5.0/13.0}, {8.0/17.0, 15.0/17.0}
		};
		uniform_int_distribution<int> tdist(0, (int)triples.size()-1);
		int t = tdist(rng);
		double sinv = triples[t].first;
		double cosv = triples[t].second;
		// Randomly give sin and ask for cos, or vice versa. Also randomize sign of angle quadrant for challenge.
		uniform_int_distribution<int> which(0,1);
		int w = which(rng);
		// choose quadrant to possibly flip signs: 1 (both +), 2 (sin+,cos-), 3 (-,-), 4 (-,+)
		uniform_int_distribution<int> quad(1,4);
		int qd = quad(rng);
		double s = sinv, c = cosv;
		if (qd == 2) c = -c;
		else if (qd == 3) { s = -s; c = -c; }
		else if (qd == 4) s = -s;

		if (w == 0) {
			correct = c; // given sin -> compute cos
			// show the signed sin value (s) which may have been flipped by quadrant
			q = "Use the Pythagorean identity sin^2(theta)+cos^2(theta)=1. Given sin(theta)=" + to_string(s) + " and assuming theta is in quadrant " + to_string(qd) + ", compute cos(theta). Give a decimal: ";
		} else {
			correct = s; // given cos -> compute sin
			// show the signed cos value (c) which may have been flipped by quadrant
			q = "Use the Pythagorean identity sin^2(theta)+cos^2(theta)=1. Given cos(theta)=" + to_string(c) + " and assuming theta is in quadrant " + to_string(qd) + ", compute sin(theta). Give a decimal: ";
		}
	} else if (cat == 6) {
		// Sum/Difference of two angles
		uniform_int_distribution<int> op(0,1); // 0 sum, 1 diff
		uniform_int_distribution<int> fn(0,2); // sin, cos, tan
		int a = angles[angleDist(rng)];
		int b = angles[angleDist(rng)];
		int sign = op(rng);
		int f = fn(rng);
		double ra = a * PI / 180.0;
		double rb = b * PI / 180.0;
		if (f == 0) {
			if (sign == 0) { correct = sin(ra + rb); q = string("Use the sum/difference identity for sine. Compute sin(") + rad_to_string(ra) + " + " + rad_to_string(rb) + "): "; }
			else { correct = sin(ra - rb); q = string("Use the sum/difference identity for sine. Compute sin(") + rad_to_string(ra) + " - " + rad_to_string(rb) + "): "; }
		} else if (f == 1) {
			if (sign == 0) { correct = cos(ra + rb); q = string("Use the sum/difference identity for cosine. Compute cos(") + rad_to_string(ra) + " + " + rad_to_string(rb) + "): "; }
			else { correct = cos(ra - rb); q = string("Use the sum/difference identity for cosine. Compute cos(") + rad_to_string(ra) + " - " + rad_to_string(rb) + "): "; }
		} else {
			// tan sum/diff: avoid undefined
			if ( ( (a+b) % 180 == 90) || ((a-b) % 180 == 90) ) {
				// choose different b
				b = angles[(angleDist(rng)+1) % angles.size()]; rb = b * PI / 180.0;
			}
			if (sign == 0) correct = tan(ra + rb); else correct = tan(ra - rb);
			if (sign == 0) q = string("Use the sum/difference identity for tangent. Compute tan(") + rad_to_string(ra) + " + " + rad_to_string(rb) + "): ";
			else q = string("Use the sum/difference identity for tangent. Compute tan(") + rad_to_string(ra) + " - " + rad_to_string(rb) + "): ";
		}

	} else if (cat == 7) {
		// Double-angle identities
		uniform_int_distribution<int> fn(0,2); // sin2x, cos2x, tan2x
		int f = fn(rng);
		int a = angles[angleDist(rng)];
		double r = a * PI / 180.0;
		if (f == 0) { correct = sin(2.0 * r); q = string("Use the double-angle identity. Compute sin(2*") + rad_to_string(r) + "): "; }
		else if (f == 1) { correct = cos(2.0 * r); q = string("Use the double-angle identity. Compute cos(2*") + rad_to_string(r) + "): "; }
		else { if ( (2*a) % 180 == 90) { a = angles[(angleDist(rng)+1)%angles.size()]; r = a * PI / 180.0; } correct = tan(2.0 * r); q = string("Use the double-angle identity. Compute tan(2*") + rad_to_string(r) + "): "; }

	} else if (cat == 8) {
		// Power-reducing: sin^2 = (1-cos2x)/2, cos^2 = (1+cos2x)/2
		uniform_int_distribution<int> which(0,1); // 0 sin^2, 1 cos^2
		int w = which(rng);
		int a = angles[angleDist(rng)];
		double r = a * PI / 180.0;
		if (w == 0) { correct = pow(sin(r),2); q = string("Use the power-reducing identity sin^2(theta)=(1-cos(2theta))/2. Compute sin^2(") + rad_to_string(r) + "): "; }
		else { correct = pow(cos(r),2); q = string("Use the power-reducing identity cos^2(theta)=(1+cos(2theta))/2. Compute cos^2(") + rad_to_string(r) + "): "; }

	} else if (cat == 9) {
		// Half-angle: sin(theta/2), cos(theta/2). We include the quadrant of theta/2 so the student
		// knows the correct sign when applying the half-angle formula.
		uniform_int_distribution<int> which(0,1); // 0 sin(theta/2), 1 cos(theta/2)
		int w = which(rng);
		int a = angles[angleDist(rng)];
		double r = a * PI / 180.0;
		double half = a / 2.0;
		double hnorm = fmod(half, 360.0);
		if (hnorm < 0) hnorm += 360.0;
		double mod90 = fmod(hnorm, 90.0);
		bool onAxis = fabs(mod90) < 1e-9 || fabs(mod90 - 90.0) < 1e-9;
		string quadStr;
		if (onAxis) {
			// show axis angle in radians
			double hrad = hnorm * PI / 180.0;
			quadStr = string("on an axis (angle=") + rad_to_string(hrad,3) + ")";
		} else if (hnorm > 0.0 && hnorm < 90.0) quadStr = "1";
		else if (hnorm > 90.0 && hnorm < 180.0) quadStr = "2";
		else if (hnorm > 180.0 && hnorm < 270.0) quadStr = "3";
		else quadStr = "4";

		if (w == 0) {
			correct = sin(r / 2.0);
			double half_rad = (half * PI / 180.0);
			q = string("Use the half-angle identity to compute sin(") + rad_to_string(r/2.0) + "). Assume theta/2 = " + rad_to_string(half_rad) + " which is in quadrant " + quadStr + ". Compute sin(" + rad_to_string(r/2.0) + "): ";
		} else {
			correct = cos(r / 2.0);
			double half_rad = (half * PI / 180.0);
			q = string("Use the half-angle identity to compute cos(") + rad_to_string(r/2.0) + "). Assume theta/2 = " + rad_to_string(half_rad) + " which is in quadrant " + quadStr + ". Compute cos(" + rad_to_string(r/2.0) + "): ";
		}

	} else if (cat == 10) {
		// Product-to-sum: e.g., sin a sin b = 1/2[cos(a-b)-cos(a+b)] etc.
		uniform_int_distribution<int> which(0,2); // 0 sin*sin,1 sin*cos,2 cos*cos
		int w = which(rng);
		int a = angles[angleDist(rng)];
		int b = angles[angleDist(rng)];
		double ra = a * PI / 180.0, rb = b * PI / 180.0;
		if (w == 0) { correct = sin(ra) * sin(rb); q = string("Use the product-to-sum identity. Compute sin(") + rad_to_string(ra) + "*" + rad_to_string(rb) + ": "; }
		else if (w == 1) { correct = sin(ra) * cos(rb); q = string("Use the product-to-sum identity. Compute sin(") + rad_to_string(ra) + "*" + rad_to_string(rb) + ": "; }
		else { correct = cos(ra) * cos(rb); q = string("Use the product-to-sum identity. Compute cos(") + rad_to_string(ra) + "*" + rad_to_string(rb) + ": "; }

	} else if (cat == 11) {
		// Sum-to-product: e.g., sin a + sin b = 2 sin((a+b)/2) cos((a-b)/2)
		uniform_int_distribution<int> which(0,2); // 0 sin+sin,1 cos+cos,2 sin-sin maybe
		int w = which(rng);
		int a = angles[angleDist(rng)];
		int b = angles[angleDist(rng)];
		double ra = a * PI / 180.0, rb = b * PI / 180.0;
		if (w == 0) { correct = sin(ra) + sin(rb); q = string("Use the sum-to-product identity. Compute sin(") + rad_to_string(ra) + "+" + rad_to_string(rb) + ": "; }
		else if (w == 1) { correct = cos(ra) + cos(rb); q = string("Use the sum-to-product identity. Compute cos(") + rad_to_string(ra) + "+" + rad_to_string(rb) + ": "; }
		else { correct = sin(ra) - sin(rb); q = string("Use the sum-to-product identity. Compute sin(") + rad_to_string(ra) + "-" + rad_to_string(rb) + ": "; }
	}

	else if (cat == 17) {
		// VECTORS category: produce scalar or (magnitude, angle) questions
		// Subtypes:
		// 0 dot product (scalar)
		// 1 magnitude of v (scalar)
		// 2 P1P2 magnitude (scalar)
		// 3 angle between vectors (radians) (scalar)
		// 4 scalar projection of v onto w (scalar)
		// 5 work = F dot d (scalar)
		// 6 area of parallelogram = |v x w| (scalar)
		// 7 torque magnitude = |r x F| (scalar)
		// 8 2D magnitude and direction angle (real,angle) (complex-style two numbers)
		// 9 direction angle with x-axis for 3D vector (radians)
		uniform_int_distribution<int> subtype(0,9);
		int s = subtype(rng);
		uniform_int_distribution<int> comp(-5,5);
		if (s == 0) {
			// dot product of two 3D vectors
			int a = comp(rng), b = comp(rng), c = comp(rng);
			int d = comp(rng), e = comp(rng), f = comp(rng);
			correct = double(a*d + b*e + c*f);
			q = "Compute the dot product of v=<" + to_string(a) + "," + to_string(b) + "," + to_string(c) + "> and w=<" + to_string(d) + "," + to_string(e) + "," + to_string(f) + ">. Give a decimal: ";
		} else if (s == 1) {
			int a = comp(rng), b = comp(rng), c = comp(rng);
			double mag = sqrt(double(a)*a + double(b)*b + double(c)*c);
			correct = mag;
			q = "Given v=<" + to_string(a) + "," + to_string(b) + "," + to_string(c) + ">, compute ||v|| (magnitude). Give a decimal: ";
		} else if (s == 2) {
			// P1(x1,y1,z1), P2(x2,y2,z2)
			int x1 = comp(rng), y1 = comp(rng), z1 = comp(rng);
			int x2 = comp(rng)+2, y2 = comp(rng)+2, z2 = comp(rng)+2; // offset to vary
			double dx = double(x2 - x1), dy = double(y2 - y1), dz = double(z2 - z1);
			correct = sqrt(dx*dx + dy*dy + dz*dz);
			q = "Given P1=(" + to_string(x1) + "," + to_string(y1) + "," + to_string(z1) + ") and P2=(" + to_string(x2) + "," + to_string(y2) + "," + to_string(z2) + "), compute the distance ||P1P2||. Give a decimal: ";
		} else if (s == 3) {
			// angle between two nonzero vectors
			int a = comp(rng), b = comp(rng), c = comp(rng);
			int d = comp(rng), e = comp(rng), f = comp(rng);
			double dot = double(a*d + b*e + c*f);
			double mag1 = sqrt(double(a)*a + double(b)*b + double(c)*c);
			double mag2 = sqrt(double(d)*d + double(e)*e + double(f)*f);
			if (mag1 < 1e-9 || mag2 < 1e-9) {
				// fallback to nonzero vectors
				a = 1; b = 0; c = 0; d = 0; e = 1; f = 0;
				dot = 0; mag1 = 1; mag2 = 1;
			}
			double cosv = clamp_t(dot / (mag1 * mag2), -1.0, 1.0);
			// use radians
			correct = acos(cosv);
			q = string("Compute the angle (radians) between v=<") + to_string(a) + "," + to_string(b) + "," + to_string(c) + "> and w=<" + to_string(d) + "," + to_string(e) + "," + to_string(f) + ">. Give radians: ";
		} else if (s == 4) {
			// scalar projection of v onto w: (v·w)/||w||
			int a = comp(rng), b = comp(rng), c = comp(rng);
			int d = comp(rng), e = comp(rng), f = comp(rng);
			double dot = double(a*d + b*e + c*f);
			double magw = sqrt(double(d)*d + double(e)*e + double(f)*f);
			if (magw < 1e-9) { d = 1; e = 0; f = 0; magw = 1; }
			correct = dot / magw;
			q = "Compute the scalar projection of v onto w (comp_w v) where v=<" + to_string(a) + "," + to_string(b) + "," + to_string(c) + "> and w=<" + to_string(d) + "," + to_string(e) + "," + to_string(f) + ">. Give a decimal: ";
		} else if (s == 5) {
			// Work = F dot d
			int fx = comp(rng), fy = comp(rng), fz = comp(rng);
			int dx = comp(rng)+1, dy = comp(rng)+1, dz = comp(rng)+1;
			correct = double(fx*dx + fy*dy + fz*dz);
			q = "Compute the work done W = F·d where F=(" + to_string(fx) + "," + to_string(fy) + "," + to_string(fz) + ") and d=(" + to_string(dx) + "," + to_string(dy) + "," + to_string(dz) + "). Give a decimal: ";
		} else if (s == 6) {
			// area of parallelogram = |v x w|
			int a = comp(rng), b = comp(rng), c = comp(rng);
			int d = comp(rng), e = comp(rng), f = comp(rng);
			double cx = double(b*f - c*e);
			double cy = double(c*d - a*f);
			double cz = double(a*e - b*d);
			double area = sqrt(cx*cx + cy*cy + cz*cz);
			correct = area;
			q = "Compute the area of the parallelogram spanned by v=<" + to_string(a) + "," + to_string(b) + "," + to_string(c) + "> and w=<" + to_string(d) + "," + to_string(e) + "," + to_string(f) + "> (i.e., |v x w|). Give a decimal: ";
		} else if (s == 7) {
			// torque magnitude = |r x F|
			int rx = comp(rng), ry = comp(rng), rz = comp(rng);
			int fx = comp(rng), fy = comp(rng), fz = comp(rng);
			double cx = double(ry*fz - rz*fy);
			double cy = double(rz*fx - rx*fz);
			double cz = double(rx*fy - ry*fx);
			double torque = sqrt(cx*cx + cy*cy + cz*cz);
			correct = torque;
			q = "Compute the magnitude of torque |r x F| for r=(" + to_string(rx) + "," + to_string(ry) + "," + to_string(rz) + ") and F=(" + to_string(fx) + "," + to_string(fy) + "," + to_string(fz) + "). Give a decimal: ";
		} else if (s == 8) {
			// 2D magnitude and direction angle
			int a = comp(rng), b = comp(rng);
			while (a == 0 && b == 0) { a = comp(rng); b = comp(rng); }
			double mag = sqrt(double(a)*a + double(b)*b);
			double theta = atan2((double)b, (double)a); // radians
			isComplexAnswer = true; complex_real = mag; complex_imag = theta; complex_format = 2;
			q = string("Given v=(") + to_string(a) + "," + to_string(b) + "), compute its magnitude and direction angle (radians). Enter magnitude and angle separated by space: ";
		} else if (s == 9) {
			// direction angle with x-axis for 3D vector (use alpha = acos(a/||v||))
			int a = comp(rng), b = comp(rng), c = comp(rng);
			double mag = sqrt(double(a)*a + double(b)*b + double(c)*c);
			if (mag < 1e-9) { a = 1; mag = 1; }
			double cosalpha = clamp_t(double(a) / mag, -1.0, 1.0);
			correct = acos(cosalpha); // radians
			q = string("Given v=<") + to_string(a) + "," + to_string(b) + "," + to_string(c) + ">, compute its direction angle with the x-axis (radians). Give a decimal: ";
		}
	}
	else if (cat == 15) {
		// Polar graph identification question.
		// Types: Cardioid, Limacon Without Inner Loop, Limacon With Inner Loop, Lemniscate, Rose (n odd), Rose (n even)
		vector<string> types = {"Cardioid","Limacon without inner loop","Limacon with inner loop","Lemniscate","Rose (n=Odd)","Rose (n=Even)"};
		// Generate an actual equation and determine its type
		uniform_int_distribution<int> pick(0,5);
		int actual = pick(rng);
		string eq;
		if (actual == 0) {
			// Cardioid: r = a(1 + cos theta) or r = a(1 + sin theta)
			uniform_int_distribution<int> aDist(1,5);
			int a = aDist(rng);
			if (angleDist(rng) % 2 == 0) eq = "r = " + to_string(a) + "(1 + cos theta)";
			else eq = "r = " + to_string(a) + "(1 + sin theta)";
		} else if (actual == 1 || actual == 2) {
			// Limacon: r = a + b cos theta (or sin). Choose a,b such that inner loop or not.
			uniform_int_distribution<int> aDist(1,6);
			uniform_int_distribution<int> bDist(1,6);
			int a = aDist(rng);
			int b = bDist(rng);
			// force type: actual==1 -> without inner loop (a >= b), actual==2 -> with inner loop (a < b)
			if (actual == 1 && a < b) swap(a,b);
			if (actual == 2 && a >= b) { // ensure inner loop
				// make b larger
				b = a + 1;
			}
			if (angleDist(rng) % 2 == 0) eq = "r = " + to_string(a) + " + " + to_string(b) + " cos theta";
			else eq = "r = " + to_string(a) + " + " + to_string(b) + " sin theta";
		} else if (actual == 3) {
			// Lemniscate: r^2 = a^2 cos(2theta) or r^2 = a^2 sin(2theta)
			uniform_int_distribution<int> aDist(1,5);
			int a = aDist(rng);
			if (angleDist(rng) % 2 == 0) eq = "r^2 = " + to_string(a) + "^2 cos(2theta)";
			else eq = "r^2 = " + to_string(a) + "^2 sin(2theta)";
		} else {
			// Rose: r = a cos(n theta) or r = a sin(n theta)
			uniform_int_distribution<int> aDist(1,5);
			uniform_int_distribution<int> nDist(1,6);
			int a = aDist(rng);
			int n = nDist(rng);
			// For classification, if n odd -> Rose odd; if n even -> Rose even
			if (angleDist(rng) % 2 == 0) eq = "r = " + to_string(a) + " cos(" + to_string(n) + "theta)";
			else eq = "r = " + to_string(a) + " sin(" + to_string(n) + "theta)";
			// set actual to reflect odd/even
			if (n % 2 == 0) actual = 5; else actual = 4;
		}

		// Now pick a suggested type (50% chance correct)
		uniform_real_distribution<double> prob(0.0,1.0);
		int suggested;
		if (prob(rng) < 0.5) suggested = actual;
		else {
			// pick a different one
			do { suggested = pick(rng); } while (suggested == actual);
		}

		q = "Given the polar equation: " + eq + ", would this produce a '" + types[suggested] + "'? Enter 1 for yes, 0 for no.";
		correct = (suggested == actual) ? 1.0 : 0.0;
	}

	else if (cat == 12) {
		// Law of Sines: support solving for 'side', 'angle', or 'ratio' (AAA)
		cout << "[Law of Sines] Would you like to solve for 'side', 'angle', or 'ratio'? ";
		cout << flush;
		string choice;
		if (!getline(cin, choice)) choice = "side";
		for (auto &ch : choice) ch = tolower(ch);
		if (choice == "ratio") {
			// AAA: give two angles, ask for ratio a/b = sinA/sinB
			int A = angles[angleDist(rng)];
			int B = angles[angleDist(rng)];
			while (B == A) B = angles[angleDist(rng)];
			correct = sin(A * PI / 180.0) / sin(B * PI / 180.0);
			q = string("(Law of Sines, AAA) Given angles A=") + rad_to_string(A * PI / 180.0) + string(" and B=") + rad_to_string(B * PI / 180.0) + string(", compute the ratio a/b using the Law of Sines (a/b = sin A / sin B).");
		} else if (choice == "angle") {
			// Solve for angle B given a, A, and b: sin B = b * sin A / a
			int A = angles[angleDist(rng)];
			uniform_int_distribution<int> sideDist(5,25);
			int a_side = sideDist(rng);
			int b_side = sideDist(rng);
			double val = double(b_side) * sin(A * PI / 180.0) / double(a_side);
			// ensure |val|<=1; if not, adjust b_side
			while (fabs(val) > 1.0) {
				b_side = sideDist(rng);
				val = double(b_side) * sin(A * PI / 180.0) / double(a_side);
			}
			correct = asin(val); // radians
			q = string("(Law of Sines, angle) Given A=") + rad_to_string(A * PI / 180.0) + string(", a=") + to_string(a_side) + string(", and b=") + to_string(b_side) + string(", compute angle B (in radians) using the Law of Sines.");
		} else {
			// default: side (AAS/ASA/SSA): given angle A, side a, and angle B, find side b
			int A = angles[angleDist(rng)];
			int B = angles[angleDist(rng)];
			// ensure angles sum < 180
			while (A == B || A + B >= 180) {
				A = angles[angleDist(rng)];
				B = angles[angleDist(rng)];
			}
			uniform_int_distribution<int> sideDist(5,25);
			int a_side = sideDist(rng);
			// compute b using law of sines: b = a * sin(B) / sin(A)
			correct = double(a_side) * sin(B * PI / 180.0) / sin(A * PI / 180.0);
			q = string("(Law of Sines, side) Given A=") + rad_to_string(A * PI / 180.0) + string(", a=") + to_string(a_side) + string(", and B=") + rad_to_string(B * PI / 180.0) + string(", compute side b using the Law of Sines.");
		}

	} else if (cat == 13) {
		// Law of Cosines: ask whether to solve for 'angle' (SSS) or 'side' (SAS)
		cout << "[Law of Cosines] Would you like to solve for 'angle' or 'side'? "; cout << flush;
		string choice;
		if (!getline(cin, choice)) choice = "angle";
		for (auto &ch : choice) ch = tolower(ch);
		if (choice == "angle") {
			// SSS: generate valid sides
			int a_side, b_side, c_side;
			uniform_int_distribution<int> sideDist(5,25);
			do {
				a_side = sideDist(rng);
				b_side = sideDist(rng);
				c_side = sideDist(rng);
			} while (!(a_side + b_side > c_side && a_side + c_side > b_side && b_side + c_side > a_side));
			// find angle A opposite a: cos A = (b^2 + c^2 - a^2)/(2bc)
			double num = double(b_side)*b_side + double(c_side)*c_side - double(a_side)*a_side;
			double den = 2.0 * b_side * c_side;
			double cosA = clamp_t(num/den, -1.0, 1.0);
			correct = acos(cosA); // radians
			q = string("(Law of Cosines, SSS) Given sides a=") + to_string(a_side) + string(", b=") + to_string(b_side) + string(", c=") + to_string(c_side) + string(", compute angle A (opposite side a) in radians using the Law of Cosines.");
		} else {
			// SAS: given b,c and included angle A, compute side a
			uniform_int_distribution<int> sideDist(5,25);
			int b_side = sideDist(rng);
			int c_side = sideDist(rng);
			int A = angles[angleDist(rng)];
			double rad = A * PI / 180.0;
			double a_val = sqrt(double(b_side)*b_side + double(c_side)*c_side - 2.0*b_side*c_side*cos(rad));
			correct = a_val;
			q = string("(Law of Cosines, SAS) Given sides b=") + to_string(b_side) + string(", c=") + to_string(c_side) + string(", and included angle A=") + rad_to_string(rad) + string(", compute side a using the Law of Cosines.");
		}

	} else if (cat == 14) {
		// Area of a triangle: ask if user wants SSS (Heron) or SAS (0.5*b*c*sin A)
		cout << "[Area] Compute area from 'SSS' or 'SAS'? "; cout << flush;
		string choice;
		if (!getline(cin, choice)) choice = "SSS";
		for (auto &ch : choice) ch = tolower(ch);
		if (choice.rfind("sss",0) == 0) {
			int a_side, b_side, c_side;
			uniform_int_distribution<int> sideDist(5,25);
			do {
				a_side = sideDist(rng);
				b_side = sideDist(rng);
				c_side = sideDist(rng);
			} while (!(a_side + b_side > c_side && a_side + c_side > b_side && b_side + c_side > a_side));
			double s = (a_side + b_side + c_side) / 2.0;
			correct = sqrt(max(0.0, s*(s-a_side)*(s-b_side)*(s-c_side)));
			q = "(Area, SSS) Given sides a=" + to_string(a_side) + ", b=" + to_string(b_side) + ", c=" + to_string(c_side) + ", compute the area using Heron's formula.";
		} else {
			uniform_int_distribution<int> sideDist(5,25);
			int b_side = sideDist(rng);
			int c_side = sideDist(rng);
			int A = angles[angleDist(rng)];
			correct = 0.5 * b_side * c_side * sin(A * PI / 180.0);
			q = string("(Area, SAS) Given sides b=") + to_string(b_side) + string(", c=") + to_string(c_side) + string(", and included angle A=") + rad_to_string(A * PI / 180.0) + string(", compute the area (0.5*b*c*sin A).");
		}

	}

	else if (cat == 16) {
		// Complex numbers: several subtypes
		// 0 = magnitude (real), 1 = product, 2 = quotient, 3 = power, 4 = principal sqrt, 5 = trig form (r, theta)
		uniform_int_distribution<int> ctype(0,5);
		int t = ctype(rng);
		uniform_int_distribution<int> small(-5,5);
		if (t == 0) {
			int a = small(rng), b = small(rng);
			while (a == 0 && b == 0) { a = small(rng); b = small(rng); }
			correct = sqrt(double(a*a + b*b));
			q = "Compute the magnitude |z| of z = " + to_string(a) + " + " + to_string(b) + "i. Give a decimal: ";
		} else if (t == 1) {
			int a = small(rng), b = small(rng), c = small(rng), d = small(rng);
			// product: (a+bi)*(c+di) = (ac-bd) + (ad+bc)i
			complex_real = double(a*c - b*d);
			complex_imag = double(a*d + b*c);
			isComplexAnswer = true; complex_format = 3;
			q = "Compute the product z1*z2 where z1 = " + to_string(a) + " + " + to_string(b) + "i and z2 = " + to_string(c) + " + " + to_string(d) + "i. Enter real and imaginary parts separated by space: ";
		} else if (t == 2) {
			int a = small(rng), b = small(rng), c = small(rng), d = small(rng);
			// ensure denominator not zero
			while (c == 0 && d == 0) { c = small(rng); d = small(rng); }
			// division: (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c^2 + d^2)
			double den = double(c*c + d*d);
			complex_real = double(a*c + b*d) / den;
			complex_imag = double(b*c - a*d) / den;
			isComplexAnswer = true; complex_format = 3;
			q = "Compute the quotient z1/z2 where z1 = " + to_string(a) + " + " + to_string(b) + "i and z2 = " + to_string(c) + " + " + to_string(d) + "i. Enter real and imaginary parts separated by space: ";
		} else if (t == 3) {
			int a = small(rng), b = small(rng);
			int n = uniform_int_distribution<int>(2,3)(rng);
			// compute (a+bi)^n
			double xr = a, xi = b;
			double rr = 1.0, ri = 0.0;
			for (int i = 0; i < n; ++i) {
				double nr = rr * xr - ri * xi;
				double ni = rr * xi + ri * xr;
				rr = nr; ri = ni;
			}
			complex_real = rr; complex_imag = ri; isComplexAnswer = true; complex_format = 3;
			q = "Compute (a+bi)^n for z = " + to_string(a) + " + " + to_string(b) + "i with n=" + to_string(n) + ". Enter real and imaginary parts separated by space: ";
		} else if (t == 4) {
			int a = small(rng), b = small(rng);
			// principal square root of z. Use formula
			double x = double(a), y = double(b);
			double mag = sqrt(x*x + y*y);
			double real_part = sqrt((mag + x) / 2.0);
			double imag_part = 0.0;
			if (y >= 0) imag_part = sqrt((mag - x) / 2.0);
			else imag_part = -sqrt((mag - x) / 2.0);
			complex_real = real_part; complex_imag = imag_part; isComplexAnswer = true; complex_format = 3;
			q = "Compute the principal square root of z = " + to_string(a) + " + " + to_string(b) + "i. Enter real and imaginary parts separated by space: ";
		} else {
			// trig form: modulus and argument (radians)
			int a = small(rng), b = small(rng);
			while (a == 0 && b == 0) { a = small(rng); b = small(rng); }
			double r = sqrt(double(a*a + b*b));
			double theta = atan2((double)b, (double)a); // radians
			complex_real = r; complex_imag = theta; isComplexAnswer = true; complex_format = 2;
			q = string("Compute modulus and argument (radians) of z = ") + to_string(a) + " + " + to_string(b) + "i. Enter modulus and argument separated by space: ";
		}
	}

		else if (cat == 18) {
			// Conic sections: Parabolas, Ellipses, Hyperbolas (horizontal & vertical)
			vector<string> types = {"Parabola (vertical)", "Parabola (horizontal)", "Ellipse (vertical major)", "Ellipse (horizontal major)", "Hyperbola (vertical transverse)", "Hyperbola (horizontal transverse)"};
			uniform_int_distribution<int> pick(0, (int)types.size()-1);
			int actual = pick(rng);
			string eq;
			if (actual == 0) {
				// vertical parabola: y = a(x-h)^2 + k
				uniform_int_distribution<int> aDist(1,4);
				int a = aDist(rng);
				int h = uniform_int_distribution<int>(-3,3)(rng);
				int k = uniform_int_distribution<int>(-3,3)(rng);
				eq = "y = " + to_string(a) + "(x - " + to_string(h) + ")^2 + " + to_string(k);
			} else if (actual == 1) {
				// horizontal parabola: x = a(y-k)^2 + h
				uniform_int_distribution<int> aDist(1,4);
				int a = aDist(rng);
				int h = uniform_int_distribution<int>(-3,3)(rng);
				int k = uniform_int_distribution<int>(-3,3)(rng);
				eq = "x = " + to_string(a) + "(y - " + to_string(k) + ")^2 + " + to_string(h);
			} else if (actual == 2 || actual == 3) {
				// ellipse: (x-h)^2/a^2 + (y-k)^2/b^2 = 1 ; decide orientation by a vs b
				int a = uniform_int_distribution<int>(2,6)(rng);
				int b = uniform_int_distribution<int>(2,6)(rng);
				int h = uniform_int_distribution<int>(-3,3)(rng);
				int k = uniform_int_distribution<int>(-3,3)(rng);
				// force actual to match a vs b
				if (a >= b) actual = 3; else actual = 2; // 3 -> horizontal major (a along x), 2 -> vertical major
				eq = "(x - " + to_string(h) + ")^2/" + to_string(a*a) + " + (y - " + to_string(k) + ")^2/" + to_string(b*b) + " = 1";
			} else {
				// hyperbola: either (x-h)^2/a^2 - (y-k)^2/b^2 = 1 (horizontal) or the reverse (vertical)
				int a = uniform_int_distribution<int>(2,6)(rng);
				int b = uniform_int_distribution<int>(2,6)(rng);
				int h = uniform_int_distribution<int>(-3,3)(rng);
				int k = uniform_int_distribution<int>(-3,3)(rng);
				// pick actual to be either vertical transverse (4) or horizontal transverse (5)
				int pickHV = uniform_int_distribution<int>(0,1)(rng);
				if (pickHV == 0) {
					actual = 4; // vertical transverse: (y-k)^2/a^2 - (x-h)^2/b^2 = 1
					eq = "(y - " + to_string(k) + ")^2/" + to_string(a*a) + " - (x - " + to_string(h) + ")^2/" + to_string(b*b) + " = 1";
				} else {
					actual = 5; // horizontal transverse
					eq = "(x - " + to_string(h) + ")^2/" + to_string(a*a) + " - (y - " + to_string(k) + ")^2/" + to_string(b*b) + " = 1";
				}
			}

			// Suggest a type (50% correct)
			uniform_real_distribution<double> prob(0.0,1.0);
			int suggested;
			if (prob(rng) < 0.5) suggested = actual;
			else {
				do { suggested = pick(rng); } while (suggested == actual);
			}

			q = "Given the conic equation: " + eq + ", would this be a '" + types[suggested] + "'? Enter 1 for yes, 0 for no.";
			correct = (suggested == actual) ? 1.0 : 0.0;
		}

		else if (cat == 19) {
			// New: Asymptotes (hyperbola) and Polar equations of conics (identify by eccentricity e)
			uniform_int_distribution<int> modeDist(0,1);
			int mode = modeDist(rng);
			if (mode == 0) {
				// Asymptote slope question for a hyperbola
				// Choose horizontal or vertical transverse
				uniform_int_distribution<int> hv(0,1);
				int which = hv(rng); // 0 horizontal transverse, 1 vertical transverse
				int h = uniform_int_distribution<int>(-3,3)(rng);
				int k = uniform_int_distribution<int>(-3,3)(rng);
				int a = uniform_int_distribution<int>(2,6)(rng);
				int b = uniform_int_distribution<int>(2,6)(rng);
				double m = 0.0;
				string orient;
				if (which == 0) {
					// (x-h)^2/a^2 - (y-k)^2/b^2 = 1 -> asymptotes y-k = +/- (b/a)(x-h)
					m = double(b) / double(a);
					orient = "(x - " + to_string(h) + ")^2/" + to_string(a*a) + " - (y - " + to_string(k) + ")^2/" + to_string(b*b) + " = 1";
				} else {
					// (y-k)^2/a^2 - (x-h)^2/b^2 = 1 -> asymptotes y-k = +/- (a/b)(x-h)
					m = double(a) / double(b);
					orient = "(y - " + to_string(k) + ")^2/" + to_string(a*a) + " - (x - " + to_string(h) + ")^2/" + to_string(b*b) + " = 1";
				}
				// Ask user for the positive slope magnitude m (since asymptotes are +/- m)
				q = "Given the hyperbola: " + orient + ", its asymptotes are y - " + to_string(k) + " = +/- m (x - " + to_string(h) + "). Enter the positive slope m (decimal): ";
				correct = m;
			} else {
				// Polar equation identification by eccentricity e
				// Use forms r = ed / (1 + e cos θ) or r = ed / (1 + e sin θ)
				uniform_real_distribution<double> eDist(0.5,1.8);
				double e = 0.0;
				// pick some representative eccentricities including exactly 1
				uniform_int_distribution<int> pickE(0,4);
				int pe = pickE(rng);
				if (pe == 0) e = 1.0; // parabola
				else if (pe == 1) e = 0.5; // ellipse
				else if (pe == 2) e = 0.8; // ellipse
				else if (pe == 3) e = 1.2; // hyperbola
				else e = 1.5; // hyperbola
				int d = uniform_int_distribution<int>(1,5)(rng);
				string form;
				if (angleDist(rng) % 2 == 0) form = "cos"; else form = "sin";
				q = "Given the polar equation r = (" + to_string(e*d) + ")/(1 + " + to_string(e) + " " + form + " theta), identify the conic by entering 0 for ellipse, 1 for parabola, 2 for hyperbola: ";
				int corr;
				if (fabs(e - 1.0) < 1e-9) corr = 1;
				else if (e < 1.0) corr = 0;
				else corr = 2;
				correct = double(corr);
			}
		}
		else if (cat == 20) {
			// Projectile motion questions (safe numeric values)
			// g = 9.8 m/s^2
			const double g = 9.8;
			// subtypes: 0 range, 1 max height, 2 time of flight, 3 horizontal displacement at time t
			uniform_int_distribution<int> ptype(0,3);
			int p = ptype(rng);
			// choose initial speed (m/s) and launch angle (radians displayed)
			uniform_int_distribution<int> v0dist(10,30);
			vector<int> angleChoices = {15,20,25,30,35,40,45,50,55,60};
			uniform_int_distribution<int> angIndex(0, (int)angleChoices.size()-1);
			int v0 = v0dist(rng);
			int th = angleChoices[angIndex(rng)];
			double thrad = th * PI / 180.0;
			if (p == 0) {
				// Range on level ground: R = v0^2 * sin(2θ) / g
				double R = double(v0) * v0 * sin(2.0 * thrad) / g;
				correct = R;
				q = string("A projectile is launched at v0=") + to_string(v0) + " m/s at " + rad_to_string(thrad) + " from horizontal. Compute its range R on level ground (m). Give a decimal: ";
			} else if (p == 1) {
				// Max height: H = v0^2 * sin^2 θ / (2g)
				double H = double(v0) * v0 * pow(sin(thrad),2) / (2.0 * g);
				correct = H;
				q = string("A projectile is launched at v0=") + to_string(v0) + " m/s at " + rad_to_string(thrad) + ". Compute its maximum height H (m). Give a decimal: ";
			} else if (p == 2) {
				// Time of flight: T = 2 v0 sin θ / g
				double T = 2.0 * double(v0) * sin(thrad) / g;
				correct = T;
				q = string("A projectile is launched at v0=") + to_string(v0) + " m/s at " + rad_to_string(thrad) + ". Compute its time of flight T until it lands (s). Give a decimal: ";
			} else {
				// horizontal displacement at time t: x = v0 cos θ * t. choose t half of time of flight to be safe
				double T = 2.0 * double(v0) * sin(thrad) / g;
				double t = max(0.1, T / 2.0);
				double x = double(v0) * cos(thrad) * t;
				correct = x;
				q = string("A projectile is launched at v0=") + to_string(v0) + " m/s at " + rad_to_string(thrad) + ". Compute its horizontal displacement x at t=" + to_string(round(t*100.0)/100.0) + " s. Give a decimal: ";
			}
		}
		else if (cat == 21) {
			// Cycloid questions
			// Cycloid generated by a circle radius r: x = r(t - sin t), y = r(1 - cos t)
			// One arch corresponds t in [0, 2pi]
			// Arc length of one arch = 8r
			// Area under one arch = 3pi r^2
			uniform_int_distribution<int> ctype(0,2);
			int c = ctype(rng);
			uniform_int_distribution<int> rdist(1,5);
			int r = rdist(rng);
			if (c == 0) {
				// arc length of one arch
				correct = 8.0 * double(r);
				q = "For a cycloid generated by a circle of radius r=" + to_string(r) + ", compute the arc length of one arch. Give a decimal: ";
			} else if (c == 1) {
				// area under one arch
				correct = 3.0 * PI * double(r) * double(r);
				q = "For a cycloid generated by a circle of radius r=" + to_string(r) + ", compute the area under one arch. Give a decimal: ";
			} else {
				// coordinates at a nice parameter t (use t = pi/2 or pi)
				vector<double> tvals = {PI/2.0, PI, 3.0*PI/2.0};
				uniform_int_distribution<int> tidx(0, (int)tvals.size()-1);
				double tv = tvals[tidx(rng)];
				double xv = double(r) * (tv - sin(tv));
				double yv = double(r) * (1.0 - cos(tv));
				isComplexAnswer = true; complex_real = xv; complex_imag = yv; complex_format = 1;
				// show tv nicely as a pi-fraction (e.g. "3π/2") when possible and also show decimal approx
				string tv_str = rad_to_string(tv, 3);
				// rad_to_string appends " rad" for decimal outputs; strip it for a compact expression
				if (tv_str.size() >= 4 && tv_str.substr(tv_str.size()-4) == " rad") tv_str = tv_str.substr(0, tv_str.size()-4);
				double tv_dec = round(tv * 100.0) / 100.0;
				q = "For cycloid with r=" + to_string(r) + ", compute x and y at t=" + tv_str + " (≈ " + to_string(tv_dec) + ") (enter x and y separated by space): ";
			}
		}

	// Ask users to round to nearest hundredth for all questions. Include colon+space
	// so the user's input appears after the instruction instead of concatenated.
	q += " Round your answer to the nearest hundredth: ";

	QuestionAnswer out;
	out.q = q;
	if (isComplexAnswer) {
		out.complex = true;
		out.real = complex_real;
		out.imag = complex_imag;
		// default to complex format (real + imag i); specific generators will override
		out.answerFormat = complex_format;
	} else {
		out.complex = false;
		out.real = correct;
		out.imag = 0.0;
	}
	return out;
}

// Format a correct answer for sample/non-interactive output.
string format_correct_answer(const QuestionAnswer &out) {
	ostringstream oss;
	oss << fixed << setprecision(2);
	if (!out.complex) {
		double corr = round(out.real * 100.0) / 100.0;
		oss << corr;
		return oss.str();
	}
	// two-value answers
	if (out.answerFormat == 1) {
		double x = round(out.real * 100.0) / 100.0;
		double y = round(out.imag * 100.0) / 100.0;
		oss << x << " " << y;
		return oss.str();
	} else if (out.answerFormat == 2) {
		double r = round(out.real * 100.0) / 100.0;
		string ang = rad_to_string(out.imag, 3);
		if (ang.size() >= 4 && ang.substr(ang.size()-4) == " rad") ang = ang.substr(0, ang.size()-4);
		double angd = round(out.imag * 100.0) / 100.0;
		oss << "r=" << r << " theta=" << ang << " (" << angd << ")";
		return oss.str();
	} else {
		// complex a + bi
		double a = round(out.real * 100.0) / 100.0;
		double b = round(out.imag * 100.0) / 100.0;
		oss << a << " + " << b << "i";
		return oss.str();
	}
}

int main() {
	ios::sync_with_stdio(false);
	cin.tie(nullptr);

	// Allow non-interactive sample mode for CI/testing: --sample [seed] [count]
	// Example: ./math_rpg --sample 12345 10
	vector<string> args;
	for (int i = 1; i < __argc; ++i) args.push_back(string(__argv[i]));
	if (!args.empty() && (args[0] == "--sample" || args[0] == "--seed")) {
		unsigned seed = 12345u;
		int count = 10;
		if (args.size() >= 2) seed = (unsigned)stoi(args[1]);
		if (args.size() >= 3) count = stoi(args[2]);
		mt19937 rng(seed);
		for (int i = 0; i < count; ++i) {
			QuestionAnswer out = generate_question(rng);
			cout << out.q << "\n";
			cout << "Correct: " << format_correct_answer(out) << "\n\n";
		}
		return 0;
	}
	cout << "Welcome to Math RPG (Trigonometry)\n";
	cout << "Answer trig problems. Wrong answers cost HP. Type 'quit' to exit.\n\n";

	int hp = MAX_HP;

	// Random engine
	mt19937 rng((unsigned)chrono::high_resolution_clock::now().time_since_epoch().count());
	uniform_real_distribution<double> multDist(0.5, 2.0);

	int questionCount = 0;

	while (hp > 0) {
		display_hp_bar(hp);

		QuestionAnswer out = generate_question(rng);
	cout << out.q << flush;

		string line;
		if (!getline(cin, line)) break; // EOF
		if (line.size() == 0) { cout << "Please enter an answer (or 'quit').\n"; continue; }
		// allow exit
		string low;
		low.resize(line.size());
		transform(line.begin(), line.end(), low.begin(), ::tolower);
		if (low == "quit" || low == "exit") break;

		// parse response: either a single decimal (real answer) or two numbers
		// (for complex-style answers we expect two values separated by space: real imag OR r theta).
		double ans = numeric_limits<double>::quiet_NaN();
		double ans2 = numeric_limits<double>::quiet_NaN();
		if (!out.complex) {
			if (!parse_number_with_pi(line, ans)) {
				cout << "Couldn't parse numeric answer. Please enter a decimal or a radian expression using 'pi' (e.g. pi/4, 3pi/2).\n";
				continue;
			}
		} else {
			// try to parse two numbers from the line
			istringstream iss(line);
			string t1, t2;
			if (!(iss >> t1 >> t2)) {
				cout << "Couldn't parse two values. For this question enter two numbers separated by space (e.g. 1.23 pi/4).\n";
				continue;
			}
			if (!parse_number_with_pi(t1, ans) || !parse_number_with_pi(t2, ans2)) {
				cout << "Couldn't parse numeric values. Use decimals or radian expressions with 'pi' (e.g. 1.23 3pi/2).\n";
				continue;
			}
		}

		questionCount++;

		// check answer: accept if user's answer rounded to 2 decimals equals
		// the correct value rounded to 2 decimals (nearest hundredth). For
		// complex-style answers we compare both components separately.
		bool ok = false;
		if (!out.complex) {
			double correct = out.real;
			if (isfinite(correct)) {
				double user_r = round(ans * 100.0) / 100.0;
				double corr_r = round(correct * 100.0) / 100.0;
				if (fabs(user_r - corr_r) <= 1e-9) {
					ok = true;
				} else {
					double tol = 1e-3;
					if (fabs(ans - correct) <= tol) ok = true;
					else if (fabs(correct) > 1e-6 && fabs((ans - correct) / correct) <= 1e-3) ok = true;
				}
			} else {
				ok = false;
			}
		} else {
			// complex-style: compare both components
			double corr_r = out.real;
			double corr_i = out.imag;
			double user_r = round(ans * 100.0) / 100.0;
			double user_i = round(ans2 * 100.0) / 100.0;
			double corr_r_r = round(corr_r * 100.0) / 100.0;
			double corr_i_r = round(corr_i * 100.0) / 100.0;
			if (fabs(user_r - corr_r_r) <= 1e-9 && fabs(user_i - corr_i_r) <= 1e-9) ok = true;
			else {
				// small tolerance fallback for each component
				if (fabs(ans - corr_r) <= 1e-3 && fabs(ans2 - corr_i) <= 1e-3) ok = true;
			}
		}

		if (ok) {
			cout << "Correct!\n\n";
		} else {
			double mult = multDist(rng);
			int damage = (int)round(1400.0 * mult);
			damage = clamp_t(damage, 700, 2800);
			hp -= damage;
			if (hp < 0) hp = 0;
			// show the correct value rounded to nearest hundredth
			if (!out.complex) {
				double corr_r = round(out.real * 100.0) / 100.0;
				cout << "Incorrect. The correct value is approx " << fixed << setprecision(2) << corr_r << ".\n" << defaultfloat;
			} else {
				// Friendly display depending on answer format
				if (out.answerFormat == 1) {
					// x, y pair
					double corr_x = round(out.real * 100.0) / 100.0;
					double corr_y = round(out.imag * 100.0) / 100.0;
					cout << "Incorrect. The correct answer is approx x=" << fixed << setprecision(2) << corr_x << " y=" << corr_y << ".\n" << defaultfloat;
				} else if (out.answerFormat == 2) {
					// magnitude and angle (show angle as pi-fraction when possible and decimal approx)
					double corr_r = round(out.real * 100.0) / 100.0;
					string ang_str = rad_to_string(out.imag, 3);
					if (ang_str.size() >= 4 && ang_str.substr(ang_str.size()-4) == " rad") ang_str = ang_str.substr(0, ang_str.size()-4);
					double ang_dec = round(out.imag * 100.0) / 100.0;
					cout << "Incorrect. The correct answer is approx r=" << fixed << setprecision(2) << corr_r << " theta=" << ang_str << " (≈ " << ang_dec << ")\n" << defaultfloat;
				} else {
					// default: complex number a + bi
					double corr_r = round(out.real * 100.0) / 100.0;
					double corr_i = round(out.imag * 100.0) / 100.0;
					cout << "Incorrect. The correct answer is approx " << fixed << setprecision(2) << corr_r << " + " << corr_i << "i.\n" << defaultfloat;
				}
			}
			cout << "You take " << damage << " damage (multiplier: " << fixed << setprecision(2) << mult << ")\n\n";
		}

		if (hp <= 0) {
			display_hp_bar(hp);
			cout << "You have been defeated after " << questionCount << " questions.\n";
			break;
		}
	}

	if (hp > 0) {
		cout << "Exiting. Final HP: " << hp << " / " << MAX_HP << "\n";
	} else {
		cout << "Game over.\n";
	}
	return 0;
}

