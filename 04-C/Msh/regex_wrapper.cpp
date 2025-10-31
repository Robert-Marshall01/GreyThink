/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * C++ regex wrapper used by C code in Msh. See LICENSES.md for full license and attribution.
 */

#include "regex_wrapper.h"
#include <regex>
#include <string>
#include <cstring>
#include <cstdlib>

using namespace std;

extern "C" {

int c_regex_valid(const char *pattern) {
    if (!pattern) return 0;
    try {
        std::regex r(pattern, std::regex::extended);
        (void)r;
        return 1;
    } catch (std::regex_error &) {
        return 0;
    }
}

int c_regex_match(const char *pattern, const char *text) {
    if (!pattern || !text) return 0;
    try {
        std::regex r(pattern, std::regex::extended);
        return std::regex_search(std::string(text), r) ? 1 : 0;
    } catch (std::regex_error &) {
        return -1;
    }
}

char *c_regex_replace(const char *pattern, const char *text, const char *repl, int global, int *changed_out) {
    if (!pattern || !text || !repl) {
        if (changed_out) *changed_out = 0;
        return NULL;
    }
    try {
        std::regex r(pattern, std::regex::extended);
        std::string s(text);
        std::string replacement(repl);
        std::string out;
        if (global) {
            out = std::regex_replace(s, r, replacement);
            if (changed_out) *changed_out = (out != s) ? 1 : 0;
        } else {
            std::smatch m;
            if (std::regex_search(s, m, r)) {
                out = s.substr(0, m.position()) + replacement + s.substr(m.position() + m.length());
                if (changed_out) *changed_out = 1;
            } else {
                out = s;
                if (changed_out) *changed_out = 0;
            }
        }
        char *res = (char*)malloc(out.size() + 1);
        if (!res) return NULL;
        memcpy(res, out.c_str(), out.size() + 1);
        return res;
    } catch (std::regex_error &) {
        if (changed_out) *changed_out = 0;
        return NULL;
    }
}

} // extern C
