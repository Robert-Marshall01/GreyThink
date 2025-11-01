// Shared library: password scoring utilities used by both CLI and GUI

pub fn mask_password(pw: &str) -> String {
    let len = pw.chars().count();
    if len <= 2 {
        "*".repeat(len)
    } else {
        let first = pw.chars().next().unwrap();
        let last = pw.chars().last().unwrap();
        format!("{}{}{}", first, "*".repeat(len - 2), last)
    }
}

pub fn score_password(pw: &str) -> (i32, Vec<String>) {
    let mut score: i32 = 0;
    let mut suggestions = Vec::new();

    let len = pw.chars().count();

    // Common passwords quick check
    let lowered = pw.to_lowercase();
    let common = ["password", "123456", "123456789", "qwerty", "letmein", "admin", "welcome"];
    if common.iter().any(|c| lowered.contains(c)) {
        return (0, vec!["Avoid common passwords or sequences (e.g. '123456', 'password').".into()]);
    }

    // Length scoring
    if len >= 16 {
        score += 3;
    } else if len >= 12 {
        score += 2;
    } else if len >= 8 {
        score += 1;
    } else {
        suggestions.push("Make it longer (at least 12 characters).".into());
    }

    // Character variety
    let has_lower = pw.chars().any(|c| c.is_lowercase());
    let has_upper = pw.chars().any(|c| c.is_uppercase());
    let has_digit = pw.chars().any(|c| c.is_ascii_digit());
    let has_symbol = pw.chars().any(|c| !c.is_alphanumeric());

    if has_lower { score += 1; } else { suggestions.push("Add lowercase letters.".into()); }
    if has_upper { score += 1; } else { suggestions.push("Add uppercase letters.".into()); }
    if has_digit { score += 1; } else { suggestions.push("Add digits.".into()); }
    if has_symbol { score += 1; } else { suggestions.push("Add symbols (e.g. !@#$%).".into()); }

    // Penalize very short repeats
    if len < 6 {
        score = score.saturating_sub(2);
    }

    suggestions.dedup();
    (score, suggestions)
}

pub fn classify_score(score: i32) -> &'static str {
    match score {
        i if i <= 0 => "Very Weak",
        1 | 2 => "Weak",
        3 | 4 | 5 => "Medium",
        6 => "Strong",
        _ => "Very Strong",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_common_password() {
        let (score, sugg) = score_password("password123");
        assert_eq!(score, 0);
        assert!(sugg.iter().any(|s| s.contains("common")));
    }

    #[test]
    fn test_strong_password() {
        let (score, sugg) = score_password("S3cure!VeryLongKey2025");
        assert!(score >= 7);
        assert!(sugg.is_empty());
    }

    #[test]
    fn test_masking() {
        assert_eq!(mask_password("ab"), "**");
        assert_eq!(mask_password("abc"), "a*c");
        assert_eq!(mask_password("password"), "p******d");
    }
}
